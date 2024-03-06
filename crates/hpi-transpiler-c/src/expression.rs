use crate::{gc::Scope, Transpiler};

use std::collections::{HashMap, VecDeque};

use hpi_analyzer::{ast::*, AssignOp, InfixOp, Type};

use crate::c_ast::*;

impl<'src> Transpiler<'src> {
    pub fn expression(
        &mut self,
        node: AnalyzedExpression<'src>,
    ) -> (Vec<Statement>, Option<Expression>) {
        let expr = match node {
            AnalyzedExpression::Block(node) => return self.block_expr(*node, true),
            AnalyzedExpression::If(node) => return self.if_expr(*node),
            AnalyzedExpression::Int(value) => Some(Expression::Int(value)),
            AnalyzedExpression::Float(value) => Some(Expression::Float(value)),
            AnalyzedExpression::Bool(value) => Some(Expression::Bool(value)),
            AnalyzedExpression::Char(value) => Some(Expression::Char(value)),
            AnalyzedExpression::Ident(ident) => match ident.result_type {
                Type::Never | Type::Any => None,
                _ => Some(Expression::Ident(
                    self.resolve_name(ident.ident).to_string(),
                )),
            },
            AnalyzedExpression::Prefix(node) => return self.prefix_expr(*node),
            AnalyzedExpression::Infix(node) => return self.infix_expr(*node),
            AnalyzedExpression::Assign(node) => return (self.assign_expr(*node), None),
            AnalyzedExpression::Call(node) => return self.call_expr(*node),
            AnalyzedExpression::Cast(node) => return self.cast_expr(*node),
            AnalyzedExpression::Grouped(node) => return self.grouped_expr(*node),
            AnalyzedExpression::String(inner) => {
                let raw_string = Expression::Call(Box::new(CallExpr {
                    func: "dynstring_from".to_string(),
                    args: vec![Expression::StringLiteral(
                        inner.replace('\n', "\\n").replace('"', "\\\""),
                    )],
                }));

                let dynstr_ident = format!("string_temp{}", self.let_cnt);
                self.let_cnt += 1;

                let mut stmts = vec![Statement::VarDeclaration(VarDeclaration {
                    name: dynstr_ident.clone(),
                    type_: Type::String(0).into(),
                    expr: raw_string,
                })];

                if self.user_config.gc_enable {
                    stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                        func: "gc_add_to_trace".into(),
                        args: vec![
                            Expression::Ident(dynstr_ident.clone()),
                            Expression::Ident(self.get_type_reflector(Type::String(0))),
                            Expression::Ident("NULL".to_string()),
                        ],
                    }))));
                }

                return (stmts, Some(Expression::Ident(dynstr_ident)));
            }
            AnalyzedExpression::List(list) => return self.list_literal(list),
            AnalyzedExpression::Object(inner) => return self.obj_literal(*inner),
            AnalyzedExpression::Index(index) => {
                let (mut base_stmts, base_expr) = self.expression(index.expr.clone());
                let (mut index_stmts, index_expr) = self.expression(index.index.clone());

                base_stmts.append(&mut index_stmts);

                match (index.expr.result_type(), index.index.result_type()) {
                    (Type::List(_, 0), Type::Int(0)) => {
                        self.required_includes.insert("./libSAP/libList.h");

                        // let get_result_ident = format!("list_index_res{}", self.let_cnt);
                        // self.let_cnt += 1;

                        let list_index_call_expr = Expression::Call(Box::new(CallExpr {
                            func: "__hpi_internal_list_index".to_string(),
                            args: vec![
                                base_expr.expect("unreachable"),
                                index_expr.expect("unreachable"),
                            ],
                        }));

                        // base_stmts.push(Statement::VarDeclaration(VarDeclaration {
                        //     name: get_result_ident.clone(),
                        //     type_: CType::Ident(0, "ListGetResult".to_string()),
                        //     expr: list_index_call_expr.clone(),
                        // }));

                        let deref_expr = Expression::Prefix(Box::new(PrefixExpr {
                            expr: Expression::Cast(Box::new(CastExpr {
                                expr: list_index_call_expr,
                                type_: index
                                    .result_type
                                    .clone()
                                    .add_ref()
                                    .unwrap_or_else(|| {
                                        panic!(
                                            "Unsupported type to reference: {}",
                                            index.result_type
                                        )
                                    })
                                    .into(),
                            })),
                            op: PrefixOp::Deref,
                        }));

                        return (base_stmts, Some(deref_expr));
                    }
                    (_, _) => todo!("TODO"),
                }
            }
            AnalyzedExpression::Member(node) => {
                let (mut base_stmts, base_expr) = self.expression(node.expr.clone());
                self.required_includes.insert("assert.h");

                let member_res_ident = format!("m_{}_{}", node.member, self.let_cnt);
                self.let_cnt += 1;

                base_stmts.push(Statement::VarDeclaration(VarDeclaration {
                    name: member_res_ident.clone(),
                    type_: CType::Ident(0, "MapGetResult".to_string()),
                    expr: Expression::Call(Box::new(CallExpr {
                        func: "hashmap_get".to_string(),
                        args: vec![
                            base_expr.clone().unwrap(),
                            Expression::StringLiteral(node.member.to_string()),
                        ],
                    })),
                }));

                // TODO: compile member access
                // TODO: plan:
                // 1. Get a void * to the member by doing a HashMap lookup
                // 2. Cast and dereference the pointer

                base_stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                    func: "assert".to_string(),
                    args: vec![Expression::Member(Box::new(MemberExpr {
                        expr: Expression::Ident(member_res_ident.clone()),
                        member: "found".to_string(),
                        base_is_ptr: false,
                    }))],
                }))));

                return (
                    base_stmts,
                    Some(Expression::Prefix(Box::new(PrefixExpr {
                        expr: Expression::Cast(Box::new(CastExpr {
                            expr: Expression::Member(Box::new(MemberExpr {
                                expr: Expression::Ident(member_res_ident),
                                member: "value".to_string(),
                                base_is_ptr: false,
                            })),
                            type_: node.result_type.add_ref().unwrap().into(),
                        })),
                        op: PrefixOp::Deref,
                    }))),
                );
            }
            AnalyzedExpression::Nichts => return (vec![], None),
        };
        (vec![], expr)
    }

    fn obj_literal(&mut self, obj: AnalyzedObjectExpr<'src>) -> (Vec<Statement>, Option<Expression>) {
        let obj_temp_ident = match self.user_config.emit_readable_names {
            true => format!("object_temp{}", self.let_cnt),
            false => format!("obj{}", self.let_cnt),
        };
        self.let_cnt += 1;

        let mut stmts = vec![Statement::VarDeclaration(VarDeclaration {
            name: obj_temp_ident.clone(),
            type_: CType::Ident(1, "HashMap".to_string()),
            // expr: Expression::Call(Box::new(CallExpr {
            //     func: "hashmap_new".to_string(), // TODO: use vec in the long run?
            //     args: vec![],
            // })),
            expr: self.malloc(Type::Object(vec![], 0)),
        })];

        for value in obj.members.iter() {
            let temp_ident = match self.user_config.emit_readable_names {
                true => format!("object_member_{}_n{}", value.key, self.let_cnt),
                false => format!("om{}", self.let_cnt),
            };
            self.let_cnt += 1;

            let (mut expr_stmts, expr) = self.expression(value.value.clone());
            stmts.append(&mut expr_stmts);

            if let Some(expr) = expr {
                stmts.push(Statement::VarDeclaration(VarDeclaration {
                    name: temp_ident.clone(),
                    type_: value.value.result_type().add_ref().unwrap().into(),
                    expr: self.malloc(value.value.result_type().add_ref().unwrap()),
                }));
                self.let_cnt += 1;

                // TODO: use expr
                stmts.push(Statement::Assign(AssignStmt {
                    assignee: temp_ident.clone(),
                    assignee_ptr_count: 1,
                    op: AssignOp::Basic,
                    expr,
                }));

                // BUG: this is not enough, heap allocation is required!
                // THe same goes for list literal creation

                stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                    func: "hashmap_insert".to_string(),
                    args: vec![
                        Expression::Ident(obj_temp_ident.clone()),
                        Expression::StringLiteral(value.key.clone()),
                        Expression::Ident(temp_ident),
                    ],
                }))));
            }
        }

        return (stmts, Some(Expression::Ident(obj_temp_ident)));
    }

    fn list_literal(
        &mut self,
        literal: AnalyzedListExpression<'src>,
    ) -> (Vec<Statement>, Option<Expression>) {
        let list_temp_ident = format!("list_temp{}", self.let_cnt);
        self.let_cnt += 1;

        let mut stmts = vec![Statement::VarDeclaration(VarDeclaration {
            name: list_temp_ident.clone(),
            type_: CType::Ident(1, "ListNode".to_string()),
            // expr: Expression::Call(Box::new(CallExpr {
            //     func: "list_new".to_string(), // TODO: use vec in the long run?
            //     args: vec![],
            // })),
            // expr: self.malloc(Type::List(Type::Unknown.into(), 0)),
            expr: self.malloc(Type::List(literal.inner_type.into(), 0)),
        })];

        for (idx, value) in literal.values.iter().enumerate() {
            let temp_ident = format!("list_idx_{}_n{}", idx, self.let_cnt);

            let (mut expr_stmts, expr) = self.expression(value.clone());
            stmts.append(&mut expr_stmts);

            if let Some(expr) = expr {
                let res_type = value.result_type().add_ref().unwrap();

                stmts.push(Statement::VarDeclaration(VarDeclaration {
                    name: temp_ident.clone(),
                    type_: res_type.clone().into(),
                    expr: self.malloc(res_type),
                }));
                self.let_cnt += 1;

                // stmts.push(Statement::VarDeclaration(VarDeclaration {
                //     name: temp_ident.clone(),
                //     type_: value.result_type().into(),
                //     expr,
                // }));
                // self.let_cnt += 1;

                stmts.push(Statement::Assign(AssignStmt {
                    assignee: temp_ident.clone(),
                    assignee_ptr_count: 1,
                    op: AssignOp::Basic,
                    expr,
                }));

                stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                    func: "list_append".to_string(),
                    args: vec![
                        Expression::Ident(list_temp_ident.clone()),
                        // Expression::Prefix(Box::new(PrefixExpr {
                        //     expr: Expression::Ident(temp_ident),
                        //     op: PrefixOp::Ref,
                        // })),
                        Expression::Ident(temp_ident),
                    ],
                }))));
            }
        }

        return (stmts, Some(Expression::Ident(list_temp_ident)));
    }

    fn body(&mut self, body: AnalyzedBlock<'src>) -> Vec<Statement> {
        let mut body_stmts: Vec<Statement> = body
            .stmts
            .into_iter()
            .flat_map(|s| self.statement(s))
            .collect();

        if let Some(raw_expr) = body.expr.clone() {
            let (mut stmts, expr) = self.expression(raw_expr.clone());
            body_stmts.append(&mut stmts);
            let mut stmts = match (self.in_main_fn, expr) {
                (true, Some(expr)) => {
                    vec![
                        Statement::Expr(expr),
                        Statement::Return(Some(Expression::Int(0))),
                    ]
                }
                (true, None) => vec![Statement::Return(Some(Expression::Int(0)))],
                (false, expr) => {
                    if raw_expr.result_type() == Type::Nichts {
                        vec![Statement::Return(None)]
                    } else {
                        vec![Statement::Return(expr)]
                    }
                }
            };
            body_stmts.append(&mut stmts);
        };

        body_stmts
    }

    pub(super) fn block_expr(
        &mut self,
        node: AnalyzedBlock<'src>,
        scoping: bool,
    ) -> (Vec<Statement>, Option<Expression>) {
        if scoping {
            self.scopes.push(Scope::new());
        }
        let mut block = vec![];

        comment!(self, block, "begin block".into());

        let mut stmts: Vec<Statement> = node
            .stmts
            .into_iter()
            .flat_map(|s| self.statement(s))
            .collect();

        block.append(&mut stmts);

        let expr = match node.expr {
            Some(expr) => {
                let (mut expr_stmts, expr) = self.expression(expr);
                block.append(&mut expr_stmts);
                expr
            }
            None => None,
        };

        if scoping {
            if let Some(stmt) = self.pop_scope(true) {
                block.push(stmt);
            }
        }
        comment!(self, block, "end block".into());

        (block, expr)
    }

    fn if_expr(&mut self, node: AnalyzedIfExpr<'src>) -> (Vec<Statement>, Option<Expression>) {
        let mut stmts = vec![];
        let (mut cond_stmts, cond) = match self.expression(node.cond) {
            (stmts, Some(cond)) => (stmts, cond),
            (stmts, None) => return (stmts, None),
        };

        stmts.append(&mut cond_stmts);

        let res_ident = match node.result_type {
            Type::Never => None,
            _ => {
                self.let_cnt += 1;
                let ident = format!("if_res{}", self.let_cnt);
                stmts.push(Statement::VarDefinition(
                    ident.clone(),
                    node.result_type.into(),
                ));
                Some(ident)
            }
        };

        let then_block = match self.block_expr(node.then_block, true) {
            (mut stmts, Some(expr)) => {
                stmts.push(Statement::Assign(AssignStmt {
                    assignee: res_ident.clone().expect("was declared above"),
                    assignee_ptr_count: 0,
                    op: AssignOp::Basic,
                    expr,
                }));
                stmts
            }
            (stmts, None) => stmts,
        };

        let else_block = match node.else_block {
            Some(block) => match self.block_expr(block, true) {
                (mut stmts, Some(expr)) => {
                    stmts.push(Statement::Assign(AssignStmt {
                        assignee: res_ident.clone().expect("was declared above"),
                        assignee_ptr_count: 0,
                        op: AssignOp::Basic,
                        expr,
                    }));
                    Some(stmts)
                }
                (stmts, None) => Some(stmts),
            },
            None => None,
        };

        stmts.push(Statement::If(IfStmt {
            cond,
            then_block,
            else_block,
        }));

        let res_expr = res_ident.map(Expression::Ident);

        (stmts, res_expr)
    }

    fn prefix_expr(
        &mut self,
        node: AnalyzedPrefixExpr<'src>,
    ) -> (Vec<Statement>, Option<Expression>) {
        let expr_type = node.expr.result_type();
        let (stmts, expr) = self.expression(node.expr);
        let expr = expr.map(|expr| {
            let op = match (expr_type, node.op) {
                (Type::Int(0), hpi_analyzer::PrefixOp::Not) => PrefixOp::BinNot,
                (_, hpi_analyzer::PrefixOp::Not) => PrefixOp::BoolNot,
                (_, op) => op.into(),
            };
            Expression::Prefix(Box::new(PrefixExpr { expr, op }))
        });
        (stmts, expr)
    }

    fn infix_expr(
        &mut self,
        node: AnalyzedInfixExpr<'src>,
    ) -> (Vec<Statement>, Option<Expression>) {
        let (lhs_type, rhs_type) = (node.lhs.result_type(), node.rhs.result_type());
        let (mut lhs_stmts, lhs) = self.expression(node.lhs);
        let (mut rhs_stmts, rhs) = self.expression(node.rhs);

        match (lhs_type, rhs_type, node.op) {
            (Type::Char(0), Type::Char(0), InfixOp::Plus | InfixOp::Minus)
            | (Type::Int(0), Type::Int(0), InfixOp::Pow) => {
                lhs_stmts.append(&mut rhs_stmts);
                let func = match node.op {
                    InfixOp::Plus => "__hpi_internal_add_char",
                    InfixOp::Minus => "__hpi_internal_sub_char",
                    InfixOp::Pow => "__hpi_internal_pow_int",
                    _ => unreachable!("these operators cannot occur here"),
                };
                self.required_corelib_functions.insert(func);
                (
                    lhs_stmts,
                    Some(Expression::Call(Box::new(CallExpr {
                        func: func.to_string(),
                        args: vec![
                            lhs.expect("exprs cannot be `None `when used here"),
                            rhs.expect("exprs cannot be None when used here"),
                        ],
                    }))),
                )
            }
            (_, _, InfixOp::Or | InfixOp::And) => {
                self.let_cnt += 1;
                let ident = format!("logical_res{}", self.let_cnt);

                rhs_stmts.push(Statement::Assign(AssignStmt {
                    assignee: ident.clone(),
                    assignee_ptr_count: 0,
                    op: AssignOp::Basic,
                    expr: match rhs {
                        Some(expr) => expr,
                        // if the rhs is [`None`], use a dummy value
                        None => Expression::Bool(false),
                    },
                }));

                let mut stmts = vec![
                    Statement::VarDefinition(ident.clone(), CType::from(*node.result_type)),
                    Statement::If(IfStmt {
                        // if the lhs is [`None`], use a dummy value
                        cond: match lhs {
                            Some(expr) => match node.op == InfixOp::Or {
                                true => expr,
                                false => Expression::Prefix(Box::new(PrefixExpr {
                                    expr: Expression::Grouped(Box::new(expr)),
                                    op: PrefixOp::BoolNot,
                                })),
                            },
                            None => Expression::Bool(true),
                        },
                        then_block: vec![Statement::Assign(AssignStmt {
                            assignee: ident.clone(),
                            assignee_ptr_count: 0,
                            op: AssignOp::Basic,
                            // when the operator is `||` return true in this branch
                            // otherwise, return `false` in this branch
                            expr: Expression::Bool(node.op == InfixOp::Or),
                        })],
                        else_block: Some(rhs_stmts),
                    }),
                ];

                lhs_stmts.append(&mut stmts);
                (lhs_stmts, Some(Expression::Ident(ident)))
            }
            (Type::Nichts | Type::Never, _, _) | (_, Type::Nichts | Type::Never, _) => {
                lhs_stmts.append(&mut rhs_stmts);
                if let Some(lhs) = lhs {
                    lhs_stmts.push(Statement::Expr(lhs));
                };
                if let Some(rhs) = rhs {
                    lhs_stmts.push(Statement::Expr(rhs));
                };
                (lhs_stmts, None)
            }
            (Type::String(0), Type::String(0), op @ InfixOp::Eq | op @ InfixOp::Neq) => {
                let mut expr = Expression::Call(Box::new(CallExpr {
                    func: "dynstring_strcmp".to_string(),
                    args: vec![lhs.expect("This is a str"), rhs.expect("This as well")],
                }));

                if op == InfixOp::Neq {
                    expr = Expression::Prefix(Box::new(PrefixExpr {
                        expr,
                        op: PrefixOp::BoolNot,
                    }));
                }

                lhs_stmts.append(&mut rhs_stmts);

                (lhs_stmts, Some(expr))
            }
            (_, _, _) => {
                lhs_stmts.append(&mut rhs_stmts);
                (
                    lhs_stmts,
                    match (lhs, rhs) {
                        (None, None) => None,
                        (Some(lhs), None) => Some(lhs),
                        (None, Some(rhs)) => Some(rhs),
                        (Some(lhs), Some(rhs)) => Some(Expression::Infix(Box::new(InfixExpr {
                            lhs,
                            rhs,
                            op: node.op,
                        }))),
                    },
                )
            }
        }
    }

    fn assign_expr(&mut self, node: AnalyzedAssignExpr<'src>) -> Vec<Statement> {
        let type_ = node.expr.result_type();
        let (mut stmts, expr) = self.expression(node.expr);
        let assignee = self.resolve_name(node.assignee).to_string();

        if let Some(expr) = expr {
            match (type_, node.op) {
                (Type::Int(_), AssignOp::Pow) => {
                    self.required_corelib_functions
                        .insert("__hpi_internal_pow_int");

                    stmts.push(Statement::Assign(AssignStmt {
                        assignee: assignee.clone(),
                        assignee_ptr_count: node.assignee_ptr_count,
                        op: AssignOp::Basic,
                        expr: Expression::Call(Box::new(CallExpr {
                            func: "__hpi_internal_pow_int".to_string(),
                            args: vec![
                                Expression::Deref((node.assignee_ptr_count, assignee)),
                                expr,
                            ],
                        })),
                    }))
                }
                (Type::Char(_), AssignOp::Plus | AssignOp::Minus) => {
                    let func = match node.op == AssignOp::Plus {
                        true => "__hpi_internal_add_char",
                        false => "__hpi_internal_sub_char",
                    };
                    self.required_corelib_functions.insert(func);

                    stmts.push(Statement::Assign(AssignStmt {
                        assignee: assignee.clone(),
                        assignee_ptr_count: node.assignee_ptr_count,
                        op: AssignOp::Basic,
                        expr: Expression::Call(Box::new(CallExpr {
                            func: func.to_string(),
                            args: vec![
                                Expression::Deref((node.assignee_ptr_count, assignee)),
                                expr,
                            ],
                        })),
                    }))
                }
                (_, op) => {
                    stmts.push(Statement::Assign(AssignStmt {
                        assignee,
                        assignee_ptr_count: node.assignee_ptr_count,
                        op,
                        expr,
                    }));
                }
            }
        }

        stmts
    }

    pub(super) fn call_expr(
        &mut self,
        node: AnalyzedCallExpr<'src>,
    ) -> (Vec<Statement>, Option<Expression>) {
        let mut stmts = vec![];
        let mut args = VecDeque::new();

        let mut none_arg = false;

        // TODO: this is ugly

        // let type_list: Vec<Type> = node
        //     .args
        //     .iter()
        //     .map(|arg| self.lookup_type(arg.result_type()))
        //     .collect();

        let mut type_list = vec![];

        for expr in node.args.iter() {
            let type_ = expr.result_type();
            let (mut expr_stmts, new_expr) = self.expression(expr.clone());

            type_list.push(type_.clone());
            stmts.append(&mut expr_stmts);

            if new_expr.is_none() && type_ != Type::Nichts {
                none_arg = true;
            }

            if let Some(expr) = new_expr {
                args.push_back(expr);
            }
        }

        let func = match node.func {
            AnalyzedCallBase::Ident("Aufgeben") => {
                "cexit".to_string()
            }
            AnalyzedCallBase::Ident("Reinigung") => match self.user_config.gc_enable {
                true => "gc_run_cycle".to_string(),
                false => return (vec![], None),
            },
            AnalyzedCallBase::Ident("ReinigungsPlan") => match self.user_config.gc_enable {
                true => "external_print_state".to_string(),
                false => return (vec![], None),
            },
            AnalyzedCallBase::Ident("cexit") => "cexit".to_string(),
            AnalyzedCallBase::Ident("type_descriptor_setup") => "type_descriptor_setup".to_string(),
            AnalyzedCallBase::Ident("type_descriptor_teardown") => "type_descriptor_teardown".to_string(),
            AnalyzedCallBase::Ident("gc_init") => "gc_init".to_string(),
            AnalyzedCallBase::Ident("gc_die") => "gc_die".to_string(),
            AnalyzedCallBase::Ident("global_variable_setup") => "global_variable_setup".to_string(),
            AnalyzedCallBase::Ident("bewerbung") => "bewerbung".to_string(),
            AnalyzedCallBase::Ident("einschreibung") => "einschreibung".to_string(),
            AnalyzedCallBase::Ident("studium") => "studium".to_string(),
            AnalyzedCallBase::Ident("Zergliedere_JSON") => {
                self.required_includes.insert("./libSAP/libJson.h");
                args.push_back(Expression::Ident(
                    if self.user_config.gc_enable {
                        "gc_alloc".to_string()
                    } else {
                        "non_tracing_alloc".to_string()
                    }
                ));

                args.push_back(Expression::Ident(
                    if self.user_config.gc_enable {
                        "gc_add_to_trace".to_string()
                    } else {
                        "NULL".to_string()
                    }
                ));

                "__hpi_internal_parse_json".to_string()
            }
            AnalyzedCallBase::Ident("Gliedere_JSON") => {
                self.required_includes.insert("./libSAP/libJson.h");

                let marshal_ident = format!("marshal_temp{}", self.let_cnt);
                self.let_cnt += 1;

                stmts.push(Statement::VarDeclaration(VarDeclaration {
                    name: marshal_ident.clone(),
                    type_: type_list[0].clone().into(),
                    expr: args.pop_back().unwrap(),
                }));

                args.push_back(Expression::Ident(
                    self.get_type_reflector(type_list[0].clone()),
                ));

                args.push_back(Expression::Prefix(Box::new(PrefixExpr {
                    expr: Expression::Ident(marshal_ident),
                    op: PrefixOp::Ref,
                })));

                "__hpi_internal_marshal_json".to_string()
            }
            AnalyzedCallBase::Ident("Drucke") => {
                self.required_includes.insert("./libSAP/libSAP.h");
                "__hpi_internal_print".to_string()
            }
            AnalyzedCallBase::Ident("Umgebungsvariablen") => {
                self.required_includes.insert("./libSAP/libSAP.h");
                "__hpi_internal_env".to_string()
            }
            AnalyzedCallBase::Ident("Argumente") => {
                self.required_includes.insert("./libSAP/libSAP.h");
                "__hpi_internal_args".to_string()
            }
            AnalyzedCallBase::Ident("Http") => {
                self.required_includes.insert("./libSAP/libHttp.h");
                "__hpi_internal_http".to_string()
            }
            AnalyzedCallBase::Ident("Formatiere") => {
                self.required_includes.insert("./libSAP/libSAP.h");
                "__hpi_internal_fmt".to_string()
            }
            AnalyzedCallBase::Ident("Zeit") => {
                self.required_includes.insert("./libSAP/libTime.h");
                "__hpi_internal_time".to_string()
            }
            AnalyzedCallBase::Ident("Schlummere") => {
                self.required_includes.insert("./libSAP/libSAP.h");
                "__hpi_internal_sleep".to_string()
            }
            AnalyzedCallBase::Ident("__hpi_internal_generate_matrikelnummer") => {
                self.required_includes.insert("./libSAP/libSAP.h");
                "__hpi_internal_generate_matrikelnummer".to_string()
            }
            AnalyzedCallBase::Ident("__hpi_internal_init_libSAP") => {
                self.required_includes.insert("./libSAP/libSAP.h");
                "__hpi_internal_init_libSAP".to_string()
            }
            AnalyzedCallBase::Ident(other) => self
                .funcs
                .get(other)
                .unwrap_or_else(|| panic!("the analyzer guarantees valid function calls: {other}"))
                .clone(),
            AnalyzedCallBase::Expr(inner) => match *inner {
                AnalyzedExpression::Member(member) => {
                    let (mut member_stmts, member_expr) = self.expression(member.expr.clone());
                    stmts.append(&mut member_stmts);

                    match (member.expr.result_type(), member.member) {
                        (Type::List(_, 0), "Länge") => {
                            self.required_includes.insert("./libSAP/libList.h");
                            args.push_back(member_expr.expect("A list always produces a value"));
                            "__hpi_internal_list_len".to_string()
                        }
                        (Type::List(_, 0), "Hinzufügen") => {
                            self.required_includes.insert("./libSAP/libList.h");

                            let temp_ident = format!("push_ptr_{}", self.let_cnt);

                            stmts.push(Statement::VarDeclaration(VarDeclaration {
                                name: temp_ident.clone(),
                                type_: type_list[0].clone().add_ref().unwrap().into(),
                                // expr: Expression::Call(Box::new(CallExpr {
                                //     func: "malloc".to_string(),
                                //     args: vec![Expression::Call(Box::new(CallExpr {
                                //         func: "sizeof".to_string(),
                                //         args: vec![Expression::TypeExpr(
                                //             type_list[0].clone().into(),
                                //         )],
                                //     }))],
                                // })),
                                expr: self.malloc(type_list[0].clone()),
                            }));

                            stmts.push(Statement::Assign(AssignStmt {
                                assignee: temp_ident.clone(),
                                assignee_ptr_count: 1,
                                op: AssignOp::Basic,
                                expr: args
                                    .pop_back()
                                    .expect("This function always takes exactly 2 args"),
                            }));

                            self.let_cnt += 1;

                            args.push_front(member_expr.expect("A list always produces a value"));

                            args.push_back(Expression::Ident(temp_ident));

                            "__hpi_internal_list_push".to_string()
                        }
                        (Type::List(_, 0), "Enthält") => {
                            self.required_includes.insert("./libSAP/libList.h");
                            let temp_ident = format!("contains_ptr_{}", self.let_cnt);

                            stmts.push(Statement::VarDeclaration(VarDeclaration {
                                name: temp_ident.clone(),
                                type_: type_list[0].clone().into(),
                                expr: args
                                    .pop_back()
                                    .expect("This function always takes exactly 3 args"),
                            }));
                            self.let_cnt += 1;

                            args.push_back(member_expr.expect("A list always produces a value"));

                            args.push_back(Expression::Ident(
                                self.get_type_reflector(node.args[0].result_type()),
                            ));

                            args.push_back(Expression::Prefix(Box::new(PrefixExpr {
                                expr: Expression::Ident(temp_ident),
                                op: PrefixOp::Ref,
                            })));

                            "__hpi_internal_list_contains".to_string()
                        }
                        (Type::AnyObject(0), "Nehmen") => {
                            args.push_front(
                                member_expr.expect("An anyobj always produces a value"),
                            );
                            "__hpi_internal_anyobj_take".to_string()
                        }
                        (Type::AnyObject(0), "Schlüssel") => {
                            args.push_back(member_expr.expect("An anyobj always produces a value"));
                            "__hpi_internal_anyobj_keys".to_string()
                        }
                        (Type::String(0), "Zertrenne") => {
                            self.required_includes.insert("./libSAP/libString.h");
                            args.push_front(member_expr.expect("A string always produces a value"));
                            "__hpi_internal_string_split".to_string()
                        }
                        (Type::String(0), "Startet_Mit") => {
                            self.required_includes.insert("./libSAP/libString.h");
                            args.push_front(member_expr.expect("A string always produces a value"));
                            "__hpi_internal_string_starts_with".to_string()
                        }
                        (Type::String(0), "Enthält") => {
                            self.required_includes.insert("./libSAP/libString.h");
                            args.push_front(member_expr.expect("A string always produces a value"));
                            "__hpi_internal_string_contains".to_string()
                        }
                        (Type::String(0), "Ersetze") => {
                            self.required_includes.insert("./libSAP/libString.h");
                            args.push_front(member_expr.expect("A string always produces a value"));
                            "__hpi_internal_string_replace".to_string()
                        }
                        (base_type, member) => {
                            unreachable!("Not supported: member `{member}` of base `{base_type}`")
                        }
                    }
                }
                other => unreachable!("Not supported: {other:?}"),
            },
        };

        match func.as_str() {
            "__hpi_internal_fmt" => {
                let mut new_args = vec![];

                new_args.push(Expression::Int(args.len() as i64 - 1));

                new_args.push(args[0].clone());

                for (idx, _arg) in args.iter().skip(1).enumerate() {
                    new_args.push(Expression::Ident(
                        self.get_type_reflector(type_list[idx + 1].clone()),
                    ));

                    let temp_ident = format!("fmt_ptr_{}", self.let_cnt);

                    stmts.push(Statement::VarDeclaration(VarDeclaration {
                        name: temp_ident.clone(),
                        type_: type_list[idx + 1].clone().into(),
                        expr: _arg.clone(),
                    }));
                    self.let_cnt += 1;

                    stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                        func: "__hpi_internal_libSAP_reset".to_string(),
                        args: vec![],
                    }))));

                    new_args.push(Expression::Prefix(Box::new(PrefixExpr {
                        expr: Expression::Ident(temp_ident),
                        op: PrefixOp::Ref,
                    })));
                }

                args = new_args.into();
            }
            "__hpi_internal_print" => {
                let mut new_args = vec![];

                new_args.push(Expression::Int(type_list.len() as i64));

                for (idx, _type) in type_list.clone().iter().enumerate() {
                    new_args.push(Expression::Ident(self.get_type_reflector(_type.clone())));

                    stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                        func: "__hpi_internal_libSAP_reset".to_string(),
                        args: vec![],
                    }))));

                    if let Some(arg) = args.get(idx) {
                        let temp_ident = format!("fmt_ptr_{}", self.let_cnt);

                        stmts.push(Statement::VarDeclaration(VarDeclaration {
                            name: temp_ident.clone(),
                            type_: _type.clone().into(), // TODO: fix `Nichts`
                            expr: arg.clone(),
                        }));

                        self.let_cnt += 1;

                        new_args.push(Expression::Prefix(Box::new(PrefixExpr {
                            expr: Expression::Ident(temp_ident),
                            op: PrefixOp::Ref,
                        })));
                    }
                }

                args = new_args.into();
            }
            _ => {}
        }

        let expr = Box::new(CallExpr {
            func: func.to_string(),
            args: args.into(),
        });

        match node.result_type {
            Type::Never => {
                if !none_arg {
                    stmts.push(Statement::Expr(Expression::Call(expr)));
                }
                (stmts, None)
            }
            _ => {
                let expr = match none_arg {
                    true => None,
                    false => Some(Expression::Call(expr)),
                };
                (stmts, expr)
            }
        }
    }

    pub(super) fn cast_expr(
        &mut self,
        node: AnalyzedCastExpr<'src>,
    ) -> (Vec<Statement>, Option<Expression>) {
        let type_ = node.expr.result_type();
        let (mut stmts, expr) = match self.expression(node.expr) {
            (stmts, Some(expr)) => (stmts, expr),
            (stmts, None) => return (stmts, None),
        };

        let expr = match (&type_, &node.result_type) {
            (Type::Int(0), Type::Char(0)) | (Type::Float(0), Type::Char(0)) => {
                let func = match type_ == Type::Int(0) {
                    true => "__hpi_internal_cast_int_to_char",
                    false => "__hpi_internal_cast_float_to_char",
                };

                self.required_corelib_functions.insert(func);

                Expression::Call(Box::new(CallExpr {
                    func: func.to_string(),
                    args: vec![expr],
                }))
            }
            (Type::Any, as_type) => {
                // TODO: insert runtime cast validation
                // TODO: this function cannot just validate the inner type,
                // it must also return a void* which is then casted to the correct end result.
                // void __hpi_internal_validate_runtime_cast(TypeDescriptor as_type, TypeDescriptor from_type);

                let from_expr_ident = format!("runtime_cast_from{}", self.let_cnt);
                self.let_cnt += 1;

                stmts.push(Statement::VarDeclaration(VarDeclaration {
                    name: from_expr_ident.clone(),
                    type_: CType::Ident(0, "AnyValue".to_string()),
                    expr,
                }));

                // source is a AnyValue*
                Expression::Prefix(Box::new(PrefixExpr {
                    expr: Expression::Cast(Box::new(CastExpr {
                        expr: Expression::Call(Box::new(CallExpr {
                            func: "__hpi_internal_runtime_cast".to_string(),
                            args: vec![
                                Expression::Ident(from_expr_ident.clone()),
                                Expression::Ident(self.get_type_reflector(as_type.clone())),
                            ],
                        })),
                        type_: as_type.clone().add_ref().unwrap().into(),
                    })),
                    op: PrefixOp::Deref,
                }))
            }
            _ => Expression::Cast(Box::new(CastExpr {
                expr,
                type_: node.result_type.into(),
            })),
        };

        (stmts, Some(expr))
    }

    pub(super) fn grouped_expr(
        &mut self,
        node: AnalyzedExpression<'src>,
    ) -> (Vec<Statement>, Option<Expression>) {
        let (stmts, expr) = self.expression(node);
        (stmts, expr.map(|expr| Expression::Grouped(Box::new(expr))))
    }
}
