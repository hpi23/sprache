use hpi_analyzer::{
    ast::{
        AnalyzedAendereStmt, AnalyzedExpression, AnalyzedLetStmt, AnalyzedStatement,
        AnalyzedWhileStmt,
    },
    AssignOp, Type,
};

use crate::{
    c_ast::{
        AssignStmt, CType, CallExpr, Expression, IfStmt, PrefixExpr, PrefixOp, Statement,
        VarDeclaration,
    },
    gc::Scope,
    transpiler::Loop,
    Transpiler,
};

impl<'src> Transpiler<'src> {
    pub(super) fn statement(&mut self, node: AnalyzedStatement<'src>) -> Vec<Statement> {
        match node {
            AnalyzedStatement::Let(node) => self.let_stmt(node, false),
            AnalyzedStatement::Aendere(node) => self.aendere_stmt(node),
            AnalyzedStatement::Return(node) => self.return_stmt(node),
            AnalyzedStatement::While(node) => self.while_stmt(node),
            // for `break` and `continue` jumps, `goto` is used because of HPI's semantics
            AnalyzedStatement::Break => {
                let loop_ = self.loops.last_mut().expect("there is always a loop");
                vec![Statement::Goto(loop_.break_label.clone())]
            }
            AnalyzedStatement::Continue => {
                let gc_remove_roots = self.pop_scope(false);
                let loop_ = self.loops.last_mut().expect("there is always a loop");
                vec![gc_remove_roots, Statement::Goto(loop_.head_label.clone())]
            }
            AnalyzedStatement::Expr(node) => {
                let (mut stmts, expr) = self.expression(node);
                if let Some(expr) = expr {
                    stmts.push(Statement::Expr(expr));
                }
                stmts
            }
            other => unreachable!("Not supported: {other:?}"),
        }
    }

    fn aendere_stmt(&mut self, node: AnalyzedAendereStmt<'src>) -> Vec<Statement> {
        let (mut stmts, expr) = self.expression(node.expr);
        let assignee = self.resolve_name(node.assignee).to_string();

        if let Some(expr) = expr {
            stmts.push(Statement::Assign(AssignStmt {
                assignee,
                assignee_ptr_count: 0,
                op: AssignOp::Basic,
                expr,
            }));
        }

        stmts
    }

    pub(super) fn let_stmt(
        &mut self,
        node: AnalyzedLetStmt<'src>,
        is_global: bool,
    ) -> Vec<Statement> {
        let type_ = node.expr.result_type();
        let (mut stmts, expr) = self.expression(node.expr.clone());

        let name = self.insert_into_scope(node.name);

        if is_global {
            match node.expr.result_type() {
                Type::String(0) => {
                    comment!(
                        self,
                        self.global_variable_setup,
                        format!("Setup for global variable `{name}`").into()
                    );
                    self.global_variable_setup.append(&mut stmts);

                    if let Some(expr) = expr {
                        self.global_variable_setup
                            .push(Statement::Assign(AssignStmt {
                                assignee: name.clone(),
                                assignee_ptr_count: 0,
                                op: AssignOp::Basic,
                                expr,
                            }));
                    }

                    return vec![Statement::VarDefinition(
                        name,
                        node.expr.result_type().into(),
                    )];
                }
                Type::List(_, _) => todo!(),
                Type::Object(_, _) => todo!(),
                _ => {}
            }
        }

        if let Some(mut expr) = expr {
            let ctype = CType::from(type_.clone());

            // Use this variable as a root if it is heap-allocated.
            if ctype.pointer_count() > 0 {
                let (mut stmts_temp, expr_temp) = self.gc_add_root(expr, type_);
                stmts.append(&mut stmts_temp);

                expr = expr_temp;
            }

            let stmt = match ctype {
                CType::Void(0) => Statement::Expr(expr),
                _ => Statement::VarDeclaration(VarDeclaration {
                    name,
                    type_: ctype,
                    expr,
                }),
            };

            stmts.push(stmt)
        }

        stmts
    }

    fn return_stmt(&mut self, node: Option<AnalyzedExpression<'src>>) -> Vec<Statement> {
        match (node, self.in_main_fn) {
            (Some(expr), _) => {
                if expr.result_type() == Type::Nichts {
                    vec![Statement::Return(None)]
                } else {
                    let res_type = expr.result_type();
                    let (mut stmts, expr) = self.expression(expr);

                    let pointer_count = CType::from(res_type.clone()).pointer_count();
                    println!("{pointer_count}");

                    // If the return value is being tracked by the GC (is heap), add a ref to it.
                    if pointer_count > 0 {
                        let ident = format!("return_value{}", self.let_cnt);
                        self.let_cnt += 1;

                        if let Some(expr) = expr {
                            let expr_stmt = Statement::VarDeclaration(VarDeclaration {
                                name: ident.clone(),
                                type_: res_type.into(),
                                expr,
                            });

                            stmts.push(expr_stmt);

                            stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                                func: "gc_ref".into(),
                                args: vec![Expression::Ident(ident.clone())],
                            }))));

                            stmts.push(Statement::Return(Some(Expression::Ident(ident))));
                            return stmts;
                        }

                        // TODO: should drop?
                    }

                    stmts.push(Statement::Return(expr));
                    stmts
                }
            }
            (None, false) => vec![Statement::Return(None)],
            (None, true) => vec![Statement::Return(Some(Expression::Int(0)))],
        }
    }

    fn while_stmt(&mut self, node: AnalyzedWhileStmt<'src>) -> Vec<Statement> {
        let break_label = format!("break_{}", self.break_label_cnt);
        let head_label = format!("head_{}", self.break_label_cnt);
        self.break_label_cnt += 1;

        let mut cond_stmts = vec![];
        comment!(self, cond_stmts, "while".into());
        cond_stmts.push(Statement::Label(head_label.clone()));

        let (mut stmts, cond) = match self.expression(node.cond) {
            (stmts, Some(expr)) => (stmts, expr),
            (stmts, None) => return stmts,
        };
        cond_stmts.append(&mut stmts);

        let cond_check = Statement::If(IfStmt {
            cond: Expression::Prefix(Box::new(PrefixExpr {
                expr: Expression::Grouped(Box::new(cond)),
                op: PrefixOp::BoolNot,
            })),
            then_block: vec![Statement::Goto(break_label.clone())],
            else_block: None,
        });

        cond_stmts.push(cond_check);

        self.loops.push(Loop {
            head_label: head_label.clone(),
            break_label: break_label.to_string(),
        });

        self.scopes.push(Scope::new());
        let mut body = match self.block_expr(node.block, false) {
            (mut stmts, Some(expr)) => {
                stmts.push(Statement::Expr(expr));
                stmts
            }
            (stmts, None) => stmts,
        };
        body.append(&mut stmts);
        cond_stmts.append(&mut body);
        cond_stmts.push(self.pop_scope(false));

        self.loops.pop();

        cond_stmts.push(Statement::Goto(head_label));
        cond_stmts.push(Statement::Label(break_label));
        cond_stmts.push(self.pop_scope(true));

        cond_stmts
    }
}
