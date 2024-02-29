use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    panic::Location,
};

use hpi_analyzer::{AssignOp, Type};

use crate::{
    c_ast::{
        ArrayExpr, AssignStmt, CType, CTypeKind, CallExpr, Expression, PrefixExpr, PrefixOp,
        Statement, StructExpr, VarDeclaration,
    },
    Transpiler,
};
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub(super) struct Scope<'src> {
    pub(super) variables: HashMap<&'src str, String>,
    pub(super) gc_roots: Vec<String>,
}

impl<'src> Scope<'src> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            gc_roots: Vec::new(),
        }
    }
}

impl<'src> Transpiler<'src> {
    pub(super) fn malloc(&mut self, type_: Type, gc_tracing: bool) -> Expression {
        match gc_tracing {
            false => {
                Expression::Call(Box::new(CallExpr {
                    func: "malloc".to_string(),
                    args: vec![Expression::Call(Box::new(CallExpr {
                        func: "sizeof".to_string(),
                        args: vec![Expression::TypeExpr(type_.into())],
                    }))],
                    // func: "gc_alloc".to_string(),
                    // args: vec![
                    //     // Expression::Call(Box::new(CallExpr {
                    //     //     func: todo!(),
                    //     //     args: todo!(),
                    //     // }))
                    //     Expression::Ident(self.get_type_reflector(type_)),
                    // ],
                }))
            }
            true => Expression::Call(Box::new(CallExpr {
                func: "gc_alloc".to_string(),
                args: vec![Expression::Ident(
                    self.get_type_reflector(type_),
                )],
            })),
        }
    }

    pub(super) fn gc_add_root(
        &mut self,
        expr: Expression,
        type_: Type,
    ) -> (Vec<Statement>, Expression) {
        // Create a variable for this expression.
        let mut stmts = vec![];

        let ident = format!("gc_root_temp{}", self.let_cnt);
        self.let_cnt += 1;

        stmts.push(Statement::VarDeclaration(VarDeclaration {
            name: ident.clone(),
            type_: type_.clone().into(),
            expr: expr.clone(),
        }));

        stmts.push(Statement::Expr(Expression::Call(Box::new(CallExpr {
            func: "gc_add_root".to_string(),
            args: vec![
                Expression::Ident(ident.clone()),
                Expression::Ident(self.get_type_reflector(type_)),
                // Expression::Struct(StructExpr {
                //     name: "Location".to_string(),
                //     values: HashMap::from([
                //         ("line".to_string(), Expression::Int(loc.line() as i64)),
                //         ("column".to_string(), Expression::Int(loc.column() as i64))
                //     ]),
                // }),
                Expression::StringLiteral(expr.to_string()),
            ],
        }))));
        // Expression::Call(Box::new(CallExpr {
        //     func: "gc_add_root".to_string(),
        //     args: vec![
        //         Expression::Ident(ident),
        //         Expression::Ident(self.get_type_reflector(type_)),
        //     ],
        // }));

        self.scopes.last_mut().unwrap().gc_roots.push(ident.clone());

        (stmts, Expression::Ident(ident))
    }

    pub(super) fn pop_scope(&mut self, also_pop: bool) -> Statement {
        let last = if also_pop {
            self.scopes.pop().unwrap()
        } else {
            self.scopes.last().cloned().unwrap()
        };

        // Generate statement to remove roots added in this scope.
        let list_inner_length = last.gc_roots.len();
        let list_inner = last
            .gc_roots
            .into_iter()
            .map(|r| Expression::Ident(r))
            .collect();

        Statement::Expr(Expression::Call(Box::new(CallExpr {
            func: "gc_remove_roots".to_string(),
            args: vec![
                Expression::Int(list_inner_length as i64),
                Expression::Array(ArrayExpr {
                    inner_type: CType::Void(1),
                    values: list_inner,
                }),
            ],
        })))
    }

    pub(super) fn lookup_type(&self, type_: Type) -> Type {
        match type_ {
            Type::Ident(ident, ptr) => self.types[ident.as_str()].clone().with_ref(ptr),
            other => other,
        }
    }

    pub(super) fn get_type_reflector(&mut self, src_type_: Type) -> String {
        let type_ = self.lookup_type(src_type_);
        let ptr_count = type_.ptr_count();

        let c_type_kind = CTypeKind::from(&type_);

        match self.type_descriptor_map.get(&type_) {
            Some(old) => old.clone(),
            None => {
                // Generate type descriptor id
                let type_descriptor = match self.style_config.emit_readable_names {
                    true => format!(
                        "type_descriptor_{}",
                        type_.sanitized_name().replace(' ', "_")
                    ),
                    false => format!("td_{}", {
                        let mut hasher = DefaultHasher::new();
                        type_.hash(&mut hasher);
                        hasher.finish()
                    }),
                };

                self.type_descriptor_declarations
                    .push(Statement::VarDefinition(
                        type_descriptor.clone(),
                        CType::Ident(0, "TypeDescriptor".to_string()),
                    ));

                let mut inner_type_expr = Expression::Ident("NULL".to_string());

                if let Type::List(inner, _) = &type_ {
                    let ident = self.get_type_reflector(*inner.clone());
                    inner_type_expr = Expression::Prefix(Box::new(PrefixExpr {
                        expr: Expression::Ident(ident),
                        op: PrefixOp::Ref,
                    }));
                };

                comment!(
                    self,
                    self.type_descriptor_setup,
                    format!("Type descriptor `{}`", type_.sanitized_name()).into()
                );

                self.type_descriptor_setup
                    .push(Statement::Assign(AssignStmt {
                        assignee: format!("{type_descriptor}.kind"),
                        assignee_ptr_count: 0,
                        op: AssignOp::Basic,
                        expr: Expression::Ident(c_type_kind.to_string()),
                    }));

                self.type_descriptor_setup
                    .push(Statement::Assign(AssignStmt {
                        assignee: format!("{type_descriptor}.ptr_count"),
                        assignee_ptr_count: 0,
                        op: AssignOp::Basic,
                        expr: Expression::Int(match ptr_count {
                            None => 0,
                            Some(ptr) => ptr as i64,
                        }),
                    }));

                self.type_descriptor_setup
                    .push(Statement::Assign(AssignStmt {
                        assignee: format!("{type_descriptor}.list_inner"),
                        assignee_ptr_count: 0,
                        op: AssignOp::Basic,
                        expr: inner_type_expr.clone(),
                    }));

                if let Type::Object(inner, _) = type_.clone() {
                    let malloc_call = self.malloc(Type::Object(vec![], 0), false);
                    // BUG: free

                    self.type_descriptor_setup
                        .push(Statement::Assign(AssignStmt {
                            assignee: format!("{type_descriptor}.obj_fields"),
                            assignee_ptr_count: 0,
                            op: AssignOp::Basic,
                            // expr: Expression::Call(Box::new(CallExpr {
                            //     func: "hashmap_new".to_string(),
                            //     args: vec![],
                            // })),
                            expr: malloc_call,
                        }));

                    for field in inner {
                        let field_type_descriptor = self.get_type_reflector(*field.type_.clone());
                        self.type_descriptor_setup
                            .push(Statement::Expr(Expression::Call(Box::new(CallExpr {
                                func: "hashmap_insert".to_string(),
                                args: vec![
                                    Expression::Ident(format!("{type_descriptor}.obj_fields")),
                                    Expression::StringLiteral(field.key),
                                    Expression::Prefix(Box::new(PrefixExpr {
                                        expr: Expression::Ident(field_type_descriptor),
                                        op: PrefixOp::Ref,
                                    })),
                                ],
                            }))));
                    }
                }

                self.type_descriptor_map
                    .insert(type_, type_descriptor.clone());

                type_descriptor
            }
        }
    }
}
