use std::{
    collections::{HashMap, HashSet, VecDeque},
    mem,
};

use hpi_analyzer::{ast::*, Type};

use crate::c_ast::*;
use crate::gc::Scope;

#[derive(Clone, Copy)]
pub struct TranspileArgs {
    /// If set to `true`, the transpiler will emit some comments in the `C` code.
    pub emit_comments: bool,
    /// If set to `true`, the compiler will generate readable type descriptors
    pub emit_readable_names: bool,
    /// If enabled, GC code is inserted into the final program.
    pub gc_enable: bool,
    /// If enabled, the GC runs a final time before exiting
    pub gc_cleanup_on_exit: bool,
}

pub struct Transpiler<'src> {
    /// Specifies whether the transpiler is currently inside the `main` fn.
    pub(super) in_main_fn: bool,
    /// The first element is the global scope while last element is the current scope.
    pub(super) scopes: Vec<Scope<'src>>,
    /// Type map created by the analyzer.
    pub(super) types: HashMap<&'src str, Type>,
    /// Maps a function's name to a mangeled name.
    pub(super) funcs: HashMap<&'src str, String>,
    /// Counter which is increased if a variable is declared.
    pub(super) let_cnt: usize,
    /// Specifies which header files need to be included.
    pub(super) required_includes: HashSet<&'static str>,
    /// Hashmap of type descriptors and their C-structs
    pub(super) type_descriptor_map: HashMap<Type, String>,
    /// List of type descriptors which need to be set up
    pub(super) type_descriptor_declarations: Vec<Statement>,
    /// Global variable setup function
    pub(super) global_variable_setup: Vec<Statement>,

    /// Type descriptor setup function
    pub(super) type_descriptor_setup: Vec<Statement>,
    /// Type descriptor teardown function
    pub(super) type_descriptor_teardown: Vec<Statement>,

    /// The first element is the most outer loop while the last element is the current loop.
    pub(super) loops: Vec<Loop>,
    /// Counter for `break` labels which is increased during loop generation.
    pub(super) break_label_cnt: usize,
    /// Specifies which functions from the corelib are required.
    pub(super) required_corelib_functions: HashSet<&'static str>,
    /// Configures code generation style and user settings.
    pub(super) user_config: TranspileArgs,
}

pub(super) struct Loop {
    pub(super) head_label: String,
    pub(super) break_label: String,
}

impl<'src> Transpiler<'src> {
    /// Creates a new [`Transpiler`].
    pub fn new(config: TranspileArgs) -> Self {
        let mut required_includes = HashSet::new();
        // usages of booleans are hard to track, therefore `stdbool.h` is always included
        required_includes.insert("./libSAP/dynstring/dynstring.h");
        required_includes.insert("stdbool.h");
        // required_includes.insert("./hpi-c-tests/list/list.h");
        // required_includes.insert("./hpi-c-tests/hashmap/map.h");

        if config.gc_enable {
            required_includes.insert("./libSAP/libGC.h");
        } else {
            required_includes.insert("./libSAP/libMem.h");
        }

        Self {
            in_main_fn: false,
            types: HashMap::new(),
            scopes: vec![Scope::new()],
            funcs: HashMap::new(),
            let_cnt: 0,
            required_includes,
            user_config: config,
            loops: vec![],
            break_label_cnt: 0,
            required_corelib_functions: HashSet::new(),
            type_descriptor_declarations: if config.emit_comments {
                vec![Statement::Comment(
                    "Type definitions for runtime 'reflection'".into(),
                )]
            } else {
                vec![]
            },
            type_descriptor_map: HashMap::new(),
            type_descriptor_setup: vec![],
            type_descriptor_teardown: vec![],
            global_variable_setup: vec![],
        }
    }

    /// Helper function for creating variable identifiers.
    /// Automatically generates an identifier and increases the `let_cnt`.
    /// Inserts the variable's name into the current scope and returns the ident.
    pub(super) fn insert_into_scope(&mut self, name: &'src str) -> String {
        let ident = format!("{name}{}", self.let_cnt);
        self.let_cnt += 1;
        self.scopes
            .last_mut()
            .expect("there is always a scope")
            .variables
            .insert(name, ident.clone());
        ident
    }

    /// Helper function for getting the mangeled name from a pure identifier.
    pub(super) fn resolve_name(&'src self, name: &str) -> &'src str {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.variables.get(name) {
                return value;
            }
        }
        unreachable!("the analyzer guarantees valid variable references")
    }

    pub fn transpile(&mut self, tree: AnalyzedProgram<'src>) -> CProgram {
        self.types = tree.types;

        let globals = tree
            .globals
            .into_iter()
            .flat_map(|g| self.let_stmt(g, true))
            .collect();

        for func in &tree.functions {
            self.fn_signature(func)
        }

        let mut functions: VecDeque<FnDefinition> = tree
            .functions
            .into_iter()
            .filter(|f| f.used)
            .map(|f| self.fn_declaration(f))
            .collect();

        let main_fn = AnalyzedBlock {
            result_type: Type::Int(0),
            stmts: vec![
                Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                    AnalyzedCallExpr {
                        result_type: Type::String(0),
                        func: AnalyzedCallBase::Ident("type_descriptor_setup"),
                        args: vec![],
                    },
                )))),
                if self.user_config.gc_enable {
                    // Initialize the garbage collector.
                    Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                        AnalyzedCallExpr {
                            result_type: Type::Nichts,
                            func: AnalyzedCallBase::Ident("gc_init"),
                            args: vec![AnalyzedExpression::Bool(
                                self.user_config.gc_cleanup_on_exit,
                            )],
                        },
                    ))))
                } else {
                    None
                },
                Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                    AnalyzedCallExpr {
                        result_type: Type::Nichts,
                        func: AnalyzedCallBase::Ident("global_variable_setup"),
                        args: vec![],
                    },
                )))),
                Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                    AnalyzedCallExpr {
                        result_type: Type::Nichts,
                        func: AnalyzedCallBase::Ident("__hpi_internal_init_libSAP"),
                        args: vec![
                            AnalyzedExpression::Ident(AnalyzedIdentExpr {
                                result_type: Type::Int(0),
                                ident: "argc",
                            }),
                            AnalyzedExpression::Ident(AnalyzedIdentExpr {
                                result_type: Type::Char(2),
                                ident: "argv",
                            }),
                        ],
                    },
                )))),
                Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                    AnalyzedCallExpr {
                        result_type: Type::String(0),
                        func: AnalyzedCallBase::Ident("bewerbung"),
                        args: vec![],
                    },
                )))),
                Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                    AnalyzedCallExpr {
                        result_type: Type::Nichts,
                        func: AnalyzedCallBase::Ident("einschreibung"),
                        args: vec![AnalyzedExpression::Call(Box::new(AnalyzedCallExpr {
                            result_type: Type::Nichts,
                            func: AnalyzedCallBase::Ident("__hpi_internal_generate_matrikelnummer"),
                            args: vec![],
                        }))],
                    },
                )))),
                Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                    AnalyzedCallExpr {
                        result_type: Type::Nichts,
                        func: AnalyzedCallBase::Ident("studium"),
                        args: vec![],
                    },
                )))),
                Some(AnalyzedStatement::Expr(AnalyzedExpression::Call(Box::new(
                    AnalyzedCallExpr {
                        result_type: Type::Nichts,
                        func: AnalyzedCallBase::Ident("cexit"),
                        args: vec![AnalyzedExpression::Int(0)],
                    },
                )))),
            ]
            .into_iter()
            .flatten()
            .collect(),
            // TODO: call exit function, do not do this
            expr: None,
        };

        functions.push_back(self.fn_declaration(AnalyzedFunctionDefinition {
            used: true,
            name: "bewerbung",
            params: vec![],
            return_type: Type::String(0),
            block: tree.bewerbung_fn,
        }));

        functions.push_back(self.fn_declaration(AnalyzedFunctionDefinition {
            used: true,
            name: "einschreibung",
            params: vec![AnalyzedParameter {
                name: "Matrikelnummer",
                type_: Type::Int(0),
            }],
            return_type: Type::Nichts,
            block: tree.einschreibung_fn,
        }));

        functions.push_back(self.fn_declaration(AnalyzedFunctionDefinition {
            used: true,
            name: "studium",
            params: vec![],
            return_type: Type::Nichts,
            block: tree.studium_fn,
        }));

        functions.push_back(FnDefinition {
            name: "type_descriptor_setup".to_string(),
            type_: Type::Nichts.into(),
            params: vec![],
            body: self.type_descriptor_setup.clone(),
        });

        functions.push_back(FnDefinition {
            name: "type_descriptor_teardown".to_string(),
            type_: Type::Nichts.into(),
            params: vec![],
            body: self.type_descriptor_teardown.clone(),
        });

        functions.push_back(FnDefinition {
            name: "global_variable_setup".to_string(),
            type_: Type::Nichts.into(),
            params: vec![],
            body: self.global_variable_setup.clone(),
        });

        functions.push_back(FnDefinition {
            name: "cexit".to_string(),
            type_: Type::Nichts.into(),
            params: vec![("code".to_string(), CType::Int(0))],
            body: vec![
                self.pop_scope(true),
                if self.user_config.gc_enable {
                    Some(Statement::Expr(Expression::Call(Box::new(CallExpr {
                        func: "gc_die".to_string(),
                        args: vec![],
                    }))))
                } else {
                    None
                },
                Some(Statement::Expr(Expression::Call(Box::new(CallExpr {
                    func: "type_descriptor_teardown".to_string(),
                    args: vec![],
                })))),
                Some(Statement::Expr(Expression::Call(Box::new(CallExpr {
                    func: "exit".to_string(),
                    args: vec![Expression::Ident("code".to_string())],
                })))),
            ]
            .into_iter()
            .flatten()
            .collect(),
        });

        // functions.push_back(FnDefinition {
        //     name: "main".to_string(),
        //     type_: CType::Int(0),
        //     params: vec![
        //         ("argc".to_string(), CType::Int(0)),
        //         ("argv".to_string(), CType::Char(2)),
        //     ],
        //     body: self.body(main_fn.clone())
        // });

        self.in_main_fn = true;
        functions.push_back(self.fn_declaration(AnalyzedFunctionDefinition {
            used: true,
            name: "main",
            params: vec![
                AnalyzedParameter {
                    name: "argc",
                    type_: Type::SystemInt(0),
                },
                AnalyzedParameter {
                    name: "argv",
                    type_: Type::Char(2),
                },
            ],
            return_type: Type::Int(0),
            block: main_fn.clone(),
        }));
        self.in_main_fn = false;

        // TODO: prettify this code
        if self
            .required_corelib_functions
            .contains("__hpi_internal_pow_int")
        {
            let (tree, _) = hpi_analyzer::analyze(include_str!("./pow.hpi"), "pow.hpi")
                .expect("this is valid HPI code");

            let func = self.fn_declaration(
                tree.functions
                    .into_iter()
                    .find(|f| f.name == "__hpi_internal_pow_int")
                    .expect("this function exists"),
            );

            functions.push_front(func)
        }

        if self
            .required_corelib_functions
            .contains("__hpi_internal_cast_float_to_char")
        {
            let (tree, _) = hpi_analyzer::analyze(include_str!("./char.hpi"), "char.hpi")
                .expect("this is valid HPI code");

            let func = self.fn_declaration(
                tree.functions
                    .into_iter()
                    .find(|f| f.name == "__hpi_internal_cast_float_to_char")
                    .expect("this function exists"),
            );

            functions.push_front(func)
        }

        if self
            .required_corelib_functions
            .contains("__hpi_internal_add_char")
        {
            let (tree, _) = hpi_analyzer::analyze(include_str!("./char.hpi"), "char.hpi")
                .expect("this is valid HPI code");

            let func = self.fn_declaration(
                tree.functions
                    .into_iter()
                    .find(|f| f.name == "__hpi_internal_add_char")
                    .expect("this function exists"),
            );

            functions.push_front(func)
        }

        if self
            .required_corelib_functions
            .contains("__hpi_internal_sub_char")
        {
            let (tree, _) = hpi_analyzer::analyze(include_str!("./char.hpi"), "char.hpi")
                .expect("this is valid HPI code");

            let func = self.fn_declaration(
                tree.functions
                    .into_iter()
                    .find(|f| f.name == "__hpi_internal_sub_char")
                    .expect("this function exists"),
            );

            functions.push_front(func)
        }

        if self
            .required_corelib_functions
            .contains("__hpi_internal_cast_int_to_char")
            || self
                .required_corelib_functions
                .contains("__hpi_internal_add_char")
            || self
                .required_corelib_functions
                .contains("__hpi_internal_sub_char")
        {
            let (tree, _) = hpi_analyzer::analyze(include_str!("./char.hpi"), "char.hpi")
                .expect("this is valid HPI code");

            let func = self.fn_declaration(
                tree.functions
                    .into_iter()
                    .find(|f| f.name == "__hpi_internal_cast_int_to_char")
                    .expect("this function exists"),
            );

            functions.push_front(func)
        }

        CProgram {
            includes: mem::take(&mut self.required_includes),
            globals,
            type_descriptors: self.type_descriptor_declarations.clone(),
            type_defs: vec![],
            functions,
        }
    }

    /// Required for adding the function prototypes first.
    /// In HPI, order of functions is irrelevant.
    /// Therefore, the `C` code must also not rely on function order.
    fn fn_signature(&mut self, node: &AnalyzedFunctionDefinition<'src>) {
        let name = format!("{name}{cnt}", name = node.name, cnt = self.funcs.len());
        self.funcs.insert(node.name, name);
    }

    fn fn_declaration(&mut self, node: AnalyzedFunctionDefinition<'src>) -> FnDefinition {
        self.scopes.push(Scope::new());

        let name = match node.name {
            "main" => "main".to_string(),
            "bewerbung" => "bewerbung".to_string(),
            "einschreibung" => "einschreibung".to_string(),
            "studium" => "studium".to_string(),
            "type_descriptor_setup" => "type_descriptor_setup".to_string(),
            "__hpi_internal_pow_int"
            | "__hpi_internal_cast_int_to_char"
            | "__hpi_internal_cast_float_to_char"
            | "__hpi_internal_add_char"
            | "__hpi_internal_sub_char" => node.name.to_string(),
            _ => self
                .funcs
                .get(node.name)
                .unwrap_or_else(|| panic!("declared previously: {}", node.name))
                .to_string(),
        };

        let params = node
            .params
            .into_iter()
            .filter_map(|p| {
                let ident = self.insert_into_scope(p.name);

                match self.lookup_type(p.type_) {
                    Type::Never => None,
                    other => Some((ident, other.into())),
                }
            })
            .collect();

        // let body = self.body(node.block);

        let mut body: Vec<Statement> = node
            .block
            .stmts
            .into_iter()
            .flat_map(|s| self.statement(s))
            .collect();

        if let Some(raw_expr) = node.block.expr.clone() {
            let (mut stmts, expr) = self.expression(raw_expr.clone());
            body.append(&mut stmts);

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

            if let Some(stmt) = self.pop_scope(true) {
                body.push(stmt);
            }
            body.append(&mut stmts);
        } else if let Some(stmt) = self.pop_scope(true) {
            body.push(stmt);
        }

        FnDefinition {
            name,
            type_: node.return_type.into(),
            params,
            body,
        }
    }
}
