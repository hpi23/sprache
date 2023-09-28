use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    vec,
};

use hpi_parser::{ast::*, Span};

use crate::{ast::*, Diagnostic, DiagnosticLevel, ErrorKind};

#[derive(Default, Debug)]
pub struct Analyzer<'src> {
    functions: HashMap<&'src str, Function<'src>>,
    diagnostics: Vec<Diagnostic<'src>>,
    scopes: Vec<HashMap<&'src str, Variable<'src>>>,
    curr_func_name: &'src str,
    /// Specifies the depth of loops, `break` / `continue` legal if > 0.
    loop_count: usize,
    builtin_functions: HashMap<&'static str, BuiltinFunction>,
    /// The names of all used builtin functions
    used_builtins: HashSet<&'src str>,
    /// Specifies whether there is at least one `break` statement inside the current loop.
    current_loop_is_terminated: bool,
    /// The source code of the program to be analyzed
    source: &'src str,
}

#[derive(Debug, Clone)]
struct Function<'src> {
    pub ident: Spanned<'src, &'src str>,
    pub params: Spanned<'src, Vec<Parameter<'src>>>,
    pub return_type: Spanned<'src, Type>,
    pub used: bool,
}

#[derive(Debug, Clone)]
enum ParamTypes {
    VarArgs(Type),
    Normal(Vec<Type>),
}

#[derive(Debug, Clone)]
struct BuiltinFunction {
    param_types: ParamTypes,
    return_type: Type,
}

impl BuiltinFunction {
    fn new(param_types: ParamTypes, return_type: Type) -> Self {
        Self {
            param_types,
            return_type,
        }
    }
}

#[derive(Debug)]
struct Variable<'src> {
    pub type_: Type,
    pub span: Span<'src>,
    pub used: bool,
    pub mutated: bool,
}

impl<'src> Analyzer<'src> {
    /// Creates a new [`Analyzer`].
    pub fn new(source: &'src str) -> Self {
        Self {
            builtin_functions: HashMap::from([(
                "schlummere",
                BuiltinFunction::new(ParamTypes::Normal(vec![Type::Float(0)]), Type::Nichts),
            )]),
            source,
            scopes: vec![HashMap::new()], // start with empty global scope
            ..Default::default()
        }
    }

    /// Adds a new diagnostic with the `Hint` level.
    fn hint(&mut self, message: impl Into<Cow<'static, str>>, span: Span<'src>) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticLevel::Hint,
            message,
            vec![],
            span,
            self.source,
        ))
    }

    /// Adds a new diagnostic with the `Info` level.
    fn info(
        &mut self,
        message: impl Into<Cow<'static, str>>,
        notes: Vec<Cow<'static, str>>,
        span: Span<'src>,
    ) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticLevel::Info,
            message,
            notes,
            span,
            self.source,
        ))
    }

    /// Adds a new diagnostic with the `Warning` level.
    fn warn(
        &mut self,
        message: impl Into<Cow<'static, str>>,
        notes: Vec<Cow<'static, str>>,
        span: Span<'src>,
    ) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticLevel::Warning,
            message,
            notes,
            span,
            self.source,
        ))
    }

    /// Adds a new diagnostic with the `Error` level using the specified error kind.
    fn error(
        &mut self,
        kind: ErrorKind,
        message: impl Into<Cow<'static, str>>,
        notes: Vec<Cow<'static, str>>,
        span: Span<'src>,
    ) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticLevel::Error(kind),
            message,
            notes,
            span,
            self.source,
        ))
    }

    /// Analyzes a parsed AST and returns an analyzed AST whilst emitting diagnostics.
    pub fn analyze(
        mut self,
        program: Program<'src>,
    ) -> Result<(AnalyzedProgram<'src>, Vec<Diagnostic>), Vec<Diagnostic>> {
        // visit all import statements in the beginning
        let imports = program
            .imports
            .iter()
            .map(|item| self.beantrage(item))
            .collect();

        // add all function signatures first
        for func in &program.functions {
            // check for duplicate function names
            if let Some(prev_def) = self.functions.get(func.name.inner) {
                let prev_def_span = prev_def.ident.span;
                self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Doppelte Funktionsdefinition der Funktion `{}` entdeckt.",
                        func.name.inner
                    ),
                    vec![],
                    func.name.span,
                );
                self.hint(
                    format!(
                        "Die Funktion `{}` wurde zuvor hier definiert.",
                        func.name.inner
                    ),
                    prev_def_span,
                );
            }
            self.functions.insert(
                func.name.inner,
                Function {
                    ident: func.name.clone(),
                    params: func.params.clone(),
                    return_type: func.return_type.clone(),
                    used: false,
                },
            );
        }

        // analyze global let stmts
        // `self.global(node)` has side effects that have to happen here
        #[allow(clippy::needless_collect)]
        let globals: Vec<AnalyzedLetStmt> = program
            .globals
            .into_iter()
            .map(|node| self.global(node))
            .collect();

        // then analyze each function body
        let mut functions = vec![];

        let mut bewerbung_fn = None;
        let mut einschreibung_fn = None;
        let mut studium_fn = None;

        for func in program.functions {
            let func = self.function_definition(func);
            match func.name {
                "Bewerbung" => {
                    bewerbung_fn = Some(func.block);
                }
                "Einschreibung" => {
                    einschreibung_fn = Some(func.block);
                }
                "Studium" => {
                    studium_fn = Some(func.block);
                }
                _ => functions.push(func),
            }
        }

        // pop the global scope
        let unused_globals = self.pop_scope();
        let globals: Vec<AnalyzedLetStmt> = globals
            .into_iter()
            .map(|g| AnalyzedLetStmt {
                used: !unused_globals.contains(&g.name),
                ..g
            })
            .collect();

        // check if there are any unused functions
        let unused_funcs: Vec<_> = self
            .functions
            .values()
            .filter(|func| {
                !matches!(func.ident.inner, "Bewerbung" | "Einschreibung" | "Studium")
                    && !func.ident.inner.starts_with('_')
                    && !func.used
            })
            .map(|func| {
                // set used = false in tree
                functions
                    .iter_mut()
                    .find(|func_def| func_def.name == func.ident.inner)
                    .expect("every unused function is defined")
                    .used = false;

                func.ident.clone()
            })
            .collect();

        // add warnings to unused functions
        for ident in unused_funcs {
            self.warn(
                format!("Die Funktion mit dem Namen `{}` wird nie aufgerufen.", ident.inner),
                vec![format!(
                    "Wenn dies ihre Intention ist, können Sie den Namen der Funktion zu `_{}` ändern, um diese Warnung zu unterdrücken.",
                    ident.inner,
                )
                .into()],
                ident.span,
            )
        }

        match (bewerbung_fn, einschreibung_fn, studium_fn) {
            (Some(bewerbung_fn), Some(einschreibung_fn), Some(studium_fn)) => Ok((
                AnalyzedProgram {
                    imports,
                    globals,
                    functions,
                    bewerbung_fn,
                    einschreibung_fn,
                    studium_fn,
                    used_builtins: self.used_builtins,
                },
                self.diagnostics,
            )),
            (None, _, _) => {
                self.error(
                    ErrorKind::Semantic,
                    "Fehlende Funktion `Bewerbung`.",
                    vec![
                        "Die Funktion `Bewerbung` kann wie folgt implementiert werden: `funk Bewerbung() ergibt Zeichenkette { ... }`"
                            .into(),
                    ],
                    // empty span including filename
                    program.span.start.until(program.span.start),
                );
                Err(self.diagnostics)
            }
            (_, None, _) => {
                self.error(
                    ErrorKind::Semantic,
                    "Fehlende Funktion `Einschreibung`.",
                    vec![
                        "Die Funktion `Einschreibung` kann wie folgt implementiert werden: `funk Einschreibung(Zahl Matrikelnummer) ergibt Nichts { ... }`"
                            .into(),
                    ],
                    // empty span including filename
                    program.span.start.until(program.span.start),
                );
                Err(self.diagnostics)
            }
            (_, _, None) => {
                self.error(
                    ErrorKind::Semantic,
                    "Fehlende Funktion `Studium`.",
                    vec![
                        "Die Funktion `Studium` kann wie folgt implementiert werden: `funk Studium() ergibt Nichts { ... }`"
                            .into(),
                    ],
                    // empty span including filename
                    program.span.start.until(program.span.start),
                );
                Err(self.diagnostics)
            }
        }
    }

    fn check_any(&self, typ: Type) -> bool {
        match typ {
            Type::List(inner, _) => *inner == Type::Any,
            Type::Any => true,
            _ => false,
        }
    }

    fn beantrage(&mut self, node: &BeantrageStmt<'src>) -> AnalyzedBeantrageStmt<'src> {
        match (node.value_name.inner, node.von_name.inner) {
            ("drucke", "Drucker") => {
                self.builtin_functions.insert(
                    "drucke",
                    BuiltinFunction::new(ParamTypes::VarArgs(Type::Unknown), Type::Nichts),
                );
            }
            ("geld", "Hasso") => {
                self.builtin_functions.insert(
                    "geld",
                    BuiltinFunction::new(ParamTypes::Normal(vec![]), Type::String(0)),
                );
            }
            ("http", "Netzwerk") => {
                self.builtin_functions.insert(
                    "http",
                    BuiltinFunction::new(ParamTypes::Normal(vec![
                                            Type::String(0), // method
                                            Type::String(0), // url
                                            Type::String(0), // body
                                            Type::String(1), // body dest
                    ]), Type::Int(0)),
                );
            }
            ("aufgeben", "libSAP") => {
                self.builtin_functions.insert("aufgeben", BuiltinFunction::new(ParamTypes::Normal(vec![Type::Int(0)]), Type::Never));
            },
            (value, module) => self.error(
                ErrorKind::Reference,
                format!("Dieser Antrag `{value}` von `{module}` wurde aufgrund falscher Angaben abgelehnt."),
                vec!["Vielleicht existiert diese Wert / Modul Kombination nicht.".into()],
                node.span,
            ),
        }

        AnalyzedBeantrageStmt {
            import: node.von_name.inner,
            from_module: node.von_name.inner,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Removes the current scope of the function and checks whether the
    /// variables in the scope have been used and/or mutated.
    /// Returns the names of variables which are unused and those which do
    /// not need to be mutable.
    fn pop_scope(&mut self) -> Vec<&'src str> {
        // consume / drop the scope
        let scope = self.scopes.pop().expect("is only called after a scope");

        let mut unused = vec![];

        // analyze its values for their use
        for (name, var) in scope {
            if name.len() > 1
                && name
                    .chars()
                    .nth(0)
                    .expect("every ident is at least one character")
                    != '_'
                && !name
                    .chars()
                    .nth(0)
                    .expect("every ident is at least one character")
                    .is_uppercase()
            {
                let new_name: String = name
                    .chars()
                    .enumerate()
                    .map(|(idx, char)| {
                        if idx == 0 {
                            char.to_uppercase().collect()
                        } else {
                            char.to_string()
                        }
                    })
                    .collect();

                self.warn(
                    format!("Rechtschreibfehler im Namen der Variable `{}` erkannt.", name),
                    vec![format!(
                        "Sofern dies Ihre Intention ist, ändern Sie den Namen der Variable zu `{new_name}`, um diese Warnung zu unterbinden.",
                    )
                    .into(),
                        "In den meisten Fällen sind Variablennamen Nomen, weshalb wir einfach mal provisorisch alles in Großschreibung erwarten.".into(),
                    ],
                    var.span,
                );
            }

            // allow unused values if they start with `_`
            if !name.starts_with('_') && !var.used {
                unused.push(name);
                self.warn(
                    format!("Unbenutzte Variable mit dem Namen `{}` wurde aufgespürt.", name),
                    vec![format!(
                        "Sofern dies Ihre Intention ist, ändern Sie den Namen der Variable zu `_{name}`, um diese Warnung zu unterbinden."
                    )
                    .into()],
                    var.span,
                );
            }
        }

        unused
    }

    // Returns a mutable reference to the current scope
    fn scope_mut(&mut self) -> &mut HashMap<&'src str, Variable<'src>> {
        self.scopes.last_mut().expect("only called in scopes")
    }

    fn warn_unreachable(
        &mut self,
        unreachable_span: Span<'src>,
        causing_span: Span<'src>,
        expr: bool,
    ) {
        self.warn(
            match expr {
                true => "Unereichbarer Ausdruck.",
                false => "Unereichbare Anweisung.",
            },
            vec![],
            unreachable_span,
        );
        self.hint(
            "Quelltext nach diesem Ausdruck ist unereichbar.",
            causing_span,
        );
    }

    fn global(&mut self, node: SetzeStmt<'src>) -> AnalyzedLetStmt<'src> {
        // analyze the right hand side first
        let expr_span = node.expr.span();
        let expr = self.expression(node.expr);

        // check if the expression is constant
        if !expr.constant() {
            self.error(
                ErrorKind::Semantic,
                "Initialisierung von globaler Variablen wurde als nicht konstant aufgespürt.",
                vec![
                    "Globale Variablen müssen mittels konstanten Ausdrücken initialisiert werden."
                        .into(),
                ],
                expr_span,
            );
        }

        // check if the type conflicts with the rhs
        self.type_check(
            &Spanned {
                span: expr_span,
                inner: expr.result_type(),
            },
            &node.type_,
            false,
        );

        // do not allow duplicate globals
        if let Some(prev) = self.scopes[0].get(node.name.inner) {
            let prev_span = prev.span;
            self.error(
                ErrorKind::Semantic,
                format!(
                    "Doppelte Definition der globalen Variable `{}`.",
                    node.name.inner
                ),
                vec![],
                node.name.span,
            );
            self.hint(
                format!(
                    "Die globale Variable `{}` wurde zuvor hier definiert.",
                    node.name.inner
                ),
                prev_span,
            );
        } else {
            self.scopes[0].insert(
                node.name.inner,
                Variable {
                    // use `{unknown}` type for non-constant globals to prevent further misleading
                    // warnings
                    type_: match expr.constant() {
                        true => node.type_.inner,
                        false => Type::Unknown,
                    },
                    span: node.name.span,
                    used: false,
                    mutated: false,
                },
            );
        }

        AnalyzedLetStmt {
            name: node.name.inner,
            expr,
            used: false,
        }
    }

    fn function_definition(
        &mut self,
        node: FunctionDefinition<'src>,
    ) -> AnalyzedFunctionDefinition<'src> {
        // set the function name
        self.curr_func_name = node.name.inner;

        match node.name.inner {
            "Bewerbung" => {
                // the function must have 0 parameters
                if !node.params.inner.is_empty() {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Bewerbung` muss genau 0 Parameter besitzen, es wurden allerdings {} definiert.",
                        node.params.inner.len(),
                    ),
                    vec!["Entfernen Sie die Parameter: `funk Bewerbung() ergibt Zeichenkette { ... }`".into()],
                    node.params.span,
                );
                }

                // the `bewerbung` function must return `Zeichenkette`
                if node.return_type.inner != Type::String(0) {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Bewerbung` muss einen Wert des Datentyps `Zeichenkette` ergeben.\n Es wurde der Datentyp `{}` aufgespürt.",
                        node.return_type.inner,
                    ),
                    vec!["Das HPI muss ihr Bewerbungsschreiben schließlich bewerten können.".into()],
                    node.return_type.span,
                );
                }
            }
            "Einschreibung" => {
                // the function must have 1 parameter
                if node.params.inner.len() != 1 {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Einschreibung` muss genau einen Parameter besitzen, es wurden allerdings {} definiert.",
                        node.params.inner.len(),
                    ),
                    vec!["Entfernen Sie alle unnötigen Parameter oder fügen Sie welche hinzu: `funk Einschreibung(Zahl Matrikelnummer) ergibt Nichts { ... }`".into()],
                    node.params.span,
                );
                } else if node.params.inner[0].type_.inner != Type::Int(0) {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Einschreibung` muss genau einen Parameter des Datentyps `Zahl` besitzen.\n Es wurde allerdings ein Parameter des Datentyps `{}` aufgespürt.",
                        node.params.inner[0].type_.inner,
                    ),
                    vec![
                    "Entfernen Sie alle unnötigen Parameter oder fügen Sie welche hinzu.".into(),
                    "Hier ein Beispiel: `funk Einschreibung(Zahl Matrikelnummer) ergibt Nichts { ... }`".into(),
                    "Wenn Sie sich am HPI immatrikulieren, erhalten Sie ja schließlich auch eine numerische Matrikelnummer.".into(),
                    ],
                    node.params.span,
                );
                } else if node.params.inner[0].name.inner != "Matrikelnummer" {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Einschreibung` muss genau einen Parameter mit dem Namen `Matrikelnummer` besitzen.\n Es wurde allerdings ein Parameter mit dem Namen `{}` aufgespürt.",
                        node.params.inner[0].name.inner,
                    ),
                    vec![
                    "Benennen Sie ihren aktuellen Parameter einfach in `Matrikelnummer` um.".into(),
                    "Hier ein Beispiel: `funk Einschreibung(Zahl Matrikelnummer) ergibt Nichts { ... }`".into(),
                    "Wenn Sie sich an der UP 🤮 immatrikulieren, erhalten Sie ja schließlich auch eine numerische MATRIKELNUMMER.".into(),
                    ],
                    node.params.span,
                );
                }

                // the `Einschreibung` function must return `Nichts`
                if node.return_type.inner != Type::Nichts {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Einschreibung` darf keinen Wert zurückgeben, daher muss der Datentyp `Nichts` sein.\n Es wurde der Datentyp `{}` aufgespürt.",
                        node.return_type.inner,
                    ),
                    vec!["Das HPI möchte jetzt von Ihnen nichts mehr.".into()],
                    node.return_type.span,
                );
                }
            }
            "Studium" => {
                // the function must have 0 parameters
                if !node.params.inner.is_empty() {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Studium` darf keine Parameter besitzen, es wurden allerdings {} definiert.",
                        node.params.inner.len(),
                    ),
                    vec!["Entfernen Sie alle unnötigen Parameter.".into(), "Hier ein Beispiel: `funk Studium() ergibt Nichts { ... }`".into()],
                    node.params.span,
                );
                }

                // the `Studium` function must return `Nichts`
                if node.return_type.inner != Type::Nichts {
                    self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Die Funktion `Studium` darf keinen Wert zurückgeben, daher muss der Datentyp `Nichts` sein.\n Es wurde der Datentyp `{}` aufgespürt.",
                        node.return_type.inner,
                    ),
                    vec!["Das HPI möchte jetzt von Ihnen nichts mehr.".into()],
                    node.return_type.span,
                );
                }
            }
            _ => {}
        }

        // push a new scope for the new function
        self.push_scope();

        // check the function parameters
        let mut params = vec![];
        let mut param_names = HashSet::new();

        // only analyze parameters if this is not a main function
        for param in node.params.inner {
            // check for duplicate function parameters
            if !param_names.insert(param.name.inner)
                && !matches!(node.name.inner, "Bewerbung" | "Studium")
            {
                self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Doppelte Parameter mit dem Namen `{}` aufgespürt.",
                        param.name.inner
                    ),
                    vec![],
                    param.name.span,
                );
            }
            self.scope_mut().insert(
                param.name.inner,
                Variable {
                    type_: param.type_.inner.clone(),
                    span: param.name.span,
                    used: false,
                    mutated: false,
                },
            );
            params.push(AnalyzedParameter {
                name: param.name.inner,
                type_: param.type_.inner,
            });
        }

        // analyze the function body
        let block_result_span = node.block.result_span();
        let block = self.block(node.block, false);

        // check that the block results in the expected type
        self.type_check(
            &node.return_type,
            &Spanned {
                span: block_result_span,
                inner: block.result_type.clone(),
            },
            true,
        );

        // drop the scope when finished
        self.pop_scope();

        // issue a warning if there are more than 5 parameters
        if params.len() > 5 {
            self.warn(
                "Diese Funktion nutzt mehr als sechs Parameter, nun wird es gottlos.".to_string(),
                vec![
                    "Diese Warnung existiert, weil so viele Parameter einfach hässlich sind."
                        .into(),
                ],
                node.params.span,
            )
        }

        AnalyzedFunctionDefinition {
            used: true, // is modified in Self::analyze()
            name: node.name.inner,
            params,
            return_type: node.return_type.inner,
            block,
        }
    }

    fn block(&mut self, node: Block<'src>, new_scope: bool) -> AnalyzedBlock<'src> {
        if new_scope {
            self.push_scope();
        }

        let mut stmts = vec![];

        let mut never_type_span = None;
        let mut warned_unreachable = false;

        for stmt in node.stmts {
            if let Some(span) = never_type_span {
                if !warned_unreachable {
                    self.warn("Unereichbare Anweisung.", vec![], stmt.span());
                    self.hint(
                        "Programmzeilen nach dieser Anweisung ist unereichbar.",
                        span,
                    );
                    warned_unreachable = true;
                }
            }
            let stmt_span = stmt.span();
            if let Some(stmt) = self.statement(stmt) {
                if stmt.result_type() == Type::Never {
                    never_type_span = Some(stmt_span);
                }
                stmts.push(stmt);
            }
        }

        // possibly mark trailing expression as unreachable
        if let (Some(expr), Some(span), false) = (&node.expr, never_type_span, warned_unreachable) {
            self.warn("Unereichbarer Ausdruck.", vec![], expr.span());
            self.hint(
                "Programmzeilen nach diesem Ausdruck sind unereichbar.",
                span,
            );
        }

        // analyze the expression
        let expr = node.expr.clone().map(|expr| self.expression(expr));

        // result type is `!` when any statement had type `!`, otherwise the type of the expr
        let result_type = match never_type_span {
            Some(_) => Type::Never,
            // None => expr.clone().map_or(Type::Nichts, |expr| expr.result_type().clone()),
            None => expr
                .as_ref()
                .map_or(Type::Nichts, |expr| expr.result_type()),
        };

        if new_scope {
            self.pop_scope();
        }

        AnalyzedBlock {
            result_type,
            stmts,
            expr,
        }
    }

    /// Analyzes a [`Statement`].
    /// Can return [`None`] if the statement is a `while` loop which never loops.
    fn statement(&mut self, node: Statement<'src>) -> Option<AnalyzedStatement<'src>> {
        Some(match node {
            Statement::Setze(node) => self.setze_stmt(node),
            Statement::Aendere(node) => self.aendere_stmt(node),
            Statement::Ueberweise(node) => self.return_stmt(node),
            Statement::Solange(node) => return self.while_stmt(node),
            Statement::Abbrechen(node) => self.break_stmt(node),
            Statement::Weitermachen(node) => self.continue_stmt(node),
            Statement::Expr(node) => AnalyzedStatement::Expr(self.expression(node.expr)),
        })
    }

    fn type_check(
        &mut self,
        lhs_type: &Spanned<'src, Type>,
        rhs_type: &Spanned<'src, Type>,
        is_function_return_value: bool,
    ) -> Type {
        match (&lhs_type.inner, &rhs_type.inner) {
            (_, rhs @ Type::Unknown | rhs @ Type::Never) => rhs.clone(),
            (Type::List(linner, mut lptr), Type::List(rinner, mut rptr)) if lptr == rptr => {
                let mut linner = *linner.clone();
                let mut rinner = *rinner.clone();
                let mut fail = false;

                loop {
                    if let Type::List(typ, ptr) = linner {
                        linner = *typ;
                        lptr = ptr;
                    } else {
                        break;
                    }

                    if let Type::List(typ, ptr) = rinner {
                        rinner = *typ;
                        rptr = ptr
                    } else {
                        break;
                    }

                    if lptr != rptr || linner != rinner {
                        fail = true;
                        break;
                    }
                }

                if fail || linner != rinner {
                    match (linner, rinner) {
                        (typ @ Type::Unknown | typ @ Type::Never, _)
                        | (_, typ @ Type::Unknown | typ @ Type::Never) => typ,
                        (_, _) => {
                            self.error(
                                ErrorKind::Type,
                                format!(
                                    "Datentypkonflikt: erwartetete `{}`, `{}` wurde aufgespürt.",
                                    lhs_type.inner, rhs_type.inner,
                                ),
                                vec![],
                                lhs_type.span,
                            );
                            if lhs_type.span != Span::dummy() {
                                if is_function_return_value {
                                    self.hint(
                                        "Der Funktionsrückgabewert in Frage wurde hier definiert.",
                                        rhs_type.span,
                                    );
                                } else {
                                    self.hint(
                                        format!(
                                            "Erwarte Datentyp `{}` aufgrund dieser Definition.",
                                            rhs_type.inner,
                                        ),
                                        rhs_type.span,
                                    );
                                }
                            }
                            Type::Unknown
                        }
                    }
                } else {
                    lhs_type.inner.clone()
                }

                // match (&**l_inner, &**r_inner) {
                //     (_, rhs @ Type::Unknown | rhs @ Type::Never) => rhs.clone(),
                //     (lhs, rhs) if lhs != rhs => {
                //         self.error(
                //             ErrorKind::Type,
                //             format!(
                //                 "Datentypkonflikt: erwartetete `{}`, `{}` wurde aufgespürt.",
                //                 lhs, rhs,
                //             ),
                //             vec![],
                //             lhs_type.span,
                //         );
                //         if lhs_type.span != Span::dummy() {
                //             if is_function_return_value {
                //                 self.hint(
                //                     "Der Funktionsrückgabewert in Frage wurde hier definiert.",
                //                     rhs_type.span,
                //                 );
                //             } else {
                //                 self.hint(
                //                     format!(
                //                         "Erwarte Datentyp `{}` aufgrund dieser Definition.",
                //                         rhs,
                //                     ),
                //                     rhs_type.span,
                //                 );
                //             }
                //         }
                //
                //         Type::Unknown
                //     }
                //     (_, rhs) => rhs.clone(),
                // }
            }
            (lhs, rhs) if lhs != rhs => {
                self.error(
                    ErrorKind::Type,
                    format!(
                        "Datentypkonflikt: erwartetete `{}`, `{}` wurde aufgespürt.",
                        lhs, rhs,
                    ),
                    vec![],
                    lhs_type.span,
                );

                if lhs_type.span != Span::dummy() {
                    if is_function_return_value {
                        self.hint(
                            "Der Funktionsrückgabewert in Frage wurde hier definiert.",
                            rhs_type.span,
                        );
                    } else {
                        self.hint(
                            format!("Erwarte Datentyp `{}` aufgrund dieser Definition.", rhs,),
                            rhs_type.span,
                        );
                    }
                }

                Type::Unknown
            }
            (_, rhs) => rhs.clone(),
        }
    }

    fn setze_stmt(&mut self, node: SetzeStmt<'src>) -> AnalyzedStatement<'src> {
        // save the expression's span for later use
        let expr_span = node.expr.span();

        // analyze the right hand side first
        let expr = self.expression(node.expr);

        self.type_check(
            &Spanned {
                span: expr_span,
                inner: expr.result_type(),
            },
            &node.type_,
            false,
        );

        // warn unreachable if never type
        if expr.result_type() == Type::Never {
            self.warn_unreachable(node.span, expr_span, false);
        }

        // insert and do additional checks if variable is shadowed
        if let Some(shadowed) = self.scope_mut().insert(
            node.name.inner,
            Variable {
                type_: match node.type_.inner {
                    // map `!` to `{unknown}` to prevent misleading warnings
                    Type::Never => Type::Unknown,
                    type_ => type_,
                },
                span: node.name.span,
                used: false,
                mutated: false,
            },
        ) {
            // a previous variable is shadowed by this declaration, analyze its use
            if !shadowed.used && !node.name.inner.starts_with('_') {
                self.warn(
                    format!("Unbenutzte Variable mit dem Namen `{}` wurde aufgespürt.", node.name.inner),
                    vec![format!(
                        "Sofern dies ihre Intention ist, können Sie den Namen zu `_{}` ändern, um diese Meldung zu unterdrücken.",
                        node.name.inner
                    )
                    .into()],
                    shadowed.span,
                );
                self.hint(
                    format!("Die Variable mit dem Namen `{}` wurde hier erneut definiert / in den Schatten gestellt.", node.name.inner),
                    node.name.span,
                );
            }
        }

        AnalyzedStatement::Let(AnalyzedLetStmt {
            name: node.name.inner,
            expr,
            used: true,
        })
    }

    fn aendere_stmt(&mut self, node: AendereStmt<'src>) -> AnalyzedStatement<'src> {
        let var_type = match self
            .scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(node.assignee.inner))
        {
            Some(var) => {
                var.mutated = true;
                var.used = true;
                let mut type_ = var.type_.clone();

                for _ in 0..node.assignee_ptr_count {
                    type_ = match type_.clone().sub_deref() {
                        Some(type_) => type_,
                        None => {
                            self.error(
                                ErrorKind::Type,
                                format!("Werte des Datentyps `{type_}` können nicht dereferenziert werden."),
                                vec!["Ausschließlich Zeiger `Zeiger zu` können dereferenziert werden.".into()],
                                node.assignee.span,
                            );
                            Type::Unknown
                        }
                    };
                }

                type_
            }
            None => match self.functions.get(node.assignee.inner) {
                Some(_) => {
                    self.error(
                        ErrorKind::Type,
                        "Der Wert einer Funktion kann nicht geändert werden.",
                        vec![],
                        node.assignee.span,
                    );
                    Type::Unknown
                }
                None if node.assignee.inner.is_empty() => Type::Unknown,
                None => {
                    self.error(
                        ErrorKind::Reference,
                        format!(
                            "Nutzung von undeklarierter Variable mit dem Namen `{}`",
                            node.assignee.inner
                        ),
                        vec![],
                        node.assignee.span,
                    );
                    Type::Unknown
                }
            },
        };

        let expr_span = node.expr.span();
        let expr = self.expression(node.expr);

        let result_type = self.type_check(
            &Spanned {
                span: expr_span,
                inner: expr.result_type(),
            },
            &Spanned {
                span: node.assignee.span,
                inner: var_type,
            },
            false,
        );

        AnalyzedStatement::Aendere(AnalyzedAendereStmt {
            assignee: node.assignee.inner,
            assignee_ptr_count: node.assignee_ptr_count,
            expr,
            result_type: if matches!(result_type, Type::Unknown | Type::Never) {
                result_type
            } else {
                Type::Nichts
            },
        })
    }

    // TODO: this breaks if there is no function
    fn return_stmt(&mut self, node: UeberweiseStmt<'src>) -> AnalyzedStatement<'src> {
        // if there is an expression, visit it
        let expr_span = node.expr.as_ref().map(|expr| expr.span());
        let expr = node.expr.map(|expr| self.expression(expr));

        // get the return type based on the expr (Unit as fallback)
        let expr_type = expr
            .as_ref()
            .map_or(Type::Nichts, |expr| expr.result_type());

        if expr_type == Type::Never {
            self.warn_unreachable(
                node.span,
                expr_span.expect("the never type was caused by an expression"),
                false,
            );
        }

        let curr_fn = self.functions[self.curr_func_name].clone();

        self.type_check(
            &Spanned {
                span: expr_span.unwrap_or_else(Span::dummy),
                inner: expr_type,
            },
            &curr_fn.return_type,
            true,
        );

        AnalyzedStatement::Return(expr)
    }

    /// Analyzes a [`WhileStmt`].
    /// Will return [`None`] if the loop never iterates (condition is constant `false`)
    /// Can also return an [`AnalyzedLoopStmt`] if the expression is constant `true`.
    fn while_stmt(&mut self, node: SolangeStmt<'src>) -> Option<AnalyzedStatement<'src>> {
        let mut condition_is_const_true = false;
        let mut never_loops = false;

        let cond_span = node.cond.span();
        let cond = self.expression(node.cond);

        // check that the condition is of type bool
        if !matches!(
            cond.result_type(),
            Type::Bool(0) | Type::Never | Type::Unknown
        ) {
            self.error(
                ErrorKind::Type,
                format!(
                    "Erwartete einen Ausdruck mit dem Datentyp `Wahrheitswert`, spürte hingegen `{}` auf.",
                    cond.result_type()
                ),
                vec!["Eine Bedingung muss immer ein Wahrheitswert (`ja` / `nein`) sein.".into()],
                cond_span,
            )
        } else {
            // check that the condition is non-constant
            if cond.constant() {
                let cond_val = match cond {
                    AnalyzedExpression::Bool(true) => {
                        condition_is_const_true = true;
                        true
                    }
                    AnalyzedExpression::Bool(false) => {
                        never_loops = true;
                        false
                    }
                    _ => unreachable!("type is checked above and expr is constant"),
                };
                self.warn(
                    format!("Redundante Solange-Schleife: Die Bedingung im Schleifenkopf ist immer `{}`", match cond_val { true => "ja", false => "nein" }),
                    match cond_val {
                        true => vec!["Das ist jetzt aber blöd gelaufen, Sie können eigentlich auch nicht wirklich was machen?!".into()],
                        false => vec![
                            "Da die Bedingung immer `nein` ist, wird die Schleife nie iterieren.".into(),
                        ],
                    },
                    cond_span,
                )
            }
        }

        let old_loop_is_terminated = self.current_loop_is_terminated;

        self.loop_count += 1;
        let block_result_span = node.block.result_span();
        let body_is_empty = node.block.stmts.is_empty() && node.block.expr.is_none();
        let block = self.block(node.block, true);
        self.loop_count -= 1;

        if body_is_empty {
            self.warn(
                "Leerer Schleifenkörper aufgespürt.",
                vec!["Leere Schleifenköpfe verschwenden Prozessorzyklen.".into()],
                node.span,
            )
        }

        if !matches!(
            block.result_type,
            Type::Nichts | Type::Never | Type::Unknown
        ) {
            self.error(
                ErrorKind::Type,
                format!(
                    "Schleife erwartete einen block, der einen Wert des Datentyps `Nichts` oder `Nie` erzeugt,\n stattdessen wurde `{}` aufgespürt.",
                    block.result_type
                ),
                vec![],
                block_result_span,
            );
        }

        // restore loop termination count
        let never_terminates = condition_is_const_true && !self.current_loop_is_terminated;
        self.current_loop_is_terminated = old_loop_is_terminated;

        match (never_loops, condition_is_const_true) {
            // if the condition is always `false`, return nothing
            (true, _) => None,
            // otherwise, return an `AnalyzedWhileStmt`
            (_, _) => Some(AnalyzedStatement::While(AnalyzedWhileStmt {
                cond,
                block,
                never_terminates,
            })),
        }
    }

    fn break_stmt(&mut self, node: AbbrechenStmt<'src>) -> AnalyzedStatement<'src> {
        if self.loop_count == 0 {
            self.error(
                ErrorKind::Semantic,
                "Die Anweisung `abbrechen` kann nur innerhalb einer Schleife verwendet werden.",
                vec![],
                node.span,
            );
        }
        self.current_loop_is_terminated = true;
        AnalyzedStatement::Break
    }

    fn continue_stmt(&mut self, node: WeitermachenStmt<'src>) -> AnalyzedStatement<'src> {
        if self.loop_count == 0 {
            self.error(
                ErrorKind::Semantic,
                "Die Anweisung `weitermachen` kann nur innerhalb einer Schleife verwendet werden.",
                vec![],
                node.span,
            );
        }
        AnalyzedStatement::Continue
    }

    fn expression(&mut self, node: Expression<'src>) -> AnalyzedExpression<'src> {
        let node_span = node.span();
        let res = match node {
            Expression::Int(node) => AnalyzedExpression::Int(node.inner),
            Expression::Float(node) => AnalyzedExpression::Float(node.inner),
            Expression::Bool(node) => AnalyzedExpression::Bool(node.inner),
            Expression::Char(node) => {
                if node.inner > 0x7f {
                    self.error(
                        ErrorKind::Type,
                        "Zeichen ist außerhalb seines zulässigen Wertebereichs.".to_string(),
                        vec![
                            format!("Der zulässige Wertebereich ist `0x00..=0x7f`, `0x{:x}` wurde aufgespürt.", node.inner)
                                .into(),
                        ],
                        node.span,
                    )
                }
                AnalyzedExpression::Char(node.inner)
            }
            Expression::String(node) => AnalyzedExpression::String(node.inner),
            Expression::List(node) => self.list_expr(node),
            Expression::Ident(node) => self.ident_expr(node),
            Expression::Prefix(node) => self.prefix_expr(*node),
            Expression::Infix(node) => self.infix_expr(*node),
            Expression::Assign(node) => self.assign_expr(*node),
            Expression::Call(node) => self.call_expr(*node),
            Expression::Cast(node) => self.cast_expr(*node),
            Expression::Member(node) => self.member_expr(*node),
            Expression::Index(node) => self.index_expr(*node),
            Expression::If(node) => self.if_expr(*node),
            Expression::Block(node) => self.block_expr(*node),
            Expression::Grouped(node) => {
                let expr = self.expression(*node.inner);
                match expr.as_constant() {
                    Some(expr) => expr,
                    None => AnalyzedExpression::Grouped(expr.into()),
                }
            }
        };

        if self.check_any(res.result_type()) {
            self.error(
                ErrorKind::Semantic,
                "Implizite Nutzung des `Unbekannt` Datentypen: explizite Annotation erforderlich.",
                vec![
                    "Denken Sie darüber nach, den Datentypen des Ausdrucks manuell umzuwandeln."
                        .into(),
                    "... als Datentyp".into(),
                ],
                node_span,
            );
        }

        // if this is a `!` expression, count it like a loop termination
        if res.result_type() == Type::Never {
            self.current_loop_is_terminated = true;
        }

        res
    }

    fn block_expr(&mut self, node: Block<'src>) -> AnalyzedExpression<'src> {
        let block = self.block(node, true);

        match Self::eval_block(&block) {
            Some(expr) => expr,
            None => AnalyzedExpression::Block(block.into()),
        }
    }

    fn eval_block(block: &AnalyzedBlock<'src>) -> Option<AnalyzedExpression<'src>> {
        if block.stmts.iter().all(|stmt| stmt.constant()) {
            if let Some(expr) = block.expr.as_ref().and_then(|expr| expr.as_constant()) {
                return Some(expr);
            }
        }
        None
    }

    fn if_expr(&mut self, node: IfExpr<'src>) -> AnalyzedExpression<'src> {
        let cond_span = node.cond.span();
        let cond = self.expression(node.cond);

        // check that the condition is of type bool
        if !matches!(
            cond.result_type(),
            Type::Bool(0) | Type::Never | Type::Unknown
        ) {
            self.error(
                ErrorKind::Type,
                format!(
                    "Eine falls-Verzweigung benötigt eine Bedingung, die einen Wert mit dem Datentyp `Wahrheitswert` erzeugt.\n Stattdessen wurde ein Wert mit dem Datentyp `{}` aufgespürt.",
                    cond.result_type()
                ),
                vec!["Eine Bedingung muss immer ein Wahrheitswert sein.".into()],
                cond_span,
            )
        } else {
            // check that the condition is non-constant
            if cond.constant() {
                self.warn(
                    format!(
                        "Redundante falls-Verzweigung: Bedingung ist immer `{}`",
                        match cond {
                            AnalyzedExpression::Bool(true) => "ja",
                            AnalyzedExpression::Bool(false) => "nein",
                            _ => unreachable!("type is checked above and expr is constant"),
                        }
                    ),
                    vec![],
                    cond_span,
                )
            }
        }

        // analyze then_block
        let then_result_span = node.then_block.result_span();
        let then_block = self.block(node.then_block, true);

        // analyze else_block if it exists
        let result_type;
        let else_block = match node.else_block {
            Some(else_block) => {
                let else_result_span = else_block.result_span();
                let else_block = self.block(else_block, true);

                // check type equality of the `then` and `else` branches
                result_type = match (
                    then_block.clone().result_type,
                    else_block.clone().result_type,
                ) {
                    // unknown when any branch is unknown
                    (Type::Unknown, _) | (_, Type::Unknown) => Type::Unknown,
                    // never when both branches are never
                    (Type::Never, Type::Never) => Type::Never,
                    // the type of the non-never branch when one branch is never
                    (type_, Type::Never) | (Type::Never, type_) => type_,
                    // the then_type when both branches have the same type
                    (then_type, else_type) if then_type == else_type => then_type,
                    // unknown and error otherwise
                    _ => {
                        self.error(
                            ErrorKind::Type,
                            format!(
                    "Datentypkonflikt: erwartetete `{}`, `{}` wurde aufgespürt.",
                                then_block.result_type, else_block.result_type
                            ),
                            vec!["Die `falls` und `sonst` Zweige müssen Werte mit dem identischen Datentyp produzieren.".into()],
                            else_result_span,
                        );
                        self.hint(
                            "Datentyp `{then_type}` aufgrund dieser Verzweigung erwartet.",
                            then_result_span,
                        );
                        Type::Unknown
                    }
                };

                Some(else_block)
            }
            None => {
                result_type = match then_block.result_type {
                    Type::Unknown => Type::Unknown,
                    Type::Nichts | Type::Never => Type::Nichts,
                    _ => {
                        self.error(
                            ErrorKind::Type,
                            format!(
                    "Datentypkonflikt: Es fehlt ein `oder` Zweig, welcher einen Wert des Datentyps `{}` erzeugt.",
                                then_block.result_type
                            ),
                            vec![format!("Der `falls` Zweig erzeugt einen Wert des Datentyps `{}`, demnach ist ein `oder` Zweig erforderlich.", then_block.result_type).into()],
                            node.span,
                        );
                        Type::Unknown
                    }
                };

                None
            }
        };

        // evaluate constant if-exprs
        match (
            cond.as_constant(),
            Self::eval_block(&then_block),
            else_block
                .clone()
                .and_then(|block| Self::eval_block(&block)),
        ) {
            (Some(AnalyzedExpression::Bool(true)), Some(val), Some(_)) => return val,
            (Some(AnalyzedExpression::Bool(false)), Some(_), Some(val)) => return val,
            _ => {}
        }

        AnalyzedExpression::If(
            AnalyzedIfExpr {
                result_type,
                cond,
                then_block,
                else_block,
            }
            .into(),
        )
    }

    fn list_expr(
        &mut self,
        node: Spanned<'src, Vec<Expression<'src>>>,
    ) -> AnalyzedExpression<'src> {
        let type_: Option<Type> = None;

        let new_list = node
            .inner
            .iter()
            .map(move |element| {
                let expr = self.expression(element.clone());
                if let Some(old_type) = type_.clone() {
                    if old_type != expr.result_type() {
                        self.error(
                            ErrorKind::Semantic,
                            "Illegaler Datentyp in dieser Liste gefunden",
                            vec![
                                "Alle Werte in einer Liste müssen den Selben Datentyp nutzen."
                                    .into(),
                            ],
                            element.span(),
                        )
                    }
                }
                expr
            })
            .collect();

        AnalyzedExpression::List(AnalyzedListExpression { values: new_list })
    }

    /// Searches all scopes for the requested variable.
    /// Starts at the current scope (last) and works its way down to the global scope (first).
    fn ident_expr(&mut self, node: Spanned<'src, &'src str>) -> AnalyzedExpression<'src> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(var) = scope.get_mut(node.inner) {
                var.used = true;
                return AnalyzedExpression::Ident(AnalyzedIdentExpr {
                    result_type: var.type_.clone(),
                    ident: node.inner,
                });
            };
        }

        // ignore empty identifiers (cannot be created by users)
        if !node.inner.is_empty() {
            self.error(
                ErrorKind::Reference,
                format!(
                    "Unzulässliche Nutzung einer undefinierten Variable mit dem Namen `{}`.",
                    node.inner
                ),
                vec![],
                node.span,
            );
        }

        AnalyzedExpression::Ident(AnalyzedIdentExpr {
            result_type: Type::Unknown,
            ident: node.inner,
        })
    }

    fn prefix_expr(&mut self, node: PrefixExpr<'src>) -> AnalyzedExpression<'src> {
        let expr_span = node.expr.span();
        let expr = self.expression(node.expr);

        let result_type = match node.op {
            PrefixOp::Not => match expr.result_type() {
                Type::Bool(0) => Type::Bool(0),
                Type::Int(0) => Type::Int(0),
                Type::Unknown => Type::Unknown,
                Type::Never => {
                    self.warn_unreachable(node.span, expr_span, true);
                    Type::Never
                }
                _ => {
                    self.error(
                        ErrorKind::Type,
                        format!(
                            "Der Präfixoperator `!` kann nicht auf Werten des Datentyps `{}` angewandt werden.",
                            expr.result_type()
                        ),
                        vec![],
                        node.span,
                    );
                    Type::Unknown
                }
            },
            PrefixOp::Neg => match expr.result_type() {
                Type::Int(0) => Type::Int(0),
                Type::Float(0) => Type::Float(0),
                Type::Unknown => Type::Unknown,
                Type::Never => {
                    self.warn_unreachable(node.span, expr_span, true);
                    Type::Never
                }
                _ => {
                    self.error(
                        ErrorKind::Type,
                        format!(
                            "Der Präfixoperator `-` kann nicht auf Werten des Datentyps `{}` angewandt werden.",
                            expr.result_type()
                        ),
                        vec![],
                        node.span,
                    );
                    Type::Unknown
                }
            },
            PrefixOp::Ref => {
                match &expr {
                    AnalyzedExpression::Ident(ident) => match ident.result_type.clone().add_ref() {
                        Some(res) => {
                            let var = self
                                .scopes
                                .iter_mut()
                                .rev()
                                .find_map(|s| s.get_mut(ident.ident))
                                .expect("variable references are valid here");

                            // references (`&`) count as mutable variable accesses
                            var.mutated = true;

                            res
                        }
                        None if ident.result_type == Type::Unknown => Type::Unknown,
                        None => {
                            self.error(
                            ErrorKind::Type,
                            format!("Variablen des Datentyps `{}` können nicht referenziert werden.", ident.result_type),
                            vec![],
                            node.span,
                        );
                            Type::Unknown
                        }
                    },
                    _ => unreachable!("parser guarantees that only identifiers are referenced"),
                }
            }
            PrefixOp::Deref => match &expr {
                // TODO: improve this
                AnalyzedExpression::Ident(ident) => {
                    match ident.result_type.clone().sub_deref() {
                        Some(res) => res,
                        None => {
                            self.error(
                            ErrorKind::Type,
                            format!("Wert des Datentyps `{}` kann nicht dereferenziert werden.", ident.result_type),
                            vec!["Nur Zeiger `Zeiger auf` können dereferenziert werden.".into()],
                            node.span,
                        );
                            dbg!(ident);
                            Type::Unknown
                        }
                    }
                }
                AnalyzedExpression::Prefix(expr) => {
                    match expr.result_type.clone().sub_deref() {
                        Some(res) => res,
                        None => {
                            self.error(
                            ErrorKind::Type,
                            format!("Wert des Datentyps `{}` kann nicht dereferenziert werden.", expr.result_type),
                            vec!["Nur Zeiger `Zeiger auf` können dereferenziert werden.".into()],
                            node.span,
                        );
                            Type::Unknown
                        }
                    }
                }
                _ => unreachable!("can only dereference identifiers or prefix expressions"),
            },
        };

        // evaluate constant expressions
        match (&expr, node.op) {
            (AnalyzedExpression::Int(num), PrefixOp::Not) => return AnalyzedExpression::Int(!num),
            (AnalyzedExpression::Int(num), PrefixOp::Neg) => {
                return AnalyzedExpression::Int(num.wrapping_neg())
            }
            (AnalyzedExpression::Float(num), PrefixOp::Neg) => {
                return AnalyzedExpression::Float(-num)
            }
            (AnalyzedExpression::Bool(bool), PrefixOp::Not) => {
                return AnalyzedExpression::Bool(!bool)
            }
            _ => {}
        }

        AnalyzedExpression::Prefix(
            AnalyzedPrefixExpr {
                result_type,
                op: node.op,
                expr,
            }
            .into(),
        )
    }

    fn infix_expr(&mut self, node: InfixExpr<'src>) -> AnalyzedExpression<'src> {
        let lhs_span = node.lhs.span();
        let rhs_span = node.rhs.span();
        let lhs = self.expression(node.lhs);
        let rhs = self.expression(node.rhs);

        let allowed_types: &[Type];
        let mut override_result_type = None;
        let mut inherits_never_type = true;
        match node.op {
            InfixOp::Plus | InfixOp::Minus => {
                allowed_types = &[Type::Int(0), Type::Char(0), Type::Float(0)];
            }
            InfixOp::Mul | InfixOp::Div => {
                allowed_types = &[Type::Int(0), Type::Float(0)];
            }
            InfixOp::Lt | InfixOp::Gt | InfixOp::Lte | InfixOp::Gte => {
                allowed_types = &[Type::Int(0), Type::Char(0), Type::Float(0)];
                override_result_type = Some(Type::Bool(0));
            }
            InfixOp::Rem | InfixOp::Shl | InfixOp::Shr | InfixOp::Pow => {
                allowed_types = &[Type::Int(0)];
            }
            InfixOp::Eq | InfixOp::Neq => {
                allowed_types = &[Type::Int(0), Type::Float(0), Type::Bool(0), Type::Char(0)];
                override_result_type = Some(Type::Bool(0));
            }
            InfixOp::BitOr | InfixOp::BitAnd | InfixOp::BitXor => {
                allowed_types = &[Type::Int(0), Type::Bool(0)];
            }
            InfixOp::And | InfixOp::Or => {
                allowed_types = &[Type::Bool(0)];
                inherits_never_type = false;
            }
        }

        let result_type = match (lhs.clone().result_type(), rhs.clone().result_type()) {
            (Type::Unknown, _) | (_, Type::Unknown) => Box::new(Type::Unknown),
            (Type::Never, Type::Never) => {
                self.warn_unreachable(node.span, lhs_span, true);
                self.warn_unreachable(node.span, rhs_span, true);
                Box::new(Type::Never)
            }
            (Type::Never, _) if inherits_never_type => {
                self.warn_unreachable(node.span, lhs_span, true);
                Box::new(Type::Never)
            }
            (_, Type::Never) if inherits_never_type => {
                self.warn_unreachable(node.span, rhs_span, true);
                Box::new(Type::Never)
            }
            (Type::Never, _) => Box::new(rhs.result_type()),
            (_, Type::Never) => Box::new(lhs.result_type()),
            (left, right) if left == right && allowed_types.contains(&left) => {
                Box::new(override_result_type.unwrap_or(left))
            }
            (left, right) if left != right => {
                self.error(
                    ErrorKind::Type,
                    format!(
                        "Die Operanten eines Infixausdrucks müssen jeweils den Selben Datentyp besitzen,\n `{left}` sowohl `{right}` wurden aufgespürt."
                    ),
                    vec![],
                    node.span,
                );
                Box::new(Type::Unknown)
            }
            (type_, _) => {
                self.error(
                    ErrorKind::Type,
                    format!(
                        "Der Infixoperator `{}` ist für Werte des Datentyps `{type_}` unzulässlich.",
                        node.op
                    ),
                    vec![],
                    node.span,
                );
                Box::new(Type::Unknown)
            }
        };

        // evaluate constant expressions
        match (&lhs, &rhs) {
            (AnalyzedExpression::Char(left), AnalyzedExpression::Char(right)) => match node.op {
                InfixOp::Plus => return AnalyzedExpression::Char(left.wrapping_add(*right) & 0x7f),
                InfixOp::Minus => {
                    return AnalyzedExpression::Char(left.wrapping_sub(*right) & 0x7f)
                }
                InfixOp::Eq => return AnalyzedExpression::Bool(left == right),
                InfixOp::Neq => return AnalyzedExpression::Bool(left != right),
                InfixOp::Lt => return AnalyzedExpression::Bool(left < right),
                InfixOp::Lte => return AnalyzedExpression::Bool(left <= right),
                InfixOp::Gt => return AnalyzedExpression::Bool(left > right),
                InfixOp::Gte => return AnalyzedExpression::Bool(left >= right),
                _ => {}
            },
            (AnalyzedExpression::Int(left), AnalyzedExpression::Int(right)) => match node.op {
                InfixOp::Plus => return AnalyzedExpression::Int(left.wrapping_add(*right)),
                InfixOp::Minus => return AnalyzedExpression::Int(left.wrapping_sub(*right)),
                InfixOp::Mul => return AnalyzedExpression::Int(left.wrapping_mul(*right)),
                InfixOp::Div if *right == 0 => self.error(
                    ErrorKind::Semantic,
                    format!("Kann nicht {left} durch 0 teilen."),
                    vec!["Fragen Sie Timo, weshalb Mathe das nicht erlaubt.".into()],
                    node.span,
                ),
                InfixOp::Div => return AnalyzedExpression::Int(left.wrapping_div(*right)),
                InfixOp::Rem if *right == 0 => self.error(
                    ErrorKind::Semantic,
                    format!(
                        "Der Rest von {left} mit einem Divisor von 0 kann nicht berechnet werden."
                    ),
                    vec!["Fragen Sie Timo, weshalb Mathe das nicht erlaubt.".into()],
                    node.span,
                ),
                InfixOp::Rem => return AnalyzedExpression::Int(left.wrapping_rem(*right)),
                InfixOp::Pow => {
                    return AnalyzedExpression::Int(if *right < 0 {
                        0
                    } else {
                        left.wrapping_pow(*right as u32)
                    })
                }
                InfixOp::Eq => return AnalyzedExpression::Bool(left == right),
                InfixOp::Neq => return AnalyzedExpression::Bool(left != right),
                InfixOp::Lt => return AnalyzedExpression::Bool(left < right),
                InfixOp::Gt => return AnalyzedExpression::Bool(left > right),
                InfixOp::Lte => return AnalyzedExpression::Bool(left <= right),
                InfixOp::Gte => return AnalyzedExpression::Bool(left >= right),
                InfixOp::Shl | InfixOp::Shr => match *right {
                    0..=63 => {
                        return AnalyzedExpression::Int(match node.op == InfixOp::Shl {
                            true => left << right,
                            false => left >> right,
                        })
                    }
                    _ => self.error(
                        ErrorKind::Semantic,
                        format!("Kann nicht um {right} verschieben."),
                        vec![
                            "Verschiebungen außerhalb des Intervalls `0..=63` sind unzulässlich."
                                .into(),
                        ],
                        node.span,
                    ),
                },
                InfixOp::BitOr => return AnalyzedExpression::Int(left | right),
                InfixOp::BitAnd => return AnalyzedExpression::Int(left & right),
                InfixOp::BitXor => return AnalyzedExpression::Int(left ^ right),
                _ => {}
            },
            (AnalyzedExpression::Float(left), AnalyzedExpression::Float(right)) => match node.op {
                InfixOp::Plus => return AnalyzedExpression::Float(left + right),
                InfixOp::Minus => return AnalyzedExpression::Float(left - right),
                InfixOp::Mul => return AnalyzedExpression::Float(left * right),
                InfixOp::Div => return AnalyzedExpression::Float(left / right),
                InfixOp::Eq => return AnalyzedExpression::Bool(left == right),
                InfixOp::Neq => return AnalyzedExpression::Bool(left != right),
                InfixOp::Lt => return AnalyzedExpression::Bool(left < right),
                InfixOp::Gt => return AnalyzedExpression::Bool(left > right),
                InfixOp::Lte => return AnalyzedExpression::Bool(left <= right),
                InfixOp::Gte => return AnalyzedExpression::Bool(left >= right),
                _ => {}
            },
            (AnalyzedExpression::Bool(left), AnalyzedExpression::Bool(right)) => match node.op {
                InfixOp::Eq => return AnalyzedExpression::Bool(left == right),
                InfixOp::Neq => return AnalyzedExpression::Bool(left != right),
                InfixOp::BitOr => return AnalyzedExpression::Bool(left | right),
                InfixOp::BitAnd => return AnalyzedExpression::Bool(left & right),
                InfixOp::BitXor => return AnalyzedExpression::Bool(left ^ right),
                InfixOp::And => return AnalyzedExpression::Bool(*left && *right),
                InfixOp::Or => return AnalyzedExpression::Bool(*left || *right),
                _ => {}
            },
            _ => {}
        }

        AnalyzedExpression::Infix(
            AnalyzedInfixExpr {
                result_type,
                lhs,
                op: node.op,
                rhs,
            }
            .into(),
        )
    }

    fn assign_type_error(&mut self, op: AssignOp, type_: Type, span: Span<'src>) -> Type {
        self.error(
            ErrorKind::Type,
            format!("Zuweisungsoperator `{op}` kann nicht auf Werten des Datentyps `{type_}` angewandt werden."),
            vec![],
            span,
        );
        Type::Unknown
    }

    fn assign_expr(&mut self, node: AssignExpr<'src>) -> AnalyzedExpression<'src> {
        let var_type = match self
            .scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(node.assignee.inner))
        {
            Some(var) => {
                var.mutated = true;
                var.used = true;
                let mut type_ = var.type_.clone();

                for _ in 0..node.assignee_ptr_count {
                    type_ = match type_.clone().sub_deref() {
                        Some(type_) => type_,
                        None => {
                            self.error(
                                ErrorKind::Type,
                                format!("Werte des Datentyps `{type_}` können nicht dereferenziert werden."),
                                vec!["Nur Zeiger `Zeiger zu` können dereferenziert werden.".into()],
                                node.assignee.span,
                            );
                            Type::Unknown
                        }
                    };
                }

                type_
            }
            None => match self.functions.get(node.assignee.inner) {
                Some(_) => {
                    self.error(
                        ErrorKind::Type,
                        "Zuweisungen zu Funktionen sind unzulässlich.",
                        vec![],
                        node.assignee.span,
                    );
                    Type::Unknown
                }
                None if node.assignee.inner.is_empty() => Type::Unknown,
                None => {
                    self.error(
                        ErrorKind::Reference,
                        format!(
                            "Nutzung einer undefinierten Variable mit dem Namen `{}`.",
                            node.assignee.inner
                        ),
                        vec![],
                        node.assignee.span,
                    );
                    Type::Unknown
                }
            },
        };

        let expr_span = node.expr.span();
        let expr = self.expression(node.expr);
        let result_type = match (node.op, var_type, expr.result_type()) {
            (_, Type::Unknown, _) | (_, _, Type::Unknown) => Type::Unknown,
            (_, _, Type::Never) => {
                self.warn_unreachable(node.span, expr_span, true);
                Type::Never
            }
            (_, left, right) if left != right => {
                self.error(
                    ErrorKind::Type,
                    format!("Datentypkonflikt: erwartete `{left}`, `{right}` wurde aufgespürt."),
                    vec![],
                    expr_span,
                );
                self.hint(
                    format!("Diese Variable hat den Datentyp `{left}`."),
                    node.assignee.span,
                );
                Type::Unknown
            }
            (AssignOp::Plus | AssignOp::Minus, _, type_)
                if ![Type::Int(0), Type::Float(0), Type::Char(0)].contains(&type_) =>
            {
                self.assign_type_error(node.op, type_, expr_span)
            }
            (AssignOp::Mul | AssignOp::Div, _, type_)
                if ![Type::Int(0), Type::Float(0)].contains(&type_) =>
            {
                self.assign_type_error(node.op, type_, expr_span)
            }
            (AssignOp::Rem | AssignOp::Pow | AssignOp::Shl | AssignOp::Shr, _, type_)
                if type_ != Type::Int(0) =>
            {
                self.assign_type_error(node.op, type_, expr_span)
            }
            (AssignOp::BitOr | AssignOp::BitAnd | AssignOp::BitXor, _, type_)
                if ![Type::Int(0), Type::Bool(0)].contains(&type_) =>
            {
                self.assign_type_error(node.op, type_, expr_span)
            }
            (_, _, _) => Type::Nichts,
        };

        AnalyzedExpression::Assign(
            AnalyzedAssignExpr {
                result_type,
                assignee: node.assignee.inner,
                assignee_ptr_count: node.assignee_ptr_count,
                op: node.op,
                expr,
            }
            .into(),
        )
    }

    fn validate_args(
        &mut self,
        args: Spanned<'src, Vec<Expression<'src>>>,
        params: Spanned<'src, Vec<Parameter<'src>>>,
        fn_name: &str,
        span: Span<'src>,
    ) -> Vec<AnalyzedExpression<'src>> {
        if args.inner.len() != params.inner.len() {
            self.error(
                ErrorKind::Reference,
                format!(
                    "Die Funktion `{}` erwartet {} Argumente, allerdings wurde{} {} Stück übergeben.",
                    fn_name,
                    params.inner.len(),
                    if args.inner.len() == 1 { "" } else { "n" },
                    args.inner.len()
                ),
                vec![],
                span,
            );
            self.hint(
                format!(
                    "Die Funktion `{}` wurde hier mit {} Parameter{} definiert.",
                    fn_name,
                    params.inner.len(),
                    if params.inner.len() > 1 { "n" } else { "" }
                ),
                params.span,
            );
            vec![]
        } else {
            let mut result_type = Type::Never;

            args.inner
                .into_iter()
                .zip(params.inner)
                .map(|(arg, param)| self.arg(arg, &param.type_.inner, span, &mut result_type))
                .collect()
        }
    }

    fn call_expr(&mut self, node: CallExpr<'src>) -> AnalyzedExpression<'src> {
        match node.func {
            CallBase::Ident(ident) => {
                let func = match (
                    self.functions.get_mut(ident.inner),
                    self.builtin_functions.get(ident.inner),
                ) {
                    (Some(func), _) => {
                        // only mark the function as used if it is called from outside of its body
                        if self.curr_func_name != ident.inner {
                            func.used = true;
                        }
                        Some((func.return_type.inner.clone(), func.params.clone()))
                    }
                    (_, Some(builtin)) => {
                        self.used_builtins.insert(ident.inner);
                        let builtin = builtin.clone();

                        let (result_type, args) = match builtin.param_types {
                            ParamTypes::VarArgs(inner_type) => {
                                let mut result_type = builtin.return_type;
                                let args = node
                                    .args
                                    .into_iter()
                                    .map(|arg| {
                                        self.arg(arg, &inner_type, node.span, &mut result_type)
                                    })
                                    .collect();
                                (result_type, args)
                            }
                            ParamTypes::Normal(param_types) => {
                                if param_types.len() != node.args.len() {
                                    self.error(
                                        ErrorKind::Reference,
                                        format!(
                                    "Die Funktion `{}` erwaretete {} Argumente, allerdings wurde{} {} übergeben.",
                                    ident.inner,
                                    param_types.len(),
                                    if node.args.len() == 1 { "" } else {"n"},
                                    node.args.len()
                                ),
                                        vec![],
                                        node.span,
                                    );
                                    (builtin.return_type, vec![])
                                } else {
                                    let mut result_type = builtin.return_type;
                                    let args = node
                                        .args
                                        .into_iter()
                                        .zip(param_types)
                                        .map(|(arg, param_type)| {
                                            self.arg(arg, &param_type, node.span, &mut result_type)
                                        })
                                        .collect();
                                    (result_type, args)
                                }
                            }
                        };

                        return AnalyzedExpression::Call(
                            AnalyzedCallExpr {
                                result_type,
                                func: AnalyzedCallBase::Ident(ident.inner),
                                args,
                            }
                            .into(),
                        );
                    }
                    (None, None) => {
                        self.error(
                            ErrorKind::Reference,
                            format!("Nutung einer undefinerten Funktion mit dem Namen `{}`.", ident.inner),
                            vec![format!(
                                "Eine Funktion kann wie folgt definiert werden: `funk {}(...) ergibt Datentyp {{ ... }}`",
                                ident.inner,
                            )
                            .into()],
                            ident.span,
                        );
                        None
                    }
                };
                let (result_type, args) = match func {
                    Some((_, func_params)) => {
                        let mut result_type = Type::Unknown;
                        let args = node
                            .args
                            .into_iter()
                            .zip(func_params.inner)
                            .map(|(arg, param)| {
                                self.arg(arg, &param.type_.inner, node.span, &mut result_type)
                            })
                            .collect();
                        (result_type, args)
                    }
                    None => {
                        let mut result_type = Type::Unknown;
                        let args = node
                            .args
                            .into_iter()
                            .map(|arg| {
                                let arg = self.expression(arg);
                                if arg.result_type() == Type::Never {
                                    result_type = Type::Never;
                                }
                                arg
                            })
                            .collect();
                        (result_type, args)
                    }
                };

                AnalyzedExpression::Call(
                    AnalyzedCallExpr {
                        result_type,
                        func: AnalyzedCallBase::Ident(ident.inner),
                        args,
                    }
                    .into(),
                )
            }
            CallBase::Expr(expr) => {
                let func = self.expression(*expr.clone());
                match func.result_type() {
                    Type::Function {
                        params,
                        result_type: _,
                    } => {
                        let mut result_type = Type::Unknown;
                        let args = node
                            .args
                            .into_iter()
                            .zip(params)
                            .map(|(arg, param)| self.arg(arg, &param, node.span, &mut result_type))
                            .collect();

                        AnalyzedExpression::Call(Box::new(AnalyzedCallExpr {
                            result_type,
                            func: AnalyzedCallBase::Expr(Box::new(func)),
                            args,
                        }))
                    }
                    other @ Type::Never | other @ Type::Unknown => {
                        AnalyzedExpression::Call(Box::new(AnalyzedCallExpr {
                            result_type: other,
                            func: AnalyzedCallBase::Expr(Box::new(func)),
                            args: vec![],
                        }))
                    }
                    _ => {
                        self.error(
                            ErrorKind::Semantic,
                            "Dieser Ausdruck kann nicht aufgerufen werden.",
                            vec![],
                            expr.span(),
                        );

                        AnalyzedExpression::Call(Box::new(AnalyzedCallExpr {
                            result_type: Type::Unknown,
                            func: AnalyzedCallBase::Expr(Box::new(func)),
                            args: vec![],
                        }))
                    }
                }
            }
        }
    }

    fn arg(
        &mut self,
        arg: Expression<'src>,
        param_type: &Type,
        call_span: Span<'src>,
        result_type: &mut Type,
    ) -> AnalyzedExpression<'src> {
        let arg_span = arg.span();
        let arg = self.expression(arg);

        match (arg.result_type(), param_type) {
            (Type::Unknown, _) | (_, Type::Unknown) => {}
            (Type::Never, _) => {
                self.warn_unreachable(call_span, arg_span, true);
                *result_type = Type::Never;
            }
            (arg_type, param_type) => {
                self.type_check(
                    &Spanned {
                        span: arg_span,
                        inner: arg_type,
                    },
                    &Spanned {
                        span: Span::dummy(),
                        inner: param_type.clone(),
                    },
                    false,
                );
                // (arg_type, param_type) if arg_type != *param_type => self.error(
                //     ErrorKind::Type,
                //     format!(
                //         "Datentypkonflikt: erwartetete `{param_type}`, `{arg_type}` wurde aufgespürt."
                //     ),
                //     vec![],
                //     arg_span,
                // ),
                // _ => {}
            }
        }

        arg
    }

    fn cast_expr(&mut self, node: CastExpr<'src>) -> AnalyzedExpression<'src> {
        let expr_span = node.expr.span();
        let expr = self.expression(node.expr);

        let result_type = match (expr.result_type(), node.type_.inner.clone()) {
            (Type::Unknown, _) => Type::Unknown,
            (Type::Never, _) => {
                self.warn_unreachable(node.span, expr_span, true);
                Type::Never
            }
            (left, right) if left == right => {
                self.info(
                    "Unnötige Umwandlung zum selben Datentyp.",
                    vec![],
                    node.span,
                );
                node.type_.inner.clone()
            }
            (
                Type::Int(0) | Type::Float(0) | Type::Bool(0) | Type::Char(0),
                Type::Int(0) | Type::Float(0) | Type::Bool(0) | Type::Char(0),
            ) => node.type_.inner.clone(),
            (Type::List(mut linner, mut lptr), Type::List(mut rinner, mut rptr)) => {
                let mut fail = false;

                loop {
                    if let Type::List(typ, ptr) = *linner {
                        linner = typ;
                        lptr = ptr;
                    } else {
                        break;
                    }

                    if let Type::List(typ, ptr) = *rinner {
                        rinner = typ;
                        rptr = ptr
                    } else {
                        break;
                    }

                    if lptr != rptr {
                        fail = true;
                        break;
                    }
                }

                if fail {
                    match (*linner, *rinner) {
                        (typ @ Type::Unknown | typ @ Type::Never, _)
                        | (_, typ @ Type::Unknown | typ @ Type::Never) => typ,
                        (_, _) => {
                            self.error(
                    ErrorKind::Type,
                    format!(
                        "Unzulässliche Typumwandlung: der Datentyp `{}` kann nicht in `{}` umgewandelt werden.",
                        expr.result_type(),
                        node.type_.inner
                    ),
                    vec![],
                    node.span,
                );
                            Type::Unknown
                        }
                    }
                } else {
                    node.type_.inner.clone()
                }
            }
            _ => {
                self.error(
                    ErrorKind::Type,
                    format!(
                        "Unzulässliche Typumwandlung: der Datentyp `{}` kann nicht in `{}` umgewandelt werden.",
                        expr.result_type(),
                        node.type_.inner
                    ),
                    vec![],
                    node.span,
                );
                Type::Unknown
            }
        };

        // evaluate constant expressions
        match (expr.clone(), result_type) {
            (AnalyzedExpression::Int(val), Type::Int(0)) => AnalyzedExpression::Int(val),
            (AnalyzedExpression::Int(val), Type::Float(0)) => AnalyzedExpression::Float(val as f64),
            (AnalyzedExpression::Int(val), Type::Bool(0)) => AnalyzedExpression::Bool(val != 0),
            (AnalyzedExpression::Int(val), Type::Char(0)) => {
                AnalyzedExpression::Char(val.clamp(0, 127) as u8)
            }
            (AnalyzedExpression::Float(val), Type::Int(0)) => AnalyzedExpression::Int(val as i64),
            (AnalyzedExpression::Float(val), Type::Float(0)) => AnalyzedExpression::Float(val),
            (AnalyzedExpression::Float(val), Type::Bool(0)) => AnalyzedExpression::Bool(val != 0.0),
            (AnalyzedExpression::Float(val), Type::Char(0)) => {
                AnalyzedExpression::Char(val.clamp(0.0, 127.0) as u8)
            }
            (AnalyzedExpression::Bool(val), Type::Int(0)) => AnalyzedExpression::Int(val as i64),
            (AnalyzedExpression::Bool(val), Type::Float(0)) => {
                AnalyzedExpression::Float(val as u8 as f64)
            }
            (AnalyzedExpression::Bool(val), Type::Bool(0)) => AnalyzedExpression::Bool(val),
            (AnalyzedExpression::Bool(val), Type::Char(0)) => AnalyzedExpression::Char(val as u8),
            (AnalyzedExpression::Char(val), Type::Int(0)) => AnalyzedExpression::Int(val as i64),
            (AnalyzedExpression::Char(val), Type::Float(0)) => {
                AnalyzedExpression::Float(val as f64)
            }
            (AnalyzedExpression::Char(val), Type::Bool(0)) => AnalyzedExpression::Bool(val != 0),
            (AnalyzedExpression::Char(val), Type::Char(0)) => AnalyzedExpression::Char(val),
            (expr, result_type) => AnalyzedExpression::Cast(
                AnalyzedCastExpr {
                    result_type,
                    expr,
                    as_type: node.type_.inner,
                }
                .into(),
            ),
        }
    }

    fn member_expr(&mut self, node: MemberExpr<'src>) -> AnalyzedExpression<'src> {
        let expr = self.expression(node.expr);

        let members = match expr.result_type() {
            Type::AnyObject(0) => HashMap::from([
                (
                    "get",
                    Type::Function {
                        params: vec![Type::String(0)],
                        result_type: Box::new(Type::Any),
                    },
                ),
                (
                    "set",
                    Type::Function {
                        params: vec![Type::Unknown],
                        result_type: Box::new(Type::Nichts),
                    },
                ),
            ]),
            Type::List(inner, 0) => HashMap::from([
                (
                    "hinzufügen",
                    Type::Function {
                        params: vec![*inner.clone()],
                        result_type: Box::new(Type::Nichts),
                    },
                ),
                (
                    "aktualisieren",
                    Type::Function {
                        params: vec![Type::Int(0), *inner.clone()],
                        result_type: Box::new(Type::Nichts),
                    },
                ),
            ]),
            Type::Function {
                params: _,
                result_type: _,
            } => todo!(),
            Type::Nichts | Type::Never | Type::Unknown => HashMap::new(),
            _ => HashMap::new(),
        };

        match members.get(node.member.inner) {
            Some(result_type) => AnalyzedExpression::Member(Box::new(AnalyzedMemberExpr {
                result_type: result_type.clone(),
                expr,
                member: node.member.inner,
            })),
            None => {
                if !matches!(expr.result_type(), Type::Never | Type::Unknown) {
                    self.error(
                        ErrorKind::Semantic,
                        format!(
                            "Dieser Ausdruck besitzt kein Mitglied mit dem Namen `{}`.",
                            node.member.inner
                        ),
                        vec![],
                        node.span,
                    );
                }

                AnalyzedExpression::Member(Box::new(AnalyzedMemberExpr {
                    result_type: Type::Unknown,
                    expr,
                    member: node.member.inner,
                }))
            }
        }
    }

    fn index_expr(&mut self, node: IndexExpr<'src>) -> AnalyzedExpression<'src> {
        let expr = self.expression(node.expr);
        let index = self.expression(node.index);

        match (expr.result_type(), index.result_type()) {
            (Type::List(inner, 0), Type::Int(0)) => {
                AnalyzedExpression::Index(Box::new(AnalyzedIndexExpr {
                    result_type: *inner,
                    expr,
                    index,
                }))
            }
            (Type::Never, _) | (_, Type::Never) => {
                AnalyzedExpression::Index(Box::new(AnalyzedIndexExpr {
                    result_type: Type::Never,
                    expr,
                    index,
                }))
            }
            (expr_type, index_type) => {
                self.error(ErrorKind::Semantic, format!("Werte des Datentyps `{expr_type}` können nicht durch Werte des Datentyps `{index_type}` indiziert werden."), vec![], node.span);
                AnalyzedExpression::Index(Box::new(AnalyzedIndexExpr {
                    result_type: Type::Unknown,
                    expr,
                    index,
                }))
            }
        }
    }
}
