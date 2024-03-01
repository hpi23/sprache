use core::fmt;
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet, VecDeque},
    fmt::{Display, Formatter},
};

use hpi_analyzer::{AssignOp, InfixOp, Type};

fn display_stmts(stmts: &[Statement]) -> String {
    stmts
        .iter()
        .map(|s| format!("    {stmt}", stmt = s.to_string().replace('\n', "\n    ")))
        .collect::<Vec<String>>()
        .join("\n")
}

/// BEGIN REFLECTION
pub enum CTypeKind {
    SystemInt,
    Int64,
    Bool,
    Char,
    String,
    Float,
    List,
    Object,
    AnyObject,
    None,
}

impl Display for CTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CTypeKind::SystemInt => "TYPE_SYS_INT",
                CTypeKind::Int64 => "TYPE_INT",
                CTypeKind::Bool => "TYPE_BOOL",
                CTypeKind::Char => "TYPE_CHAR",
                CTypeKind::String => "TYPE_STRING",
                CTypeKind::Float => "TYPE_FLOAT",
                CTypeKind::List => "TYPE_LIST",
                CTypeKind::Object => "TYPE_OBJECT",
                CTypeKind::AnyObject => "TYPE_ANY_OBJECT",
                CTypeKind::None => "TYPE_NONE",
            }
        )
    }
}

impl From<&Type> for CTypeKind {
    fn from(value: &Type) -> Self {
        match value {
            Type::SystemInt(_) => Self::SystemInt,
            Type::Int(_) => Self::Int64,
            Type::Float(_) => Self::Float,
            Type::Bool(_) => Self::Bool,
            Type::Char(_) => Self::Char,
            Type::String(_) => Self::String,
            Type::List(_, _) => Self::List,
            Type::Object(_, _) => Self::Object,
            Type::AnyObject(_) => Self::AnyObject,
            Type::Nichts => Self::None,
            Type::Unknown => Self::None,
            _ => unreachable!("These types cannot be converted: {value}"),
        }
    }
}

/// END REFLECTION

#[derive(Debug, Clone)]
pub enum CType {
    Int(usize),
    LongLongInt(usize),
    Bool(usize),
    Char(usize),
    Ident(usize, String),
    Struct(StructDefinition),
    Double(usize),
    Void(usize),
}

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub fields: HashMap<String, CType>,
}

impl CType {
    pub fn pointer_count(&self) -> usize {
        match self {
            CType::Int(ptr) => *ptr,
            CType::LongLongInt(ptr) => *ptr,
            CType::Bool(ptr) => *ptr,
            CType::Char(ptr) => *ptr,
            CType::Ident(ptr, _) => *ptr,
            CType::Struct(_) => 0,
            CType::Double(ptr) => *ptr,
            CType::Void(ptr) => *ptr,
        }
    }
}

impl Display for CType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CType::Int(ptr) => write!(f, "int{}", "*".repeat(*ptr)),
            CType::LongLongInt(ptr) => write!(f, "int64_t{}", "*".repeat(*ptr)),
            CType::Bool(ptr) => write!(f, "bool{}", "*".repeat(*ptr)),
            CType::Char(ptr) => write!(f, "char{}", "*".repeat(*ptr)),
            CType::Double(ptr) => write!(f, "double{}", "*".repeat(*ptr)),
            CType::Ident(ptr, value) => write!(f, "{value}{}", "*".repeat(*ptr)),
            CType::Struct(inner) => {
                let inner = inner
                    .fields
                    .iter()
                    .map(|(key, type_)| {
                        format!("    {} {key}", type_.to_string().replace('\n', "\n    "))
                    })
                    .collect::<Vec<String>>()
                    .join(";\n");
                write!(f, "struct {{\n{inner}\n}}")
            }
            CType::Void(ptr) => write!(f, "void{}", "*".repeat(*ptr)),
        }
    }
}

impl From<Type> for CType {
    fn from(src: Type) -> Self {
        match src {
            Type::SystemInt(ptr) => Self::Int(ptr),
            Type::Int(ptr) => Self::LongLongInt(ptr),
            Type::Float(ptr) => Self::Double(ptr),
            Type::Bool(ptr) => Self::Bool(ptr),
            Type::Char(ptr) => Self::Char(ptr),
            Type::String(ptr) => Self::Ident(ptr + 1, "DynString".to_string()),
            Type::List(_, ptr) => Self::Ident(ptr + 1, "ListNode".to_string()),
            Type::Nichts | Type::Never => Self::Void(0),
            Type::Object(_, ptr) => Self::Ident(ptr + 1, "HashMap".to_string()),
            Type::AnyObject(ptr) => Self::Ident(ptr + 1, "AnyObject".to_string()),
            Type::Unknown => panic!("tried to convert unknown type to CType"),
            other => unreachable!("Unsupported type conversion to CType: {other}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CProgram {
    pub includes: HashSet<&'static str>,
    pub type_defs: Vec<TypeDef>,
    pub globals: Vec<Statement>,
    pub type_descriptors: Vec<Statement>,
    pub functions: VecDeque<FnDefinition>,
}

impl Display for CProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let includes = match self.includes.is_empty() {
            true => String::new(),
            false => format!(
                "{}\n",
                self.includes
                    .iter()
                    .map(|s| if s.starts_with("./") || s.starts_with('/') {
                        format!("#include \"{s}\"\n")
                    } else {
                        format!("#include <{s}>\n")
                    })
                    .collect::<String>()
            ),
        };

        let type_defs = if self.type_defs.is_empty() {
            String::new()
        } else {
            self.type_defs
                .iter()
                .map(|t| format!("{t}\n"))
                .collect::<String>()
        };

        let type_descriptors = match self.type_descriptors.is_empty() {
            true => String::new(),
            false => format!(
                "{}\n",
                self.type_descriptors
                    .iter()
                    .map(|g| format!("{g}\n"))
                    .collect::<String>()
            ),
        };

        let globals = match self.globals.is_empty() {
            true => String::new(),
            false => format!(
                "{}\n",
                self.globals
                    .iter()
                    .map(|g| format!("{g}\n"))
                    .collect::<String>()
            ),
        };

        let func_signatures = match self.functions.is_empty() {
            true => String::new(),
            false => format!(
                "{}\n",
                self.functions
                    .iter()
                    .map(|f| format! {"{};\n", FnSignature::from(f)})
                    .collect::<String>()
            ),
        };

        let func_definitions = match self.functions.is_empty() {
            true => String::new(),
            false => self
                .functions
                .iter()
                .map(|f| f.to_string())
                .collect::<Vec<String>>()
                .join("\n\n"),
        };

        write!(
            f,
            "{includes}{type_defs}{type_descriptors}{globals}{func_signatures}{func_definitions}"
        )
    }
}

#[derive(Debug, Clone)]
pub struct FnSignature {
    pub name: String,
    pub type_: CType,
    pub params: Vec<(String, CType)>,
}

impl From<&FnDefinition> for FnSignature {
    fn from(value: &FnDefinition) -> Self {
        Self {
            name: value.name.clone(),
            type_: value.type_.clone(),
            params: value.params.clone(),
        }
    }
}

impl Display for FnSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|(_, type_)| type_.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(
            f,
            "{type_} {name}({params})",
            type_ = if self.name == "main" {
                CType::Int(0)
            } else {
                self.type_.clone()
            },
            name = self.name,
        )
    }
}

#[derive(Debug, Clone)]
pub struct FnDefinition {
    pub name: String,
    pub type_: CType,
    pub params: Vec<(String, CType)>,
    pub body: Vec<Statement>,
}

impl Display for FnDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|(name, type_)| format!("{type_} {name}"))
            .collect::<Vec<String>>()
            .join(", ");

        let block = display_stmts(&self.body);
        let body = match block.contains('\n') {
            true => format!("{{\n{block}\n}}"),
            false => format!("{{ {block} }}", block = block.trim_start()),
        };

        write!(
            f,
            "{type_} {name}({params}) {body}",
            type_ = if self.name == "main" {
                "int".to_string()
            } else {
                self.type_.clone().to_string()
            },
            name = self.name,
        )
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub type_: CType,
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "typedef {} {};", self.type_, self.name)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Comment(Cow<'static, str>),
    VarDeclaration(VarDeclaration),
    VarDefinition(String, CType),
    Return(Option<Expression>),
    While(WhileStmt),
    Break,
    Goto(String),
    Label(String),
    Expr(Expression),
    Assign(AssignStmt),
    If(IfStmt),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::VarDeclaration(node) => write!(f, "{node}"),
            Statement::VarDefinition(ident, type_) => {
                let new_type = match type_ {
                    CType::Void(0) => CType::Void(1),
                    _ => type_.clone(),
                };
                write!(f, "{new_type} {ident};")
            }
            Statement::Return(expr) => match expr {
                Some(expr) => write!(f, "return {expr};"),
                None => write!(f, "return;"),
            },
            Statement::While(node) => write!(f, "{node}"),
            Statement::Break => write!(f, "break;"),
            Statement::Goto(label) => write!(f, "goto {label};"),
            Statement::Label(label) => write!(f, "{label}:;"),
            Statement::Expr(expr) => write!(f, "{expr};"),
            Statement::Assign(node) => write!(f, "{node}"),
            Statement::Comment(msg) => write!(f, "// {msg}"),
            Statement::If(node) => write!(f, "{node}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub name: String,
    pub type_: CType,
    pub expr: Expression,
}

impl Display for VarDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let new_type = match self.type_ {
            CType::Void(0) => CType::Void(1),
            _ => self.type_.clone(),
        };

        write!(
            f,
            "{type_} {ident} = {expr};",
            type_ = new_type,
            ident = self.name,
            expr = self.expr
        )
    }
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: Expression,
    pub body: Vec<Statement>,
}

impl Display for WhileStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "while ({cond}) {{\n{body}\n}}",
            cond = self.cond,
            body = display_stmts(&self.body)
        )
    }
}

#[derive(Debug, Clone)]
pub struct AssignStmt {
    pub assignee: String,
    pub assignee_ptr_count: usize,
    pub op: AssignOp,
    pub expr: Expression,
}

impl Display for AssignStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{ptrs}{ident} {op} {expr};",
            ptrs = "*".repeat(self.assignee_ptr_count),
            ident = self.assignee,
            op = self.op,
            expr = self.expr
        )
    }
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Expression,
    pub then_block: Vec<Statement>,
    pub else_block: Option<Vec<Statement>>,
}

impl Display for IfStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let then_block = display_stmts(&self.then_block);

        let else_part = match &self.else_block {
            Some(stmts) => {
                format!(" else {{\n{body}\n}}", body = display_stmts(stmts))
            }
            None => String::new(),
        };

        write!(
            f,
            "if ({cond}) {{\n{then_block}\n}}{else_part}",
            cond = self.cond
        )
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    TypeExpr(CType),
    Call(Box<CallExpr>),
    Prefix(Box<PrefixExpr>),
    Infix(Box<InfixExpr>),
    Deref((usize, String)),
    Cast(Box<CastExpr>),
    Int(i64),
    SystemInt(i32),
    Bool(bool),
    Char(u8),
    StringLiteral(String),
    Float(f64),
    Ident(String),
    Member(Box<MemberExpr>),
    Grouped(Box<Expression>),
    Array(ArrayExpr),
    Struct(StructExpr),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::TypeExpr(node) => write!(f, "{node}"),
            Expression::Call(node) => write!(f, "{node}"),
            Expression::Member(node) => write!(f, "{node}"),
            Expression::Prefix(node) => write!(f, "{node}"),
            Expression::Infix(node) => write!(f, "{node}"),
            Expression::Deref((count, ident)) => {
                write!(f, "{deref}{ident}", deref = "*".repeat(*count))
            }
            Expression::Cast(node) => write!(f, "{node}"),
            Expression::Int(val) => write!(f, "{val}"),
            Expression::SystemInt(val) => write!(f, "{val}"),
            Expression::Bool(val) => write!(f, "{val}"),
            Expression::Char(val) => write!(f, "{val}"),
            Expression::StringLiteral(inner) => write!(f, "\"{inner}\""),
            Expression::Float(val) => {
                // check if number is int
                if *val == (*val as i64) as f64 {
                    write!(f, "{val:.1}")
                } else {
                    write!(f, "{val}")
                }
            }
            Expression::Ident(ident) => write!(f, "{ident}"),
            Expression::Grouped(node) => write!(f, "({node})"),
            Expression::Array(node) => write!(f, "{node}"),
            Expression::Struct(node) => write!(f, "{node}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: String,
    pub args: Vec<Expression>,
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args = self
            .args
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        let args = match args.contains('\n') {
            true => format!("(\n    {args}\n)", args = args.replace('\n', "\n    ")),
            false => format!("({args})"),
        };

        write!(f, "{func}{args}", func = self.func)
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub expr: Expression,
    pub op: PrefixOp,
}

impl Display for PrefixExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{op}{expr}", op = self.op, expr = self.expr)
    }
}

impl From<hpi_analyzer::PrefixOp> for PrefixOp {
    fn from(value: hpi_analyzer::PrefixOp) -> Self {
        match value {
            hpi_analyzer::PrefixOp::Not => unreachable!("This decision is made in the transpiler"),
            hpi_analyzer::PrefixOp::Neg => Self::Neg,
            hpi_analyzer::PrefixOp::Ref => Self::Ref,
            hpi_analyzer::PrefixOp::Deref => Self::Deref,
        }
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub lhs: Expression,
    pub rhs: Expression,
    pub op: InfixOp,
}

impl Display for InfixExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let lhs = self.lhs.to_string();
        let rhs = self.rhs.to_string();

        let split_here = lhs.split('\n').last().unwrap().len() > 80;

        match split_here {
            true => write!(f, "{lhs}\n    {op} {rhs}", op = self.op),
            false => write!(f, "{lhs} {op} {rhs}", op = self.op),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    /// !
    BoolNot,
    /// ~
    BinNot,
    /// -
    Neg,
    /// &
    Ref,
    /// *
    Deref,
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PrefixOp::BoolNot => "!",
                PrefixOp::BinNot => "~",
                PrefixOp::Neg => "-",
                PrefixOp::Ref => "&",
                PrefixOp::Deref => "*",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct CastExpr {
    pub expr: Expression,
    pub type_: CType,
}

impl Display for CastExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({type_}) {expr}", type_ = self.type_, expr = self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct MemberExpr {
    pub expr: Expression,
    pub member: String,
    pub base_is_ptr: bool,
}

impl Display for MemberExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.expr,
            if self.base_is_ptr { "->" } else { "." },
            self.member
        )
    }
}

#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub(super) inner_type: CType,
    pub(super) values: Vec<Expression>
}

impl Display for ArrayExpr {
 fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
        f,
        "({}[{}]){{{}}}",
        self.inner_type,
        self.values.len(),
        self.values.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")
    )
 }
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub(super) name: String,
    pub(super) values: HashMap<String, Expression>,
}

impl Display for StructExpr {
 fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
        f,
        "({}){{{}}}",
        self.name,
        self.values.iter().map(|(key, value)| format!(".{key} = {value}")).collect::<Vec<String>>().join(", ")
        // self.values.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")
    )
 }
}
