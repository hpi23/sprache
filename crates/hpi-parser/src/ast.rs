use std::fmt::{self, Debug, Display, Formatter};

use crate::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int(usize),
    Float(usize),
    Bool(usize),
    Char(usize),
    String(usize),
    List(Box<Type>, usize),
    Function {
        params: Vec<Type>,
        result_type: Box<Type>,
    },
    AnyObject(usize),
    Any,
    Nichts,
    /// Internal use only, used for diverging expressions
    Never,
    /// Internal use only, used if typecheck could not determine a type
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(indirections) => write!(f, "{}Zahl", "Zeiger auf ".repeat(*indirections)),
            Self::Float(indirections) => {
                write!(f, "{}Fliesskommazahl", "Zeiger auf ".repeat(*indirections))
            }
            Self::Bool(indirections) => {
                write!(f, "{}Wahrheitswert", "Zeiger auf ".repeat(*indirections))
            }
            Self::Char(indirections) => write!(f, "{}Zeichen", "Zeiger auf ".repeat(*indirections)),
            Self::String(indirections) => {
                write!(f, "{}Zeichenkette", "Zeiger auf ".repeat(*indirections))
            }
            Self::List(inner, indirections) => {
                write!(f, "{}Liste von {}", "*".repeat(*indirections), inner)
            }
            Self::AnyObject(indirections) => {
                write!(f, "{}Speicherbox", "*".repeat(*indirections))
            }
            Self::Function {
                params,
                result_type,
            } => {
                write!(
                    f,
                    "funk({}) ergibt {result_type}",
                    params
                        .iter()
                        .map(|typ| typ.to_string())
                        .collect::<Vec<String>>()
                        .join(" / ")
                )
            }
            Self::Any => write!(f, "Unbekannt"),
            Self::Nichts => write!(f, "Nichts"),
            Self::Never => write!(f, "Niemals"),
            Self::Unknown => write!(f, "{{unknown}}"),
        }
    }
}

impl Type {
    pub fn add_ref(self) -> Option<Self> {
        Some(match self {
            Type::Int(ptr) => Type::Int(ptr + 1),
            Type::Float(ptr) => Type::Float(ptr + 1),
            Type::Bool(ptr) => Type::Bool(ptr + 1),
            Type::Char(ptr) => Type::Char(ptr + 1),
            Type::String(ptr) => Type::String(ptr + 1),
            Type::List(inner, ptr) => Type::List(inner, ptr + 1),
            _ => return None,
        })
    }

    pub fn sub_deref(self) -> Option<Self> {
        Some(match self {
            Type::Int(ptr) | Type::Float(ptr) | Type::Bool(ptr) | Type::Char(ptr) if ptr == 0 => {
                return None
            }
            Type::Int(ptr) => Type::Int(ptr - 1),
            Type::Float(ptr) => Type::Float(ptr - 1),
            Type::Bool(ptr) => Type::Bool(ptr - 1),
            Type::Char(ptr) => Type::Char(ptr - 1),
            Type::String(ptr) => Type::String(ptr - 1),
            Type::List(inner, ptr) => Type::List(inner, ptr - 1),
            _ => return None,
        })
    }

    pub fn ptr_count(&self) -> Option<usize> {
        match self {
            Type::Int(ptr) => Some(*ptr),
            Type::Float(ptr) => Some(*ptr),
            Type::Bool(ptr) => Some(*ptr),
            Type::Char(ptr) => Some(*ptr),
            Type::String(ptr) => Some(*ptr),
            Type::List(_, ptr) => Some(*ptr),
            Type::Function {
                params: _,
                result_type: _,
            } => None,
            Type::AnyObject(ptr) => Some(*ptr),
            Type::Any => None,
            Type::Nichts => None,
            Type::Never => None,
            Type::Unknown => None,
        }
    }

    pub fn without_indirection(self) -> Self {
        match self {
            Self::Int(_) => Self::Int(0),
            Self::Float(_) => Self::Float(0),
            Self::Bool(_) => Self::Bool(0),
            Self::Char(_) => Self::Char(0),
            Self::String(_) => Self::String(0),
            Self::List(inner, _) => Self::List(inner, 0),
            Self::Nichts => Self::Nichts,
            Self::AnyObject(_) => Self::AnyObject(0),
            Self::Function {
                params,
                result_type,
            } => Self::Function {
                params,
                result_type,
            },
            Self::Any => Self::Any,
            Self::Never => Self::Never,
            Self::Unknown => Self::Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<'src, T> {
    pub span: Span<'src>,
    pub inner: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'src> {
    pub span: Span<'src>,
    pub imports: Vec<BeantrageStmt<'src>>,
    pub functions: Vec<FunctionDefinition<'src>>,
    pub globals: Vec<SetzeStmt<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition<'src> {
    pub span: Span<'src>,
    pub name: Spanned<'src, &'src str>,
    pub params: Spanned<'src, Vec<Parameter<'src>>>,
    pub return_type: Spanned<'src, Type>,
    pub block: Block<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter<'src> {
    pub name: Spanned<'src, &'src str>,
    pub type_: Spanned<'src, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'src> {
    pub span: Span<'src>,
    pub stmts: Vec<Statement<'src>>,
    pub expr: Option<Expression<'src>>,
}

impl<'src> Block<'src> {
    /// Returns the span responsible for the block's result type
    pub fn result_span(&self) -> Span<'src> {
        self.expr.as_ref().map_or_else(
            || self.stmts.last().map_or(self.span, |stmt| stmt.span()),
            |expr| expr.span(),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'src> {
    Setze(SetzeStmt<'src>),
    Aendere(AendereStmt<'src>),
    Ueberweise(UeberweiseStmt<'src>),
    Solange(SolangeStmt<'src>),
    Abbrechen(AbbrechenStmt<'src>),
    Weitermachen(WeitermachenStmt<'src>),
    Expr(ExprStmt<'src>),
}

impl<'src> Statement<'src> {
    pub fn span(&self) -> Span<'src> {
        match self {
            Self::Setze(stmt) => stmt.span,
            Self::Aendere(stmt) => stmt.span,
            Self::Ueberweise(stmt) => stmt.span,
            Self::Solange(stmt) => stmt.span,
            Self::Abbrechen(stmt) => stmt.span,
            Self::Weitermachen(stmt) => stmt.span,
            Self::Expr(stmt) => stmt.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BeantrageStmt<'src> {
    pub span: Span<'src>,
    pub value_name: Spanned<'src, &'src str>,
    pub von_name: Spanned<'src, &'src str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetzeStmt<'src> {
    pub span: Span<'src>,
    pub name: Spanned<'src, &'src str>,
    pub type_: Spanned<'src, Type>,
    pub expr: Expression<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AendereStmt<'src> {
    pub span: Span<'src>,
    pub assignee: Spanned<'src, &'src str>,
    pub assignee_ptr_count: usize,
    pub expr: Expression<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UeberweiseStmt<'src> {
    pub span: Span<'src>,
    pub expr: Option<Expression<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SolangeStmt<'src> {
    pub span: Span<'src>,
    pub cond: Expression<'src>,
    pub block: Block<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AbbrechenStmt<'src> {
    pub span: Span<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WeitermachenStmt<'src> {
    pub span: Span<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt<'src> {
    pub span: Span<'src>,
    pub expr: Expression<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'src> {
    Block(Box<Block<'src>>),
    If(Box<IfExpr<'src>>),
    Int(Spanned<'src, i64>),
    Float(Spanned<'src, f64>),
    Bool(Spanned<'src, bool>),
    Char(Spanned<'src, u8>),
    String(Spanned<'src, &'src str>),
    List(Spanned<'src, Vec<Expression<'src>>>),
    Ident(Spanned<'src, &'src str>),
    Prefix(Box<PrefixExpr<'src>>),
    Infix(Box<InfixExpr<'src>>),
    Assign(Box<AssignExpr<'src>>),
    Call(Box<CallExpr<'src>>),
    Cast(Box<CastExpr<'src>>),
    Member(Box<MemberExpr<'src>>),
    Index(Box<IndexExpr<'src>>),
    Grouped(Spanned<'src, Box<Expression<'src>>>),
}

impl<'src> Expression<'src> {
    pub fn span(&self) -> Span<'src> {
        match self {
            Self::Block(expr) => expr.span,
            Self::If(expr) => expr.span,
            Self::Int(expr) => expr.span,
            Self::Float(expr) => expr.span,
            Self::Bool(expr) => expr.span,
            Self::Char(expr) => expr.span,
            Self::String(expr) => expr.span,
            Self::List(expr) => expr.span,
            Self::Ident(expr) => expr.span,
            Self::Prefix(expr) => expr.span,
            Self::Infix(expr) => expr.span,
            Self::Assign(expr) => expr.span,
            Self::Call(expr) => expr.span,
            Self::Cast(expr) => expr.span,
            Self::Member(expr) => expr.span,
            Self::Index(expr) => expr.span,
            Self::Grouped(expr) => expr.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr<'src> {
    pub span: Span<'src>,
    pub cond: Expression<'src>,
    pub then_block: Block<'src>,
    pub else_block: Option<Block<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpr<'src> {
    pub span: Span<'src>,
    pub op: PrefixOp,
    pub expr: Expression<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    /// !
    Not,
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
                PrefixOp::Not => "!",
                PrefixOp::Neg => "-",
                PrefixOp::Ref => "&",
                PrefixOp::Deref => "*",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpr<'src> {
    pub span: Span<'src>,
    pub lhs: Expression<'src>,
    pub op: InfixOp,
    pub rhs: Expression<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Mul,
    /// /
    Div,
    /// %
    Rem,
    /// *
    Pow,

    /// ==
    Eq,
    /// !=
    Neq,
    /// <
    Lt,
    /// >
    Gt,
    /// <=
    Lte,
    /// >=
    Gte,

    /// <<
    Shl,
    /// >>
    Shr,
    /// |
    BitOr,
    /// &
    BitAnd,
    /// ^
    BitXor,

    /// &&
    And,
    /// ||
    Or,
}

impl From<AssignOp> for InfixOp {
    fn from(src: AssignOp) -> Self {
        match src {
            AssignOp::Plus => InfixOp::Plus,
            AssignOp::Minus => InfixOp::Minus,
            AssignOp::Mul => InfixOp::Mul,
            AssignOp::Div => InfixOp::Div,
            AssignOp::Shl => InfixOp::Shl,
            AssignOp::Shr => InfixOp::Shr,
            AssignOp::Rem => InfixOp::Rem,
            AssignOp::Pow => InfixOp::Pow,
            AssignOp::BitOr => InfixOp::BitOr,
            AssignOp::BitAnd => InfixOp::BitAnd,
            AssignOp::BitXor => Self::BitXor,
            AssignOp::Basic => panic!("cannot convert assign op basic to infix op"),
        }
    }
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Rem => "%",
                Self::Pow => "**",
                Self::Eq => "==",
                Self::Neq => "!=",
                Self::Lt => "<",
                Self::Gt => ">",
                Self::Lte => "<=",
                Self::Gte => ">=",
                Self::Shl => "<<",
                Self::Shr => ">>",
                Self::BitOr => "|",
                Self::BitAnd => "&",
                Self::BitXor => "^",
                Self::And => "&&",
                Self::Or => "||",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr<'src> {
    pub span: Span<'src>,
    pub assignee: Spanned<'src, &'src str>,
    pub assignee_ptr_count: usize,
    pub op: AssignOp,
    pub expr: Expression<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    /// =
    Basic,
    /// +=
    Plus,
    /// -=
    Minus,
    /// *=
    Mul,
    /// /=
    Div,
    /// %=
    Rem,
    /// **=
    Pow,
    /// <<=
    Shl,
    /// >>=
    Shr,
    /// |=
    BitOr,
    /// &=
    BitAnd,
    /// ^=
    BitXor,
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}=",
            match self {
                Self::Basic => "",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Rem => "%",
                Self::Pow => "**",
                Self::Shl => "<<",
                Self::Shr => ">>",
                Self::BitOr => "|",
                Self::BitAnd => "&",
                Self::BitXor => "^",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallBase<'src> {
    Ident(Spanned<'src, &'src str>),
    Expr(Box<Expression<'src>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr<'src> {
    pub span: Span<'src>,
    pub func: CallBase<'src>,
    pub args: Vec<Expression<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr<'src> {
    pub span: Span<'src>,
    pub expr: Expression<'src>,
    pub type_: Spanned<'src, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpr<'src> {
    pub span: Span<'src>,
    pub expr: Expression<'src>,
    pub member: Spanned<'src, &'src str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr<'src> {
    pub span: Span<'src>,
    pub expr: Expression<'src>,
    pub index: Expression<'src>,
}
