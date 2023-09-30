use std::collections::HashSet;

use hpi_parser::ast::{AssignOp, InfixOp, PrefixOp, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedProgram<'src> {
    pub imports: Vec<AnalyzedBeantrageStmt<'src>>,
    pub globals: Vec<AnalyzedLetStmt<'src>>,
    pub functions: Vec<AnalyzedFunctionDefinition<'src>>,
    pub bewerbung_fn: AnalyzedBlock<'src>,
    pub einschreibung_fn: AnalyzedBlock<'src>,
    pub studium_fn: AnalyzedBlock<'src>,
    pub used_builtins: HashSet<&'src str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedFunctionDefinition<'src> {
    pub used: bool,
    pub name: &'src str,
    pub params: Vec<AnalyzedParameter<'src>>,
    pub return_type: Type,
    pub block: AnalyzedBlock<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalyzedParameter<'src> {
    pub name: &'src str,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedBlock<'src> {
    pub result_type: Type,
    pub stmts: Vec<AnalyzedStatement<'src>>,
    pub expr: Option<AnalyzedExpression<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnalyzedStatement<'src> {
    Beantrage(AnalyzedBeantrageStmt<'src>),
    Let(AnalyzedLetStmt<'src>),
    Aendere(AnalyzedAendereStmt<'src>),
    Return(AnalyzedReturnStmt<'src>),
    While(AnalyzedWhileStmt<'src>),
    Break,
    Continue,
    Expr(AnalyzedExpression<'src>),
}

impl AnalyzedStatement<'_> {
    pub fn result_type(&self) -> Type {
        match self {
            Self::Beantrage(_) => Type::Nichts,
            Self::Let(_) => Type::Nichts,
            Self::Aendere(_) => Type::Nichts,
            Self::Return(_) => Type::Never,
            Self::While(node) => match node.never_terminates {
                true => Type::Never,
                false => Type::Nichts,
            }, // Used for detecting never-ending loops
            Self::Break => Type::Never,
            Self::Continue => Type::Never,
            Self::Expr(expr) => expr.result_type(),
        }
    }

    pub fn constant(&self) -> bool {
        match self {
            Self::Expr(expr) => expr.constant(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedBeantrageStmt<'src> {
    pub import: &'src str,
    pub from_module: &'src str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedLetStmt<'src> {
    pub name: &'src str,
    pub expr: AnalyzedExpression<'src>,
    pub used: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedAendereStmt<'src> {
    pub assignee: &'src str,
    pub assignee_ptr_count: usize,
    pub expr: AnalyzedExpression<'src>,
    pub result_type: Type,
}

pub type AnalyzedReturnStmt<'src> = Option<AnalyzedExpression<'src>>;

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedLoopStmt<'src> {
    pub block: AnalyzedBlock<'src>,
    pub never_terminates: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedWhileStmt<'src> {
    pub cond: AnalyzedExpression<'src>,
    pub block: AnalyzedBlock<'src>,
    pub never_terminates: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedForStmt<'src> {
    pub ident: &'src str,
    pub initializer: AnalyzedExpression<'src>,
    pub cond: AnalyzedExpression<'src>,
    pub update: AnalyzedExpression<'src>,
    pub block: AnalyzedBlock<'src>,
    pub never_terminates: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnalyzedExpression<'src> {
    Block(Box<AnalyzedBlock<'src>>),
    If(Box<AnalyzedIfExpr<'src>>),
    Nichts,
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(u8),
    String(String),
    List(AnalyzedListExpression<'src>),
    Ident(AnalyzedIdentExpr<'src>),
    Prefix(Box<AnalyzedPrefixExpr<'src>>),
    Infix(Box<AnalyzedInfixExpr<'src>>),
    Assign(Box<AnalyzedAssignExpr<'src>>),
    Call(Box<AnalyzedCallExpr<'src>>),
    Cast(Box<AnalyzedCastExpr<'src>>),
    Member(Box<AnalyzedMemberExpr<'src>>),
    Index(Box<AnalyzedIndexExpr<'src>>),
    Object(Box<AnalyzedObjectExpr<'src>>),
    Grouped(Box<AnalyzedExpression<'src>>),
}

impl AnalyzedExpression<'_> {
    pub fn result_type(&self) -> Type {
        match self {
            Self::Nichts => Type::Nichts,
            Self::Int(_) => Type::Int(0),
            Self::Float(_) => Type::Float(0),
            Self::Bool(_) => Type::Bool(0),
            Self::Char(_) => Type::Char(0),
            Self::String(_) => Type::String(0),
            Self::List(expr) => {
                let mut inner_type = Type::Unknown;

                if let Some(first) = expr.values.first() {
                    inner_type = first.result_type();
                }

                Type::List(Box::new(inner_type), 0)
            }
            Self::Ident(expr) => expr.result_type.clone(),
            Self::Prefix(expr) => expr.result_type.clone(),
            Self::Infix(expr) => *expr.result_type.clone(),
            Self::Assign(expr) => expr.result_type.clone(),
            Self::Call(expr) => expr.result_type.clone(),
            Self::Cast(expr) => expr.result_type.clone(),
            Self::Member(expr) => expr.result_type.clone(),
            Self::Index(expr) => expr.result_type.clone(),
            Self::If(expr) => expr.result_type.clone(),
            Self::Block(expr) => expr.result_type.clone(),
            Self::Grouped(expr) => expr.result_type(),
            Self::Object(expr) => {
                let members = expr
                    .members
                    .iter()
                    .map(|element| (element.key.clone(), element.value.result_type()))
                    .collect();
                Type::Object(members, 0)
            }
        }
    }

    pub fn constant(&self) -> bool {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Bool(_) | Self::Char(_) | Self::String(_) => true,
            AnalyzedExpression::List(inner) => !inner
                .values
                .iter()
                .map(|expr| expr.constant())
                .any(|is_constant| !is_constant),
            AnalyzedExpression::Object(inner) => {
                for val in &inner.members {
                    if !val.value.constant() {
                        return false;
                    }
                }

                true
            }
            AnalyzedExpression::Grouped(inner) => inner.constant(),
            _ => false,
        }
    }

    pub fn as_constant(&self) -> Option<Self> {
        match self {
            AnalyzedExpression::Int(_)
            | AnalyzedExpression::Float(_)
            | AnalyzedExpression::Bool(_)
            | AnalyzedExpression::Char(_) => {
                // this clone is cheap, as inner values of these variants all impl `Copy`
                Some(self.clone())
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedIfExpr<'src> {
    pub result_type: Type,
    pub cond: AnalyzedExpression<'src>,
    pub then_block: AnalyzedBlock<'src>,
    pub else_block: Option<AnalyzedBlock<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedListExpression<'src> {
    pub values: Vec<AnalyzedExpression<'src>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalyzedIdentExpr<'src> {
    pub result_type: Type,
    pub ident: &'src str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedObjectField<'src> {
    pub key: String,
    pub value: AnalyzedExpression<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedObjectExpr<'src> {
    pub members: Vec<AnalyzedObjectField<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedPrefixExpr<'src> {
    pub result_type: Type,
    pub op: PrefixOp,
    pub expr: AnalyzedExpression<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedInfixExpr<'src> {
    pub result_type: Box<Type>,
    pub lhs: AnalyzedExpression<'src>,
    pub op: InfixOp,
    pub rhs: AnalyzedExpression<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedAssignExpr<'src> {
    pub result_type: Type,
    pub assignee: &'src str,
    pub assignee_ptr_count: usize,
    pub op: AssignOp,
    pub expr: AnalyzedExpression<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnalyzedCallBase<'src> {
    Ident(&'src str),
    Expr(Box<AnalyzedExpression<'src>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedCallExpr<'src> {
    pub result_type: Type,
    pub func: AnalyzedCallBase<'src>,
    pub args: Vec<AnalyzedExpression<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedCastExpr<'src> {
    pub result_type: Type,
    pub expr: AnalyzedExpression<'src>,
    pub as_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedMemberExpr<'src> {
    pub result_type: Type,
    pub expr: AnalyzedExpression<'src>,
    pub member: &'src str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedIndexExpr<'src> {
    pub result_type: Type,
    pub expr: AnalyzedExpression<'src>,
    pub index: AnalyzedExpression<'src>,
}
