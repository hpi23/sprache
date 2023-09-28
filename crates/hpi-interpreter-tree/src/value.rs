use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::interpreter;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Char(u8),
    String(String),
    List(Rc<RefCell<Vec<Value>>>),
    Bool(bool),
    Unit,
    Ptr(Rc<RefCell<Value>>),
    BuiltinFunction(Box<Value>, fn(&Value, Vec<Value>) -> Value),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display())
    }
}

fn list_push(val: &Value, args: Vec<Value>) -> Value {
    match val {
        Value::List(values) => values.borrow_mut().push(args[0].clone()),
        _ => unreachable!("the analyzer prevents this: {val:?}"),
    }
    Value::Unit
}

fn list_update(val: &Value, args: Vec<Value>) -> Value {
    match (val, &args[0]) {
        (Value::List(values), &Value::Int(idx)) => {
            values.borrow_mut()[idx as usize] = args[1].clone()
        }
        (_, _) => unreachable!("the analyzer prevents this: {val:?}"),
    };
    Value::Unit
}

impl Value {
    pub fn wrapped(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }

    pub fn member(&self, member: &str) -> Value {
        match (self, member) {
            (Value::List(_), "hinzufügen") => {
                Value::BuiltinFunction(Box::new(self.clone()), list_push)
            }
            (Value::List(_), "aktualisieren") => {
                Value::BuiltinFunction(Box::new(self.clone()), list_update)
            }
            (_, _) => unreachable!("the analyzer prevents this"),
        }
    }

    fn display(&self) -> String {
        match self {
            Value::Int(inner) => inner.to_string(),
            Value::Float(inner) => inner.to_string().replace('.', ","),
            Value::Char(inner) => (*inner as char).to_string(),
            Value::String(inner) => inner.clone(),
            Value::List(inner) => format!(
                "[{}]",
                inner
                    .borrow()
                    .iter()
                    .map(|val| val.display())
                    .collect::<Vec<String>>()
                    .join(" / ")
            ),
            Value::Bool(inner) => format!("{inner}"),
            Value::Unit => "Nichts".to_string(),
            Value::BuiltinFunction(member_base, _) => "<Eingebaute-Funktion>".to_string(),
            Value::Ptr(inner) => format!("Zeiger auf {}", inner.borrow()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum InterruptKind {
    Return(Value),
    Break,
    Continue,
    Error(interpreter::Error),
    Exit(i64),
}

impl InterruptKind {
    pub fn into_value(self) -> Result<Value, InterruptKind> {
        match self {
            Self::Return(val) => Ok(val),
            kind @ (Self::Error(_) | Self::Exit(_)) => Err(kind),
            _ => Ok(Value::Unit),
        }
    }
}

impl<S> From<S> for InterruptKind
where
    S: Into<interpreter::Error>,
{
    fn from(msg: S) -> Self {
        Self::Error(msg.into())
    }
}

//////////////////////////////////

macro_rules! from_impl {
    ($variant:ident, $type:ty) => {
        impl From<$type> for Value {
            fn from(val: $type) -> Self {
                Self::$variant(val)
            }
        }

        impl From<&$type> for Value {
            fn from(val: &$type) -> Self {
                Self::from(*val)
            }
        }

        impl From<&mut $type> for Value {
            fn from(val: &mut $type) -> Self {
                Self::from(*val)
            }
        }
    };
}

from_impl!(Int, i64);
from_impl!(Float, f64);
from_impl!(Char, u8);
from_impl!(Bool, bool);

//////////////////////////////////

macro_rules! unwrap_impl {
    ($variant:ident, $res:ty, $name:ident) => {
        pub fn $name(self) -> $res {
            match self {
                Self::$variant(val) => val,
                other => panic!(concat!("called ", stringify!($name), " on `{:?}`"), other),
            }
        }
    };
}

impl Value {
    unwrap_impl!(Int, i64, unwrap_int);
    unwrap_impl!(Bool, bool, unwrap_bool);
    unwrap_impl!(Ptr, Rc<RefCell<Value>>, unwrap_ptr);
}
