use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use hpi_analyzer::Type;

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
    Speicherbox(HashMap<String, Value>),
    BuiltinFunction(Box<Value>, fn(&Value, Vec<Value>) -> Value),
}

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        value.as_type()
    }
}

impl Value {
    pub fn as_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int(0),
            Value::Float(_) => Type::Float(0),
            Value::Char(_) => Type::Char(0),
            Value::String(_) => Type::String(0),
            Value::List(values) => {
                let inner_type = values
                    .borrow()
                    .first()
                    .map_or(Type::Unknown, |val| val.as_type());
                Type::List(Box::new(inner_type), 0)
            }
            Value::Bool(_) => Type::Bool(0),
            Value::Unit => Type::Nichts,
            Value::Ptr(inner) => {
                let mut count = 1;
                let mut inner = inner.borrow().clone();
                loop {
                    if let Value::Ptr(ptr_inner) = inner {
                        inner = ptr_inner.borrow().clone();
                        count += 1;
                    } else {
                        break inner.as_type().with_ref(count);
                    }
                }
            }
            Value::Speicherbox(_) => Type::AnyObject(0),
            Value::BuiltinFunction(_, _) => unreachable!("this does not work!"),
        }
    }
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

fn list_length(val: &Value, _args: Vec<Value>) -> Value {
    match val {
        Value::List(values) => Value::Int(values.borrow().len() as i64),
        _ => unreachable!("the analyzer prevents this: {val:?}"),
    }
}

fn speicherbox_nehme(val: &Value, args: Vec<Value>) -> Value {
    match (val, &args[0]) {
        (Value::Speicherbox(inner), Value::String(key)) => match inner.get(key) {
            Some(res) => res.clone(),
            None => Value::Unit,
        },
        (_, _) => unreachable!("the analyzer prevents this: {val}"),
    }
}

fn speicherbox_datentyp_von(val: &Value, args: Vec<Value>) -> Value {
    let type_ = match (val, &args[0]) {
        (Value::Speicherbox(inner), Value::String(key)) => match inner.get(key) {
            Some(res) => res.clone(),
            None => Value::Unit,
        },
        (_, _) => unreachable!("the analyzer prevents this: {val}"),
    }.as_type();

    Value::String(type_.to_string())
}

fn value_type(val: &Value, _args: Vec<Value>) -> Value {
    Value::String(val.as_type().to_string())
}

impl Value {
    pub fn wrapped(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }

    pub fn member(&self, member: &str) -> Value {
        match (self, member) {
            (Value::List(_), "Hinzufügen") => {
                Value::BuiltinFunction(Box::new(self.clone()), list_push)
            }
            (Value::List(_), "Aktualisieren") => {
                Value::BuiltinFunction(Box::new(self.clone()), list_update)
            }
            (Value::List(_), "Länge") => {
                Value::BuiltinFunction(Box::new(self.clone()), list_length)
            }
            (Value::Speicherbox(_), "Nehmen") => {
                Value::BuiltinFunction(Box::new(self.clone()), speicherbox_nehme)
            }
            (Value::Speicherbox(_), "Datentyp_Von") => {
                Value::BuiltinFunction(Box::new(self.clone()), speicherbox_datentyp_von)
            }
            (_, "Datentyp") => {
                Value::BuiltinFunction(Box::new(self.clone()), value_type)
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
            Value::Speicherbox(inner) => {
                let inner_str = inner
                    .iter()
                    .map(|(key, value)| format!("{key}: {}",value.to_string().replace("\n", "\n    ")))
                    .collect::<Vec<String>>()
                    .join(",\n    ");
                format!("Speicherbox {{\n    {inner_str}\n}}")
            }
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
            Value::BuiltinFunction(_, _) => "<Eingebaute-Funktion>".to_string(),
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
