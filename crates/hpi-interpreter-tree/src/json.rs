use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc, str::FromStr};

use hpi_analyzer::Type;
use serde_json::Number;

use crate::value::{InterruptKind, Value};

pub fn deserialize(input: &str) -> Result<Value, InterruptKind> {
    let serde_value: serde_json::Value = serde_json::from_str(input).map_err(|err| {
        InterruptKind::Error(format!("Zergliedere_JSON Textverarbeitungsfehler: {err}").into())
    })?;
    Value::from_json(serde_value).map_err(|err| {
        InterruptKind::Error(format!("Zergliedere_JSON Umwandlungsfehler: {err}").into())
    })
}

pub fn serialize(input: Value) -> Result<String, InterruptKind> {
    let serde_value = input.to_json().map_err(|err| {
        InterruptKind::Error(format!("Gliedere_JSON Umwandlungsfehler: {err}").into())
    })?;

    Ok(serde_value.to_string())
}

#[derive(Debug)]
enum TypeError {
    ListInnerType { expected: Type, found: Type },
    UnsupportedType(Type),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::ListInnerType { expected, found } => write!(
                f,
                "Datentypfehler: Erwartete `{expected}`, `{found}` wurde aufgespürt."
            ),
            TypeError::UnsupportedType(other) => write!(
                f,
                "Datentypfehler: der Datentyp `{other}` wird nicht unterstützt."
            ),
        }
    }
}

impl Value {
    fn to_json(&self) -> Result<serde_json::Value, TypeError> {
        Ok(match self {
            Value::Int(inner) => {
                serde_json::Value::from_str(&format!("{inner}")).expect("this should not fail")
            }
            Value::Float(inner) => {
                serde_json::Value::Number(Number::from_f64(*inner).expect("this should not fail"))
            }
            Value::Char(inner) => serde_json::Value::String(inner.to_string()),
            Value::String(inner) => serde_json::Value::String(inner.clone()),
            Value::List(inner) => {
                let inner = inner
                    .borrow()
                    .iter()
                    .map(|element| element.to_json())
                    .collect::<Result<Vec<serde_json::Value>, TypeError>>()?;
                serde_json::Value::Array(inner)
            }
            Value::Bool(inner) => serde_json::Value::Bool(*inner),
            Value::Unit => serde_json::Value::Null,
            Value::Ptr(_) | Value::BuiltinFunction(_, _) => {
                return Err(TypeError::UnsupportedType(self.as_type()))
            }
            Value::Speicherbox(inner) => {
                let mut new_inner = serde_json::Map::new();
                for (key, value) in inner {
                    new_inner.insert(key.clone(), value.to_json()?);
                }
                serde_json::Value::Object(new_inner)
            }
            Value::Objekt(inner) => {
                let mut new_inner = serde_json::Map::new();
                for (key, value) in inner.borrow().iter() {
                    new_inner.insert(key.clone(), value.to_json()?);
                }
                serde_json::Value::Object(new_inner)
            }
        })
    }

    fn from_json(value: serde_json::Value) -> Result<Self, TypeError> {
        Ok(match value {
            serde_json::Value::Null => Self::Unit,
            serde_json::Value::Bool(inner) => Self::Bool(inner),
            serde_json::Value::Number(inner) => {
                if inner.is_i64() {
                    Self::Int(inner.as_i64().expect("this should not fail"))
                } else if inner.is_f64() {
                    Self::Float(inner.as_f64().expect("this should not fail"))
                } else {
                    unreachable!("this should not happen")
                }
            }
            serde_json::Value::String(inner) => Self::String(inner),
            serde_json::Value::Array(inner) => {
                let mut typ_ = Type::Unknown;

                let res = inner
                    .into_iter()
                    .map(|element| {
                        let value = Value::from_json(element)?;
                        if value.as_type() != typ_ && typ_ != Type::Unknown {
                            Err(TypeError::ListInnerType {
                                expected: typ_.clone(),
                                found: value.as_type(),
                            })
                        } else {
                            if typ_ == Type::Unknown {
                                typ_ = value.as_type();
                            }
                            Ok(value)
                        }
                    })
                    .collect::<Result<Vec<Value>, TypeError>>()?;

                Self::List(Rc::new(RefCell::new(res)))
            }
            serde_json::Value::Object(inner) => {
                let inner_map = inner
                    .into_iter()
                    .map(|(key, value)| Ok((key, Value::from_json(value)?)))
                    .collect::<Result<HashMap<String, Value>, TypeError>>()?;

                Value::Speicherbox(inner_map)
            }
        })
    }
}

mod test {
    #[test]
    fn test_deserialize() {
        super::deserialize("{\"foo\": 42}");
        super::deserialize("[ 42, 1, 3 ]");
    }
}
