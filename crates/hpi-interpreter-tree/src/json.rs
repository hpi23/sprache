use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use hpi_analyzer::Type;

use crate::value::{InterruptKind, Value};

pub fn deserialize(input: &str) -> Result<Value, InterruptKind> {
    let serde_value: serde_json::Value = serde_json::from_str(input)
        .map_err(|err| InterruptKind::Error(format!("Zergliedere_JSON Textverarbeitungsfehler: {err}").into()))?;
    Value::from_json(serde_value)
        .map_err(|err| InterruptKind::Error(format!("Zergliedere_JSON Umwandlungsfehler: {err}").into()))
}

#[derive(Debug)]
enum TypeError {
    ListInnerType { expected: Type, found: Type },
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::ListInnerType { expected, found } => write!(
                f,
                "Datentypfehler: Erwartete `{expected}`, `{found}` wurde aufgespürt."
            ),
        }
    }
}

impl Value {
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
