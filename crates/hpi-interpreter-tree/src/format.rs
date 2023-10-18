use std::str::Chars;

use crate::value::{InterruptKind, Value};

pub struct Formatter<'src> {
    input_reader: Chars<'src>,
    input_args: Vec<Value>,
    inut_args_curr_pos: usize,
    curr_char: Option<char>,
    output: String,
}

impl<'src> Formatter<'src> {
    pub fn new(input: &'src str, args: Vec<Value>) -> Self {
        println!("Created new formatter: {:?}", args);

        let mut fmt = Self {
            input_reader: input.chars(),
            input_args: args.to_vec(),
            inut_args_curr_pos: 0,
            curr_char: None,
            output: String::new(),
        };

        fmt.next();

        fmt
    }

    pub fn format(mut self) -> Result<String, InterruptKind> {
        while let Some(curr) = self.curr_char {
            match curr {
                '%' => self.start_escape()?,
                other => {
                    self.next();
                    self.output.push(other)
                }
            }
        }

        Ok(self.output)
    }

    fn next(&mut self) {
        self.curr_char = self.input_reader.next()
    }

    fn escape(&mut self, padding: usize) -> Result<(), InterruptKind> {
        match self.curr_char {
            Some(char) => {
                let curr = self.input_args.get(self.inut_args_curr_pos);
                self.inut_args_curr_pos+=1;

                match (char, curr, padding) {
                    ('d', Some(Value::Int(inner)), _) => { self.output.push_str(format!("{:0width$}", inner, width=padding).as_str())  }
                    ('f', Some(Value::Float(inner)), _) => { self.output.push_str(format!("{:0width$}", inner, width=padding).as_str())  }
                    ('t', Some(Value::Bool(inner)), 0) => { self.output.push_str(inner.to_string().as_str()) }
                    ('s', Some(Value::String(inner)), 0) => { self.output.push_str(inner) }
                    ('v', Some(other), 0) => { self.output.push_str(other.to_string().as_str()) }
                    (specifier, Some(value), _) => {
                        return Err(InterruptKind::Error(format!("Formatierungsfehler: Unzulässige Kombination aus Formatierungsanweisung `{specifier}`, Pufferung `{padding}` und Eingabewert mit dem Datentyp `{}`", value.as_type()).into()));
                    }
                    (specifier, None, _) => {
                        return Err(InterruptKind::Error(format!("Formatierungsfehler: Erwartete Eingabewert für Formatierungsanweisung `{specifier}`, allerdings endet hier die Eingabe.").into()));
                    }
                }
            },
            None => return Err(InterruptKind::Error("Formatierungsfehler: Erwartete Formatierungsanweisung, allerdings endet hier die Eingabe.".into())),
        }

        self.next();

        Ok(())
    }

    fn start_escape(&mut self) -> Result<(), InterruptKind> {
        self.next();

        match self.curr_char {
            Some(char) => {
                match char {
                    '0' ..= '9' => {
                        let mut padding = String::new();

                        while let Some(char @ '0'..='9') = self.curr_char {
                            padding.push(char);
                            self.next();
                        }

                        self.escape(padding.parse().unwrap())?;
                    }
                    _=> self.escape(0)?
                }
            },
            None => return Err(InterruptKind::Error("Formatierungsfehler: Erwartete Formatierungsanweisung, allerdings endet hier die Eingabe.".into())),
        }

        Ok(())
    }
}
