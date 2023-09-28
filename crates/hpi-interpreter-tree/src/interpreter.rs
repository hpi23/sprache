use std::{
    borrow::Cow, cell::RefCell, collections::HashMap, io::Write, rc::Rc, thread, time::Duration,
};

use hpi_analyzer::{ast::*, AssignOp, InfixOp, PrefixOp, Type};

use crate::value::{InterruptKind, Value};

pub(crate) type Error = Cow<'static, str>;
type ExprResult = Result<Value, InterruptKind>;
type StmtResult = Result<(), InterruptKind>;
type Scope<'src> = HashMap<&'src str, Rc<RefCell<Value>>>;

pub trait HPIHttpClient {
    fn request(&self, method: String, url: &str, body: String) -> Result<(u16, String), String>;
}

#[derive(Debug)]
pub struct Interpreter<'src, Output, HttpClient>
where
    Output: Write,
    HttpClient: HPIHttpClient,
{
    output: Output,
    http_client: HttpClient,
    scopes: Vec<Scope<'src>>,
    functions: HashMap<&'src str, Rc<AnalyzedFunctionDefinition<'src>>>,
}

impl<'src, Output, HttpClient> Interpreter<'src, Output, HttpClient>
where
    Output: Write,
    HttpClient: HPIHttpClient,
{
    pub fn new(output: Output, http_client: HttpClient) -> Self {
        Self {
            http_client,
            output,
            scopes: vec![],
            functions: HashMap::new(),
        }
    }

    pub fn run(mut self, tree: AnalyzedProgram<'src>) -> Result<i64, Error> {
        for func in tree.functions.into_iter().filter(|f| f.used) {
            self.functions.insert(func.name, func.into());
        }

        let mut global_scope = HashMap::new();
        for global in tree.globals.iter().filter(|g| g.used) {
            global_scope.insert(
                global.name,
                match global.expr.clone() {
                    AnalyzedExpression::Int(num) => Value::Int(num).wrapped(),
                    AnalyzedExpression::Float(num) => Value::Float(num).wrapped(),
                    AnalyzedExpression::Bool(bool) => Value::Bool(bool).wrapped(),
                    AnalyzedExpression::Char(num) => Value::Char(num).wrapped(),
                    AnalyzedExpression::String(str) => Value::String(str.to_string()).wrapped(),
                    AnalyzedExpression::List(inner) => Value::List(Rc::new(RefCell::new(
                        self.visit_list_expr_helper(&inner.values)
                            .expect("the analyzer guarantees that this cannot happen"),
                    )))
                    .wrapped(),
                    _ => unreachable!("the analyzer guarantees constant globals"),
                },
            );
        }
        self.scopes.push(global_scope);

        self.functions.insert(
            "Bewerbung",
            AnalyzedFunctionDefinition {
                used: true,
                name: "Bewerbung",
                params: vec![],
                return_type: Type::Nichts,
                block: tree.bewerbung_fn,
            }
            .into(),
        );

        self.functions.insert(
            "Einschreibung",
            AnalyzedFunctionDefinition {
                used: true,
                name: "Einschreibung",
                params: vec![AnalyzedParameter {
                    name: "Matrikelnummer",
                    type_: Type::Int(0),
                }],
                return_type: Type::Nichts,
                block: tree.einschreibung_fn,
            }
            .into(),
        );

        self.functions.insert(
            "Studium",
            AnalyzedFunctionDefinition {
                used: true,
                name: "Studium",
                params: vec![],
                return_type: Type::Nichts,
                block: tree.studium_fn,
            }
            .into(),
        );

        // ignore interruptions (e.g. break, return)
        match self.call_func(&AnalyzedCallBase::Ident("Bewerbung"), vec![]) {
            Err(InterruptKind::Error(msg)) => return Err(msg),
            Err(InterruptKind::Exit(code)) => return Ok(code),
            Ok(value) => {
                if let Value::String(inner) = value {
                    if inner.is_empty() {
                        return Err("Ihre Bewerbung hat das HPI leider nicht überzeugt.\n Ist Ihr Bewerbungsschreiben vielleicht leer?".into());
                    }
                } else {
                    unreachable!("the analyzer prevents this")
                }
            }
            Err(_) => {}
        };

        let mut rand_buffer: [u8; 4] = [0; 4];
        getrandom::getrandom(&mut rand_buffer).unwrap();
        let matrikelnummer: u32 = (rand_buffer[0] as u32)
            | (rand_buffer[1] as u32) << 8
            | (rand_buffer[2] as u32) << 16
            | (rand_buffer[3] as u32) << 24;

        match self.call_func(
            &AnalyzedCallBase::Ident("Einschreibung"),
            vec![Value::Int(matrikelnummer as i64)],
        ) {
            Err(InterruptKind::Error(msg)) => return Err(msg),
            Err(InterruptKind::Exit(code)) => return Ok(code),
            Ok(_) | Err(_) => {}
        };

        match self.call_func(
            &AnalyzedCallBase::Ident("Studium"),
            vec![Value::Int(matrikelnummer as i64)],
        ) {
            Err(InterruptKind::Error(msg)) => Err(msg),
            Err(InterruptKind::Exit(code)) => Ok(code),
            Ok(_) | Err(_) => Ok(0),
        }
    }

    //////////////////////////////////

    fn visit_list_expr_helper(
        &mut self,
        node: &[AnalyzedExpression<'src>],
    ) -> Result<Vec<Value>, InterruptKind> {
        node.iter()
            .map(|expr| -> ExprResult { self.visit_expression(expr) })
            .collect()
    }

    fn visit_list_expr(&mut self, node: &[AnalyzedExpression<'src>]) -> ExprResult {
        Ok(Value::List(Rc::new(RefCell::new(
            self.visit_list_expr_helper(node)?,
        ))))
    }

    fn get_var(&mut self, name: &'src str) -> Rc<RefCell<Value>> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Rc::clone(var);
            }
        }
        unreachable!("the analyzer guarantees valid variable references: {name}")
    }

    fn scoped<T>(&mut self, scope: Scope<'src>, callback: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.push(scope);
        let res = callback(self);
        self.scopes.pop();
        res
    }

    fn call_func(
        &mut self,
        func_name: &AnalyzedCallBase<'src>,
        mut args: Vec<Value>,
    ) -> ExprResult {
        match func_name {
            AnalyzedCallBase::Ident("aufgeben") => {
                Err(InterruptKind::Exit(args.swap_remove(0).unwrap_int()))
            }
            AnalyzedCallBase::Ident("drucke") => {
                self.output
                    .write_all(
                        (args
                            .iter()
                            .map(|val| val.to_string())
                            .collect::<Vec<String>>()
                            .join(" ")
                            + "\n")
                            .as_bytes(),
                    )
                    .expect("if this fails, we're screwed");

                Ok(Value::Unit)
            }
            AnalyzedCallBase::Ident("http") => {
                // BuiltinFunction::new(ParamTypes::Normal(vec![
                //                         Type::String(0), // method
                //                         Type::String(0), // url
                //                         Type::String(0), // body
                //                         Type::List(Box::new(Type::String(0)), 0); // headers

                let Value::String(method) = args[0].clone() else {
                    unreachable!("the analyzer prevents this");
                };

                let Value::String(url) = args[1].clone() else {
                    unreachable!("the analyzer prevents this");
                };

                let Value::String(body) = args[2].clone() else {
                    unreachable!("the analyzer prevents this");
                };

                let Value::Ptr(body_ptr) = args[3].clone() else {
                    unreachable!("the analyzer prevents this");
                };

                let res = self
                    .http_client
                    .request(method, url.as_str(), body)
                    .map_err(|err| InterruptKind::Error(err.into()))?;

                *body_ptr.borrow_mut() = Value::String(res.1);

                Ok(Value::Int(res.0 as i64))
            }
            AnalyzedCallBase::Ident("schlummere") => {
                #[cfg(target_arch = "wasm32")]
                {
                    return Err(InterruptKind::Error("Im Web wird nicht geschlafen!".into()));
                }

                if let Value::Float(duration) = args[0] {
                    thread::sleep(Duration::from_secs_f64(duration));
                } else {
                    unreachable!("the analyzer prevents this")
                }

                Ok(Value::Unit)
            }
            AnalyzedCallBase::Ident("geld") => Ok(Value::String(String::from(
                "Nun sind Sie reich, sie wurden gesponst!",
            ))),
            AnalyzedCallBase::Ident(func_name) => {
                let func = Rc::clone(&self.functions[func_name]);

                let mut scope = HashMap::new();
                for (param, arg) in func.params.iter().zip(args) {
                    scope.insert(param.name, arg.wrapped());
                }

                self.scoped(scope, |self_| match self_.visit_block(&func.block, false) {
                    Ok(val) => Ok(val),
                    Err(interrupt) => Ok(interrupt.into_value()?),
                })
            }
            AnalyzedCallBase::Expr(expr) => {
                let base = self.visit_expression(expr)?;

                match base {
                    Value::BuiltinFunction(base, func) => Ok(func(&base, args)),
                    _ => unreachable!("analyzer prevents this"),
                }
            }
        }
    }

    //////////////////////////////////

    fn visit_block(&mut self, node: &AnalyzedBlock<'src>, new_scope: bool) -> ExprResult {
        let callback = |self_: &mut Self| {
            for stmt in &node.stmts {
                self_.visit_statement(stmt)?;
            }
            node.expr
                .as_ref()
                .map_or(Ok(Value::Unit), |expr| self_.visit_expression(expr))
        };

        match new_scope {
            true => self.scoped(HashMap::new(), callback),
            false => callback(self),
        }
    }

    fn visit_statement(&mut self, node: &AnalyzedStatement<'src>) -> StmtResult {
        match node {
            AnalyzedStatement::Beantrage(_) => Ok(()),
            AnalyzedStatement::Let(node) => self.visit_let_stmt(node),
            AnalyzedStatement::Aendere(node) => self.visit_aendere_stmt(node),
            AnalyzedStatement::Return(expr) => Err(InterruptKind::Return(
                expr.as_ref()
                    .map_or(Ok(Value::Unit), |expr| self.visit_expression(expr))?,
            )),
            AnalyzedStatement::While(node) => self.visit_while_stmt(node),
            AnalyzedStatement::Break => Err(InterruptKind::Break),
            AnalyzedStatement::Continue => Err(InterruptKind::Continue),
            AnalyzedStatement::Expr(node) => self.visit_expression(node).map(|_| ()),
        }
    }

    fn visit_let_stmt(&mut self, node: &AnalyzedLetStmt<'src>) -> StmtResult {
        let value = self.visit_expression(&node.expr)?;
        self.scopes
            .last_mut()
            .expect("there should always be at least one scope")
            .insert(node.name, value.wrapped());
        Ok(())
    }

    fn visit_aendere_stmt(&mut self, node: &AnalyzedAendereStmt<'src>) -> StmtResult {
        let rhs = self.visit_expression(&node.expr)?;
        let mut var = self.get_var(node.assignee);
        for _ in 0..node.assignee_ptr_count {
            let new_ptr = var.borrow().clone().unwrap_ptr();
            var = new_ptr;
        }

        *var.borrow_mut() = rhs;

        Ok(())
    }

    fn visit_loop_stmt(&mut self, node: &AnalyzedLoopStmt<'src>) -> StmtResult {
        loop {
            match self.visit_block(&node.block, true) {
                Err(InterruptKind::Break) => break,
                Err(InterruptKind::Continue) => continue,
                res => res?,
            };
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, node: &AnalyzedWhileStmt<'src>) -> StmtResult {
        while self.visit_expression(&node.cond)?.unwrap_bool() {
            match self.visit_block(&node.block, true) {
                Err(InterruptKind::Break) => break,
                Err(InterruptKind::Continue) => continue,
                res => res?,
            };
        }
        Ok(())
    }

    fn visit_for_stmt(&mut self, node: &AnalyzedForStmt<'src>) -> StmtResult {
        // new scope just for the induction variable
        let init_val = self.visit_expression(&node.initializer)?;

        self.scoped(
            HashMap::from([(node.ident, init_val.wrapped())]),
            |self_| -> StmtResult {
                loop {
                    if !self_.visit_expression(&node.cond)?.unwrap_bool() {
                        break;
                    }

                    let res = self_.visit_block(&node.block, true);

                    self_.visit_expression(&node.update)?;

                    match res {
                        Err(InterruptKind::Break) => break,
                        Err(InterruptKind::Continue) => continue,
                        res => res?,
                    };
                }
                Ok(())
            },
        )
    }

    //////////////////////////////////

    fn visit_expression(&mut self, node: &AnalyzedExpression<'src>) -> ExprResult {
        match node {
            AnalyzedExpression::Block(block) => self.visit_block(block, true),
            AnalyzedExpression::If(node) => self.visit_if_expr(node),
            AnalyzedExpression::Int(num) => Ok(num.into()),
            AnalyzedExpression::Float(num) => Ok(num.into()),
            AnalyzedExpression::Bool(bool) => Ok(bool.into()),
            AnalyzedExpression::Char(num) => Ok(num.into()),
            AnalyzedExpression::String(str) => Ok(Value::String((*str).to_string())),
            AnalyzedExpression::List(inner) => self.visit_list_expr(&inner.values),
            AnalyzedExpression::Ident(node) => Ok(self.get_var(node.ident).borrow().clone()),
            AnalyzedExpression::Prefix(node) => self.visit_prefix_expr(node),
            AnalyzedExpression::Infix(node) => self.visit_infix_expr(node),
            AnalyzedExpression::Assign(node) => self.visit_assign_expr(node),
            AnalyzedExpression::Call(node) => self.visit_call_expr(node),
            AnalyzedExpression::Cast(node) => self.visit_cast_expr(node),
            AnalyzedExpression::Member(node) => self.visit_member_expr(node),
            AnalyzedExpression::Index(node) => self.visit_index_expr(node),
            AnalyzedExpression::Grouped(expr) => self.visit_expression(expr),
        }
    }

    fn visit_if_expr(&mut self, node: &AnalyzedIfExpr<'src>) -> ExprResult {
        if self.visit_expression(&node.cond)?.unwrap_bool() {
            self.visit_block(&node.then_block, true)
        } else if let Some(else_block) = &node.else_block {
            self.visit_block(else_block, true)
        } else {
            Ok(Value::Unit)
        }
    }

    fn visit_prefix_expr(&mut self, node: &AnalyzedPrefixExpr<'src>) -> ExprResult {
        let val = self.visit_expression(&node.expr)?;
        match node.op {
            PrefixOp::Not => Ok(!val),
            PrefixOp::Neg => Ok(-val),
            PrefixOp::Ref => match &node.expr {
                AnalyzedExpression::Ident(ident_expr) => {
                    Ok(Value::Ptr(self.get_var(ident_expr.ident)))
                }
                _ => unreachable!("the analyzer only allows referencing identifiers"),
            },
            PrefixOp::Deref => Ok(val.unwrap_ptr().borrow().clone()),
        }
    }

    fn visit_infix_expr(&mut self, node: &AnalyzedInfixExpr<'src>) -> ExprResult {
        match node.op {
            InfixOp::And => {
                return if !self.visit_expression(&node.lhs)?.unwrap_bool() {
                    Ok(false.into())
                } else {
                    self.visit_expression(&node.rhs)
                };
            }
            InfixOp::Or => {
                return if self.visit_expression(&node.lhs)?.unwrap_bool() {
                    Ok(true.into())
                } else {
                    self.visit_expression(&node.rhs)
                };
            }
            _ => {}
        }

        let lhs = self.visit_expression(&node.lhs)?;
        let rhs = self.visit_expression(&node.rhs)?;
        match node.op {
            InfixOp::Plus => Ok(lhs + rhs),
            InfixOp::Minus => Ok(lhs - rhs),
            InfixOp::Mul => Ok(lhs * rhs),
            InfixOp::Div => Ok((lhs / rhs)?),
            InfixOp::Rem => Ok((lhs % rhs)?),
            InfixOp::Pow => Ok(lhs.pow(rhs)),
            InfixOp::Eq => Ok((lhs == rhs).into()),
            InfixOp::Neq => Ok((lhs != rhs).into()),
            InfixOp::Lt => Ok((lhs < rhs).into()),
            InfixOp::Gt => Ok((lhs > rhs).into()),
            InfixOp::Lte => Ok((lhs <= rhs).into()),
            InfixOp::Gte => Ok((lhs >= rhs).into()),
            InfixOp::Shl => Ok((lhs << rhs)?),
            InfixOp::Shr => Ok((lhs >> rhs)?),
            InfixOp::BitOr => Ok(lhs | rhs),
            InfixOp::BitAnd => Ok(lhs & rhs),
            InfixOp::BitXor => Ok(lhs ^ rhs),
            InfixOp::And | InfixOp::Or => unreachable!("logical `and` and `or` are matched above"),
        }
    }

    fn visit_assign_expr(&mut self, node: &AnalyzedAssignExpr<'src>) -> ExprResult {
        let rhs = self.visit_expression(&node.expr)?;
        let mut var = self.get_var(node.assignee);
        for _ in 0..node.assignee_ptr_count {
            let new_ptr = var.borrow().clone().unwrap_ptr();
            var = new_ptr;
        }

        let new_val = match node.op {
            AssignOp::Basic => unreachable!("this operator is never used"),
            AssignOp::Plus => var.borrow().clone() + rhs,
            AssignOp::Minus => var.borrow().clone() - rhs,
            AssignOp::Mul => var.borrow().clone() * rhs,
            AssignOp::Div => (var.borrow().clone() / rhs)?,
            AssignOp::Rem => (var.borrow().clone() % rhs)?,
            AssignOp::Pow => var.borrow().clone().pow(rhs),
            AssignOp::Shl => (var.borrow().clone() << rhs)?,
            AssignOp::Shr => (var.borrow().clone() >> rhs)?,
            AssignOp::BitOr => var.borrow().clone() | rhs,
            AssignOp::BitAnd => var.borrow().clone() & rhs,
            AssignOp::BitXor => var.borrow().clone() ^ rhs,
        };
        *var.borrow_mut() = new_val;

        Ok(Value::Unit)
    }

    fn visit_call_expr(&mut self, node: &AnalyzedCallExpr<'src>) -> ExprResult {
        let args = node
            .args
            .iter()
            .map(|expr| self.visit_expression(expr))
            .collect::<Result<_, _>>()?;
        self.call_func(&node.func, args)
    }

    fn visit_cast_expr(&mut self, node: &AnalyzedCastExpr<'src>) -> ExprResult {
        let val = self.visit_expression(&node.expr)?;
        match (val, node.as_type.clone()) {
            (val @ Value::Int(_), Type::Int(0))
            | (val @ Value::Float(_), Type::Float(0))
            | (val @ Value::Char(_), Type::Char(0))
            | (val @ Value::Bool(_), Type::Bool(0)) => Ok(val),

            (Value::Int(int), Type::Float(0)) => Ok((int as f64).into()),
            (Value::Int(int), Type::Bool(0)) => Ok((int != 0).into()),
            (Value::Int(int), Type::Char(0)) => Ok((int.clamp(0, 127) as u8).into()),
            (Value::Float(float), Type::Int(0)) => Ok((float as i64).into()),
            (Value::Float(float), Type::Bool(0)) => Ok((float != 0.0).into()),
            (Value::Float(float), Type::Char(0)) => Ok((float.clamp(0.0, 127.0) as u8).into()),
            (Value::Bool(bool), Type::Int(0)) => Ok((bool as i64).into()),
            (Value::Bool(bool), Type::Float(0)) => Ok((bool as u8 as f64).into()),
            (Value::Bool(bool), Type::Char(0)) => Ok((bool as u8).into()),
            (Value::Char(char), Type::Int(0)) => Ok((char as i64).into()),
            (Value::Char(char), Type::Float(0)) => Ok((char as f64).into()),
            (Value::Char(char), Type::Bool(0)) => Ok((char != 0).into()),
            _ => unreachable!("the analyzer guarantees one of the above to match"),
        }
    }

    fn visit_member_expr(&mut self, node: &AnalyzedMemberExpr<'src>) -> ExprResult {
        let base = self.visit_expression(&node.expr)?;

        Ok(base.member(node.member))
    }

    fn visit_index_expr(&mut self, node: &AnalyzedIndexExpr<'src>) -> ExprResult {
        let base = self.visit_expression(&node.expr)?;
        let index = self.visit_expression(&node.index)?;
        match (base, index) {
            (Value::List(values), Value::Int(idx)) => {
                if idx < 0 {
                    return Err(InterruptKind::Error(
                        format!("Illegale Indizierung mittels Index: `{idx}`").into(),
                    ));
                }
                Ok(values.borrow()[idx as usize].clone())
            }
            _ => unreachable!("the analyzer prevents this"),
        }
    }
}
