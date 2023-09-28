use std::mem;

use either::Either;

use crate::{ast::*, Error, Lex, Location, Result, Span, Token, TokenKind};

pub struct Parser<'src, Lexer: Lex<'src>> {
    lexer: Lexer,
    prev_tok: Token<'src>,
    curr_tok: Token<'src>,
    errors: Vec<Error<'src>>,
}

impl<'src, Lexer: Lex<'src>> Parser<'src, Lexer> {
    /// Creates a new Parser
    pub fn new(lexer: Lexer) -> Self {
        Self {
            lexer,
            // initialize with dummy Eof tokens
            prev_tok: Token::dummy(),
            curr_tok: Token::dummy(),
            errors: vec![],
        }
    }

    /// Consumes this parser and tries to parse a [`Program`].
    ///
    /// # Returns
    /// This function returns a tuple of a `Result<Program>`
    /// and a `Vec<Error>`. Parsing can be
    /// - successful: `(Some(Program), [])`
    /// - partially successful: `(Some(Program), [..errors])`
    /// - unsuccessful: `(Err(fatal_error), [..errors])`
    pub fn parse(mut self) -> (Result<'src, Program<'src>>, Vec<Error<'src>>) {
        if let Err(err) = self.next() {
            return (Err(err), self.errors);
        }

        let program = match self.program() {
            Ok(program) => program,
            Err(err) => return (Err(err), self.errors),
        };

        if self.curr_tok.kind != TokenKind::Eof {
            self.errors.push(Error::new(
                format!("expected EOF, found `{}`", self.curr_tok.kind),
                self.curr_tok.span,
                self.lexer.source(),
            ))
        }

        (Ok(program), self.errors)
    }

    // moves cursor to next token
    fn next(&mut self) -> Result<'src, ()> {
        // swap prev_tok and curr_tok in memory so that what was curr_tok is now prev_tok
        mem::swap(&mut self.prev_tok, &mut self.curr_tok);
        // overwrite curr_tok (which is now what prev_tok was) with the next token from the lexer
        self.curr_tok = self.lexer.next_token()?;

        Ok(())
    }

    // expects the curr_tok to be of the specified kind
    fn expect(&mut self, kind: TokenKind) -> Result<'src, ()> {
        if self.curr_tok.kind != kind {
            return Err(Error::new_boxed(
                format!("expected `{kind}`, found `{}`", self.curr_tok.kind),
                self.curr_tok.span,
                self.lexer.source(),
            ));
        }
        self.next()?;
        Ok(())
    }

    // expects the curr_tok to be an identifier and returns its name if this is the case
    fn expect_ident(&mut self) -> Result<'src, Spanned<'src, &'src str>> {
        match self.curr_tok.kind {
            TokenKind::Ident(ident) => {
                let ident = Spanned {
                    span: self.curr_tok.span,
                    inner: ident,
                };
                self.next()?;
                Ok(ident)
            }
            _ => Err(Error::new_boxed(
                format!("expected identifier, found `{}`", self.curr_tok.kind),
                self.curr_tok.span,
                self.lexer.source(),
            )),
        }
    }

    // expects curr_tok to be the specified token kind and adds a soft error otherwise
    fn expect_recoverable(
        &mut self,
        kind: TokenKind,
        message: impl Into<String>,
        span: Span<'src>,
    ) -> Result<'src, Span<'src>> {
        let start_loc = self.curr_tok.span.start;
        let end_loc = if self.curr_tok.kind != kind {
            self.errors
                .push(Error::new(message.into(), span, self.lexer.source()));
            self.curr_tok.span.start
        } else {
            self.next()?;
            self.prev_tok.span.end
        };
        Ok(start_loc.until(end_loc))
    }

    //////////////////////////

    fn program(&mut self) -> Result<'src, Program<'src>> {
        let start_loc = self.curr_tok.span.start;
        let mut functions = vec![];
        let mut imports = vec![];
        let mut globals = vec![];

        while self.curr_tok.kind != TokenKind::Eof {
            match self.curr_tok.kind {
                TokenKind::Funk => functions.push(self.function_definition()?),
                TokenKind::Beantrage => imports.push(self.beantrage_stmt()?),
                TokenKind::Setze => globals.push(self.setze_stmt()?),
                _ => {
                    return Err(Error::new_boxed(
                        format!(
                            "Ich habe etweder `funk`, `beantrage` oder `setze` erwartet, jedoch `{}` gefunden. So geht das nicht weiter!",
                            self.curr_tok.kind
                        ),
                        self.curr_tok.span,
                        self.lexer.source(),
                    ))
                }
            }
        }

        Ok(Program {
            span: start_loc.until(self.prev_tok.span.end),
            imports,
            functions,
            globals,
        })
    }

    fn type_(&mut self) -> Result<'src, Spanned<'src, Type>> {
        let start_loc = self.curr_tok.span.start;

        let mut ptr_count = 0;
        while matches!(self.curr_tok.kind, TokenKind::Zeiger) {
            self.next()?;
            self.expect(TokenKind::Auf)?;
            ptr_count += 1;
        }

        let type_ = match &self.curr_tok.kind {
            TokenKind::Ident("Zahl") => Type::Int(ptr_count),
            TokenKind::Ident("Fließkommazahl") => Type::Float(ptr_count),
            TokenKind::Ident("Wahrheitswert") => Type::Bool(ptr_count),
            TokenKind::Ident("Zeichen") => Type::Char(ptr_count),
            TokenKind::Ident("Speicherbox") => Type::AnyObject(ptr_count),
            TokenKind::Ident("Liste") => {
                self.next()?;
                self.expect(TokenKind::Von)?;
                let inner = self.type_()?;
                let result = Type::List(Box::new(inner.inner), ptr_count);

                return Ok(Spanned {
                    span: start_loc.until(self.prev_tok.span.end),
                    inner: result,
                });
            }
            TokenKind::Ident("Nichts") => Type::Nichts,
            TokenKind::Ident("Zeichenkette") => Type::String(ptr_count),
            TokenKind::Ident(ident) => {
                // TODO: rechtschreibfehler
                self.errors.push(Error::new(
                    format!("Datentyp `{ident}` ist nicht bekannt."),
                    self.curr_tok.span,
                    self.lexer.source(),
                ));
                Type::Unknown
            }
            invalid => {
                return Err(Error::new_boxed(
                    format!("Erwartete einen Datentyp, fand `{invalid}`."),
                    self.curr_tok.span,
                    self.lexer.source(),
                ));
            }
        };
        self.next()?;
        Ok(Spanned {
            span: start_loc.until(self.prev_tok.span.end),
            inner: type_,
        })
    }

    fn function_definition(&mut self) -> Result<'src, FunctionDefinition<'src>> {
        let start_loc = self.curr_tok.span.start;

        self.expect(TokenKind::Funk)?;
        let name = self.expect_ident()?;
        let l_paren = self.curr_tok.span;
        self.expect(TokenKind::LParen)?;

        let mut params = vec![];
        if !matches!(self.curr_tok.kind, TokenKind::RParen | TokenKind::Eof) {
            params.push(self.parameter()?);
            while self.curr_tok.kind == TokenKind::Slash {
                self.next()?;
                if matches!(self.curr_tok.kind, TokenKind::RParen | TokenKind::Eof) {
                    break;
                }
                params.push(self.parameter()?);
            }
        }

        let r_paren = self.expect_recoverable(
            TokenKind::RParen,
            "missing closing parenthesis",
            self.curr_tok.span,
        )?;

        let params = Spanned {
            span: l_paren.start.until(r_paren.end),
            inner: params,
        };

        self.expect(TokenKind::Ergibt)?;
        let return_type = self.type_()?;

        let block = self.block()?;

        Ok(FunctionDefinition {
            span: start_loc.until(self.prev_tok.span.end),
            name,
            params,
            return_type,
            block,
        })
    }

    fn beantrage_stmt(&mut self) -> Result<'src, BeantrageStmt<'src>> {
        let start_loc = self.curr_tok.span.start;

        self.expect(TokenKind::Beantrage)?;
        let value_name = self.expect_ident()?;
        self.expect(TokenKind::Von)?;
        let von_name = self.expect_ident()?;

        self.expect(TokenKind::Semicolon)?;
        Ok(BeantrageStmt {
            span: start_loc.until(self.prev_tok.span.end),
            value_name,
            von_name,
        })
    }

    fn parameter(&mut self) -> Result<'src, Parameter<'src>> {
        let type_ = self.type_()?;
        // let type_ = match self.curr_tok.kind {
        //     TokenKind::Comma | TokenKind::RParen => {
        //         self.errors.push(Error::new(
        //             format!("missing type for parameter `{}`", name.inner),
        //             name.span,
        //             self.lexer.source(),
        //         ));
        //         Spanned {
        //             span: Span::dummy(),
        //             inner: Type::Unknown,
        //         }
        //     }
        //     _ => {
        //         self.expect(TokenKind::Colon)?;
        //         self.type_()?
        //     }
        // };
        let name = self.expect_ident()?;
        Ok(Parameter { name, type_ })
    }

    fn block(&mut self) -> Result<'src, Block<'src>> {
        let start_loc = self.curr_tok.span.start;

        self.expect(TokenKind::LBrace)?;

        let mut stmts = vec![];
        let mut expr = None;
        while !matches!(self.curr_tok.kind, TokenKind::RBrace | TokenKind::Eof) {
            match self.statement()? {
                Either::Left(stmt) => stmts.push(stmt),
                Either::Right(expression) => {
                    expr = Some(expression);
                    break;
                }
            }
        }

        self.expect_recoverable(
            TokenKind::RBrace,
            "missing closing brace",
            self.curr_tok.span,
        )?;

        Ok(Block {
            span: start_loc.until(self.prev_tok.span.end),
            stmts,
            expr,
        })
    }

    fn statement(&mut self) -> Result<'src, Either<Statement<'src>, Expression<'src>>> {
        Ok(match self.curr_tok.kind {
            TokenKind::Setze => Either::Left(Statement::Setze(self.setze_stmt()?)),
            TokenKind::Aendere => Either::Left(Statement::Aendere(self.aendere_stmt()?)),
            TokenKind::Ueberweise => Either::Left(self.return_stmt()?),
            TokenKind::Solange => Either::Left(self.while_stmt()?),
            TokenKind::Abbrechen => Either::Left(self.break_stmt()?),
            TokenKind::Weitermachen => Either::Left(self.continue_stmt()?),
            _ => self.expr_stmt()?,
        })
    }

    fn setze_stmt(&mut self) -> Result<'src, SetzeStmt<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip let token: this function is only called when self.curr_tok.kind ==
        // TokenKind::Setze
        self.next()?;

        let type_ = self.type_()?;

        let name = self.expect_ident()?;

        self.expect(TokenKind::Auf)?;
        let expr = self.expression(0)?;
        self.expect_recoverable(
            TokenKind::Semicolon,
            "Nach dieser Anweisung fehlt ein Semikolon.",
            start_loc.until(self.prev_tok.span.end),
        )?;

        Ok(SetzeStmt {
            span: start_loc.until(self.prev_tok.span.end),
            type_,
            name,
            expr,
        })
    }

    fn aendere_stmt(&mut self) -> Result<'src, AendereStmt<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip the `aendere` Token
        self.next()?;

        let name = self.expression(0)?;

        let lhs_start_loc = name.span().start;
        let (assignee, assignee_ptr_count) =
            match self.reduce_prefix_expr_to_ident(name, lhs_start_loc, 0) {
                Ok(res) => res,
                Err(err) => {
                    self.errors.push(*err);
                    (
                        Spanned {
                            span: Span::dummy(),
                            inner: "",
                        },
                        0,
                    )
                }
            };

        self.expect(TokenKind::Auf)?;

        let expr = self.expression(0)?;

        self.expect_recoverable(
            TokenKind::Semicolon,
            "Nach dieser Anweisung fehlt ein Semikolon.",
            start_loc.until(self.prev_tok.span.end),
        )?;

        Ok(AendereStmt {
            span: start_loc.until(self.curr_tok.span.end),
            assignee,
            assignee_ptr_count,
            expr,
        })
    }

    fn return_stmt(&mut self) -> Result<'src, Statement<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip return token: this function is only called when self.curr_tok.kind == TokenKind::Return
        self.next()?;

        let expr = match self.curr_tok.kind {
            TokenKind::Semicolon | TokenKind::RBrace | TokenKind::Eof => None,
            _ => Some(self.expression(0)?),
        };

        self.expect_recoverable(
            TokenKind::Semicolon,
            "Nach dieser Anweisung fehlt ein Semikolon.",
            start_loc.until(self.prev_tok.span.end),
        )?;

        Ok(Statement::Ueberweise(UeberweiseStmt {
            span: start_loc.until(self.prev_tok.span.end),
            expr,
        }))
    }

    fn while_stmt(&mut self) -> Result<'src, Statement<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip while token: this function is only called when self.curr_tok.kind == TokenKind::While
        self.next()?;

        let cond = self.expression(0)?;

        let block = self.block()?;

        // skip optional semicolon
        if self.curr_tok.kind == TokenKind::Semicolon {
            self.next()?;
        }

        Ok(Statement::Solange(SolangeStmt {
            span: start_loc.until(self.prev_tok.span.end),
            cond,
            block,
        }))
    }

    fn break_stmt(&mut self) -> Result<'src, Statement<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip break token: this function is only called when self.curr_tok.kind == TokenKind::Break
        self.next()?;

        self.expect_recoverable(
            TokenKind::Semicolon,
            "Nach dieser Anweisung fehlt ein Semikolon.",
            self.prev_tok.span,
        )?;

        Ok(Statement::Abbrechen(AbbrechenStmt {
            span: start_loc.until(self.prev_tok.span.end),
        }))
    }

    fn continue_stmt(&mut self) -> Result<'src, Statement<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip continue token: this function is only called when self.curr_tok.kind == TokenKind::Continue
        self.next()?;

        self.expect_recoverable(
            TokenKind::Semicolon,
            "Nach dieser Anweisung fehlt ein Semikolon.",
            self.prev_tok.span,
        )?;

        Ok(Statement::Weitermachen(WeitermachenStmt {
            span: start_loc.until(self.prev_tok.span.end),
        }))
    }

    fn expr_stmt(&mut self) -> Result<'src, Either<Statement<'src>, Expression<'src>>> {
        let start_loc = self.curr_tok.span.start;

        let (expr, with_block) = match self.curr_tok.kind {
            TokenKind::Falls => (Expression::If(self.falls_expr()?.into()), true),
            TokenKind::LBrace => (Expression::Block(self.block()?.into()), true),
            _ => (self.expression(0)?, false),
        };

        match (&self.curr_tok.kind, with_block) {
            (TokenKind::Semicolon, _) => self.next()?,
            (TokenKind::RBrace, _) => return Ok(Either::Right(expr)),
            (_, true) => {}
            (_, false) => self.errors.push(Error::new(
                "Nach dieser Anweisung fehlt ein Semikolon.".to_string(),
                expr.span(),
                self.lexer.source(),
            )),
        }

        Ok(Either::Left(Statement::Expr(ExprStmt {
            span: start_loc.until(self.prev_tok.span.end),
            expr,
        })))
    }

    fn expression(&mut self, prec: u8) -> Result<'src, Expression<'src>> {
        let start_loc = self.curr_tok.span.start;

        let mut lhs = match &self.curr_tok.kind {
            TokenKind::Int(num) => Expression::Int(self.atom(*num)?),
            TokenKind::Float(num) => Expression::Float(self.atom(*num)?),
            TokenKind::Ja => Expression::Bool(self.atom(true)?),
            TokenKind::Nein => Expression::Bool(self.atom(false)?),
            TokenKind::Char(char) => Expression::Char(self.atom(*char)?),
            TokenKind::String(string) => Expression::String(self.atom(string.clone())?),
            TokenKind::Ident(ident) => Expression::Ident(self.atom(*ident)?),
            TokenKind::LBrace => Expression::Block(self.block()?.into()),
            TokenKind::Falls => Expression::If(self.falls_expr()?.into()),
            TokenKind::Not => Expression::Prefix(self.prefix_expr(PrefixOp::Not, false)?.into()),
            TokenKind::Minus => Expression::Prefix(self.prefix_expr(PrefixOp::Neg, false)?.into()),
            TokenKind::Star => Expression::Prefix(self.prefix_expr(PrefixOp::Deref, false)?.into()),
            TokenKind::Pow => Expression::Prefix(self.prefix_expr(PrefixOp::Deref, true)?.into()),
            TokenKind::BitAnd => Expression::Prefix(self.prefix_expr(PrefixOp::Ref, false)?.into()),
            TokenKind::LParen => Expression::Grouped(self.grouped_expr()?),
            TokenKind::LBracket => Expression::List(self.list_expr()?),
            invalid => {
                return Err(Error::new_boxed(
                    format!("expected an expression, found `{invalid}`"),
                    self.curr_tok.span,
                    self.lexer.source(),
                ));
            }
        };

        while self.curr_tok.kind.prec().0 > prec {
            lhs = match self.curr_tok.kind {
                TokenKind::Plus => self.infix_expr(start_loc, lhs, InfixOp::Plus)?,
                TokenKind::Star => self.infix_expr(start_loc, lhs, InfixOp::Mul)?,
                TokenKind::Colon => self.infix_expr(start_loc, lhs, InfixOp::Div)?,
                TokenKind::Pow => self.infix_expr(start_loc, lhs, InfixOp::Pow)?,
                TokenKind::Minus => self.infix_expr(start_loc, lhs, InfixOp::Minus)?,
                TokenKind::Percent => self.infix_expr(start_loc, lhs, InfixOp::Rem)?,
                TokenKind::Eq => self.infix_expr(start_loc, lhs, InfixOp::Eq)?,
                TokenKind::Neq => self.infix_expr(start_loc, lhs, InfixOp::Neq)?,
                TokenKind::Lt => self.infix_expr(start_loc, lhs, InfixOp::Lt)?,
                TokenKind::Gt => self.infix_expr(start_loc, lhs, InfixOp::Gt)?,
                TokenKind::Lte => self.infix_expr(start_loc, lhs, InfixOp::Lte)?,
                TokenKind::Gte => self.infix_expr(start_loc, lhs, InfixOp::Gte)?,
                TokenKind::Shl => self.infix_expr(start_loc, lhs, InfixOp::Shl)?,
                TokenKind::Shr => self.infix_expr(start_loc, lhs, InfixOp::Shr)?,
                TokenKind::BitOr => self.infix_expr(start_loc, lhs, InfixOp::BitOr)?,
                TokenKind::BitAnd => self.infix_expr(start_loc, lhs, InfixOp::BitAnd)?,
                TokenKind::BitXor => self.infix_expr(start_loc, lhs, InfixOp::BitXor)?,
                TokenKind::And => self.infix_expr(start_loc, lhs, InfixOp::And)?,
                TokenKind::Or => self.infix_expr(start_loc, lhs, InfixOp::Or)?,
                TokenKind::Assign => self.assign_expr(start_loc, lhs, AssignOp::Basic)?,
                TokenKind::PlusAssign => self.assign_expr(start_loc, lhs, AssignOp::Plus)?,
                TokenKind::MinusAssign => self.assign_expr(start_loc, lhs, AssignOp::Minus)?,
                TokenKind::MulAssign => self.assign_expr(start_loc, lhs, AssignOp::Mul)?,
                TokenKind::DivAssign => self.assign_expr(start_loc, lhs, AssignOp::Div)?,
                TokenKind::RemAssign => self.assign_expr(start_loc, lhs, AssignOp::Rem)?,
                TokenKind::PowAssign => self.assign_expr(start_loc, lhs, AssignOp::Pow)?,
                TokenKind::ShlAssign => self.assign_expr(start_loc, lhs, AssignOp::Shl)?,
                TokenKind::ShrAssign => self.assign_expr(start_loc, lhs, AssignOp::Shr)?,
                TokenKind::BitOrAssign => self.assign_expr(start_loc, lhs, AssignOp::BitOr)?,
                TokenKind::BitAndAssign => self.assign_expr(start_loc, lhs, AssignOp::BitAnd)?,
                TokenKind::BitXorAssign => self.assign_expr(start_loc, lhs, AssignOp::BitXor)?,
                TokenKind::LParen => self.call_expr(start_loc, lhs)?,
                TokenKind::LBracket => self.index_expr(start_loc, lhs)?,
                TokenKind::Als => self.cast_expr(start_loc, lhs)?,
                TokenKind::Dot => self.member_expr(start_loc, lhs)?,
                _ => return Ok(lhs),
            };
        }

        Ok(lhs)
    }

    fn falls_expr(&mut self) -> Result<'src, IfExpr<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip if token: this function is only called when self.curr_tok.kind == TokenKind::If
        self.next()?;

        let cond = self.expression(0)?;
        let then_block = self.block()?;
        let else_block = match self.curr_tok.kind {
            TokenKind::Sonst => {
                self.next()?;
                Some(match &self.curr_tok.kind {
                    TokenKind::Falls => {
                        let if_expr = self.falls_expr()?;
                        Block {
                            span: if_expr.span,
                            stmts: vec![],
                            expr: Some(Expression::If(if_expr.into())),
                        }
                    }
                    TokenKind::LBrace => self.block()?,
                    invalid => {
                        return Err(Error::new_boxed(
                            format!(
                                "expected either `if` or block after `else`, found `{invalid}`"
                            ),
                            self.curr_tok.span,
                            self.lexer.source(),
                        ));
                    }
                })
            }
            _ => None,
        };

        Ok(IfExpr {
            span: start_loc.until(self.prev_tok.span.end),
            cond,
            then_block,
            else_block,
        })
    }

    fn atom<T>(&mut self, inner: T) -> Result<'src, Spanned<'src, T>> {
        let start_loc = self.curr_tok.span.start;
        self.next()?;
        Ok(Spanned {
            span: start_loc.until(self.prev_tok.span.end),
            inner,
        })
    }

    fn prefix_expr(
        &mut self,
        op: PrefixOp,
        deref_counts_double: bool,
    ) -> Result<'src, PrefixExpr<'src>> {
        let start_loc = self.curr_tok.span.start;

        // skip the operator token
        self.next()?;

        // PrefixExpr precedence is 27, higher than all InfixExpr precedences except CallExpr
        let expr = self.expression(27)?;

        match (op, &expr) {
            (PrefixOp::Ref | PrefixOp::Deref, Expression::Ident(_)) => Ok(PrefixExpr {
                span: start_loc.until(self.prev_tok.span.end),
                op,
                expr: match deref_counts_double {
                    true => Expression::Prefix(Box::new(PrefixExpr {
                        span: start_loc.until(self.prev_tok.span.end),
                        op,
                        expr,
                    })),
                    false => expr,
                },
            }),
            (PrefixOp::Deref, Expression::Prefix(_)) => Ok(PrefixExpr {
                span: start_loc.until(self.prev_tok.span.end),
                op,
                expr: match deref_counts_double {
                    true => Expression::Prefix(Box::new(PrefixExpr {
                        span: start_loc.until(self.prev_tok.span.end),
                        op,
                        expr,
                    })),
                    false => expr,
                },
            }),
            (PrefixOp::Ref | PrefixOp::Deref, _) => {
                self.errors.push(Error::new(
                    format!("prefix operator `{op}` requires an identifier on its right hand side"),
                    expr.span(),
                    self.lexer.source(),
                ));
                Ok(PrefixExpr {
                    span: start_loc.until(self.prev_tok.span.end),
                    op,
                    expr: Expression::Ident(Spanned {
                        span: Span::dummy(),
                        inner: "",
                    }),
                })
            }
            _ => Ok(PrefixExpr {
                span: start_loc.until(self.prev_tok.span.end),
                op,
                expr,
            }),
        }
    }

    fn grouped_expr(&mut self) -> Result<'src, Spanned<'src, Box<Expression<'src>>>> {
        let start_loc = self.curr_tok.span.start;
        // skip the opening parenthesis
        self.next()?;

        let expr = self.expression(0)?;
        self.expect_recoverable(
            TokenKind::RParen,
            "missing closing parenthesis",
            self.curr_tok.span,
        )?;

        Ok(Spanned {
            span: start_loc.until(self.prev_tok.span.end),
            inner: expr.into(),
        })
    }

    fn list_expr(&mut self) -> Result<'src, Spanned<'src, Vec<Expression<'src>>>> {
        let start_loc = self.curr_tok.span.start;
        self.next()?;

        if self.curr_tok.kind == TokenKind::RBracket {
            self.next()?;
            return Ok(Spanned {
                span: start_loc.until(self.curr_tok.span.end),
                inner: vec![],
            });
        }

        let mut contents = vec![self.expression(0)?];

        while self.curr_tok.kind == TokenKind::Slash {
            self.next()?;
            if self.curr_tok.kind == TokenKind::RBracket {
                break;
            }
            contents.push(self.expression(0)?);
        }

        self.expect(TokenKind::RBracket)?;

        return Ok(Spanned {
            span: start_loc.until(self.curr_tok.span.end),
            inner: contents,
        });
    }

    fn infix_expr(
        &mut self,
        start_loc: Location<'src>,
        lhs: Expression<'src>,
        op: InfixOp,
    ) -> Result<'src, Expression<'src>> {
        let right_prec = self.curr_tok.kind.prec().1;
        self.next()?;
        let rhs = self.expression(right_prec)?;

        Ok(Expression::Infix(
            InfixExpr {
                span: start_loc.until(self.prev_tok.span.end),
                lhs,
                op,
                rhs,
            }
            .into(),
        ))
    }

    fn assign_expr(
        &mut self,
        start_loc: Location<'src>,
        lhs: Expression<'src>,
        op: AssignOp,
    ) -> Result<'src, Expression<'src>> {
        let lhs_start_loc = lhs.span().start;
        let (assignee, assignee_ptr_count) =
            match self.reduce_prefix_expr_to_ident(lhs, lhs_start_loc, 0) {
                Ok(res) => res,
                Err(err) => {
                    self.errors.push(*err);
                    (
                        Spanned {
                            span: Span::dummy(),
                            inner: "",
                        },
                        0,
                    )
                }
            };

        let right_prec = self.curr_tok.kind.prec().1;
        self.next()?;
        let expr = self.expression(right_prec)?;

        let span = start_loc.until(self.prev_tok.span.end);

        if op == AssignOp::Basic {
            self.errors
                .push(Error::new("Der Operator `=` kann theoretisch für diesen Zweck genutzt werden,\n allerdings haben Wir uns dagegen entschieden.\n Nutzen Sie daher das `aendere` Schlüsselwort.".into(), span, self.lexer.source()));
        }

        Ok(Expression::Assign(
            AssignExpr {
                span,
                assignee,
                assignee_ptr_count,
                op,
                expr,
            }
            .into(),
        ))
    }

    /// Reduces nested [`PrefixExpr`]s which have the [`PrefixOp::Deref`] operator to an
    /// identifier. The returned span also includes the deref symbols `*`.
    fn reduce_prefix_expr_to_ident(
        &self,
        expr: Expression<'src>,
        start_loc: Location<'src>,
        count: usize,
    ) -> Result<'src, (Spanned<'src, &'src str>, usize)> {
        match expr {
            Expression::Ident(ident) => {
                let mut ident = ident;
                ident.span = start_loc.until(ident.span.end);
                Ok((ident, count))
            }
            Expression::Prefix(prefix) => {
                self.reduce_prefix_expr_to_ident(prefix.expr, start_loc, count + 1)
            }
            _ => Err(Error::new_boxed(
                "Die linke Seite einer Zuweisung darf nur ein Name sein.".to_string(),
                expr.span(),
                self.lexer.source(),
            )),
        }
    }

    fn call_expr(
        &mut self,
        start_loc: Location<'src>,
        expr: Expression<'src>,
    ) -> Result<'src, Expression<'src>> {
        let func = match expr {
            Expression::Ident(func) => CallBase::Ident(func),
            expr => CallBase::Expr(Box::new(expr)),
        };

        // skip opening parenthesis
        self.next()?;

        let mut args = vec![];
        if !matches!(self.curr_tok.kind, TokenKind::RParen | TokenKind::Eof) {
            args.push(self.expression(0)?);

            while self.curr_tok.kind == TokenKind::Slash {
                self.next()?;
                if let TokenKind::RParen | TokenKind::Eof = self.curr_tok.kind {
                    break;
                }
                args.push(self.expression(0)?);
            }
        }

        self.expect_recoverable(
            TokenKind::RParen,
            "Fehlende schließende runde Klammer.",
            self.curr_tok.span,
        )?;

        Ok(Expression::Call(
            CallExpr {
                span: start_loc.until(self.prev_tok.span.end),
                func,
                args,
            }
            .into(),
        ))
    }

    fn index_expr(
        &mut self,
        start_loc: Location<'src>,
        expr: Expression<'src>,
    ) -> Result<'src, Expression<'src>> {
        // skip opening bracket
        self.next()?;

        let index = self.expression(0)?;

        self.expect_recoverable(
            TokenKind::RBracket,
            "Fehlende schließende eckige Klammer.",
            self.curr_tok.span,
        )?;

        Ok(Expression::Index(
            IndexExpr {
                span: start_loc.until(self.prev_tok.span.end),
                expr,
                index,
            }
            .into(),
        ))
    }

    fn cast_expr(
        &mut self,
        start_loc: Location<'src>,
        expr: Expression<'src>,
    ) -> Result<'src, Expression<'src>> {
        // skip `as` token
        self.next()?;

        let type_ = self.type_()?;

        let span = start_loc.until(self.prev_tok.span.end);

        Ok(Expression::Cast(CastExpr { span, expr, type_ }.into()))
    }

    fn member_expr(
        &mut self,
        start_loc: Location<'src>,
        expr: Expression<'src>,
    ) -> Result<'src, Expression<'src>> {
        // skip the dot
        self.next()?;

        let member = self.expect_ident()?;

        Ok(Expression::Member(Box::new(MemberExpr {
            span: start_loc.until(self.curr_tok.span.end),
            expr,
            member,
        })))
    }
}
