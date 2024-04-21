use std::{fmt::Error, ops::Range, slice::Iter};

use crate::parser::error::StructureError;

use self::{
    error::{ErrorLocation, Location, ParseError, ParseResult, PropagatorError, PropagatorResult},
    lexer::token::{Token, TokenKind},
    parser::Parser,
};

pub mod error;
pub mod lexer;
pub mod parser;

/*
    S -> E end
    E -> T {("+" | "-") T}
    T -> F {("*" | "/") F}
    F -> P ["^" F]
    P -> v | "(" E ")" | "-" T
*/

#[derive(Debug)]
pub enum Value {
    Ident(String),
    Integer(i128),
}

#[derive(Debug)]
pub enum Expression {
    Value(Value),
    Paren(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    Neg(Box<Expression>),
    Array(Vec<Box<Expression>>),
}

impl Type {
    fn from_token(token: &Token) -> Type {
        match token.kind() {
            TokenKind::Tvoid => Type::Int,
            TokenKind::Tint => Type::Int,
            TokenKind::Tchar => Type::Char,
            TokenKind::Tstring => Type::String,
            TokenKind::Tfloat => Type::String,
            TokenKind::Tbool => Type::Bool,
            TokenKind::Identifier => Type::Custom(token.literal()),
            _ => unreachable!(), // Caller should check tbh but could do a funny one here.
        }
    }
}
impl<'a> Parser<'a> {
    fn array_type(&mut self) -> PropagatorResult<Type> {
        if self.is_match(TokenKind::OpenBracket) {
            self.next().unwrap();

            let ty = self.type_()?;

            if self.is_match(TokenKind::ClosedBracket) {
                self.next().unwrap();

                Ok(Type::Array(Box::new(ty)))
            } else {
                Err((
                    Type::Array(Box::new(Type::Void)),
                    PropagatorError::new("Expected `]`.", Location::new(0..0)),
                ))
            }
        } else {
            Err((
                Type::Array(Box::new(Type::Void)),
                PropagatorError::new("Expected `[`.", Location::new(0..0)),
            ))
        }
    }

    fn generic_type(&mut self) -> PropagatorResult<Type> {
        if self.is_match(TokenKind::At) {
            self.next();

            if let Some(tok) = self.peek() {
                match tok.kind() {
                    TokenKind::Identifier => Ok(Type::Generic(self.next().unwrap().literal())),
                    _ => {
                        let (start, end) = tok.pos();
                        Err((
                            Type::Generic("".to_string()),
                            PropagatorError::new(
                                "Expected a name after `@` for generic type.",
                                Location::new(start..end),
                            ),
                        ))
                    }
                }
            } else {
                let (start, end) = self.prev_pos();

                Err((
                    Type::Generic("".to_string()),
                    PropagatorError::new(
                        "Expected name for generic type but found end of file.",
                        Location::new(start..end),
                    ),
                ))
            }
        } else {
            if let None = self.next() {
                let (start, end) = self.prev_pos();

                Err((
                    Type::Generic("".to_string()),
                    PropagatorError::new(
                        "Expected '@' but found end of file.",
                        Location::new(start..end),
                    ),
                ))
            } else {
                unreachable!()
            }
        }
    }

    fn type_(&mut self) -> PropagatorResult<Type> {
        if let Some(tok) = self.peek() {
            let (start, end) = tok.pos();
            if tok.kind().is_type() {
                let tok = self.next().unwrap();
                Ok(Type::from_token(tok))
            } else {
                match tok.kind() {
                    TokenKind::OpenBracket => self.array_type(),
                    TokenKind::At => self.generic_type(),
                    _ => Err((
                        Type::Void,
                        PropagatorError::new(
                            "Expected a type but found no such thing.",
                            Location::new(start..end),
                        ),
                    )),
                }
            }
        } else {
            Err((
                Type::Void,
                PropagatorError::new(
                    "Expected a type but found end of file.",
                    Location::new(0..0),
                ),
            ))
        }
    }

    fn func_param(&mut self) -> PropagatorResult<FuncParam> {
        if self.is_match(TokenKind::Identifier) {
            let errors = Vec::<PropagatorError>::new();
            let tok = self.next().unwrap();
            let (start, _) = tok.pos();
            let name = Name::new(tok.literal());

            let ty = match self.type_() {
                Ok(ty) => ty,
                Err((ty, err)) => return Err((FuncParam::new(name, ty), err)),
            };

            let func_param = FuncParam::new(name, ty);

            Ok(func_param)
        } else {
            let tok = self.next().unwrap();
            let (start, end) = tok.pos();
            Err((
                FuncParam::new(Name::new("".to_string()), Type::Void),
                PropagatorError::new("Expected identifier.", Location::new(start..end)),
            ))
        }
    }

    fn func_params(&mut self) -> Vec<FuncParam> {
        let mut errors = Vec::<PropagatorError>::new();
        let mut params = Vec::new();
        let start: usize;
        if self.is_match(TokenKind::OpenParen) {
            start = self.next().unwrap().pos().0;
        } else {
            if let Some(tok) = self.peek() {
                start = tok.pos().0;

                let err = PropagatorError::new(
                    "Expected `(` for function parameters.",
                    Location::new(tok.pos().0..tok.pos().1),
                );

                errors.push(err);
            } else {
                self.eof("Expected `(` for function parameters but found end of file.");

                return params;
            }
        }

        'eater: while let Some(tok) = self.peek() {
            if tok.kind() == TokenKind::ClosedParen {
                self.next();
                break;
            }

            let param = match self.func_param() {
                Ok(p) => p,
                Err((func_param, err)) => {
                    errors.push(err);

                    loop {
                        if let Some(tok) = self.peek() {
                            match tok.kind() {
                                TokenKind::KwFunc => break 'eater,
                                TokenKind::NewLine => {
                                    self.next();
                                    break;
                                }
                                TokenKind::ClosedParen => {
                                    self.next();
                                    params.push(func_param);
                                    break 'eater;
                                }
                                TokenKind::Comma => {
                                    self.next();
                                    break;
                                }
                                _ => {
                                    self.next().unwrap();
                                }
                            }
                        } else {
                            break;
                        }
                    }

                    func_param
                }
            };
            params.push(param);
            if self.is_match(TokenKind::Comma) {
                self.next();
            }
        }

        let end = self.prev_pos().1;

        let span = start..end;
        errors.into_iter().for_each(|err| {
            self.report_error(StructureError::from_propagator(
                err,
                Location::new(span.clone()),
            ))
        });

        params
    }

    pub fn func(&mut self) -> Func {
        let mut errors = Vec::<PropagatorError>::new();
        let start;

        if self.is_match(TokenKind::KwFunc) {
            start = self.next().unwrap().pos().0;
        } else {
            unreachable!() // The caller must check.
        }

        if let Some(tok) = self.peek() {
            if tok.kind() == TokenKind::Identifier {
                let tok = self.next().unwrap();
                let name = Name::new(tok.literal());

                let params = self.func_params();

                let ret_ty = match self.type_() {
                    Ok(ty) => ty,
                    Err((ty, err)) => {
                        if self.is_eof() {
                            self.eof("Expected return type for function but found end of file.");

                            return Func::new(name, params, ty);
                        }

                        errors.push(err);

                        ty
                    }
                };

                // FIXME: COULD SIMPLYFY ALLAT!
                let end = self.prev_pos().1;

                let span = start..end;
                errors.into_iter().for_each(|err| {
                    self.report_error(StructureError::from_propagator(
                        err,
                        Location::new(span.clone()),
                    ))
                });

                Func::new(name, params, ret_ty)
            } else {
                // TODO: Error recovery after not matching func <ident>.
                todo!()
            }
        } else {
            self.eof("Expected a function name but found end of file.");

            Func::new(Name::new("".to_string()), vec![], Type::Void)
        }
    }

    // array := [ { expression {"," expression}* }? ]
    // Note: Expression may be an array
    fn array(&mut self) -> ParseResult<Expression> {
        if self.is_match(TokenKind::OpenBracket) {
            self.next();
            let mut array = Vec::new();
            while !self.is_match(TokenKind::ClosedBracket) {
                let expr = self.expression()?;
                array.push(Box::new(expr));
                if self.is_match(TokenKind::Comma) {
                    self.next();
                }
            }
            self.next();
            Ok(Expression::Array(array))
        } else {
            Err(ParseError::new(
                "Expected opening bracket".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    fn expression(&mut self) -> ParseResult<Expression> {
        if let Some(peek) = self.peek() {
            match peek.kind() {
                TokenKind::OpenParen => self.numeric(),
                TokenKind::Integer => self.numeric(),
                TokenKind::Min => self.numeric(),
                TokenKind::OpenBracket => self.array(),
                TokenKind::Identifier => self.numeric(),
                _ => Err(ParseError::new(
                    format!("Expected expression found: {:?}", peek.kind()),
                    ErrorLocation::new(),
                )),
            }
        } else {
            Err(ParseError::new(
                "Expected expression but found end of file.".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    fn value(&mut self) -> ParseResult<Expression> {
        if self.is_match(TokenKind::Integer) {
            let value = self.next().unwrap().literal().parse::<i128>().unwrap();
            Ok(Expression::Value(Value::Integer(value)))
        } else if self.is_match(TokenKind::Identifier) {
            let value = self.next().unwrap().literal();
            Ok(Expression::Value(Value::Ident(value)))
        } else {
            Err(ParseError::new(
                "Expected number".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    fn term(&mut self) -> ParseResult<Expression> {
        let mut expr = self.factor()?;
        while self.is_match(TokenKind::Mul) || self.is_match(TokenKind::Div) {
            let op = self.next().unwrap().clone().kind();
            let right = self.factor()?;
            expr = match op {
                TokenKind::Mul => Expression::Mul(Box::new(expr), Box::new(right)),
                TokenKind::Div => Expression::Div(Box::new(expr), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expression> {
        if self.is_match(TokenKind::OpenParen) {
            self.next();
            let expr = self.numeric().unwrap();
            if self.is_match(TokenKind::ClosedParen) {
                self.next();
                Ok(Expression::Paren(Box::new(expr)))
            } else {
                Err(ParseError::new(
                    "Expected closing parenthesis".to_string(),
                    ErrorLocation::new(),
                ))
            }
        } else if self.is_match(TokenKind::Min) {
            self.next();
            let expr = self.term()?;
            Ok(Expression::Neg(Box::new(expr)))
            // TODO: Make it work for floats too.
        } else if self.is_match(TokenKind::Integer) || self.is_match(TokenKind::Identifier) {
            self.value()
        } else {
            Err(ParseError::new(
                "Expected number, parenthesis or minus".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    fn numeric(&mut self) -> ParseResult<Expression> {
        let mut expr = self.term()?;
        while self.is_match(TokenKind::Add) || self.is_match(TokenKind::Min) {
            let op = self.next().unwrap().clone().kind();
            let right = self.term()?;
            expr = match op {
                TokenKind::Add => Expression::Add(Box::new(expr), Box::new(right)),
                TokenKind::Min => Expression::Sub(Box::new(expr), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(expr)
    }
    fn declaration(&mut self) -> ParseResult<Var> {
        let mut errors = Vec::<PropagatorError>::new();

        if self.is_match(TokenKind::Identifier) {
            let tok = self.next().unwrap();
            let (start, _) = tok.pos();
            let name = Name::new(tok.literal());
            let ty = match self.type_() {
                Ok(ty) => ty,
                Err((ty, err)) => {
                    errors.push(err);

                    ty
                }
            };

            // FIXME: COULD SIMPLYFY ALLAT!
            let end = self.prev_pos().1;

            let span = start..end;
            errors.into_iter().for_each(|err| {
                self.report_error(StructureError::from_propagator(
                    err,
                    Location::new(span.clone()),
                ))
            });

            Ok(Var::declare(name, ty))
        } else {
            Err(ParseError::new(
                "Expected identifier".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    fn name(&mut self) -> ParseResult<Name> {
        if self.is_match(TokenKind::Identifier) {
            Ok(Name::new(self.next().unwrap().literal()))
        } else {
            Err(ParseError::new(
                "Expected identifier.".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    // assign := identifier = expr
    fn assign(&mut self) -> ParseResult<Var> {
        let name = match self.name() {
            Ok(n) => n,
            // TODO: Report here.
            Err(e) => return Err(e),
        };

        if self.is_match(TokenKind::Eq) {
            self.next().unwrap();
        } else {
            return Err(ParseError::new(
                "Expected `=`.".to_string(),
                ErrorLocation::new(),
            ));
        }

        let expr = self.expression()?;

        Ok(Var::assign(name, expr))
    }

    // Attempts to syncronize after an error.
    fn sync_tokens(&mut self) {
        todo!()
    }
}

#[derive(Debug)]
pub struct FuncParam {
    name: Name,
    ty: Type,
}

impl FuncParam {
    fn new(name: Name, ty: Type) -> FuncParam {
        FuncParam { name, ty }
    }
}

#[derive(Debug)]
pub enum Type {
    Void,
    Int,
    Char,
    String,
    Bool,
    Array(Box<Type>),
    Custom(String), // Add loc
    Generic(String),
}

#[derive(Debug)]
pub struct Func {
    name: Name,
    params: Vec<FuncParam>,
    ret_ty: Type,
}

impl Func {
    fn new(name: Name, params: Vec<FuncParam>, ret_ty: Type) -> Func {
        Func {
            name,
            params,
            ret_ty,
        }
    }
}

#[derive(Debug)]
pub struct Name {
    name: String,
}

impl Name {
    fn new(name: String) -> Name {
        Name { name }
    }

    fn name(&self) -> &str {
        &self.name
    }
}

// declare := identifier ":" type
// assign := identifier "=" expression
// declare_assign := identifier ":=" expression
// complete := identifier ":" type "=" expression
#[derive(Debug)]
pub struct Var {
    is_assignment: bool,
    name: Name,
    ty: Option<Type>,
    expr: Option<Expression>,
}

impl Var {
    fn is_declaration(&self) -> bool {
        self.ty.is_some() && self.expr.is_none() && !self.is_assignment
    }

    fn is_assignment(&self) -> bool {
        self.ty.is_none() && self.expr.is_some() && self.is_assignment
    }

    fn is_declare_assign(&self) -> bool {
        self.ty.is_none() && self.expr.is_some() && !self.is_assignment
    }

    fn is_complete(&self) -> bool {
        self.ty.is_some() && self.expr.is_some()
    }

    fn declare(name: Name, ty: Type) -> Var {
        Var {
            is_assignment: false,
            name,
            ty: Some(ty),
            expr: None,
        }
    }

    fn assign(name: Name, expr: Expression) -> Var {
        Var {
            is_assignment: true,
            name,
            ty: None,
            expr: Some(expr),
        }
    }

    fn declare_assign_with_inferance(name: Name, ty: Type, expr: Expression) -> Var {
        Var {
            is_assignment: false,
            name,
            ty: Some(ty),
            expr: Some(expr),
        }
    }
}

#[test]
fn test_assign() {
    let input = "a = 5 + 5 / (10 / 2)";
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(&tokens);
    let assign = parser.assign().unwrap();
    println!("{:?}", assign)
}

#[test]
fn test_arithmetic() {
    let input = "(a + b) * 2";
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(&tokens);
    let expr = parser.numeric().unwrap();
    println!("{:?}", expr)
}

#[test]
fn test_func_param() {
    let input = "a int";
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(&tokens);
    let param = parser.func_param().unwrap();
    println!("{:?}", param)
}

#[test]
fn test_array() {
    let input = "[[[a, b, b-2], b], [c, d]]";
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(&tokens);
    let array = parser.expression().unwrap();
    println!("array: {:?}", array)
}

#[test]
fn test_func_params() {
    let input = "(a int, b char, c string)";
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(&tokens);
    let params = parser.func_params();
    println!("{:?}", params)
}

#[test]
fn test_func() {
    let input = "func foo(bar int) int";
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(&tokens);
    let func = parser.func();
    println!("{:?}", func)
}
