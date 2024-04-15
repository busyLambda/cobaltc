use std::{fmt::Error, ops::Range, slice::Iter};

use self::lexer::token::{Token, TokenKind};

mod lexer;

pub struct Parser<'a> {
    input: Iter<'a, Token>,
    errors: Vec<ParseError>,
}

#[derive(Debug)]
struct ParseError {
    message: String,
    location: ErrorLocation,
}

impl ParseError {
    fn new(message: String, location: ErrorLocation) -> ParseError {
        ParseError { message, location }
    }
}

#[derive(Debug)]
struct ErrorLocation {
    structure: Location,
    // Structures don't return errors while propagator parsers do,
    // These errors travel up the chain until the structure reports it.
    propagator: Location,
}

impl ErrorLocation {
    pub fn new() -> ErrorLocation {
        ErrorLocation {
            structure: Location {
                span: 0..0,
                line: 0,
                column: 0,
            },
            propagator: Location {
                span: 0..0,
                line: 0,
                column: 0,
            },
        }
    }
}

#[derive(Debug)]
struct Location {
    span: Range<usize>,
    line: usize,
    column: usize,
}

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

type ParseResult<T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    fn func_param(&mut self) -> ParseResult<FuncParam> {
        if self.is_match(TokenKind::Identifier) {
            let name = Name::new(self.next().unwrap().literal());
            if let Some(tok) = self.peek() {
                if tok.kind().is_type() {
                    let ty = Type::from_token(self.next().unwrap());
                    let func_param = FuncParam::new(name, ty);

                    Ok(func_param)
                } else {
                    Err(ParseError::new(
                        "Expected type but found end of file.".to_string(),
                        ErrorLocation::new(),
                    ))
                }
            } else {
                Err(ParseError::new(
                    "Expected type but found end of file.".to_string(),
                    ErrorLocation::new(),
                ))
            }
        } else {
            Err(ParseError::new(
                "Expected identifier".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    fn func_params(&mut self) -> ParseResult<Vec<FuncParam>> {
        let mut params = Vec::new();
        if self.is_match(TokenKind::OpenParen) {
            self.next();
        } else {
            return Err(ParseError::new(
                "Expected opening parenthesis".to_string(),
                ErrorLocation::new(),
            ));
        }

        while let Some(tok) = self.peek() {
            if tok.kind() == TokenKind::ClosedParen {
                self.next();
                break;
            }

            let param = self.func_param()?;
            params.push(param);
            if self.is_match(TokenKind::Comma) {
                self.next();
            }
        }
        Ok(params)
    }

    fn func(&mut self) -> ParseResult<Func> {
        if self.is_match(TokenKind::KwFunc) {
            self.next();
        } else {
            return Err(ParseError::new(
                "Expected function keyword".to_string(),
                ErrorLocation::new(),
            ));
        }

        if let Some(tok) = self.peek() {
            if tok.kind() == TokenKind:: Identifier {
                let tok = self.next().unwrap();
                let name = Name::new(tok.literal());
        
        let params = self.func_params()?;
        
        Ok(Func::new(name, params))
            } else {
                return Err(ParseError::new(
                    "Expected identifier".to_string(),
                    ErrorLocation::new(),
                ));
            }
        } else {
            return Err(ParseError::new(
                "Expected identifier".to_string(),
                ErrorLocation::new(),
            ));
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

    pub fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            input: tokens.iter(),
            errors: Vec::new(),
        }
    }

    fn raise_error(&mut self, message: String, location: ErrorLocation) {
        self.errors.push(ParseError { message, location });
    }

    // Peeks and returns.
    fn peek(&mut self) -> Option<&Token> {
        self.input.clone().next()
    }

    fn peek2(&mut self) -> Option<&Token> {
        self.input.clone().nth(1)
    }

    // Eat's and returns.
    fn next(&mut self) -> Option<&Token> {
        self.input.next()
    }

    fn is_match(&mut self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(t) => t.kind() == kind,
            None => false,
        }
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
}

#[derive(Debug)]
pub struct Func {
    name: Name,
    params: Vec<FuncParam>,
}

impl Func {
    fn new(name: Name, params: Vec<FuncParam>) -> Func {
        Func { name, params }
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
    let params = parser.func_params().unwrap();
    println!("{:?}", params)
}

#[test]
fn test_func() {
    let input = "func foo(bar int)";
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(&tokens);
    let func = parser.func().unwrap();
    println!("{:?}", func)
}
