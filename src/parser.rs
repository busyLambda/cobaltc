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
    Ident,
    Number(f64),
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
}

type ParseResult<T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    fn number(&mut self) -> ParseResult<Expression> {
        if self.is_match(TokenKind::Integer) {
            self.next();
            Ok(Expression::Value(Value::Number(0.0)))
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
            let expr = self.expression().unwrap();
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
        } else if self.is_match(TokenKind::Integer) {
            self.number()
        } else {
            Err(ParseError::new(
                "Expected number, parenthesis or minus".to_string(),
                ErrorLocation::new(),
            ))
        }
    }

    fn expression(&mut self) -> ParseResult<Expression> {
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

#[test]
fn test_arithmetic() {
    let input = "(-4 * 4) - (15 + 4)";
    let mut lexer = lexer::Lexer::new(input);
    let mut tokens = Vec::<Token>::new();

    loop {
        let token = lexer.next();
        tokens.push(token.clone());
        if token.kind() == TokenKind::EOF {
            break;
        }
    }

    let tokens = tokens
        .into_iter()
        .filter(|t| t.kind() != TokenKind::Whitespace)
        .collect::<Vec<Token>>();

    let mut parser = Parser::new(&tokens);
    let expr = parser.expression().unwrap();
    println!("{:?}", expr)
}