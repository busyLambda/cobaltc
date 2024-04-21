use std::slice::Iter;

use super::{
    error::{Location, StructureError},
    lexer::token::{Token, TokenKind},
};

pub struct Parser<'a> {
    input: Iter<'a, Token>,
    errors: Vec<StructureError>,
    prev: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn is_eof(&self) -> bool {
        self.peek().is_none()
    }

    pub fn prev_pos(&self) -> (usize, usize) {
        if let Some(tok) = self.prev.clone() {
            tok.pos()
        } else {
            (0, 0)
        }
    }

    pub fn eof(&mut self, message: &str) {
        let (start, end) = self.prev_pos();
        let location = Location::new(start..end);
        let err = StructureError::new(message, location.clone(), location);

        self.report_error(err)
    }

    pub fn errors(&self) -> &Vec<StructureError> {
        &self.errors
    }

    pub fn report_error(&mut self, err: StructureError) {
        self.errors.push(err)
    }

    pub fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            input: tokens.iter(),
            errors: Vec::new(),
            prev: None,
        }
    }

    // Peeks and returns.
    pub fn peek(&self) -> Option<&Token> {
        self.input.clone().next()
    }

    pub fn peek2(&mut self) -> Option<&Token> {
        self.input.clone().nth(1)
    }

    // Eats and returns.
    pub fn next(&mut self) -> Option<&Token> {
        let next = self.input.next();
        if let Some(n) = next {
            self.prev = Some(n.clone());
        }
        next
    }

    pub fn is_match(&mut self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(t) => t.kind() == kind,
            None => false,
        }
    }

}
