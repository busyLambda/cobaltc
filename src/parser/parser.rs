use std::{ops::Range, slice::Iter};

use super::{
    error::{Location, PropagatorError, StructureError, SyncEOF},
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

    pub fn report_propagators(&mut self, errors: Vec<PropagatorError>, span: Range<usize>) {
        errors.into_iter().for_each(|err| {
            self.report_error(StructureError::from_propagator(
                err,
                Location::new(span.clone()),
            ))
        });
    }
    
    /// Used to sync the input if we get an error given a function (predicate)
    /// that determines what tokens can be used for syncing.
    /// See [`SyncStatus`](SyncStatus) for more on this.
    pub fn sync_to(&mut self, predicate: fn(TokenKind) -> SyncStatus) -> Option<SyncEOF> {
        loop {
            if let Some(tok) = self.peek() {
                match predicate(tok.kind()) {
                    SyncStatus::Consume => {
                        self.next();
                        return None
                    }
                    SyncStatus::Continue => {
                        self.next();
                    }
                    SyncStatus::Stop => {
                        return None
                    }
                }
            } else {
                // GOT EOF
                break
            }
        }
        Some(SyncEOF)
    }
}

/// Used to make a function (`fn(TokenKind) -> SyncStatus`) for syncing tokens by passing it to [`sync_to`](Parser::sync_to).
/// - `Consume` takes the sync token.
/// - `Stop` stops at sync token.
/// - `Continue` continues since we haven't gotten a sync token yet.
pub enum SyncStatus {
    Consume,
    Stop,
    Continue,
}