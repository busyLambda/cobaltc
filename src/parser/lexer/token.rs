#[derive(Debug, Clone)]
pub struct Span {
    start: usize,
    end: usize,
    literal: String,
}

impl Span {
    pub fn new(start: usize, end: usize, literal: String) -> Self {
        Self {
            start,
            end,
            literal,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    // Types
    Tvoid,
    Tbool,
    Tint,
    Tfloat,
    Tchar,
    Tstring,
    // Keywords
    KwIf,
    KwMatch,
    KwFunc,
    KwImport,
    KwStruct,
    KwEnum,
    // Funnies
    Comma,
    RightArrow,
    Column,
    Coleq,
    Eq,
    EqEq,
    OpenParen,
    ClosedParen,
    OpenCurly,
    ClosedCurly,
    OpenBracket,
    ClosedBracket,
    // Literals
    Integer,
    Identifier,
    // Ops
    Add,
    Min,
    Mul,
    Div,
    Mod,
    Not,
    // Special
    Whitespace,
    Unknown,
    EOF,
}

impl TokenKind {
    pub fn is_type(&self) -> bool {
        matches!(
            self,
            Self::Tint
                | Self::Tvoid
                | Self::Tbool
                | Self::Tstring
                | Self::Tfloat
                | Self::Tchar
                | Self::Identifier
        )
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Self::Add
                | Self::Min
                | Self::Mul
                | Self::Div
                | Self::Mod
                | Self::Not
                | Self::Eq
                | Self::EqEq
        )
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn literal(&self) -> String {
        self.span.literal.clone()
    }

    pub fn pos(&self) -> (usize, usize) {
        (self.span.start, self.span.end)
    }
}
