use miette::SourceSpan;

pub type Range = std::ops::Range<usize>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub tpe: TokenType,
    pub line: usize,
    pub span: Span,
}

impl Token {
    pub const fn new(tpe: TokenType, line: usize, offset: usize, len: usize) -> Self {
        Self {
            tpe,
            line,
            span: Span::new(offset, len),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub offset: usize,
    pub len: usize,
}

impl Span {
    pub const fn new(offset: usize, len: usize) -> Self {
        Self { offset, len }
    }
}

impl From<Range> for Span {
    fn from(range: Range) -> Self {
        Self {
            offset: range.start,
            len: range.len(),
        }
    }
}

impl From<Span> for Range {
    fn from(span: Span) -> Self {
        span.offset..(span.offset + span.len)
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        Range::from(span).into()
    }
}

impl Span {
    pub fn union(self, other: impl Into<Self>) -> Range {
        let other = other.into();
        self.offset..(other.offset + other.len)
    }

    pub fn shrink_to_end(self) -> Self {
        Self {
            offset: self.offset + self.len,
            len: 1,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Identifiers
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl From<&str> for TokenType {
    fn from(ident: &str) -> Self {
        match ident {
            "and" => Self::And,
            "class" => Self::Class,
            "else" => Self::Else,
            "false" => Self::False,
            "for" => Self::For,
            "fun" => Self::Fun,
            "if" => Self::If,
            "nil" => Self::Nil,
            "or" => Self::Or,
            "print" => Self::Print,
            "return" => Self::Return,
            "super" => Self::Super,
            "this" => Self::This,
            "true" => Self::True,
            "var" => Self::Var,
            "while" => Self::While,
            _ => Self::Identifier,
        }
    }
}
