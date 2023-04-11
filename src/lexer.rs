use logos::{Lexer, Logos};

#[derive(Default)]
struct LexerExtras {
    line: usize,
    last_line_idx: usize,
}

#[derive(Clone, Logos, Debug, PartialEq)]
#[logos(extras = LexerExtras)]
pub enum Token<'a> {
    #[regex(r"[0-9]+", |tok| tok.slice().parse())]
    Number(i32),

    #[token("+")]
    Plus,

    #[token("-")]
    Hyphen,

    #[token("*")]
    Asterisk,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token(">")]
    GreaterThan,

    #[token("<")]
    LessThan,

    #[token(">=")]
    GreaterThanOrEquals,

    #[token("<=")]
    LessThanOrEquals,

    #[token("==")]
    EqualsEquals,

    #[token("!=")]
    NotEquals,

    #[token("!")]
    Not,

    #[token("+=")]
    AddEquals,

    #[token("-=")]
    SubtractEquals,

    #[token("*=")]
    MultiplyEquals,

    #[token("/=")]
    DivideEquals,

    #[token("%=")]
    ModuloEquals,

    #[token("(")]
    LeftParens,

    #[token(")")]
    RightParens,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(";")]
    Semicolon,

    #[token("let")]
    Let,

    #[token("fn")]
    Fn,

    #[token("return")]
    Return,

    #[token("if")]
    If,

    #[token("else", priority = 20)]
    Else,

    #[regex(r"true|false", |tok| tok.slice().parse())]
    Bool(bool),

    #[token("=")]
    Equals,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("->")]
    Arrow,

    #[regex(r"[a-zA-Z_]+[a-zA-Z0-9_]*", |tok| tok.slice())]
    Identifier(&'a str),

    #[regex(r"@[p|r|a|e|s]", |tok| tok.slice().chars().nth(1).unwrap())]
    Selector(char),

    #[token("\n", |lex| {
        lex.extras.line += 1;
        lex.extras.last_line_idx = lex.span().end;
        logos::Skip
    })]
    Newline,

    #[error]
    #[regex(r#"[ \t\f]+"#, logos::skip)]
    Error,
}

pub struct TokenStream<'a> {
    lexer: Lexer<'a, Token<'a>>,
    current: Option<Token<'a>>,
    next: Option<Token<'a>>,
    col: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Token::lexer(src);
        let next = lexer.next();
        Self {
            lexer,
            current: None,
            next,
            col: 0,
        }
    }

    pub fn next(&mut self) -> Option<&Token> {
        self.current = self.next.take();
        self.next = self.lexer.next();

        self.col = self.lexer.span().start - self.lexer.extras.last_line_idx;

        self.current.as_ref()
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.next.as_ref()
    }

    pub fn current(&mut self) -> Option<&Token> {
        self.current.as_ref()
    }

    pub fn line(&self) -> usize {
        self.lexer.extras.line
    }

    pub fn col(&self) -> usize {
        self.col
    }
}
