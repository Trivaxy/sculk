use logos::{Lexer, Logos};

#[derive(Clone, Logos, Debug, PartialEq)]
pub enum Token {
    #[regex(r"[0-9]+", |tok| tok.slice().parse())]
    Number(i32),

    #[token("+")]
    Add,

    #[token("-")]
    Subtract,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("%")]
    Modulo,

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

    #[regex(r"[a-zA-Z_]+[a-zA-Z0-9_]*", |tok| tok.slice().to_owned())]
    Identifier(String),

    #[regex(r"@[p|r|a|e|s]", |tok| tok.slice().chars().nth(1).unwrap())]
    Selector(char),

    // used internally to keep track of what line we're on
    // the parser will never receive this from the token stream
    #[token("\n")]
    Newline,

    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,
}

pub struct TokenStream<'a> {
    lexer: Lexer<'a, Token>,
    current: Option<Token>,
    next: Option<Token>,
    line: usize,
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
            line: 0,
            col: 0,
        }
    }

    pub fn next(&mut self) -> Option<&Token> {
        self.discard_newlines();

        self.current = self.next.take();
        self.next = self.lexer.next();

        self.discard_newlines();

        self.col = self.lexer.span().start;
        self.current.as_ref()
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.discard_newlines();
        self.next.as_ref()
    }

    fn discard_newlines(&mut self) {
        while let Some(Token::Newline) = self.next {
            self.next = self.lexer.next();
            self.line += 1;
            self.col = 0;
        }
    }

    pub fn current(&mut self) -> Option<&Token> {
        self.current.as_ref()
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn col(&self) -> usize {
        self.col
    }
}
