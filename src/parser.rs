use std::iter::Peekable;

use logos::{Lexer, Logos};

use crate::{lexer::{Token, TokenStream}, types::SculkType};

#[derive(Clone, Copy, Debug)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Clone, Debug)]
pub enum ParserNode {
    Program(Vec<ParserNode>),
    Block(Vec<ParserNode>),
    NumberLiteral(i32),
    SelectorLiteral(SelectorTarget),
    Identifier(String),
    TypedIdentifier { name: String, ty: SculkType },
    VariableDeclaration { name: String, expr: Box<ParserNode>, ty: Option<SculkType> },
    FunctionDeclaration { name: String, args: Vec<ParserNode>, return_ty: Option<Box<ParserNode>>, body: Box<ParserNode> },
    Return(Box<ParserNode>),
    Operation(Box<ParserNode>, Box<ParserNode>, Operation),
}

impl ParserNode {
    pub fn is_num(&self) -> bool {
        match self {
            Self::NumberLiteral(_) => true,
            _ => false
        }
    }

    fn as_num(&self) -> i32 {
        match self {
            Self::NumberLiteral(num) => *num,
            _ => panic!("tried to get number from non-number node")
        }
    }

    pub fn as_identifier(&self) -> &str {
        match self {
            Self::Identifier(name) => name,
            Self::TypedIdentifier { name, .. } => name,
            _ => panic!("tried to get identifier from non-identifier node")
        }
    }

    pub fn as_type(&self) -> SculkType {
        match self {
            Self::TypedIdentifier { ty, .. } => ty.clone(),
            _ => panic!("tried to get type from non-typed identifier node")
        }
    }
}

macro_rules! expect_tok {
    ($parser:expr, $token:pat, $err:expr) => {
        match $parser.tokens.peek() {
            Some($token) => $parser.tokens.next(),
            _ => return $parser.error($err),
        };
    };
}

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            tokens: TokenStream::new(src),
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<ParserNode, &[ParseError]> {
        let mut statements = Vec::new();

        while self.tokens.peek().is_some() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(_) => {} // error already logged and handled
            };
        }

        if !self.errors.is_empty() {
            return Err(&self.errors);
        }

        Ok(ParserNode::Program(statements))
    }

    fn parse_block(&mut self) -> ParseResult {
        expect_tok!(self, Token::LeftBrace, "expected {");

        let mut statements = Vec::new();

        while self.tokens.peek() != Some(&Token::RightBrace) {
            statements.push(self.parse_statement()?);
        }

        expect_tok!(self, Token::RightBrace, "expected }");

        Ok(ParserNode::Block(statements))
    }

    fn parse_statement(&mut self) -> ParseResult {
        match self.tokens.peek() {
            Some(Token::Let) => self.parse_var_declaration(),
            Some(Token::Fn) => self.parse_func_declaration(),
            Some(Token::LeftBrace) => self.parse_block(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => self.parse_expression_statement(), // TODO: maybe this should just become exclusive to function calls
        }
    }

    fn parse_var_declaration(&mut self) -> ParseResult {
        expect_tok!(self, Token::Let, "expected let");

        let identifier = self.parse_typed_identifier(true)?;

        expect_tok!(self, Token::Equals, "expected =");

        let expr = self.parse_expression()?;

        expect_tok!(self, Token::Semicolon, "expected ;");

        match identifier {
            ParserNode::TypedIdentifier { name, ty } => Ok(ParserNode::VariableDeclaration { name, expr: Box::new(expr), ty: Some(ty) }),
            ParserNode::Identifier(name) => Ok(ParserNode::VariableDeclaration { name, expr: Box::new(expr), ty: None }),
            _ => self.error("expected identifier")
        }
    }

    fn parse_func_declaration(&mut self) -> ParseResult {
        expect_tok!(self, Token::Fn, "expected fn");

        let name = self.parse_identifier()?;

        expect_tok!(self, Token::LeftParens, "expected (");

        let mut args = Vec::new();
        
        if self.tokens.peek() != Some(&Token::RightParens) {
            let arg = self.parse_typed_identifier(false)?;
            args.push(arg);

            while self.tokens.peek() == Some(&Token::Comma) {
                self.tokens.next(); // consume the comma
                let arg = self.parse_typed_identifier(false)?;
                args.push(arg);
            }
        }

        expect_tok!(self, Token::RightParens, "expected )");

        let return_ty = match self.tokens.peek() {
            Some(Token::Arrow) => {
                self.tokens.next(); // consume the arrow
                let return_ty = self.parse_identifier()?;
                Some(Box::new(return_ty))
            },
            _ => None
        };

        let body = self.parse_block()?;

        Ok(ParserNode::FunctionDeclaration { name: name.as_identifier().to_string(), args, return_ty, body: Box::new(body) })
    }

    fn parse_return_statement(&mut self) -> ParseResult {
        expect_tok!(self, Token::Return, "expected return");

        let expr = self.parse_expression()?;

        expect_tok!(self, Token::Semicolon, "expected ;");

        Ok(ParserNode::Return(Box::new(expr)))
    }

    fn parse_expression_statement(&mut self) -> ParseResult {
        let expr = self.parse_expression()?;
        expect_tok!(self, Token::Semicolon, "expected ;");
        Ok(expr)
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_term()
    }

    fn parse_number(&mut self) -> ParseResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Number(n)) => Ok(ParserNode::NumberLiteral(*n)),
            _ => self.error("expected number"),
        }
    }

    fn parse_selector(&mut self) -> ParseResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Selector(target)) => Ok(ParserNode::SelectorLiteral(match target {
                'a' => SelectorTarget::AllPlayers,
                'p' => SelectorTarget::NearestPlayer,
                'r' => SelectorTarget::RandomPlayer,
                'e' => SelectorTarget::AllEntities,
                's' => SelectorTarget::ExecutingEntity,
                _ => unreachable!(),
            })),
            _ => self.error("expected selector"),
        }
    }

    fn parse_identifier(&mut self) -> ParseResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Identifier(identifier)) => Ok(ParserNode::Identifier(identifier.clone())),
            _ => self.error("expected identifier"),
        }
    }

    fn parse_primary(&mut self) -> ParseResult {
        match self.tokens.peek() {
            Some(Token::Number(_)) => self.parse_number(),
            Some(Token::Identifier(_)) => self.parse_identifier(),
            Some(Token::Selector(_)) => self.parse_selector(),
            Some(Token::LeftParens) => {
                self.tokens.next();
                let expr = self.parse_term();

                match self.tokens.next() {
                    Some(Token::RightParens) => expr,
                    _ => self.error("expected ) after expression"),
                }
            }
            _ => self.error("expected value or expression"),
        }
    }

    fn parse_term(&mut self) -> ParseResult {
        let mut expr = self.parse_factor()?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::Add => Operation::Add,
                Token::Subtract => Operation::Subtract,
                _ => break,
            };

            self.tokens.next();

            let term = self.parse_factor()?;
            expr = ParserNode::Operation(Box::new(expr), Box::new(term), op);
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult {
        let mut expr = self.parse_primary()?;

        while (self.tokens.peek().is_some()) {
            let op = match self.tokens.peek().unwrap() {
                Token::Multiply => Operation::Multiply,
                Token::Divide => Operation::Divide,
                Token::Modulo => Operation::Modulo,
                _ => break,
            };

            self.tokens.next();

            let term = self.parse_primary()?;
            expr = ParserNode::Operation(Box::new(expr), Box::new(term), op);
        }

        Ok(expr)
    }

    fn parse_typed_identifier(&mut self, ty_optional: bool) -> ParseResult {
        let name = match self.parse_identifier()? {
            ParserNode::Identifier(identifier) => identifier,
            _ => unreachable!(),
        };

        if ty_optional && self.tokens.peek() != Some(&Token::Colon) {
            return Ok(ParserNode::Identifier(name));
        }

        expect_tok!(self, Token::Colon, "expected :");

        let ty = match self.tokens.next() {
            Some(Token::Identifier(name)) => match name.as_str() {
                "int" => SculkType::Integer,
                "selector" => SculkType::Selector,
                _ => SculkType::Struct(name.clone())
            }
            _ => return self.error("expected valid type"),
        };

        Ok(ParserNode::TypedIdentifier { name, ty })
    }

    fn error(&mut self, error: impl Into<String>) -> ParseResult {
        let error = ParseError::new(error, self.tokens.line(), self.tokens.col());
        self.errors.push(error);
        self.recover();
        Err(())
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    // try to recover from an error by jumping to the next statement
    fn recover(&mut self) {
        while self.tokens.peek().is_some() {
            match self.tokens.peek().unwrap() {
                Token::Semicolon => {
                    self.tokens.next();
                    return;
                }
                Token::Let => return,
                _ => self.tokens.next(),
            };
        }
    }
}

type ParseResult = Result<ParserNode, ()>;

#[derive(Clone, Debug)]
pub struct ParseError {
    error: String,
    line: usize,
    col: usize,
}

impl ParseError {
    pub fn new(error: impl Into<String>, line: usize, col: usize) -> Self {
        ParseError {
            error: error.into(),
            line,
            col,
        }
    }
}

// TODO: move out of this file
#[derive(Clone, Copy, Debug)]
pub enum SelectorTarget {
    AllPlayers,
    NearestPlayer,
    RandomPlayer,
    AllEntities,
    ExecutingEntity,
}
