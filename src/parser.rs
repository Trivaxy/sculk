use std::collections::HashMap;

use crate::lexer::{Token, TokenStream};

#[derive(Clone, Copy, Debug)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    GreaterThan,
    LessThan,
    GreaterThanOrEquals,
    LessThanOrEquals,
    CheckEquals,
    NotEquals,
    Not,
    Negate,
}

#[derive(Clone, Copy, Debug)]
pub struct JumpInfo {
    pub may_return: bool,
    pub may_break: bool,
}

impl JumpInfo {
    pub fn no_jumps() -> Self {
        Self {
            may_return: false,
            may_break: false,
        }
    }

    pub fn new(may_return: bool, may_break: bool) -> Self {
        Self {
            may_return,
            may_break,
        }
    }

    pub fn may_jump(&self) -> bool {
        self.may_return || self.may_break
    }

    pub fn aggregate(&mut self, other: Self) {
        self.may_return |= other.may_return;
        self.may_break |= other.may_break;
    }
}

#[derive(Clone, Debug)]
pub enum ParserNode {
    Program(Vec<ParserNode>),
    Block(Vec<ParserNode>),
    NumberLiteral(i32),
    BoolLiteral(bool),
    Identifier(String),
    TypedIdentifier {
        name: String,
        ty: String,
    },
    VariableDeclaration {
        name: String,
        expr: Box<ParserNode>,
        ty: Option<String>,
    },
    VariableAssignment {
        name: String,
        expr: Box<ParserNode>,
    },
    FunctionDeclaration {
        name: String,
        args: Vec<ParserNode>,
        return_ty: Option<String>,
        body: Box<ParserNode>,
    },
    Return(Option<Box<ParserNode>>),
    FunctionCall {
        name: String,
        args: Vec<ParserNode>,
    },
    Operation(Box<ParserNode>, Box<ParserNode>, Operation),
    OpEquals {
        name: String,
        expr: Box<ParserNode>,
        op: Operation,
    },
    Unary(Box<ParserNode>, Operation),
    If {
        // see rebranch::rebranch
        cond: Box<ParserNode>,
        body: Box<ParserNode>,
        else_ifs: Vec<(ParserNode, ParserNode)>,
        else_body: Option<Box<ParserNode>>,
    },
    For {
        // see rebranch::rebranch
        init: Box<ParserNode>,
        cond: Box<ParserNode>,
        step: Box<ParserNode>,
        body: Box<ParserNode>,
    },
    StructDefinition {
        name: String,
        fields: Vec<ParserNode>,
    },
    MemberAccess {
        expr: Box<ParserNode>,
        member: String,
    },
    Break,
    CommandLiteral(String),
}

impl ParserNode {
    pub fn is_num(&self) -> bool {
        match self {
            Self::NumberLiteral(_) => true,
            _ => false,
        }
    }

    fn as_num(&self) -> i32 {
        match self {
            Self::NumberLiteral(num) => *num,
            _ => panic!("tried to get number from non-number node"),
        }
    }

    pub fn as_identifier(&self) -> &str {
        match self {
            Self::Identifier(name) => name,
            Self::TypedIdentifier { name, .. } => name,
            _ => panic!("tried to get identifier from non-identifier node"),
        }
    }

    pub fn as_typed_identifier(&self) -> (&str, &str) {
        match self {
            Self::TypedIdentifier { name, ty } => (name, ty),
            _ => panic!("tried to get typed identifier from non-typed identifier node"),
        }
    }

    pub fn as_program(&self) -> &[ParserNode] {
        match self {
            Self::Program(stmts) => stmts,
            _ => panic!("tried to get program from non-program node"),
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

    pub fn parse(mut self) -> ParserOutput {
        let mut functions = Vec::new();

        while self.tokens.peek().is_some() {
            match self.parse_func_declaration() {
                Ok(stmt) => functions.push(stmt),
                Err(_) => break, // error already logged, continue parsing
            };
        }

        ParserOutput::new(ParserNode::Program(functions), self.errors)
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

    fn parse_statement_inner(&mut self) -> ParseResult {
        match self.tokens.peek() {
            Some(Token::Let) => self.parse_var_declaration(),
            Some(Token::Fn) => self.parse_func_declaration(),
            Some(Token::Struct) => self.parse_struct_definition(),
            Some(Token::If) => self.parse_if(),
            Some(Token::For) => self.parse_for(),
            Some(Token::LeftBrace) => self.parse_block(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Break) => self.parse_break_statement(),
            Some(Token::Identifier(_)) => {
                let ident = self.parse_identifier()?.as_identifier().to_string();

                match self.tokens.peek() {
                    Some(Token::Equals) => self.parse_var_assignment(ident),
                    Some(Token::LeftParens) => self.parse_func_call(ident),
                    Some(Token::AddEquals)
                    | Some(Token::SubtractEquals)
                    | Some(Token::MultiplyEquals)
                    | Some(Token::DivideEquals)
                    | Some(Token::ModuloEquals) => self.parse_op_equals(ident),
                    _ => self.error("expected statement"),
                }
            }
            Some(Token::Slash) => self.parse_command_literal(),
            _ => self.error("expected statement"),
        }
    }

    fn parse_statement(&mut self) -> ParseResult {
        let stmt = self.parse_statement_inner()?;

        match stmt {
            ParserNode::For { .. }
            | ParserNode::If { .. }
            | ParserNode::FunctionDeclaration { .. }
            | ParserNode::StructDefinition { .. }
            | ParserNode::CommandLiteral(_) // command literals are a special case and handle the semicolon themselves
            | ParserNode::Block(_) => Ok(stmt),
            _ => {
                expect_tok!(self, Token::Semicolon, "expected ;");
                Ok(stmt)
            }
        }
    }

    fn parse_var_declaration(&mut self) -> ParseResult {
        expect_tok!(self, Token::Let, "expected let");

        let identifier = self.parse_typed_identifier(true)?;

        expect_tok!(self, Token::Equals, "expected =");

        let expr = self.parse_expression()?;

        match identifier {
            ParserNode::TypedIdentifier { name, ty } => Ok(ParserNode::VariableDeclaration {
                name,
                expr: Box::new(expr),
                ty: Some(ty),
            }),
            ParserNode::Identifier(name) => Ok(ParserNode::VariableDeclaration {
                name,
                expr: Box::new(expr),
                ty: None,
            }),
            _ => self.error("expected identifier"),
        }
    }

    fn parse_var_assignment(&mut self, name: String) -> ParseResult {
        expect_tok!(self, Token::Equals, "expected =");

        let expr = self.parse_expression()?;

        Ok(ParserNode::VariableAssignment {
            name,
            expr: Box::new(expr),
        })
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
                Some(return_ty.as_identifier().to_string())
            }
            _ => None,
        };

        let body = self.parse_block()?;
        
        let name = name.as_identifier().to_string();

        Ok(ParserNode::FunctionDeclaration {
            name: name.clone(),
            args,
            return_ty,
            body: Box::new(body),
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult {
        expect_tok!(self, Token::Return, "expected return");

        let mut expr = match self.tokens.peek() {
            Some(Token::Semicolon) => None,
            _ => Some(self.parse_expression()?),
        };

        expect_tok!(self, Token::Semicolon, "expected ;");

        Ok(ParserNode::Return(expr.map(|expr| Box::new(expr))))
    }

    fn parse_break_statement(&mut self) -> ParseResult {
        expect_tok!(self, Token::Break, "expected break");

        Ok(ParserNode::Break)
    }

    fn parse_func_call(&mut self, name: String) -> ParseResult {
        expect_tok!(self, Token::LeftParens, "expected (");

        let mut args = Vec::new();

        if self.tokens.peek() != Some(&Token::RightParens) {
            let arg = self.parse_expression()?;
            args.push(arg);

            while self.tokens.peek() == Some(&Token::Comma) {
                self.tokens.next(); // consume the comma
                let arg = self.parse_expression()?;
                args.push(arg);
            }
        }

        expect_tok!(self, Token::RightParens, "expected )");

        Ok(ParserNode::FunctionCall { name, args })
    }

    fn parse_if(&mut self) -> ParseResult {
        expect_tok!(self, Token::If, "expected if");

        let cond = self.parse_expression()?;
        let body = self.parse_block()?;

        let mut else_ifs = Vec::new();
        let mut else_body = None;

        while self.tokens.peek() == Some(&Token::Else) {
            self.tokens.next(); // consume the else

            if self.tokens.peek() == Some(&Token::If) {
                self.tokens.next(); // consume the if
                let cond = self.parse_expression()?;
                let body = self.parse_block()?;

                else_ifs.push((cond, body));
            } else {
                else_body = Some(self.parse_block()?);
                break;
            }
        }

        Ok(ParserNode::If {
            cond: Box::new(cond),
            body: Box::new(body),
            else_ifs,
            else_body: else_body.map(Box::new),
        })
    }

    fn parse_for(&mut self) -> ParseResult {
        expect_tok!(self, Token::For, "expected for");

        let init = self.parse_statement()?;
        let cond = self.parse_expression_statement()?;
        let step = self.parse_statement_inner()?;
        let body = self.parse_block()?;

        Ok(ParserNode::For {
            init: Box::new(init),
            cond: Box::new(cond),
            step: Box::new(step),
            body: Box::new(body),
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult {
        let expr = self.parse_expression()?;
        expect_tok!(self, Token::Semicolon, "expected ;");
        Ok(expr)
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_equality()
    }

    fn parse_number(&mut self) -> ParseResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Number(n)) => Ok(ParserNode::NumberLiteral(*n)),
            _ => self.error("expected number"),
        }
    }

    fn parse_bool(&mut self) -> ParseResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Bool(b)) => Ok(ParserNode::BoolLiteral(*b)),
            _ => self.error("expected bool"),
        }
    }

    fn parse_identifier(&mut self) -> ParseResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Identifier(identifier)) => {
                Ok(ParserNode::Identifier(identifier.to_string()))
            }
            _ => self.error("expected identifier"),
        }
    }

    fn parse_primary(&mut self) -> ParseResult {
        match self.tokens.peek() {
            Some(Token::Number(_)) => self.parse_number(),
            Some(Token::Bool(_)) => self.parse_bool(),
            Some(Token::Identifier(_)) => {
                let identifier = self.parse_identifier()?;

                match self.tokens.peek() {
                    Some(Token::LeftParens) => {
                        self.parse_func_call(identifier.as_identifier().to_string())
                    }
                    _ => Ok(identifier),
                }
            }
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

    fn parse_unary(&mut self) -> ParseResult {
        let op = match self.tokens.peek() {
            Some(Token::Hyphen) => Operation::Negate,
            Some(Token::Not) => Operation::Not,
            _ => return self.parse_primary(),
        };

        self.tokens.next();

        let expr = self.parse_unary()?;

        Ok(ParserNode::Unary(Box::new(expr), op))
    }

    // does not necessarily parse an equality, but rather an equality or a comparison (and in turn a comparison or a term)
    fn parse_equality(&mut self) -> ParseResult {
        let mut expr = self.parse_comparison()?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::EqualsEquals => Operation::CheckEquals,
                Token::NotEquals => Operation::NotEquals,
                _ => break,
            };

            self.tokens.next();

            let comparison = self.parse_comparison()?;
            expr = ParserNode::Operation(Box::new(expr), Box::new(comparison), op);
        }

        Ok(expr)
    }

    // does not necessarily parse a comparison, but rather a comparison or a term
    // TODO: stop multiple inequalities in the same expression
    fn parse_comparison(&mut self) -> ParseResult {
        let mut expr = self.parse_term()?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::LessThan => Operation::LessThan,
                Token::LessThanOrEquals => Operation::LessThanOrEquals,
                Token::GreaterThan => Operation::GreaterThan,
                Token::GreaterThanOrEquals => Operation::GreaterThanOrEquals,
                _ => break,
            };

            self.tokens.next();

            let term = self.parse_term()?;
            expr = ParserNode::Operation(Box::new(expr), Box::new(term), op);
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult {
        let mut expr = self.parse_factor()?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::Plus => Operation::Add,
                Token::Hyphen => Operation::Subtract,
                _ => break,
            };

            self.tokens.next();

            let term = self.parse_factor()?;
            expr = ParserNode::Operation(Box::new(expr), Box::new(term), op);
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult {
        let mut expr = self.parse_unary()?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::Asterisk => Operation::Multiply,
                Token::Slash => Operation::Divide,
                Token::Percent => Operation::Modulo,
                _ => break,
            };

            self.tokens.next();

            let term = self.parse_unary()?;
            expr = ParserNode::Operation(Box::new(expr), Box::new(term), op);
        }

        Ok(expr)
    }

    fn parse_op_equals(&mut self, name: String) -> ParseResult {
        let op = match self.tokens.next() {
            Some(Token::AddEquals) => Operation::Add,
            Some(Token::SubtractEquals) => Operation::Subtract,
            Some(Token::MultiplyEquals) => Operation::Multiply,
            Some(Token::DivideEquals) => Operation::Divide,
            Some(Token::ModuloEquals) => Operation::Modulo,
            _ => unreachable!(),
        };

        let expr = self.parse_expression()?;

        Ok(ParserNode::OpEquals {
            name,
            expr: Box::new(expr),
            op,
        })
    }

    fn parse_command_literal(&mut self) -> ParseResult {
        let remainder = self.tokens.remainder();
        let mut end = 0;
        let mut in_str = false;
        let mut skip = false;
        let mut valid = false;

        for (i, c) in remainder.char_indices() {
            if c == '"' {
                if skip {
                    skip = false;
                    continue;
                }

                in_str = !in_str;
            }

            if skip {
                skip = false;
            }

            if in_str && c == '\\' {
                skip = true;
                continue;
            }

            if !in_str && c == ';' {
                end = i;
                valid = true;
                break;
            }
        }

        if !valid {
            return self.error("expected ; after command literal");
        }

        let literal = remainder[..end].to_string();
        self.tokens.bump(end + 1);

        Ok(ParserNode::CommandLiteral(literal))
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
            Some(Token::Identifier(name)) => name.to_string(),
            _ => return self.error("expected valid type"),
        };

        Ok(ParserNode::TypedIdentifier { name, ty })
    }

    fn parse_struct_definition(&mut self) -> ParseResult {
        expect_tok!(self, Token::Struct, "expected struct");

        let name = self.parse_identifier()?;

        expect_tok!(self, Token::LeftBrace, "expected {");

        let mut fields = Vec::new();

        while self.tokens.peek() != Some(&Token::RightBrace) {
            let field = self.parse_typed_identifier(false)?;

            if self.tokens.peek() != Some(&Token::RightBrace) {
                expect_tok!(self, Token::Comma, "expected , or }");
            }

            fields.push(field);
        }

        expect_tok!(self, Token::RightBrace, "expected }");

        Ok(ParserNode::StructDefinition {
            name: name.as_identifier().to_string(),
            fields,
        })
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

pub struct ParserOutput {
    pub ast: ParserNode,
    pub errs: Vec<ParseError>,
}

impl ParserOutput {
    fn new(
        ast: ParserNode,
        errs: Vec<ParseError>,
    ) -> Self {
        Self {
            ast,
            errs,
        }
    }
}

pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<String>,
}

impl FunctionDefinition {
    fn new(name: String, args: Vec<String>) -> Self {
        Self { name, args }
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
