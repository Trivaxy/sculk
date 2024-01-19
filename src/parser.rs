use std::{collections::HashMap, fmt::Display, ops::Range, process::Output};

use crate::lexer::{Token, TokenStream};

#[derive(Clone, Copy, Debug, PartialEq)]
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
    And,
    Or,
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            Operation::Add => "+",
            Operation::Subtract => "-",
            Operation::Multiply => "*",
            Operation::Divide => "/",
            Operation::Modulo => "%",
            Operation::GreaterThan => ">",
            Operation::LessThan => "<",
            Operation::GreaterThanOrEquals => ">=",
            Operation::LessThanOrEquals => "<=",
            Operation::CheckEquals => "==",
            Operation::NotEquals => "!=",
            Operation::Not => "!",
            Operation::Negate => "-",
            Operation::And => "&&",
            Operation::Or => "||",
        };

        write!(f, "{}", op)
    }
}

#[derive(Clone, Debug)]
pub struct ParserNode {
    kind: ParserNodeKind,
    span: Range<usize>,
}

impl ParserNode {
    pub fn new(kind: ParserNodeKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ParserNodeKind {
        &self.kind
    }

    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub enum ParserNodeKind {
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
        name: Box<ParserNode>,
        expr: Box<ParserNode>,
        ty: Option<String>,
    },
    VariableAssignment {
        path: Box<ParserNode>,
        expr: Box<ParserNode>,
    },
    FunctionDeclaration {
        name: String,
        args: Vec<ParserNode>,
        return_ty: Option<String>,
        body: Box<ParserNode>,
        is_static: bool,
    },
    Return(Option<Box<ParserNode>>),
    FunctionCall {
        expr: Box<ParserNode>,
        args: Vec<ParserNode>,
    },
    Expression(Box<ParserNode>),
    Operation(Box<ParserNode>, Box<ParserNode>, Operation),
    OpEquals {
        path: Box<ParserNode>,
        expr: Box<ParserNode>,
        op: Operation,
    },
    Unary(Box<ParserNode>, Operation),
    If {
        cond: Box<ParserNode>,
        body: Box<ParserNode>,
        else_ifs: Vec<(ParserNode, ParserNode)>,
        else_body: Option<Box<ParserNode>>,
    },
    For {
        init: Box<ParserNode>,
        cond: Box<ParserNode>,
        step: Box<ParserNode>,
        body: Box<ParserNode>,
    },
    StructDefinition {
        name: String,
        members: Vec<ParserNode>,
    },
    MemberAccess {
        expr: Box<ParserNode>,
        member: Box<ParserNode>,
    },
    Break,
    CommandLiteral(String),
}

impl ParserNodeKind {
    fn as_identifier(&self) -> &str {
        match self {
            ParserNodeKind::Identifier(name) => &name,
            ParserNodeKind::TypedIdentifier { name, .. } => &name,
            _ => panic!("tried to get identifier from non-identifier node"),
        }
    }
}

impl ParserNode {
    pub fn is_num(&self) -> bool {
        match self.kind {
            ParserNodeKind::NumberLiteral(_) => true,
            _ => false,
        }
    }

    pub fn is_call(&self) -> bool {
        match self.kind {
            ParserNodeKind::FunctionCall { .. } => true,
            _ => false,
        }
    }

    pub fn is_func_declaration(&self) -> bool {
        match self.kind {
            ParserNodeKind::FunctionDeclaration { .. } => true,
            _ => false,
        }
    }

    pub fn is_typed_identifier(&self) -> bool {
        match self.kind {
            ParserNodeKind::TypedIdentifier { .. } => true,
            _ => false,
        }
    }

    fn as_num(&self) -> i32 {
        match self.kind {
            ParserNodeKind::NumberLiteral(num) => num,
            _ => panic!("tried to get number from non-number node"),
        }
    }

    pub fn as_identifier(&self) -> &str {
        match &self.kind {
            ParserNodeKind::Identifier(name) => &name,
            ParserNodeKind::TypedIdentifier { name, .. } => &name,
            _ => panic!("tried to get identifier from non-identifier node"),
        }
    }

    pub fn as_typed_identifier(&self) -> (&str, &str) {
        match &self.kind {
            ParserNodeKind::TypedIdentifier { name, ty } => (&name, &ty),
            _ => panic!("tried to get typed identifier from non-typed identifier node"),
        }
    }

    pub fn as_program(&self) -> &[ParserNode] {
        match &self.kind {
            ParserNodeKind::Program(stmts) => &stmts,
            _ => panic!("tried to get program from non-program node"),
        }
    }

    pub fn as_block(&self) -> &[ParserNode] {
        match &self.kind {
            ParserNodeKind::Block(stmts) => &stmts,
            _ => panic!("tried to get block from non-block node"),
        }
    }

    pub fn as_function_call(&self) -> (&ParserNode, &[ParserNode]) {
        match &self.kind {
            ParserNodeKind::FunctionCall { expr, args } => (expr, args.as_slice()),
            _ => panic!("tried to get function call from non-call node"),
        }
    }

    pub fn as_func_name(&self) -> &str {
        match &self.kind {
            ParserNodeKind::FunctionDeclaration { name, .. } => &name,
            _ => panic!("tried to get function name from non-function node"),
        }
    }

    pub fn as_func_body(&self) -> &ParserNode {
        match &self.kind {
            ParserNodeKind::FunctionDeclaration { body, .. } => body,
            _ => panic!("tried to get function body from non-function node"),
        }
    }
}

macro_rules! expect_tok {
    ($parser:expr, $token:pat, $err:expr) => {
        match $parser.tokens.peek() {
            Some($token) => $parser.tokens.next(),
            _ => return $parser.error_at($err, $parser.tokens.peeked_span()),
        };
    };
}

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
    errors: Vec<ParseError>,
    current_node_starts: Vec<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            tokens: TokenStream::new(src),
            errors: Vec::new(),
            current_node_starts: Vec::new(),
        }
    }

    fn call(
        &mut self,
        mut parser: impl for<'b> FnOnce(&'b mut Parser<'a>) -> ParserKindResult,
    ) -> ParseResult {
        self.current_node_starts
            .push(self.tokens.peeked_span().start);

        let result = parser(self);

        let node_span = self.current_node_starts.pop().unwrap()..self.tokens.current_span().end;

        match result {
            Ok(kind) => Ok(ParserNode::new(kind, node_span)),
            Err(_) => Err(()),
        }
    }

    pub fn parse(mut self) -> ParserOutput {
        let mut nodes = Vec::new();

        while self.tokens.peek().is_some() {
            match self.tokens.peek().unwrap() {
                Token::Fn | Token::Static => match self.call(Self::parse_func_declaration) {
                    Ok(stmt) => nodes.push(stmt),
                    Err(_) => continue, // error already logged, continue parsing
                },
                Token::Struct => match self.call(Self::parse_struct_definition) {
                    Ok(stmt) => nodes.push(stmt),
                    Err(_) => continue, // error already logged, continue parsing
                },
                _ => {
                    self.error_at("unexpected token or symbol", self.tokens.peeked_span());
                    continue;
                }
            }
        }

        ParserOutput::new(
            ParserNode::new(ParserNodeKind::Program(nodes), 0..self.tokens.src_len()),
            self.errors,
        )
    }

    fn parse_block(&mut self) -> ParserKindResult {
        expect_tok!(self, Token::LeftBrace, "expected {");

        let mut statements = Vec::new();

        while self.tokens.peek() != Some(&Token::RightBrace) {
            statements.push(self.call(Self::parse_statement)?);
        }

        expect_tok!(self, Token::RightBrace, "expected }");

        Ok(ParserNodeKind::Block(statements))
    }

    fn parse_path(&mut self) -> ParserKindResult {
        let mut identifier = self.call(Self::parse_identifier)?;

        while self.tokens.peek().is_some() {
            match self.tokens.peek().unwrap() {
                Token::LeftParens => identifier = self.call(|s| s.parse_func_call(identifier))?,
                Token::Dot => identifier = self.call(|s| s.parse_member_access(identifier))?,
                _ => break,
            }
        }

        Ok(identifier.kind)
    }

    fn parse_statement_inner(&mut self) -> ParserKindResult {
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
                let path = self.call(Self::parse_path)?;

                match self.tokens.peek() {
                    Some(Token::Semicolon) => {
                        if !path.is_call() {
                            self.error("expected a function call before ;")
                        } else {
                            Ok(path.kind)
                        }
                    }
                    Some(Token::Equals) => self.parse_var_assignment(path),
                    Some(Token::AddEquals)
                    | Some(Token::SubtractEquals)
                    | Some(Token::MultiplyEquals)
                    | Some(Token::DivideEquals)
                    | Some(Token::ModuloEquals) => self.parse_op_equals(path),
                    _ => self.error("expected statement"),
                }
            }
            Some(Token::Slash) => self.parse_command_literal(),
            _ => self.error("expected statement"),
        }
    }

    fn parse_statement(&mut self) -> ParserKindResult {
        let stmt = self.parse_statement_inner()?;

        match stmt {
            ParserNodeKind::For { .. }
            | ParserNodeKind::If { .. }
            | ParserNodeKind::FunctionDeclaration { .. }
            | ParserNodeKind::StructDefinition { .. }
            | ParserNodeKind::CommandLiteral(_) // command literals are a special case and handle the semicolon themselves
            | ParserNodeKind::Block(_) => Ok(stmt),
            _ => {
                expect_tok!(self, Token::Semicolon, "expected ;");
                Ok(stmt)
            }
        }
    }

    fn parse_var_declaration(&mut self) -> ParserKindResult {
        expect_tok!(self, Token::Let, "expected let");

        let identifier = self.call(|parser| parser.parse_typed_identifier(true))?;

        expect_tok!(self, Token::Equals, "expected =");

        let expr = self.call(Self::parse_expression)?;

        Ok(ParserNodeKind::VariableDeclaration {
            name: Box::new(identifier),
            expr: Box::new(expr),
            ty: None,
        })
    }

    fn parse_var_assignment(&mut self, path: ParserNode) -> ParserKindResult {
        expect_tok!(self, Token::Equals, "expected =");

        let expr = self.call(Self::parse_expression)?;

        Ok(ParserNodeKind::VariableAssignment {
            path: Box::new(path),
            expr: Box::new(expr),
        })
    }

    fn parse_func_declaration(&mut self) -> ParserKindResult {
        let is_static = match self.tokens.peek() {
            Some(Token::Static) => {
                self.tokens.next(); // consume the static
                true
            }
            _ => false,
        };

        expect_tok!(self, Token::Fn, "expected fn");

        let name = self.call(Self::parse_identifier)?;

        expect_tok!(self, Token::LeftParens, "expected (");

        let mut args = Vec::new();

        if self.tokens.peek() != Some(&Token::RightParens) {
            let arg = self.call(|parser| parser.parse_typed_identifier(false))?;
            args.push(arg);

            while self.tokens.peek() == Some(&Token::Comma) {
                self.tokens.next(); // consume the comma
                let arg = self.call(|parser| parser.parse_typed_identifier(false))?;
                args.push(arg);
            }
        }

        expect_tok!(self, Token::RightParens, "expected )");

        let return_ty = match self.tokens.peek() {
            Some(Token::Arrow) => {
                self.tokens.next(); // consume the arrow
                let return_ty = self.call(Self::parse_identifier)?;
                Some(return_ty.as_identifier().to_string())
            }
            _ => None,
        };

        let body = self.call(Self::parse_block)?;

        let name = name.as_identifier().to_string();

        Ok(ParserNodeKind::FunctionDeclaration {
            name: name.clone(),
            args,
            return_ty,
            body: Box::new(body),
            is_static
        })
    }

    fn parse_return_statement(&mut self) -> ParserKindResult {
        expect_tok!(self, Token::Return, "expected return");

        let expr = match self.tokens.peek() {
            Some(Token::Semicolon) => None,
            _ => Some(self.call(Self::parse_expression)?),
        };

        Ok(ParserNodeKind::Return(expr.map(|expr| Box::new(expr))))
    }

    fn parse_break_statement(&mut self) -> ParserKindResult {
        expect_tok!(self, Token::Break, "expected break");

        Ok(ParserNodeKind::Break)
    }

    fn parse_func_call(&mut self, callee: ParserNode) -> ParserKindResult {
        expect_tok!(self, Token::LeftParens, "expected (");

        let mut args = Vec::new();

        if self.tokens.peek() != Some(&Token::RightParens) {
            let arg = self.call(Self::parse_expression)?;
            args.push(arg);

            while self.tokens.peek() == Some(&Token::Comma) {
                self.tokens.next(); // consume the comma
                let arg = self.call(Self::parse_expression)?;
                args.push(arg);
            }
        }

        expect_tok!(self, Token::RightParens, "expected )");

        Ok(ParserNodeKind::FunctionCall {
            expr: Box::new(callee),
            args,
        })
    }

    fn parse_member_access(&mut self, expr: ParserNode) -> ParserKindResult {
        self.tokens.next(); // consume the .
        let member = self.call(Self::parse_identifier)?;
        Ok(ParserNodeKind::MemberAccess {
            expr: Box::new(expr),
            member: Box::new(member),
        })
    }

    fn parse_if(&mut self) -> ParserKindResult {
        expect_tok!(self, Token::If, "expected if");

        let cond = self.call(Self::parse_expression)?;
        let body = self.call(Self::parse_block)?;

        let mut else_ifs = Vec::new();
        let mut else_body = None;

        while self.tokens.peek() == Some(&Token::Else) {
            self.tokens.next(); // consume the else

            if self.tokens.peek() == Some(&Token::If) {
                self.tokens.next(); // consume the if
                let cond = self.call(Self::parse_expression)?;
                let body = self.call(Self::parse_block)?;

                else_ifs.push((cond, body));
            } else {
                else_body = Some(self.call(Self::parse_block)?);
                break;
            }
        }

        Ok(ParserNodeKind::If {
            cond: Box::new(cond),
            body: Box::new(body),
            else_ifs,
            else_body: else_body.map(Box::new),
        })
    }

    fn parse_for(&mut self) -> ParserKindResult {
        expect_tok!(self, Token::For, "expected for");

        let init = self.call(Self::parse_statement)?;
        let cond = self.call(Self::parse_expression_statement)?;
        let step = self.call(Self::parse_statement_inner)?;
        let body = self.call(Self::parse_block)?;

        Ok(ParserNodeKind::For {
            init: Box::new(init),
            cond: Box::new(cond),
            step: Box::new(step),
            body: Box::new(body),
        })
    }

    fn parse_expression_statement(&mut self) -> ParserKindResult {
        let expr = self.parse_expression()?;
        expect_tok!(self, Token::Semicolon, "expected ;");
        Ok(expr)
    }

    fn parse_expression(&mut self) -> ParserKindResult {
        self.parse_or()
    }

    fn parse_number(&mut self) -> ParserKindResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Number(n)) => Ok(ParserNodeKind::NumberLiteral(*n)),
            _ => self.error("expected number"),
        }
    }

    fn parse_bool(&mut self) -> ParserKindResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Bool(b)) => Ok(ParserNodeKind::BoolLiteral(*b)),
            _ => self.error("expected bool"),
        }
    }

    fn parse_identifier(&mut self) -> ParserKindResult {
        let tok = self.tokens.next();

        match tok {
            Some(Token::Identifier(identifier)) => {
                Ok(ParserNodeKind::Identifier(identifier.to_string()))
            }
            _ => self.error("expected identifier"),
        }
    }

    fn parse_primary(&mut self) -> ParserKindResult {
        match self.tokens.peek() {
            Some(Token::Number(_)) => self.parse_number(),
            Some(Token::Bool(_)) => self.parse_bool(),
            Some(Token::Identifier(_)) => self.parse_path(),
            Some(Token::LeftParens) => {
                self.tokens.next();
                let expr = self.parse_statement()?;

                match self.tokens.next() {
                    Some(Token::RightParens) => Ok(expr),
                    _ => self.error("expected ) after expression"),
                }
            }
            _ => self.error("expected value or expression"),
        }
    }

    fn parse_unary(&mut self) -> ParserKindResult {
        let op = match self.tokens.peek() {
            Some(Token::Hyphen) => Operation::Negate,
            Some(Token::Not) => Operation::Not,
            _ => return self.parse_primary(),
        };

        self.tokens.next();

        let expr = self.call(Self::parse_unary)?;

        Ok(ParserNodeKind::Unary(Box::new(expr), op))
    }

    fn parse_or(&mut self) -> ParserKindResult {
        let mut expr = self.call(Self::parse_and)?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::Or => Operation::Or,
                _ => break,
            };

            self.tokens.next();

            let term = self.call(Self::parse_and)?;
            let span = expr.span().start..term.span().end;
            expr = ParserNode::new(
                ParserNodeKind::Operation(Box::new(expr), Box::new(term), op),
                span,
            );
        }

        Ok(ParserNodeKind::Expression(Box::new(expr)))
    }

    fn parse_and(&mut self) -> ParserKindResult {
        let mut expr = self.call(Self::parse_equality)?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::And => Operation::And,
                _ => break,
            };

            self.tokens.next();

            let term = self.call(Self::parse_equality)?;
            let span = expr.span().start..term.span().end;
            expr = ParserNode::new(
                ParserNodeKind::Operation(Box::new(expr), Box::new(term), op),
                span,
            );
        }

        Ok(ParserNodeKind::Expression(Box::new(expr)))
    }

    // does not necessarily parse an equality, but rather an equality or a comparison (and in turn a comparison or a term)
    fn parse_equality(&mut self) -> ParserKindResult {
        let mut expr = self.call(Self::parse_comparison)?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::EqualsEquals => Operation::CheckEquals,
                Token::NotEquals => Operation::NotEquals,
                _ => break,
            };

            self.tokens.next();

            let comparison = self.call(Self::parse_comparison)?;
            let span = expr.span().start..comparison.span().end;
            expr = ParserNode::new(
                ParserNodeKind::Operation(Box::new(expr), Box::new(comparison), op),
                span,
            );
        }

        Ok(ParserNodeKind::Expression(Box::new(expr)))
    }

    // does not necessarily parse a comparison, but rather a comparison or a term
    // TODO: stop multiple inequalities in the same expression
    fn parse_comparison(&mut self) -> ParserKindResult {
        let mut expr = self.call(Self::parse_term)?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::LessThan => Operation::LessThan,
                Token::LessThanOrEquals => Operation::LessThanOrEquals,
                Token::GreaterThan => Operation::GreaterThan,
                Token::GreaterThanOrEquals => Operation::GreaterThanOrEquals,
                _ => break,
            };

            self.tokens.next();

            let term = self.call(Self::parse_term)?;
            let span = expr.span().start..term.span().end;
            expr = ParserNode::new(
                ParserNodeKind::Operation(Box::new(expr), Box::new(term), op),
                span,
            );
        }

        Ok(ParserNodeKind::Expression(Box::new(expr)))
    }

    fn parse_term(&mut self) -> ParserKindResult {
        let mut expr = self.call(Self::parse_factor)?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::Plus => Operation::Add,
                Token::Hyphen => Operation::Subtract,
                _ => break,
            };

            self.tokens.next();

            let term = self.call(Self::parse_factor)?;
            let span = expr.span().start..term.span().end;
            expr = ParserNode::new(
                ParserNodeKind::Operation(Box::new(expr), Box::new(term), op),
                span,
            );
        }

        Ok(ParserNodeKind::Expression(Box::new(expr)))
    }

    fn parse_factor(&mut self) -> ParserKindResult {
        let mut expr = self.call(Self::parse_unary)?;

        while self.tokens.peek().is_some() {
            let op = match self.tokens.peek().unwrap() {
                Token::Asterisk => Operation::Multiply,
                Token::Slash => Operation::Divide,
                Token::Percent => Operation::Modulo,
                _ => break,
            };

            self.tokens.next();

            let term = self.call(Self::parse_unary)?;
            let span = expr.span().start..term.span().end;
            expr = ParserNode::new(
                ParserNodeKind::Operation(Box::new(expr), Box::new(term), op),
                span,
            );
        }

        Ok(ParserNodeKind::Expression(Box::new(expr)))
    }

    fn parse_op_equals(&mut self, path: ParserNode) -> ParserKindResult {
        let op = match self.tokens.next() {
            Some(Token::AddEquals) => Operation::Add,
            Some(Token::SubtractEquals) => Operation::Subtract,
            Some(Token::MultiplyEquals) => Operation::Multiply,
            Some(Token::DivideEquals) => Operation::Divide,
            Some(Token::ModuloEquals) => Operation::Modulo,
            _ => unreachable!(),
        };

        let expr = self.call(Self::parse_expression)?;

        Ok(ParserNodeKind::OpEquals {
            path: Box::new(path),
            expr: Box::new(expr),
            op,
        })
    }

    fn parse_command_literal(&mut self) -> ParserKindResult {
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

        Ok(ParserNodeKind::CommandLiteral(literal))
    }

    fn parse_typed_identifier(&mut self, ty_optional: bool) -> ParserKindResult {
        let name = match self.parse_identifier()? {
            ParserNodeKind::Identifier(identifier) => identifier,
            _ => unreachable!(),
        };

        if ty_optional && self.tokens.peek() != Some(&Token::Colon) {
            return Ok(ParserNodeKind::Identifier(name));
        }

        expect_tok!(self, Token::Colon, "expected :");

        let ty = match self.tokens.next() {
            Some(Token::Identifier(name)) => name.to_string(),
            _ => return self.error("expected valid type"),
        };

        Ok(ParserNodeKind::TypedIdentifier { name, ty })
    }

    fn parse_struct_definition(&mut self) -> ParserKindResult {
        expect_tok!(self, Token::Struct, "expected struct");

        let name = self.parse_identifier()?;

        expect_tok!(self, Token::LeftBrace, "expected {");

        let mut members = Vec::new();

        while self.tokens.peek() != Some(&Token::RightBrace) {
            let member = match self.tokens.peek() {
                Some(Token::Identifier(_)) => self.call(|parser| parser.parse_typed_identifier(false))?,
                Some(Token::Static) | Some(Token::Fn) => self.call(|parser| parser.parse_func_declaration())?,
                _ => return self.error("expected field, function declaration, or }")
            };

            members.push(member);
        }

        expect_tok!(self, Token::RightBrace, "expected }");

        Ok(ParserNodeKind::StructDefinition {
            name: name.as_identifier().to_string(),
            members,
        })
    }

    fn error(&mut self, error: impl Into<String>) -> ParserKindResult {
        let error = ParseError::new(
            error,
            *self.current_node_starts.last().unwrap()..self.tokens.current_span().end,
        );
        self.errors.push(error);
        self.recover();
        Err(())
    }

    fn error_at(&mut self, error: impl Into<String>, span: Range<usize>) -> ParserKindResult {
        let error = ParseError::new(error, span);
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
                Token::Let => return,
                Token::Struct => return,
                Token::Fn => return,
                _ => self.tokens.next(),
            };
        }
    }
}

pub struct ParserOutput {
    pub ast: ParserNode,
    pub errors: Vec<ParseError>,
}

impl ParserOutput {
    fn new(ast: ParserNode, errors: Vec<ParseError>) -> Self {
        Self { ast, errors }
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
type ParserKindResult = Result<ParserNodeKind, ()>;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Range<usize>,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Range<usize>) -> Self {
        ParseError {
            message: message.into(),
            span,
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
