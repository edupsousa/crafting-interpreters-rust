use std::{error, fmt};

use crate::{ast::*, tokens::*};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn match_next(&mut self, types: Vec<TokenType>) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements: Vec<Stmt> = vec![];
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    println!("{}", error);
                    self.synchronize();
                }
            }
        }
        statements
    }

    fn expression(&mut self) -> ExprParseResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprParseResult {
        let expr = self.or()?;

        if self.match_next(vec![TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expr::Variable(expr) => {
                    let name = expr.name;
                    return Ok(Expr::Assignment(Box::new(AssignExpr::new(name, value))));
                }
                _ => {
                    return Err(ParserError::new(
                        equals,
                        "Invalid assignment target.".to_string(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> ExprParseResult {
        let mut expr = self.and()?;

        while self.match_next(vec![TokenType::Or]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical(Box::new(LogicalExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn and(&mut self) -> ExprParseResult {
        let mut expr = self.equality()?;

        while self.match_next(vec![TokenType::And]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(LogicalExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ExprParseResult {
        let mut expr = self.comparison()?;

        while self.match_next(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ExprParseResult {
        let mut expr = self.term()?;

        while self.match_next(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn term(&mut self) -> ExprParseResult {
        let mut expr = self.factor()?;

        while self.match_next(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ExprParseResult {
        let mut expr = self.unary()?;

        while self.match_next(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ExprParseResult {
        if self.match_next(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(Box::new(UnaryExpr::new(operator, right))));
        }

        self.primary()
    }

    fn primary(&mut self) -> ExprParseResult {
        if self.match_next(vec![TokenType::False]) {
            return Ok(Expr::Literal(Box::new(LiteralExpr::new(
                LiteralValue::Boolean(false),
            ))));
        }
        if self.match_next(vec![TokenType::True]) {
            return Ok(Expr::Literal(Box::new(LiteralExpr::new(
                LiteralValue::Boolean(true),
            ))));
        }
        if self.match_next(vec![TokenType::Nil]) {
            return Ok(Expr::Literal(Box::new(LiteralExpr::new(LiteralValue::Nil))));
        }

        if self.match_next(vec![TokenType::Number, TokenType::String]) {
            let value = self.previous().literal.unwrap();
            match value {
                TokenLiteral::Number(n) => {
                    return Ok(Expr::Literal(Box::new(LiteralExpr::new(
                        LiteralValue::Number(n),
                    ))))
                }
                TokenLiteral::String(s) => {
                    return Ok(Expr::Literal(Box::new(LiteralExpr::new(
                        LiteralValue::String(s),
                    ))));
                }
            }
        }

        if self.match_next(vec![TokenType::Identifier]) {
            return Ok(Expr::Variable(Box::new(VariableExpr::new(self.previous()))));
        }

        if self.match_next(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(GroupingExpr::new(expr))));
        }

        Err(ParserError {
            token: self.peek(),
            message: "Expect expression.".to_string(),
        })
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, ParserError> {
        if self.check(token_type) {
            return Ok(self.advance());
        }

        Err(ParserError::new(self.peek(), message.to_string()))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn stmt_list_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn block(&mut self) -> StmtParseResult {
        let statements = self.stmt_list_block()?;
        Ok(Stmt::Block(Box::new(BlockStmt::new(statements))))
    }

    fn statement(&mut self) -> StmtParseResult {
        if self.match_next(vec![TokenType::For]) {
            return self.for_statement();
        }
        if self.match_next(vec![TokenType::If]) {
            return self.if_statement();
        }
        if self.match_next(vec![TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_next(vec![TokenType::While]) {
            return self.while_statement();
        }
        if self.match_next(vec![TokenType::LeftBrace]) {
            return self.block();
        }
        self.expression_statement()
    }

    fn for_statement(&mut self) -> StmtParseResult {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_next(vec![TokenType::Semicolon]) {
            None
        } else if self.match_next(vec![TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(Box::new(BlockStmt::new(vec![
                body,
                Stmt::Expression(Box::new(ExpressionStmt::new(increment))),
            ])));
        }

        if let Some(condition) = condition {
            body = Stmt::While(Box::new(WhileStmt::new(condition, body)));
        }

        if let Some(initializer) = initializer {
            body = Stmt::Block(Box::new(BlockStmt::new(vec![initializer, body])));
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> StmtParseResult {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;
        Ok(Stmt::While(Box::new(WhileStmt::new(condition, body))))
    }

    fn if_statement(&mut self) -> StmtParseResult {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_next(vec![TokenType::Else]) {
            Some(self.statement()?)
        } else {
            None
        };

        Ok(Stmt::If(Box::new(IfStmt::new(
            condition,
            then_branch,
            else_branch,
        ))))
    }

    fn print_statement(&mut self) -> StmtParseResult {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print(Box::new(PrintStmt::new(value))))
    }

    fn expression_statement(&mut self) -> StmtParseResult {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(Box::new(ExpressionStmt::new(expr))))
    }

    fn declaration(&mut self) -> StmtParseResult {
        if self.match_next(vec![TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> StmtParseResult {
        let name = self.consume(TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.match_next(vec![TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Stmt::Var(Box::new(VarStmt::new(name, initializer))))
    }
}

#[derive(Debug)]
struct ParserError {
    token: Token,
    message: String,
}

impl ParserError {
    fn new(token: Token, message: String) -> Self {
        Self { token, message }
    }
}

impl error::Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error", self.token.line)?;
        match &self.token.token_type {
            TokenType::Eof => write!(f, " at end")?,
            _ => write!(f, " at '{}'", self.token.lexeme)?,
        }
        write!(f, ": {}", self.message)
    }
}

type ExprParseResult = Result<Expr, ParserError>;
type StmtParseResult = Result<Stmt, ParserError>;
