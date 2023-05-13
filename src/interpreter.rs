use std::{collections::HashMap, error, fmt};

use crate::{ast::*, tokens::*};

pub struct Interpreter {
    had_error: bool,
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            had_error: false,
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) {
        for statement in statements {
            match self.execute(statement) {
                Ok(value) => println!("{}", value),
                Err(e) => {
                    self.had_error = true;
                    println!("{}", e);
                }
            }
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> RuntimeResult {
        stmt.accept(self)
    }

    fn evaluate(&mut self, expr: &Expr) -> RuntimeResult {
        expr.accept(self)
    }

    fn is_truthy(&self, value: &LiteralValue) -> bool {
        match value {
            LiteralValue::Nil => false,
            LiteralValue::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl Visitor<RuntimeResult> for Interpreter {
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> RuntimeResult {
        Ok(expr.value.clone())
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> RuntimeResult {
        let value = self.evaluate(&expr.expression)?;
        Ok(value)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> RuntimeResult {
        let right = self.evaluate(&expr.right)?;

        match expr.operator.token_type {
            TokenType::Bang => Ok(LiteralValue::Boolean(!self.is_truthy(&right))),
            TokenType::Minus => match right {
                LiteralValue::Number(n) => Ok(LiteralValue::Number(-n)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operand must be a number.".to_string(),
                )),
            },
            _ => Err(RuntimeError::new(
                expr.operator.clone(),
                "Unknown unary operator.".to_string(),
            )),
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> RuntimeResult {
        use LiteralValue::*;

        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.token_type {
            TokenType::Minus => match (left, right) {
                (Number(l), Number(r)) => Ok(Number(l - r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be numbers.".to_string(),
                )),
            },
            TokenType::Slash => match (left, right) {
                (Number(l), Number(r)) => Ok(Number(l / r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be numbers.".to_string(),
                )),
            },
            TokenType::Star => match (left, right) {
                (Number(l), Number(r)) => Ok(Number(l * r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be numbers.".to_string(),
                )),
            },
            TokenType::Plus => match (left, right) {
                (Number(l), Number(r)) => Ok(Number(l + r)),
                (String(l), String(r)) => Ok(String(l + &r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be two numbers or two strings.".to_string(),
                )),
            },
            TokenType::Greater => match (left, right) {
                (Number(l), Number(r)) => Ok(Boolean(l > r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be numbers.".to_string(),
                )),
            },
            TokenType::GreaterEqual => match (left, right) {
                (Number(l), Number(r)) => Ok(Boolean(l >= r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be numbers.".to_string(),
                )),
            },
            TokenType::Less => match (left, right) {
                (Number(l), Number(r)) => Ok(Boolean(l < r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be numbers.".to_string(),
                )),
            },
            TokenType::LessEqual => match (left, right) {
                (Number(l), Number(r)) => Ok(Boolean(l <= r)),
                _ => Err(RuntimeError::new(
                    expr.operator.clone(),
                    "Operands must be numbers.".to_string(),
                )),
            },
            TokenType::BangEqual => Ok(Boolean(left != right)),
            TokenType::EqualEqual => Ok(Boolean(left == right)),
            _ => Err(RuntimeError::new(
                expr.operator.clone(),
                "Unknown binary operator.".to_string(),
            )),
        }
    }

    fn visit_expression_stmt(&mut self, stmt: &ExpressionStmt) -> RuntimeResult {
        self.evaluate(&stmt.expression)?;
        Ok(LiteralValue::Nil)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> RuntimeResult {
        let value = self.evaluate(&stmt.expression)?;
        println!("{}", value);
        Ok(LiteralValue::Nil)
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> RuntimeResult {
        let value = match &stmt.initializer {
            Some(expr) => self.evaluate(expr)?,
            None => LiteralValue::Nil,
        };

        self.environment.define(stmt.name.lexeme.as_str(), value);
        Ok(LiteralValue::Nil)
    }

    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> RuntimeResult {
        self.environment.get(&expr.name)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> RuntimeResult {
        let value = self.evaluate(&expr.value)?;
        self.environment.assign(&expr.name, value.clone())?;
        Ok(value)
    }
}

#[derive(Debug)]
struct RuntimeError {
    token: Token,
    message: String,
}

impl RuntimeError {
    fn new(token: Token, message: String) -> Self {
        Self { token, message }
    }
}

impl error::Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error", self.token.line)?;
        match &self.token.token_type {
            TokenType::Eof => write!(f, " at end")?,
            _ => write!(f, " at '{}'", self.token.lexeme)?,
        }
        write!(f, ": {}", self.message)
    }
}

type RuntimeResult = Result<LiteralValue, RuntimeError>;

#[derive(Debug, Clone)]
struct Environment {
    values: HashMap<String, LiteralValue>,
}

impl Environment {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn define(&mut self, name: &str, value: LiteralValue) {
        self.values.insert(name.to_string(), value);
    }

    fn get(&self, token: &Token) -> RuntimeResult {
        match self.values.get(&token.lexeme) {
            Some(value) => Ok(value.clone()),
            None => Err(RuntimeError::new(
                token.clone(),
                format!("Undefined variable '{}'.", token.lexeme),
            )),
        }
    }

    fn assign(&mut self, token: &Token, value: LiteralValue) -> RuntimeResult {
        match self.values.get_mut(&token.lexeme) {
            Some(v) => {
                *v = value;
                Ok(LiteralValue::Nil)
            }
            None => Err(RuntimeError::new(
                token.clone(),
                format!("Undefined variable '{}'.", token.lexeme),
            )),
        }
    }
}
