use crate::tokens::Token;
use std::fmt;

// Expressions

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(Box<LiteralExpr>),
    Unary(Box<UnaryExpr>),
    Variable(Box<VariableExpr>),
    Assignment(Box<AssignExpr>),
    Logical(Box<LogicalExpr>),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Self::Binary(expr) => visitor.visit_binary_expr(expr),
            Self::Grouping(expr) => visitor.visit_grouping_expr(expr),
            Self::Literal(expr) => visitor.visit_literal_expr(expr),
            Self::Unary(expr) => visitor.visit_unary_expr(expr),
            Self::Variable(expr) => visitor.visit_variable_expr(expr),
            Self::Assignment(expr) => visitor.visit_assign_expr(expr),
            Self::Logical(expr) => visitor.visit_logical_expr(expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl LogicalExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub name: Token,
    pub value: Expr,
}

impl AssignExpr {
    pub fn new(name: Token, value: Expr) -> Self {
        Self { name, value }
    }
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub name: Token,
}

impl VariableExpr {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl BinaryExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GroupingExpr {
    pub expression: Expr,
}

impl GroupingExpr {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(value) => write!(f, "{}", value),
            Self::String(value) => write!(f, "{}", value),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub value: LiteralValue,
}

impl LiteralExpr {
    pub fn new(value: LiteralValue) -> Self {
        Self { value }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Expr) -> Self {
        Self { operator, right }
    }
}

// Statements

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Box<ExpressionStmt>),
    Print(Box<PrintStmt>),
    Var(Box<VarStmt>),
    Block(Box<BlockStmt>),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Stmt::Expression(stmt) => visitor.visit_expression_stmt(stmt),
            Stmt::Print(stmt) => visitor.visit_print_stmt(stmt),
            Stmt::Var(stmt) => visitor.visit_var_stmt(stmt),
            Stmt::Block(stmt) => visitor.visit_block_stmt(stmt),
            Stmt::If(stmt) => visitor.visit_if_stmt(stmt),
            Stmt::While(stmt) => visitor.visit_while_stmt(stmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Stmt,
}

impl WhileStmt {
    pub fn new(condition: Expr, body: Stmt) -> Self {
        Self { condition, body }
    }
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

impl IfStmt {
    pub fn new(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Self {
            condition,
            then_branch,
            else_branch,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

impl ExpressionStmt {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct PrintStmt {
    pub expression: Expr,
}

impl PrintStmt {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
}

impl VarStmt {
    pub fn new(name: Token, initializer: Option<Expr>) -> Self {
        Self { name, initializer }
    }
}

pub trait Visitor<T> {
    // Expressions
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> T;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> T;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> T;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> T;
    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> T;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> T;
    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> T;
    // Statements
    fn visit_expression_stmt(&mut self, stmt: &ExpressionStmt) -> T;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> T;
    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> T;
    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> T;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> T;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> T;
}
