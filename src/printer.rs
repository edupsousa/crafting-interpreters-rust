use crate::ast::*;

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn print_statements(&mut self, statements: &[Stmt]) -> String {
        let mut result = String::new();
        for statement in statements {
            result.push_str(&self.print_stmt(statement));
        }
        result
    }

    fn print_stmt(&mut self, stmt: &Stmt) -> String {
        stmt.accept(self)
    }

    fn print_expr(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> String {
        format!(
            "({} {} {})",
            expr.operator.lexeme,
            self.print_expr(&expr.left),
            self.print_expr(&expr.right)
        )
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> String {
        format!("(group {})", self.print_expr(&expr.expression))
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> String {
        match &expr.value {
            LiteralValue::Number(n) => n.to_string(),
            LiteralValue::String(s) => s.clone(),
            LiteralValue::Boolean(b) => b.to_string(),
            LiteralValue::Nil => "nil".to_string(),
        }
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> String {
        format!(
            "({} {})",
            expr.operator.lexeme,
            self.print_expr(&expr.right)
        )
    }

    fn visit_expression_stmt(&mut self, stmt: &ExpressionStmt) -> String {
        self.print_expr(&stmt.expression)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> String {
        self.print_expr(&stmt.expression)
    }
}
