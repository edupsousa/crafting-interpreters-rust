use std::io::Write;
use std::{error, fmt, fs, io};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut program = Program::new();
    match args.len() {
        1 => {
            println!("Prompt mode");
            program.run_prompt();
        }
        2 => {
            let path = &args[1];
            println!("Input file: {}", path);
            program.run_file(path);
        }
        _ => {
            println!("Usage: {} [path]", args[0]);
        }
    }
}

struct Program {
    interpreter: Interpreter,
    show_ast: bool,
}

impl Program {
    fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
            show_ast: false,
        }
    }

    fn run_file(&mut self, path: &str) {
        let file = fs::read_to_string(path).expect("Failed to read file");
        self.run(file.as_str());
    }

    fn run_prompt(&mut self) {
        loop {
            print!("> ");
            let _ = io::stdout().flush();
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");
            if input.trim().is_empty() {
                break;
            }
            self.run(input.as_str());
        }
    }

    fn run(&mut self, source: &str) {
        let mut scanner = Scanner::new(source);
        scanner.scan_tokens();

        if scanner.has_errors() {
            for error in scanner.errors {
                println!("{}", error);
            }
        }

        let mut parser = Parser::new(scanner.tokens);
        let expression = parser.parse();
        if self.show_ast {
            let mut printer = AstPrinter::new();
            println!("{}", printer.print(&expression));
        }
        self.interpreter.interpret(&expression);
    }
}

#[derive(Debug, Clone)]
struct LexerError {
    line: usize,
    message: String,
}

impl LexerError {
    fn new(line: usize, message: String) -> Self {
        Self { line, message }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.message)
    }
}

impl error::Error for LexerError {}

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    // Single-character tokens.
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
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
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

impl TokenType {
    fn from_keyword(keyword: &str) -> Option<TokenType> {
        match keyword {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "fun" => Some(TokenType::Fun),
            "if" => Some(TokenType::If),
            "nil" => Some(TokenType::Nil),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum LiteralNode {
    String(String),
    Number(f64),
}

impl LiteralNode {
    fn some_string(string: String) -> Option<Self> {
        Some(Self::String(string))
    }
    fn some_number(number: f64) -> Option<Self> {
        Some(Self::Number(number))
    }
}

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<LiteralNode>,
    line: usize,
}

impl Token {
    fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<LiteralNode>,
        line: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}

#[derive(Debug, Clone)]
struct Scanner<'source> {
    source: &'source str,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    errors: Vec<LexerError>,
}

impl<'source> Scanner<'source> {
    fn new(source: &'source str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            errors: Vec::new(),
        }
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.add_token(TokenType::Eof, None);
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    // TODO: Add support for multiline comments
    fn scan_token(&mut self) {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen, None),
            ')' => self.add_token(TokenType::RightParen, None),
            '{' => self.add_token(TokenType::LeftBrace, None),
            '}' => self.add_token(TokenType::RightBrace, None),
            ',' => self.add_token(TokenType::Comma, None),
            '.' => self.add_token(TokenType::Dot, None),
            '-' => self.add_token(TokenType::Minus, None),
            '+' => self.add_token(TokenType::Plus, None),
            ';' => self.add_token(TokenType::Semicolon, None),
            '*' => self.add_token(TokenType::Star, None),
            '!' => self.add_matched_token('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.add_matched_token('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => self.add_matched_token('=', TokenType::LessEqual, TokenType::Less),
            '>' => self.add_matched_token('=', TokenType::GreaterEqual, TokenType::Greater),
            '/' => {
                if self.match_next('/') {
                    self.advance_to_eol();
                } else {
                    self.add_token(TokenType::Slash, None);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => self.eat_string(),
            c if c.is_digit(10) => self.eat_number(),
            c if c.is_alphabetic() || c == '_' => self.eat_identifier(),
            unexpected => self.add_error(format!("Unexpected character: {}", unexpected)),
        }
    }

    fn eat_identifier(&mut self) {
        while (self.peek().is_alphanumeric() || self.peek() == '_') && !self.is_at_end() {
            self.advance();
        }
        let text = &self.source[self.start..self.current];
        let token_type = TokenType::from_keyword(text).unwrap_or(TokenType::Identifier);
        self.add_token(token_type, None);
    }

    fn eat_number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
            while self.peek().is_digit(10) {
                self.advance();
            }
        }
        let value = self.source[self.start..self.current]
            .parse::<f64>()
            .unwrap();
        self.add_token(TokenType::Number, LiteralNode::some_number(value));
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap()
        }
    }

    fn eat_string(&mut self) {
        self.advance_to_char_ml('"');
        if self.is_at_end() {
            self.add_error("Unterminated string.".to_string());
            return;
        }
        self.advance();
        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token(TokenType::String, LiteralNode::some_string(value));
    }

    fn advance_to_char_ml(&mut self, expected: char) {
        while self.peek() != expected && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
    }

    fn advance_to_eol(&mut self) {
        while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap()
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn add_error(&mut self, message: String) {
        let error = LexerError::new(self.line, message);
        self.errors.push(error);
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<LiteralNode>) {
        let token = Token::new(
            token_type,
            self.source[self.start..self.current].to_string(),
            literal,
            self.line,
        );
        self.tokens.push(token);
    }

    fn add_matched_token(
        &mut self,
        expect: char,
        match_token: TokenType,
        no_match_token: TokenType,
    ) {
        let token_type = if self.match_next(expect) {
            match_token
        } else {
            no_match_token
        };
        self.add_token(token_type, None);
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }
        self.current += 1;
        true
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Binary(Box<BinaryExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(Box<LiteralExpr>),
    Unary(Box<UnaryExpr>),
}

impl Expr {
    fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Self::Binary(expr) => expr.accept(visitor),
            Self::Grouping(expr) => expr.accept(visitor),
            Self::Literal(expr) => expr.accept(visitor),
            Self::Unary(expr) => expr.accept(visitor),
        }
    }
}

#[derive(Debug, Clone)]
struct BinaryExpr {
    left: Expr,
    operator: Token,
    right: Expr,
}

impl BinaryExpr {
    fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }

    fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_binary_expr(self)
    }
}

#[derive(Debug, Clone)]
struct GroupingExpr {
    expression: Expr,
}

impl GroupingExpr {
    fn new(expression: Expr) -> Self {
        Self { expression }
    }

    fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_grouping_expr(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum LiteralValue {
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
struct LiteralExpr {
    value: LiteralValue,
}

impl LiteralExpr {
    fn new(value: LiteralValue) -> Self {
        Self { value }
    }

    fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_literal_expr(self)
    }
}

#[derive(Debug, Clone)]
struct UnaryExpr {
    operator: Token,
    right: Expr,
}

impl UnaryExpr {
    fn new(operator: Token, right: Expr) -> Self {
        Self { operator, right }
    }

    fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_unary_expr(self)
    }
}

trait Visitor<T> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> T;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> T;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> T;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> T;
}

struct AstPrinter;

impl AstPrinter {
    fn new() -> Self {
        Self {}
    }

    fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> String {
        format!(
            "({} {} {})",
            expr.operator.lexeme,
            self.print(&expr.left),
            self.print(&expr.right)
        )
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> String {
        format!("(group {})", self.print(&expr.expression))
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
        format!("({} {})", expr.operator.lexeme, self.print(&expr.right))
    }
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
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

    fn parse(&mut self) -> Expr {
        match self.expression() {
            Ok(expr) => expr,
            Err(_) => Expr::Literal(Box::new(LiteralExpr::new(LiteralValue::Nil))),
        }
    }

    fn expression(&mut self) -> ParserResult {
        self.equality()
    }

    fn equality(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;

        while self.match_next(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult {
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

    fn term(&mut self) -> ParserResult {
        let mut expr = self.factor()?;

        while self.match_next(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult {
        let mut expr = self.unary()?;

        while self.match_next(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult {
        if self.match_next(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(Box::new(UnaryExpr::new(operator, right))));
        }

        self.primary()
    }

    fn primary(&mut self) -> ParserResult {
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
                LiteralNode::Number(n) => {
                    return Ok(Expr::Literal(Box::new(LiteralExpr::new(
                        LiteralValue::Number(n),
                    ))))
                }
                LiteralNode::String(s) => {
                    return Ok(Expr::Literal(Box::new(LiteralExpr::new(
                        LiteralValue::String(s),
                    ))));
                }
            }
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

type ParserResult = Result<Expr, ParserError>;

struct Interpreter {
    had_error: bool,
}

impl Interpreter {
    fn new() -> Self {
        Self { had_error: false }
    }

    fn interpret(&mut self, expr: &Expr) {
        match self.evaluate(expr) {
            Ok(value) => println!("{}", value),
            Err(e) => {
                self.had_error = true;
                println!("{}", e);
            }
        }
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
