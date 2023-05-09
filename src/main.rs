use std::fmt::Display;
use std::io::Write;
use std::{error, fmt, fs, io};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => {
            println!("Prompt mode");
            run_prompt();
        }
        2 => {
            let path = &args[1];
            println!("Input file: {}", path);
            run_file(path);
        }
        _ => {
            println!("Usage: {} [path]", args[0]);
        }
    }
}

fn run_file(path: &str) {
    let file = fs::read_to_string(path).expect("Failed to read file");
    run(file.as_str());
}

fn run_prompt() {
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
        run(input.as_str());
    }
}

fn run(source: &str) {
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens();
    if scanner.has_errors() {
        for error in scanner.errors {
            println!("{}", error);
        }
    } else {
        for token in scanner.tokens {
            println!("{:?}", token);
        }
    }
}

#[derive(Debug, Clone)]
struct InterpreterError {
    line: usize,
    message: String,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.message)
    }
}

impl error::Error for InterpreterError {}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<String>,
    line: usize,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, literal: Option<String>, line: usize) -> Self {
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
    errors: Vec<InterpreterError>,
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
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        match (self.advance()) {
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
            unexpected => self.add_error(format!("Unexpected character: {}", unexpected)),
        }
    }

    fn eat_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            self.add_error("Unterminated string.".to_string());
            return;
        }
        self.advance();
        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token(TokenType::String, Some(value));
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
        let error = InterpreterError {
            line: self.line,
            message,
        };
        self.errors.push(error);
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<String>) {
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
