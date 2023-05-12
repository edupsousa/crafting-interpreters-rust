use std::{error, fmt};

use crate::tokens::{Token, TokenLiteral, TokenType};

#[derive(Debug, Clone)]
pub struct Scanner<'source> {
    source: &'source str,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    pub errors: Vec<LexerError>,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            errors: Vec::new(),
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn scan_tokens(&mut self) {
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
        self.add_token(TokenType::Number, TokenLiteral::some_number(value));
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
        self.add_token(TokenType::String, TokenLiteral::some_string(value));
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

    fn add_token(&mut self, token_type: TokenType, literal: Option<TokenLiteral>) {
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
pub struct LexerError {
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
