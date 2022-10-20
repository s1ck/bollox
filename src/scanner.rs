use std::{fmt::Display, iter::Chain};

use crate::token::{Literal, Token, TokenType};

#[derive(Debug)]
pub struct ParseError {
    desc: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.desc)?;
        Ok(())
    }
}

impl std::error::Error for ParseError {}

pub struct Source<'a> {
    source: &'a str,
}

impl<'a> Source<'a> {
    pub fn new(source: &'a str) -> Self {
        Source { source }
    }
}

impl<'a> IntoIterator for Source<'a> {
    type Item = Result<Token, ParseError>;

    type IntoIter = Chain<Scanner<'a>, core::option::IntoIter<Result<Token, ParseError>>>;

    fn into_iter(self) -> Self::IntoIter {
        Scanner::new(self.source).chain(Some(Ok(Token::new(
            TokenType::Eof,
            "".to_string(),
            None,
            usize::MAX,
        ))))
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.is_at_end() {
                return None;
            }
            self.start = self.current;
            let token = self.scan_token();
            if token.is_some() {
                return token;
            }
        }
    }
}

impl<'a> Scanner<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Option<Result<Token, ParseError>> {
        let token = match self.advance() {
            '(' => self.create_token(TokenType::LeftParen),
            ')' => self.create_token(TokenType::RightParen),
            '{' => self.create_token(TokenType::LeftBrace),
            '}' => self.create_token(TokenType::RightBrace),
            ',' => self.create_token(TokenType::Comma),
            '.' => self.create_token(TokenType::Dot),
            '-' => self.create_token(TokenType::Minus),
            '+' => self.create_token(TokenType::Plus),
            ';' => self.create_token(TokenType::Semicolon),
            '*' => self.create_token(TokenType::Star),
            '!' => {
                let token_type = if self.next_matches('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.create_token(token_type)
            }
            '=' => {
                let token_type = if self.next_matches('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.create_token(token_type)
            }
            '<' => {
                let token_type = if self.next_matches('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.create_token(token_type)
            }
            '>' => {
                let token_type = if self.next_matches('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.create_token(token_type)
            }
            '/' => {
                if self.next_matches('/') {
                    while self.peek() != '\n' && self.is_at_end() {
                        self.advance();
                    }
                    return None;
                } else {
                    self.create_token(TokenType::Slash)
                }
            }
            ' ' | '\r' | '\t' => return None,
            '\n' => {
                self.line += 1;
                return None;
            }
            '"' => return Some(self.string()),
            c if c.is_ascii_digit() => return Some(self.number()),
            c if c.is_alphabetic() => self.identifier(),
            c => {
                return Some(Err(ParseError {
                    desc: format!("Unexpected character {}.", c),
                }))
            }
        };

        Some(Ok(token))
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let token_type = if let Some(token_type) = Self::keyword(text) {
            token_type
        } else {
            TokenType::Identifier
        };

        self.create_token(token_type)
    }

    fn number(&mut self) -> Result<Token, ParseError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the '.'
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let number = &self.source[self.start..self.current];
        let number = if let Ok(n) = number.parse::<f64>() {
            n
        } else {
            return Err(ParseError {
                desc: format!("Cannot parse nummber: {number}",),
            });
        };
        Ok(self.create_token_literal(TokenType::Number, Some(Literal::Number(number))))
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap()
        }
    }

    fn string(&mut self) -> Result<Token, ParseError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(ParseError {
                desc: "Unterminated string.".to_string(),
            });
        }

        self.advance(); // The closing ".

        let value = &self.source[self.start + 1..self.current - 1];

        Ok(self.create_token_literal(TokenType::String, Some(Literal::String(value.to_string()))))
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap()
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        c
    }

    fn next_matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap_or_default() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn create_token(&mut self, token_type: TokenType) -> Token {
        self.create_token_literal(token_type, None)
    }

    fn create_token_literal(&mut self, token_type: TokenType, literal: Option<Literal>) -> Token {
        let text = &self.source[self.start..self.current];
        Token::new(token_type, text.to_string(), literal, self.line)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn keyword(text: &str) -> Option<TokenType> {
        let token_type = match text {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => return None,
        };
        Some(token_type)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::BolloxError;
    use test::Bencher;

    const EOF_TOKEN: Token = Token::new(TokenType::Eof, String::new(), None, usize::MAX);

    #[test]
    fn test_left_paren() -> Result<(), BolloxError> {
        let s = Source::new(r#"("#);
        let tokens = s.into_iter().collect::<Result<Vec<_>, ParseError>>()?;

        let expected_token = Token::new(TokenType::LeftParen, "(".to_string(), None, 1);
        assert_eq!(tokens, vec![expected_token, EOF_TOKEN]);

        Ok(())
    }

    #[bench]
    fn bench_scanner(b: &mut Bencher) {
        let input = include_str!("../tests/classes.crox");
        b.bytes = input.len() as u64;
        b.iter(|| {
            let _ = Scanner::new(input).collect::<Result<Vec<_>, _>>();
        });
    }
}
