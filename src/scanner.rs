use std::{fmt::Display, iter::Chain, str::CharIndices};

use crate::token::{Token, TokenType};

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

struct PeekPeekIterator<I: Iterator> {
    iter: I,
    peek_one: Option<Option<I::Item>>,
    peek_two: Option<Option<I::Item>>,
}

impl<I: Iterator> PeekPeekIterator<I> {
    fn new(iter: I) -> Self {
        PeekPeekIterator {
            iter,
            peek_one: None,
            peek_two: None,
        }
    }

    fn peek(&mut self) -> Option<&I::Item> {
        let iter = &mut self.iter;
        self.peek_one.get_or_insert_with(|| iter.next()).as_ref()
    }

    fn peek_peek(&mut self) -> Option<&I::Item> {
        self.peek();
        let iter = &mut self.iter;
        self.peek_two.get_or_insert_with(|| iter.next()).as_ref()
    }
}

impl<I: Iterator> Iterator for PeekPeekIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_one.take() {
            Some(v) => {
                self.peek_one = self.peek_two.take();
                v
            }
            None => match self.peek_two.take() {
                Some(v) => v,
                None => self.iter.next(),
            },
        }
    }
}

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
        let line = self.source.lines().count();
        let offset = self.source.len();
        let len = 0;
        Scanner::new(self).chain(Some(Ok(Token::new(TokenType::Eof, line, offset, len))))
    }
}

pub struct Scanner<'a> {
    source: Source<'a>,
    chars: PeekPeekIterator<CharIndices<'a>>,
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
    pub(crate) fn new(source: Source<'a>) -> Self {
        let chars = PeekPeekIterator::new(source.source.char_indices());
        Self {
            source,
            chars,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Option<Result<Token, ParseError>> {
        let token_type = match self.advance() {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            '!' => {
                if self.next_matches('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            '=' => {
                if self.next_matches('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            '<' => {
                if self.next_matches('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            '>' => {
                if self.next_matches('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            '/' => {
                if self.next_matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    return None;
                } else {
                    TokenType::Slash
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

        Some(Ok(Token::new(
            token_type,
            self.line,
            self.start,
            self.current - self.start,
        )))
    }

    fn identifier(&mut self) -> TokenType {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        match &self.source.source[self.start..self.current] {
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
            _ => TokenType::Identifier,
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

        // extract string value without surrounding "
        let offset = self.start + 1;
        let len = self.current - self.start - 2;
        Ok(Token::new(TokenType::String, self.line, offset, len))
    }

    fn number(&mut self) -> Result<Token, ParseError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_peek().is_ascii_digit() {
            // Consume the '.'
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        Ok(Token::new(
            TokenType::Number,
            self.line,
            self.start,
            self.current - self.start,
        ))
    }

    fn peek(&mut self) -> char {
        if let Some((_, c)) = self.chars.peek() {
            *c
        } else {
            '\0'
        }
    }

    fn peek_peek(&mut self) -> char {
        if let Some((_, c)) = self.chars.peek_peek() {
            *c
        } else {
            '\0'
        }
    }

    fn advance(&mut self) -> char {
        if let Some((idx, c)) = self.chars.next() {
            self.current = idx + 1;
            c
        } else {
            '\0'
        }
    }

    fn next_matches(&mut self, expected: char) -> bool {
        if let Some((idx, next)) = self.chars.peek() {
            if *next != expected {
                return false;
            } else {
                self.current = *idx;
                return true;
            }
        }
        false
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::BolloxError;

    fn eof(source: &str) -> Token {
        Token::new(TokenType::Eof, source.lines().count(), source.len(), 0)
    }

    #[test]
    fn test_left_paren() -> Result<(), BolloxError> {
        let input = "(";
        let source = Source::new(input);
        let tokens = source.into_iter().collect::<Result<Vec<_>, ParseError>>()?;
        let expected_token = Token::new(TokenType::LeftParen, 1, 0, 1);
        assert_eq!(tokens, vec![expected_token, eof(input)]);
        Ok(())
    }
}
