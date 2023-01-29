use std::{iter::Chain, str::CharIndices};

use crate::{
    error::{BolloxError, ScanError},
    token::{Token, TokenType},
    util::PeekPeekIterator,
    Result,
};

#[derive(Clone, Copy)]
pub struct Source<'a> {
    pub(crate) source: &'a str,
}

impl<'a> Source<'a> {
    pub fn new(source: &'a str) -> Self {
        Source { source }
    }

    pub fn slice(&self, range: impl Into<std::ops::Range<usize>>) -> &'a str {
        &self.source[range.into()]
    }
}

impl<'a> IntoIterator for Source<'a> {
    type Item = Result<Token>;

    type IntoIter = Chain<Scanner<'a>, core::option::IntoIter<Result<Token>>>;

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
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.is_at_end() {
                return None;
            }
            self.start = self.current;

            if let Some(result) = self.scan_token() {
                return Some(result.map_err(BolloxError::from));
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

    fn scan_token(&mut self) -> Option<Result<Token>> {
        let token_type = match self.advance()? {
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
                    self.advance();
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            '=' => {
                if self.next_matches('=') {
                    self.advance();
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            '<' => {
                if self.next_matches('=') {
                    self.advance();
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            '>' => {
                if self.next_matches('=') {
                    self.advance();
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            '/' => {
                if self.next_matches('/') {
                    self.advance_while(|c| *c != '\n');
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
                return Some(Err(ScanError::unexpected_token(
                    (self.start, self.current - self.start).into(),
                    c,
                )))
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
        self.advance_while(|c| matches!(c, '0'..='9' | 'A'..='Z' | 'a'..='z' | '_'));
        TokenType::from(&self.source.source[self.start..self.current])
    }

    fn string(&mut self) -> Result<Token> {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(ScanError::unterminated_string(
                (self.start..self.current - self.start).into(),
            ));
        }

        self.advance(); // The closing ".

        // extract string value without surrounding "
        let offset = self.start + 1;
        let len = self.current - self.start - 2;
        Ok(Token::new(TokenType::String, self.line, offset, len))
    }

    fn number(&mut self) -> Result<Token> {
        self.advance_while(char::is_ascii_digit);

        match (self.peek(), self.peek_peek()) {
            (Some('.'), Some(c)) if c.is_ascii_digit() => {
                self.advance();
                self.advance_while(char::is_ascii_digit);
            }
            _ => {}
        }

        Ok(Token::new(
            TokenType::Number,
            self.line,
            self.start,
            self.current - self.start,
        ))
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn peek_peek(&mut self) -> Option<char> {
        self.chars.peek_peek().map(|(_, c)| *c)
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next().map(|(idx, c)| {
            self.current = idx + 1;
            c
        })
    }

    fn advance_while<F>(&mut self, pred: F)
    where
        for<'c> F: Fn(&'c char) -> bool,
    {
        while let Some(c) = self.peek() {
            if pred(&c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn next_matches(&mut self, expected: char) -> bool {
        match self.chars.peek() {
            Some((_, c)) if *c != expected => false,
            Some((idx, _)) => {
                self.current = *idx;
                true
            }
            None => false,
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eof(source: &str) -> Token {
        Token::new(TokenType::Eof, source.lines().count(), source.len(), 0)
    }

    #[test]
    fn test_left_paren() {
        let input = "(";
        let source = Source::new(input);
        let tokens = source
            .into_iter()
            .filter_map(|r| match r {
                Ok(t) => Some(t),
                _ => None,
            })
            .collect::<Vec<_>>();
        let expected_token = Token::new(TokenType::LeftParen, 1, 0, 1);
        assert_eq!(tokens, vec![expected_token, eof(input)]);
    }
}
