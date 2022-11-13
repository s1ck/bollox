use std::{iter::Peekable, ops::Range};

use crate::{
    ast::{BinaryOp, Expr, Literal, Node, UnaryOp},
    error::{BolloxError, SyntaxError},
    token::{Span, Token, TokenType},
    Source,
};
use TokenType::*;

type Tok = (TokenType, Span);

pub struct TokIterator<T> {
    inner: T,
}

impl<T: Iterator<Item = Token>> Iterator for TokIterator<T> {
    type Item = Tok;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|tok| (tok.tpe, tok.span))
    }
}

pub fn parser<I>(source: Source<'_>, tokens: I) -> Parser<TokIterator<I::IntoIter>>
where
    I: IntoIterator<Item = Token>,
{
    let tokens = TokIterator {
        inner: tokens.into_iter(),
    }
    .peekable();

    Parser { source, tokens }
}

pub struct Parser<'a, I: Iterator<Item = Tok>> {
    source: Source<'a>,
    tokens: Peekable<I>,
}

impl<'a, I: Iterator<Item = Tok>> Parser<'a, I> {
    // expression → equality ;
    fn expression(&mut self) -> Result<Expr<'a>, SyntaxError> {
        self.equality()
    }
    // equality   → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr<'a>, SyntaxError> {
        let mut lhs = self.comparison()?;
        loop {
            let op = match self.tokens.peek() {
                Some(&(BangEqual, _)) => BinaryOp::NotEquals,
                Some(&(EqualEqual, _)) => BinaryOp::Equals,
                _ => break,
            };
            let _ = self.tokens.next();
            let rhs = self.comparison()?;
            let range = lhs.span().union(rhs.span());
            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }
        Ok(lhs)
    }
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr<'a>, SyntaxError> {
        let mut lhs = self.term()?;
        loop {
            let op = match self.tokens.peek() {
                Some(&(Greater, _)) => BinaryOp::GreaterThan,
                Some(&(GreaterEqual, _)) => BinaryOp::GreaterThanOrEqual,
                Some(&(Less, _)) => BinaryOp::LessThan,
                Some(&(LessEqual, _)) => BinaryOp::LessThanOrEqual,
                _ => break,
            };
            let _ = self.tokens.next();
            let rhs = self.term()?;
            let range = lhs.span().union(rhs.span());
            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }
        Ok(lhs)
    }
    // term       → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr<'a>, SyntaxError> {
        let mut lhs = self.factor()?;
        loop {
            let op = match self.tokens.peek() {
                Some(&(Minus, _)) => BinaryOp::Sub,
                Some(&(Plus, _)) => BinaryOp::Add,
                _ => break,
            };
            let _ = self.tokens.next();
            let rhs = self.factor()?;
            let range = lhs.span().union(rhs.span());
            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }
        Ok(lhs)
    }
    // factor     → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr<'a>, SyntaxError> {
        let mut lhs = self.unary()?;
        loop {
            let op = match self.tokens.peek() {
                Some(&(Slash, _)) => BinaryOp::Div,
                Some(&(Star, _)) => BinaryOp::Mul,
                _ => break,
            };
            let _ = self.tokens.next();
            let rhs = self.unary()?;
            let range = lhs.span().union(rhs.span());
            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }
        Ok(lhs)
    }
    // unary      → ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr<'a>, SyntaxError> {
        let (op, span) = match self.tokens.peek() {
            Some(&(Bang, span)) => (UnaryOp::Not, span),
            Some(&(Minus, span)) => (UnaryOp::Neg, span),
            _ => return self.primary(),
        };
        let _ = self.tokens.next();
        let inner = self.unary()?;
        let range = span.union(inner.span());
        Ok(Node::unary(op, inner).into_expr(range))
    }
    // primary    → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr<'a>, SyntaxError> {
        let (node, span): (Node<Expr<'a>>, _) = match self.tokens.next() {
            Some((False, span)) => (Node::fals(), span),
            Some((True, span)) => (Node::tru(), span),
            Some((Nil, span)) => (Node::nil(), span),
            Some((Number, span)) => {
                let value = &self.source.source[Range::<usize>::from(span)];
                let value = value.parse::<f64>().unwrap();
                let value = Literal::Number(value);
                (Node::literal(value), span)
            }
            Some((String, span)) => {
                let value = &self.source.source[Range::<usize>::from(span)];
                let value = Literal::String(value);
                (Node::literal(value), span)
            }
            Some((LeftParen, span)) => {
                let expr = self.expression()?;
                let span = span.union(expr.span());

                match self.tokens.next() {
                    Some((RightParen, r_span)) => {
                        let span = r_span.union(span.into());
                        (Node::group(expr), span.into())
                    }
                    _ => return Err(SyntaxError::missing_closing_parenthesis(span.into())),
                }
            }
            Some((token, span)) => return Err(SyntaxError::unsupported_token_type(token, span)),
            None => return Err(SyntaxError::unexpected_eoi()),
        };

        Ok(node.into_expr(span.into()))
    }
}

impl<'a, I: Iterator<Item = Tok>> Iterator for Parser<'a, I> {
    type Item = Result<Expr<'a>, BolloxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let _ = self.tokens.peek()?; // check for EOI
        Some(self.expression().map_err(BolloxError::from))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_parser() -> Result<(), SyntaxError> {
        let source = "4 + 2";
        let source = Source::new(source);
        let tokens = source.scan_all().unwrap();
        let parser = parser(source, tokens);
        let expres = parser
            .filter_map(|e| match e {
                Ok(e) => Some(e),
                _ => None,
            })
            .collect::<Option<Expr>>();

        let num0 = Node::number(4_f64).into_expr(0..1);
        let num1 = Node::number(2_f64).into_expr(4..5);
        let expr = Node::add(num0, num1).into_expr(0..5);

        assert_eq!(expres, Some(expr));

        Ok(())
    }
}
