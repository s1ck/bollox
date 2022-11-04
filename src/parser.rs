// LOX Grammar
//
// expression → equality ;
// equality   → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term       → factor ( ( "-" | "+" ) factor )* ;
// factor     → unary ( ( "/" | "*" ) unary )* ;
// unary      → ( "!" | "-" ) unary | primary ;
// primary    → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

use std::ops::Range;

use crate::{
    ast::{BinaryOp, Expr, Literal, Node, UnaryOp},
    token::{Token, TokenType},
    Source,
};
use TokenType::*;

pub trait Parser<'a> {
    fn parse(self) -> Expr<'a>;
}

pub struct RecursiveDescent<'a> {
    source: Source<'a>,
    tokens: Vec<Token>,
    current: usize,
}

impl<'a> RecursiveDescent<'a> {
    pub fn new(source: Source<'a>, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            current: 0,
        }
    }
}

impl<'a> Parser<'a> for RecursiveDescent<'a> {
    fn parse(mut self) -> Expr<'a> {
        self.expression()
    }
}

impl<'a> RecursiveDescent<'a> {
    // expression → equality ;
    fn expression(&mut self) -> Expr<'a> {
        self.equality()
    }
    // equality   → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Expr<'a> {
        let mut lhs = self.comparison();

        while self.matches([BangEqual, EqualEqual]) {
            let op = self.previous();
            let rhs = self.comparison();
            let range = lhs.span().union(rhs.span());

            let op = match op.tpe {
                BangEqual => BinaryOp::NotEquals,
                EqualEqual => BinaryOp::Equals,
                _ => unreachable!(),
            };

            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }

        lhs
    }
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Expr<'a> {
        let mut lhs = self.term();

        while self.matches([Greater, GreaterEqual, Less, LessEqual]) {
            let op = self.previous();
            let rhs = self.term();
            let range = lhs.span().union(rhs.span());

            let op = match op.tpe {
                Greater => BinaryOp::GreaterThan,
                GreaterEqual => BinaryOp::GreaterThanOrEqual,
                Less => BinaryOp::LessThan,
                LessEqual => BinaryOp::LessThanOrEqual,
                _ => unreachable!(),
            };

            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }

        lhs
    }
    // term       → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Expr<'a> {
        let mut lhs = self.factor();

        while self.matches([Minus, Plus]) {
            let op = self.previous();
            let rhs = self.factor();
            let range = lhs.span().union(rhs.span());

            let op = match op.tpe {
                Minus => BinaryOp::Sub,
                Plus => BinaryOp::Add,
                _ => unreachable!(),
            };

            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }

        lhs
    }
    // factor     → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Expr<'a> {
        let mut lhs = self.unary();

        while self.matches([Slash, Star]) {
            let op = self.previous();
            let rhs = self.unary();
            let range = lhs.span().union(rhs.span());

            let op = match op.tpe {
                Slash => BinaryOp::Div,
                Star => BinaryOp::Mul,
                _ => unreachable!(),
            };

            lhs = Node::binary(lhs, op, rhs).into_expr(range);
        }

        lhs
    }
    // unary      → ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Expr<'a> {
        if self.matches([Bang, Minus]) {
            let op = self.previous();
            let rhs = self.unary();
            let range = op.span().union(rhs.span());

            let op = match op.tpe {
                Bang => UnaryOp::Not,
                Minus => UnaryOp::Neg,
                _ => unreachable!(),
            };

            Node::unary(op, rhs).into_expr(range)
        } else {
            self.primary()
        }
    }
    // primary    → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Expr<'a> {
        if self.matches([False]) {
            return Node::fals().into_expr(self.previous().span().into());
        } else if self.matches([True]) {
            return Node::tru().into_expr(self.previous().span().into());
        } else if self.matches([Nil]) {
            return Node::nil().into_expr(self.previous().span().into());
        };

        if self.matches([Number, String]) {
            let lit = self.previous();
            let span = lit.span();
            let value = &self.source.source[Range::<usize>::from(span)];

            let lit = match lit.tpe {
                Number => Literal::Number(value.parse::<f64>().unwrap()),
                String => Literal::String(value),
                _ => unreachable!(),
            };

            return Node::literal(lit).into_expr(Range::<usize>::from(span));
        }

        if self.matches([LeftParen]) {
            let expr = self.expression();
            let span = expr.span();
            self.consume(RightParen, "Expect ')' after expression.");
            return Node::group(expr).into_expr(span.into());
        }

        // TODO error handling
        todo!()
    }
}

impl RecursiveDescent<'_> {
    fn matches(&mut self, token_types: impl IntoIterator<Item = TokenType>) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().tpe == token_type
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().tpe == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current]
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1]
    }

    fn consume(&mut self, token_type: TokenType, _error_message: &str) -> Token {
        if self.check(token_type) {
            return self.advance();
        }

        // TODO error handling
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_parser() {
        let source = "4 + 2";
        let source = Source::new(source);
        let tokens = source.scan_all().unwrap();
        let parser = RecursiveDescent::new(source, tokens);
        let expres = parser.parse();

        let num0 = Node::number(4_f64).into_expr(0..1);
        let num1 = Node::number(2_f64).into_expr(4..5);
        let expr = Node::add(num0, num1).into_expr(0..5);

        assert_eq!(expres, expr);
    }
}
