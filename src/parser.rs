use std::{iter::Peekable, ops::Range};

use crate::{
    ast::{BinaryOp, Expr, Literal, Node, Stmt, UnaryOp},
    error::{BolloxError, SyntaxError},
    token::{Span, Token, TokenType},
    Result, Source,
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
    // program     -> declaration* EOF ;
    // declaration -> var_decl | statement ;
    fn declaration(&mut self) -> Result<Stmt<'a>> {
        let res = match self.tokens.peek() {
            Some(&(Var, _)) => self.var_decl(),
            _ => self.statement(),
        };

        match res {
            Ok(stmt) => Ok(stmt),
            Err(e) => {
                self.synchronize();
                Err(e)
            }
        }
    }
    // var_decl -> "var" IDENTIFER ( "=" expression )? ";" ;
    fn var_decl(&mut self) -> Result<Stmt<'a>> {
        let (_, var_span) = self.tokens.next().unwrap(); // consume VAR token
        let (name, name_span) = match self.tokens.next() {
            Some((Identifier, span)) => (&self.source.source[Range::<usize>::from(span)], span),
            _ => return Err(SyntaxError::missing_variable_name(var_span)),
        };
        let initializer = match self.tokens.peek() {
            Some(&(Equal, _)) => {
                let _ = self.tokens.next(); // consume EQUAL
                Some(self.expression()?)
            }
            _ => None,
        };
        match self.tokens.next() {
            Some((Semicolon, _)) => Ok(Stmt::Var(name, initializer)),
            _ => Err(SyntaxError::missing_semicolon(
                initializer
                    .map(|e| e.span)
                    .unwrap_or_else(|| name_span.shrink_to_end()),
            )),
        }
    }
    // statement -> expr_stmt | print_stmt | block_stmt ;
    fn statement(&mut self) -> Result<Stmt<'a>> {
        match self.tokens.peek() {
            Some(&(Print, _)) => self.print_stmt(),
            Some(&(LeftBrace, _)) => self.block_stmt(),
            _ => self.expr_stmt(),
        }
    }
    // expr_stmt -> expression ";" ;
    fn expr_stmt(&mut self) -> Result<Stmt<'a>> {
        let expr = self.expression()?;
        match self.tokens.next() {
            Some((Semicolon, _)) => Ok(Stmt::Expression(expr)),
            _ => Err(SyntaxError::missing_semicolon(expr.span)),
        }
    }
    // print_stmt -> "print" expression ";" ;
    fn print_stmt(&mut self) -> Result<Stmt<'a>> {
        let _ = self.tokens.next(); // consume PRINT token
        let expr = self.expression()?;
        match self.tokens.next() {
            Some((Semicolon, _)) => Ok(Stmt::Print(expr)),
            _ => Err(SyntaxError::missing_semicolon(expr.span)),
        }
    }
    // block_stmt -> "{" declaration* "}"
    fn block_stmt(&mut self) -> Result<Stmt<'a>> {
        let _ = self.tokens.next(); // consume { token
        let mut stmts = Vec::new();

        loop {
            match self.tokens.peek() {
                Some(&(Eof, _)) | Some(&(RightBrace, _)) => break,
                _ => stmts.push(self.declaration()?),
            }
        }

        match self.tokens.next() {
            Some((RightBrace, _)) => Ok(Stmt::Block(stmts)),
            // TODO pick proper span
            _ => Err(SyntaxError::missing_closing_parenthesis((0..1).into())),
        }
    }

    // expression → assignment ;
    fn expression(&mut self) -> Result<Expr<'a>> {
        self.assignment()
    }
    // assignment -> IDENTIFIER "=" assignment | equality ;
    fn assignment(&mut self) -> Result<Expr<'a>> {
        let lhs = self.equality()?;
        if let Some(&(Equal, eq_span)) = self.tokens.peek() {
            let _ = self.tokens.next();
            let rhs = self.assignment()?;
            // if lhs is a variable, we got an assignment
            return match *lhs.node {
                Node::Variable { name } => {
                    let range = lhs.span().union(rhs.span());
                    Ok(Node::assign(name, rhs).into_expr(range))
                }
                _ => Err(SyntaxError::invalid_assignment_target(eq_span)),
            };
        }

        Ok(lhs)
    }
    // equality   → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr<'a>> {
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
    fn comparison(&mut self) -> Result<Expr<'a>> {
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
    fn term(&mut self) -> Result<Expr<'a>> {
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
    fn factor(&mut self) -> Result<Expr<'a>> {
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
    fn unary(&mut self) -> Result<Expr<'a>> {
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
    // primary    → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
    fn primary(&mut self) -> Result<Expr<'a>> {
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
            Some((Identifier, span)) => {
                let value = &self.source.source[Range::<usize>::from(span)];
                (Node::variable(value), span)
            }

            Some((token, span)) => return Err(SyntaxError::unsupported_token_type(token, span)),
            None => return Err(SyntaxError::unexpected_eoi()),
        };

        Ok(node.into_expr(span.into()))
    }
}

impl<I: Iterator<Item = Tok>> Parser<'_, I> {
    fn synchronize(&mut self) {
        while let Some((tt, _)) = self.tokens.peek() {
            match *tt {
                Semicolon => {
                    let _ = self.tokens.next();
                    break;
                }
                Class | Fun | For | If | Print | Return | Var | While => break,
                _ => {
                    // skip any other token in order to synchronize
                    let _ = self.tokens.next();
                }
            }
        }
    }
}

impl<'a, I: Iterator<Item = Tok>> Iterator for Parser<'a, I> {
    type Item = Result<Stmt<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&(Eof, _)) | None = self.tokens.peek() {
            return None;
        }
        Some(self.declaration().map_err(BolloxError::from))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_parser() -> Result<()> {
        let source = "4 + 2;";
        let source = Source::new(source);
        let tokens = source
            .into_iter()
            .filter_map(|r| match r {
                Ok(t) => Some(t),
                _ => None,
            })
            .collect::<Vec<_>>();

        let parser = parser(source, tokens);
        let expres = parser
            .filter_map(|e| match e {
                Ok(e) => Some(e),
                _ => None,
            })
            .collect::<Vec<Stmt>>();

        let num0 = Node::number(4_f64).into_expr(0..1);
        let num1 = Node::number(2_f64).into_expr(4..5);
        let expr = Node::add(num0, num1).into_expr(0..5);
        let stmt = Stmt::Expression(expr);

        assert_eq!(expres[0], stmt);

        Ok(())
    }
}
