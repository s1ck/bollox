use std::{iter::Peekable, ops::Range};

use crate::{
    error::{BolloxError, SyntaxError},
    expr::{BinaryOp, Expr, ExprNode, Literal, UnaryOp},
    stmt::{Stmt, StmtNode},
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
    fn declaration(&mut self) -> Result<StmtNode<'a>> {
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
    fn var_decl(&mut self) -> Result<StmtNode<'a>> {
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
            Some((Semicolon, _)) => Ok(Stmt::var(name, initializer).at(var_span)),
            _ => Err(SyntaxError::missing_semicolon(
                initializer
                    .map(|e| e.span)
                    .unwrap_or_else(|| name_span.shrink_to_end()),
            )),
        }
    }
    // statement -> expr_stmt | print_stmt | block_stmt ;
    fn statement(&mut self) -> Result<StmtNode<'a>> {
        match self.tokens.peek() {
            Some(&(Print, _)) => self.print_stmt(),
            Some(&(LeftBrace, span)) => self.block_stmt(span),
            Some(&(If, _)) => self.if_stmt(),
            _ => self.expr_stmt(),
        }
    }
    // expr_stmt -> expression ";" ;
    fn expr_stmt(&mut self) -> Result<StmtNode<'a>> {
        let expr = self.expression()?;
        let span = expr.span;
        match self.tokens.next() {
            Some((Semicolon, _)) => Ok(Stmt::expression(expr).at(span)),
            _ => Err(SyntaxError::missing_semicolon(span)),
        }
    }
    // print_stmt -> "print" expression ";" ;
    fn print_stmt(&mut self) -> Result<StmtNode<'a>> {
        let _ = self.tokens.next(); // consume PRINT token
        let expr = self.expression()?;
        let span = expr.span;
        match self.tokens.next() {
            Some((Semicolon, _)) => Ok(Stmt::print(expr).at(span)),
            _ => Err(SyntaxError::missing_semicolon(span)),
        }
    }
    // block_stmt -> "{" declaration* "}"
    fn block_stmt(&mut self, start: Span) -> Result<StmtNode<'a>> {
        let _ = self.tokens.next(); // consume { token
        let mut stmts = Vec::new();
        let end = loop {
            match self.tokens.peek() {
                Some(&(RightBrace, span)) => {
                    let _ = self.tokens.next(); // consume } token
                    break span;
                }
                None | Some(&(Eof, _)) => {
                    return Err(SyntaxError::missing_closing_parenthesis(start))
                }
                _ => stmts.push(self.declaration()?),
            }
        };

        Ok(Stmt::block(stmts).at(start.union(end)))
    }
    // if_stmt -> "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_stmt(&mut self) -> Result<StmtNode<'a>> {
        let _ = self.tokens.next(); // consume If token

        let cond = match self.tokens.peek() {
            Some(&(LeftParen, _)) => {
                let _ = self.tokens.next(); // consume ( token
                self.expression()?
            }
            _ => todo!("error"),
        };

        match self.tokens.peek() {
            Some(&(RightParen, _)) => {
                let _ = self.tokens.next(); // consume ) token
            }
            _ => todo!("error"),
        }

        let then_ = self.statement()?;

        match self.tokens.peek() {
            Some(&(Else, _)) => {
                let _ = self.tokens.next(); // consume Else token
                let else_ = self.statement()?;
                let span = cond.span.union(else_.span);
                Ok(Stmt::if_else(cond, then_, else_).at(span))
            }
            _ => {
                let span = cond.span.union(then_.span);
                Ok(Stmt::if_(cond, then_).at(span))
            }
        }
    }

    // expression → assignment ;
    fn expression(&mut self) -> Result<ExprNode<'a>> {
        self.assignment()
    }
    // assignment -> IDENTIFIER "=" assignment | equality ;
    fn assignment(&mut self) -> Result<ExprNode<'a>> {
        let lhs = self.equality()?;
        if let Some(&(Equal, eq_span)) = self.tokens.peek() {
            let _ = self.tokens.next();
            let rhs = self.assignment()?;
            // if lhs is a variable, we got an assignment
            return match *lhs.item {
                Expr::Variable { name } => {
                    let range = lhs.span.union(rhs.span);
                    Ok(Expr::assign(name, rhs).at(range))
                }
                _ => Err(SyntaxError::invalid_assignment_target(eq_span)),
            };
        }

        Ok(lhs)
    }
    // equality   → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<ExprNode<'a>> {
        let mut lhs = self.comparison()?;
        loop {
            let op = match self.tokens.peek() {
                Some(&(BangEqual, _)) => BinaryOp::NotEquals,
                Some(&(EqualEqual, _)) => BinaryOp::Equals,
                _ => break,
            };
            let _ = self.tokens.next();
            let rhs = self.comparison()?;
            let range = lhs.span.union(rhs.span);
            lhs = Expr::binary(lhs, op, rhs).at(range);
        }
        Ok(lhs)
    }
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<ExprNode<'a>> {
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
            let range = lhs.span.union(rhs.span);
            lhs = Expr::binary(lhs, op, rhs).at(range);
        }
        Ok(lhs)
    }
    // term       → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<ExprNode<'a>> {
        let mut lhs = self.factor()?;
        loop {
            let op = match self.tokens.peek() {
                Some(&(Minus, _)) => BinaryOp::Sub,
                Some(&(Plus, _)) => BinaryOp::Add,
                _ => break,
            };
            let _ = self.tokens.next();
            let rhs = self.factor()?;
            let range = lhs.span.union(rhs.span);
            lhs = Expr::binary(lhs, op, rhs).at(range);
        }
        Ok(lhs)
    }
    // factor     → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<ExprNode<'a>> {
        let mut lhs = self.unary()?;
        loop {
            let op = match self.tokens.peek() {
                Some(&(Slash, _)) => BinaryOp::Div,
                Some(&(Star, _)) => BinaryOp::Mul,
                _ => break,
            };
            let _ = self.tokens.next();
            let rhs = self.unary()?;
            let range = lhs.span.union(rhs.span);
            lhs = Expr::binary(lhs, op, rhs).at(range);
        }
        Ok(lhs)
    }
    // unary      → ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<ExprNode<'a>> {
        let (op, span) = match self.tokens.peek() {
            Some(&(Bang, span)) => (UnaryOp::Not, span),
            Some(&(Minus, span)) => (UnaryOp::Neg, span),
            _ => return self.primary(),
        };
        let _ = self.tokens.next();
        let inner = self.unary()?;
        let range = span.union(inner.span);
        Ok(Expr::unary(op, inner).at(range))
    }
    // primary    → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
    fn primary(&mut self) -> Result<ExprNode<'a>> {
        let (node, span) = match self.tokens.next() {
            Some((False, span)) => (Expr::fals(), span),
            Some((True, span)) => (Expr::tru(), span),
            Some((Nil, span)) => (Expr::nil(), span),
            Some((Number, span)) => {
                let value = &self.source.source[Range::<usize>::from(span)];
                let value = value.parse::<f64>().unwrap();
                let value = Literal::Number(value);
                (Expr::literal(value), span)
            }
            Some((String, span)) => {
                let value = &self.source.source[Range::<usize>::from(span)];
                let value = Literal::String(value);
                (Expr::literal(value), span)
            }
            Some((LeftParen, span)) => {
                let expr = self.expression()?;
                let span = span.union(expr.span);

                match self.tokens.next() {
                    Some((RightParen, r_span)) => {
                        let span = r_span.union(span);
                        (Expr::group(expr), span.into())
                    }
                    _ => return Err(SyntaxError::missing_closing_parenthesis(span)),
                }
            }
            Some((Identifier, span)) => {
                let value = &self.source.source[Range::<usize>::from(span)];
                (Expr::variable(value), span)
            }

            Some((token, span)) => return Err(SyntaxError::unsupported_token_type(token, span)),
            None => return Err(SyntaxError::unexpected_eoi()),
        };

        Ok(node.at(span))
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
    type Item = Result<StmtNode<'a>>;

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
        let actual = parser
            .filter_map(|e| match e {
                Ok(e) => Some(e),
                _ => None,
            })
            .collect::<Option<StmtNode<'_>>>();

        let num0 = Expr::number(4_f64).at(0..1);
        let num1 = Expr::number(2_f64).at(4..5);
        let expr = Expr::add(num0, num1).at(0..5);
        let expected = Stmt::expression(expr).at(0..5);

        assert_eq!(actual, Some(expected));

        Ok(())
    }
}
