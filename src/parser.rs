use std::iter::Peekable;

use crate::{
    error::{BolloxError, SyntaxError},
    expr::{BinaryOp, Expr, ExprNode, Literal, LogicalOp, UnaryOp},
    node::Node,
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

macro_rules! peek {
    ($this:ident, { $($pat:pat => $expr:expr),+ $(,)? }) => {
        match $this.tokens.peek() {
            $(Some(&$pat) => {
                let _ = $this.tokens.next();
                Some($expr)
            }),+,
            _ => None,
        }
    };

    ($this:ident, $($pat:pat),+ $(,)?) => {
        match $this.tokens.peek() {
            $(Some(&($pat, _)) => {
                let _ = $this.tokens.next();
                true
            }),+,
            _ => false,
        }
    };
}

macro_rules! bin_op {
    ($this:ident, $descent:ident, $construct:expr, { $($pat:pat => $expr:expr),+ $(,)? }) => {{
        let mut lhs = $this.$descent()?;
        loop {
            let op = match peek!($this, { $($pat => $expr),+ }) {
                Some(op) => op,
                None => break,
            };
            let rhs = $this.$descent()?;
            let span = lhs.span.union(rhs.span);
            lhs = $construct(lhs, op, rhs).at(span);
        }
        Ok(lhs)
    }};
}

pub struct Parser<'a, I: Iterator<Item = Tok>> {
    source: Source<'a>,
    tokens: Peekable<I>,
}

impl<'a, I: Iterator<Item = Tok>> Parser<'a, I> {
    // program     -> declaration* EOF ;
    // declaration -> var_decl | statement ;
    fn declaration(&mut self) -> Result<StmtNode<'a>> {
        let res = peek!(self, {
            (Var, span) => self.var_decl(span),
        })
        .transpose()?;

        match res {
            Some(stmt) => Ok(stmt),
            None => match self.statement() {
                Ok(stmt) => Ok(stmt),
                Err(e) => {
                    self.synchronize();
                    Err(e)
                }
            },
        }
    }
    // var_decl -> "var" IDENTIFER ( "=" expression )? ";" ;
    fn var_decl(&mut self, var_span: Span) -> Result<StmtNode<'a>> {
        let ident = self.identifier(var_span)?;
        let init = peek!(self, { (Equal, _) => self.expression()? });
        let end = self.expect(Semicolon)?;
        Ok(Stmt::var(ident, init).at(var_span.union(end)))
    }
    // statement -> expr_stmt | print_stmt | block_stmt | if_stmt | while_stmt ;
    fn statement(&mut self) -> Result<StmtNode<'a>> {
        let stmt = peek!(self, {
            (Print, span) => self.print_stmt(span),
            (LeftBrace, span) => self.block_stmt(span),
            (If, span) => self.if_stmt(span),
            (While, span) => self.while_stmt(span),
            (For, span) => self.for_stmt(span),
        });
        match stmt {
            Some(Ok(stmt)) => Ok(stmt),
            Some(Err(err)) => Err(err),
            None => self.expr_stmt(),
        }
    }
    // expr_stmt -> expression ";" ;
    fn expr_stmt(&mut self) -> Result<StmtNode<'a>> {
        let expr = self.expression()?;
        let end = self.expect(Semicolon)?;
        let span = expr.span.union(end);
        Ok(Stmt::expression(expr).at(span))
    }
    // print_stmt -> "print" expression ";" ;
    fn print_stmt(&mut self, print_span: Span) -> Result<StmtNode<'a>> {
        let expr = self.expression()?;
        let span = self.expect(Semicolon)?;
        Ok(Stmt::print(expr).at(print_span.union(span)))
    }
    // block_stmt -> "{" declaration* "}"
    fn block_stmt(&mut self, start: Span) -> Result<StmtNode<'a>> {
        let mut stmts = Vec::new();
        let end = loop {
            match self.tokens.peek() {
                Some(&(RightBrace, span)) => {
                    let _ = self.tokens.next();
                    break span;
                }
                Some(&(Eof, span)) => {
                    return Err(SyntaxError::unexpected_token(RightBrace, Eof, span))
                }
                None => return Err(SyntaxError::unexpected_eoi()),
                _ => stmts.push(self.declaration()?),
            }
        };
        Ok(Stmt::block(stmts).at(start.union(end)))
    }
    // if_stmt -> "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_stmt(&mut self, if_span: Span) -> Result<StmtNode<'a>> {
        self.expect(LeftParen)?;
        let cond = self.expression()?;
        self.expect(RightParen)?;
        let then_ = self.statement()?;

        match self.tokens.peek() {
            Some(&(Else, _)) => {
                let _ = self.tokens.next();
                let else_ = self.statement()?;
                let span = if_span.union(else_.span);
                Ok(Stmt::if_else(cond, then_, else_).at(span))
            }
            _ => {
                let span = if_span.union(then_.span);
                Ok(Stmt::if_(cond, then_).at(span))
            }
        }
    }
    // while_stmt -> "while" "(" expression ")" statement ;
    fn while_stmt(&mut self, while_span: Span) -> Result<StmtNode<'a>> {
        self.expect(LeftParen)?;
        let cond = self.expression()?;
        self.expect(RightParen)?;
        let stmt = self.statement()?;
        let span = while_span.union(stmt.span);
        Ok(Stmt::while_(cond, stmt).at(span))
    }
    // for_stmt -> "for" "(" ( varDecl | exprStmt | ";" )
    //             expression? ";"
    //             expression? ")" statement ;
    //
    // Gets desugared into a while loop
    fn for_stmt(&mut self, for_span: Span) -> Result<StmtNode<'a>> {
        self.expect(LeftParen)?;

        let initializer = match self.tokens.peek() {
            Some(&(Semicolon, _)) => {
                let _ = self.tokens.next();
                None
            }
            Some(&(Var, span)) => {
                let _ = self.tokens.next();
                Some(self.var_decl(span)?)
            }
            Some(_) => {
                // we do not consume in the catch all case
                Some(self.expr_stmt()?)
            }
            None => Err(SyntaxError::unexpected_eoi())?,
        };

        let condition = match self.tokens.peek() {
            Some(&(Semicolon, span)) => Expr::tru().at(span),
            Some(_) => self.expression()?,
            _ => Err(SyntaxError::unexpected_eoi())?,
        };
        self.expect(Semicolon)?;

        let (increment, inc_span) = match self.tokens.peek() {
            Some(&(RightParen, span)) => (None, span),
            Some(_) => {
                let expr = self.expression()?;
                let span = expr.span;
                (Some(expr), span)
            }
            None => Err(SyntaxError::unexpected_eoi())?,
        };
        self.expect(RightParen)?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            let increment = Stmt::expression(increment).at(inc_span);
            let span = body.span.union(increment.span);
            body = Stmt::block([body, increment]).at(span);
        }

        let span = condition.span.union(body.span);
        let body = Stmt::while_(condition, body).at(span);
        let body_span = body.span;

        let body = match initializer {
            Some(initializer) => Stmt::block([initializer, body]),
            None => *body.item,
        };

        Ok(body.at(for_span.union(body_span)))
    }
    // expression -> assignment ;
    fn expression(&mut self) -> Result<ExprNode<'a>> {
        self.assignment()
    }
    // assignment -> IDENTIFIER "=" assignment | logic_or ;
    fn assignment(&mut self) -> Result<ExprNode<'a>> {
        let lhs = self.logic_or()?;

        if peek!(self, Equal) {
            let assignment = self.assignment()?;
            return match *lhs.item {
                Expr::Variable { name } => {
                    let range = lhs.span.union(assignment.span);
                    Ok(Expr::assign(name, assignment).at(range))
                }
                _ => Err(SyntaxError::invalid_assignment_target(lhs.span)),
            };
        }

        Ok(lhs)
    }
    // logic_or -> logic_and ( "or" logic_and )* ;
    fn logic_or(&mut self) -> Result<ExprNode<'a>> {
        bin_op!(self, logic_and, Expr::logical, {
            (Or, _) => LogicalOp::Or,
        })
    }
    // logic_and -> equality ( "and" equality )* ;
    fn logic_and(&mut self) -> Result<ExprNode<'a>> {
        bin_op!(self, equality, Expr::logical, {
            (And, _) => LogicalOp::And,
        })
    }

    // equality -> comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<ExprNode<'a>> {
        bin_op!(self, comparison, Expr::binary, {
            (BangEqual, _) => BinaryOp::NotEquals,
            (EqualEqual, _) => BinaryOp::Equals,
        })
    }
    // comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<ExprNode<'a>> {
        bin_op!(self, term, Expr::binary, {
            (Greater, _) => BinaryOp::GreaterThan,
            (GreaterEqual, _) => BinaryOp::GreaterThanOrEqual,
            (Less, _) => BinaryOp::LessThan,
            (LessEqual, _) => BinaryOp::LessThanOrEqual,
        })
    }
    // term -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<ExprNode<'a>> {
        bin_op!(self, factor, Expr::binary, {
            (Minus, _) => BinaryOp::Sub,
            (Plus, _) => BinaryOp::Add,
        })
    }
    // factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<ExprNode<'a>> {
        bin_op!(self, unary, Expr::binary, {
            (Slash, _) => BinaryOp::Div,
            (Star, _) => BinaryOp::Mul,
        })
    }
    // unary -> ( "!" | "-" ) unary | primary ;
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
    // primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
    fn primary(&mut self) -> Result<ExprNode<'a>> {
        let (node, span) = match self.tokens.next() {
            Some((False, span)) => (Expr::fals(), span),
            Some((True, span)) => (Expr::tru(), span),
            Some((Nil, span)) => (Expr::nil(), span),
            Some((Number, span)) => {
                let value = self.source.slice(span);
                let value = value.parse::<f64>().unwrap();
                let value = Literal::Number(value);
                (Expr::literal(value), span)
            }
            Some((String, span)) => {
                let value = self.source.slice(span);
                let value = Literal::String(value);
                (Expr::literal(value), span)
            }
            Some((LeftParen, span)) => {
                let expr = self.expression()?;
                let end = self.expect(RightParen)?;
                (Expr::group(expr), span.union(end).into())
            }
            Some((Identifier, span)) => {
                let value = self.source.slice(span);
                (Expr::variable(value), span)
            }
            Some((token, span)) => return Err(SyntaxError::unsupported_token_type(token, span)),
            None => return Err(SyntaxError::unexpected_eoi()),
        };

        Ok(node.at(span))
    }

    fn identifier(&mut self, span: Span) -> Result<Node<&'a str>> {
        let (name, name_span) = match self.tokens.next() {
            Some((Identifier, span)) => (self.source.slice(span), span),
            _ => return Err(SyntaxError::missing_variable_name(span)),
        };

        Ok(Node::new(name, name_span))
    }

    fn expect(&mut self, expected: TokenType) -> Result<Span> {
        match self.tokens.next() {
            Some((typ, span)) if expected == typ => Ok(span),
            Some((typ, span)) => Err(SyntaxError::unexpected_token(expected, typ, span)),
            None => Err(SyntaxError::unexpected_token(
                expected,
                Eof,
                self.source.source.len(),
            )),
        }
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
        let expected = Stmt::expression(expr).at(0..6);

        assert_eq!(actual, Some(expected));

        Ok(())
    }
}
