use crate::{
    callable::{Callable, Function},
    env::{Environment, EnvironmentRef},
    error::{BolloxError, RuntimeError, SyntaxError},
    expr::{BinaryOp, Expr, ExprNode, LogicalOp, UnaryOp},
    stmt::{Stmt, StmtNode},
    Result,
};

pub fn resolver<'a, I>(statements: I) -> Resolver<'a, I::IntoIter>
where
    I: IntoIterator<Item = StmtNode<'a>>,
{
    Resolver::new(statements.into_iter())
}

pub struct Resolver<'a, I: IntoIterator<Item = StmtNode<'a>>> {
    statements: I,
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Resolver<'a, I> {
    fn new(statements: I) -> Self {
        Self { statements }
    }
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Iterator for Resolver<'a, I> {
    type Item = Result<StmtNode<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements
            .next()
            .map(|stmt| match ResolverOps::eval_stmt(stmt) {
                Ok(stmt) => Ok(stmt),
                Err(e) => Err(e),
            })
    }
}

type ResolverResult<'a> = Result<StmtNode<'a>>;

pub(crate) struct ResolverOps;

impl ResolverOps {
    pub(crate) fn eval_stmt<'a>(stmt: StmtNode<'a>) -> ResolverResult<'a> {
        Ok(stmt)
    }
}
