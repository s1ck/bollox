use std::collections::HashMap;

use crate::{
    expr::{Expr, ExprNode},
    interp::InterpreterContext,
    node::Node,
    stmt::{FunctionDeclaration, Stmt, StmtNode},
    Result,
};

type ResolverResult<'a> = Result<StmtNode<'a>>;

pub fn resolver<'a, 'i, I>(
    statements: I,
    interpreter: InterpreterContext<'a>,
) -> Resolver<'a, I::IntoIter>
where
    I: IntoIterator<Item = StmtNode<'a>>,
{
    Resolver::new(statements.into_iter(), interpreter)
}

pub struct Resolver<'a, I: IntoIterator<Item = StmtNode<'a>>> {
    context: ResolverContext<'a>,
    statements: I,
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Resolver<'a, I> {
    fn new(statements: I, interpreter: InterpreterContext<'a>) -> Self {
        Self {
            context: ResolverContext::new(interpreter),
            statements,
        }
    }

    pub(crate) fn interpreter_context(self) -> InterpreterContext<'a> {
        self.context.interpreter
    }
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Iterator for Resolver<'a, I> {
    type Item = ResolverResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements.next().map(|stmt| {
            match ResolverOps::resolve_stmt(&mut self.context, &stmt) {
                Ok(()) => Ok(stmt),
                Err(e) => Err(e),
            }
        })
    }
}

pub(crate) struct ResolverOps;

impl ResolverOps {
    pub(crate) fn resolve_stmt<'a>(
        context: &mut ResolverContext<'a>,
        stmt: &StmtNode<'a>,
    ) -> Result<()> {
        match &*stmt.item {
            Stmt::Block(stmts) => Self::resolve_block(context, stmts),
            Stmt::Expression(expr) => Self::resolve_expr(context, expr),
            Stmt::Print(expr) => Self::resolve_expr(context, expr),
            Stmt::Var(name, initializer) => Self::resolve_variable(context, name, initializer),
            Stmt::Func(declaration) => Self::resolve_function(context, declaration),
            Stmt::If(condition, then_branch, else_branch) => {
                Self::resolve_expr(context, condition)?;
                Self::resolve_stmt(context, then_branch)?;
                if let Some(else_branch) = else_branch {
                    Self::resolve_stmt(context, else_branch)?;
                }
                Ok(())
            }
            Stmt::While(condition, statement) => {
                Self::resolve_expr(context, condition)?;
                Self::resolve_stmt(context, statement)?;
                Ok(())
            }
            Stmt::Return(value) => {
                if let Some(value) = value {
                    Self::resolve_expr(context, value)?
                }
                Ok(())
            }
        }
    }

    fn resolve_expr<'a>(context: &mut ResolverContext<'a>, expr: &ExprNode<'a>) -> Result<()> {
        match &*expr.item {
            Expr::Unary { op: _, expr } => Self::resolve_expr(context, expr),
            Expr::Binary { lhs, op: _, rhs } => {
                Self::resolve_expr(context, lhs)?;
                Self::resolve_expr(context, rhs)?;
                Ok(())
            }
            Expr::Call { callee, args } => {
                Self::resolve_expr(context, callee)?;
                args.iter()
                    .map(|arg| Self::resolve_expr(context, arg))
                    .collect::<Result<()>>()?;
                Ok(())
            }
            Expr::Logical { lhs, op: _, rhs } => {
                Self::resolve_expr(context, lhs)?;
                Self::resolve_expr(context, rhs)?;
                Ok(())
            }
            Expr::Group { expr } => Self::resolve_expr(context, expr),
            Expr::Literal { lit: _ } => Ok(()),
            Expr::Variable { name } => match context.var_state(name) {
                Some(VarState::Defined) | None => Ok(context.resolve_local(expr, name)),
                _ => todo!("error: can't read local variable in its own initializer"),
            },
            Expr::Assign { name, expr } => {
                Self::resolve_expr(context, expr)?;
                Ok(context.resolve_local(expr, name))
            }
            Expr::Lambda { declaration } => Self::resolve_function(context, declaration),
        }
    }

    fn resolve_stmts<'a>(context: &mut ResolverContext<'a>, stmts: &[StmtNode<'a>]) -> Result<()> {
        stmts
            .iter()
            .map(|stmt| Self::resolve_stmt(context, stmt))
            .collect::<Result<()>>()
    }

    fn resolve_block<'a>(context: &mut ResolverContext<'a>, stmts: &[StmtNode<'a>]) -> Result<()> {
        context.begin_scope();
        Self::resolve_stmts(context, stmts)?;
        context.end_scope();
        Ok(())
    }

    fn resolve_variable<'a>(
        context: &mut ResolverContext<'a>,
        name: &Node<&'a str>,
        initializer: &Option<ExprNode<'a>>,
    ) -> Result<()> {
        context.declare(name.item);
        if let Some(expr) = initializer {
            Self::resolve_expr(context, expr)?;
        }
        context.define(name.item);
        Ok(())
    }

    fn resolve_function<'a>(
        context: &mut ResolverContext<'a>,
        declaration: &FunctionDeclaration<'a>,
    ) -> Result<()> {
        // resolve function name first, to allow for recursive calls
        context.declare(declaration.name.item);
        context.define(declaration.name.item);

        context.begin_scope();
        declaration.params.iter().for_each(|param| {
            context.declare(param.item);
            context.define(param.item);
        });
        Self::resolve_stmts(context, &*declaration.body)?;
        context.end_scope();
        Ok(())
    }
}

pub(crate) struct ResolverContext<'a> {
    interpreter: InterpreterContext<'a>,
    // A scope maps variables to their state, where
    // `false` indicates that the variable exists,
    // but is not yet initialized.
    scopes: Vec<HashMap<&'a str, VarState>>,
}

#[derive(Debug, Clone, Copy)]
enum VarState {
    Declared,
    Defined,
}

impl<'a> ResolverContext<'a> {
    fn new(interpreter: InterpreterContext<'a>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
        }
    }

    // Creates a new block scope and pushes it to the stack.
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // Pops the last scope from the stack.
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &'a str) {
        match self.scopes.last_mut() {
            Some(scope) => scope.insert(name, VarState::Declared),
            None => None,
        };
    }

    fn define(&mut self, name: &'a str) {
        match self.scopes.last_mut() {
            Some(scope) => scope.insert(name, VarState::Defined),
            None => None,
        };
    }

    fn var_state(&self, name: &'a str) -> Option<VarState> {
        match self.scopes.last() {
            Some(scope) => scope.get(name).cloned(),
            None => None,
        }
    }

    // Finds the innermost scope, that contains the given variable name.
    // Uses the index of that scope to resolve the expression.
    fn resolve_local(&mut self, expr: &ExprNode<'a>, name: &'a str) {
        match self
            .scopes
            .iter()
            .rev()
            .position(|scope| scope.contains_key(name))
        {
            Some(depth) => self.interpreter.locals.insert(expr.span, depth),
            None => None,
        };
    }
}
