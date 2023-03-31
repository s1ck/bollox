use std::collections::HashMap;

use crate::{
    error::ResolverError,
    expr::{Expr, ExprNode},
    interp::InterpreterContext,
    node::Node,
    stmt::{ClassDeclaration, FunctionDeclaration, Stmt, StmtNode},
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
            Stmt::Func(declaration) => {
                Self::resolve_function(context, declaration, SectionType::Function)
            }
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
            Stmt::Return(Some(value)) => {
                if context.is_top_level() {
                    return Err(ResolverError::top_level_return(stmt.span));
                }
                if context.in_section(SectionType::Initializer) {
                    return Err(ResolverError::return_in_init(stmt.span));
                }
                Self::resolve_expr(context, value)?;
                Ok(())
            }
            Stmt::Return(None) => Ok(()),
            Stmt::Class(declaration) => Self::resolve_class(context, declaration),
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
                    .try_for_each(|arg| Self::resolve_expr(context, arg))?;
                Ok(())
            }
            Expr::Get { object, name: _ } => {
                Self::resolve_expr(context, object)?;
                Ok(())
            }
            Expr::Set {
                object,
                name: _,
                value,
            } => {
                Self::resolve_expr(context, value)?;
                Self::resolve_expr(context, object)?;
                Ok(())
            }
            Expr::This { keyword } => {
                if !context.in_section(SectionType::Class) {
                    return Err(ResolverError::this_outside_class(expr.span));
                }
                context.resolve_local(expr, keyword.item);
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
                Some(VarState::Defined) | None => {
                    context.resolve_local(expr, name);
                    Ok(())
                }
                _ => Err(ResolverError::undefined_variable(*name, expr.span)),
            },
            Expr::Assign { name, expr } => {
                Self::resolve_expr(context, expr)?;
                context.resolve_local(expr, name);
                Ok(())
            }
            Expr::Lambda { declaration } => {
                Self::resolve_function(context, declaration, SectionType::Lambda)
            }
        }
    }

    fn resolve_stmts<'a>(context: &mut ResolverContext<'a>, stmts: &[StmtNode<'a>]) -> Result<()> {
        stmts
            .iter()
            .try_for_each(|stmt| Self::resolve_stmt(context, stmt))
    }

    fn resolve_block<'a>(context: &mut ResolverContext<'a>, stmts: &[StmtNode<'a>]) -> Result<()> {
        context.begin_scope();
        Self::resolve_stmts(context, stmts)?;
        context.end_scope();
        Ok(())
    }

    fn resolve_class<'a>(
        context: &mut ResolverContext<'a>,
        class: &ClassDeclaration<'a>,
    ) -> Result<()> {
        context.declare(class.name.item);
        context.define(class.name.item);

        context.begin_section(SectionType::Class);
        context.begin_scope();

        context.declare("this");
        context.define("this");

        class.methods.iter().try_for_each(|method| {
            let section_type = match method.item.name.item {
                "init" => SectionType::Initializer,
                _ => SectionType::Method,
            };
            Self::resolve_function(context, &method.item, section_type)
        })?;

        context.end_scope();
        context.end_section();

        Ok(())
    }

    fn resolve_variable<'a>(
        context: &mut ResolverContext<'a>,
        name: &Node<&'a str>,
        initializer: &Option<ExprNode<'a>>,
    ) -> Result<()> {
        if context.declare(name.item).is_some() {
            return Err(ResolverError::redefined_variable(name.item, name.span));
        }
        if let Some(expr) = initializer {
            Self::resolve_expr(context, expr)?;
        }
        context.define(name.item);
        Ok(())
    }

    fn resolve_function<'a>(
        context: &mut ResolverContext<'a>,
        declaration: &FunctionDeclaration<'a>,
        function_type: SectionType,
    ) -> Result<()> {
        context.begin_section(function_type);
        // resolve function name first, to allow for recursive calls
        if context.declare(declaration.name.item).is_some() {
            return Err(ResolverError::redefined_function(
                declaration.name.item,
                declaration.name.span,
            ));
        }
        context.define(declaration.name.item);

        context.begin_scope();
        declaration.params.iter().for_each(|param| {
            context.declare(param.item);
            context.define(param.item);
        });
        Self::resolve_stmts(context, &declaration.body)?;
        context.end_scope();
        context.end_section();

        Ok(())
    }
}

pub(crate) struct ResolverContext<'a> {
    interpreter: InterpreterContext<'a>,
    // A scope maps variables within that scope to their state.
    // Variables are first declared and then defined. Those two
    // states are not necessarily entered in the same statement.
    scopes: Vec<HashMap<&'a str, VarState>>,
    // Tracks the call stack and in what kind of section we are in.
    // A section in Bollox is, e.g., a function, method or class.
    sections: Vec<SectionType>,
}

#[derive(Debug, Clone, Copy)]
enum VarState {
    Declared,
    Defined,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum SectionType {
    Function,
    Lambda,
    Method,
    Class,
    Initializer,
}

impl<'a> ResolverContext<'a> {
    fn new(interpreter: InterpreterContext<'a>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            sections: Vec::new(),
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

    fn declare(&mut self, name: &'a str) -> Option<VarState> {
        match self.scopes.last_mut() {
            Some(scope) => scope.insert(name, VarState::Declared),
            None => None,
        }
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

    fn is_top_level(&self) -> bool {
        self.sections.is_empty()
    }

    fn in_section(&self, section_type: SectionType) -> bool {
        self.sections.iter().any(|st| *st == section_type)
    }

    fn begin_section(&mut self, function_type: SectionType) {
        self.sections.push(function_type);
    }

    fn end_section(&mut self) {
        self.sections.pop();
    }
}
