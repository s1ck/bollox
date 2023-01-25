use crate::{expr::ExprNode, node::Node, token::Span};

pub type StmtNode<'a> = Node<Box<Stmt<'a>>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'a> {
    Block(Vec<StmtNode<'a>>),
    Expression(ExprNode<'a>),
    Print(ExprNode<'a>),
    Var(Node<&'a str>, Option<ExprNode<'a>>),
    Function(FunctionDeclaration<'a>),
    If(ExprNode<'a>, StmtNode<'a>, Option<StmtNode<'a>>),
    While(ExprNode<'a>, StmtNode<'a>),
}

impl<'a> Stmt<'a> {
    pub fn at(self, span: impl Into<Span>) -> StmtNode<'a> {
        StmtNode::new(Box::new(self), span)
    }
}

impl<'a> FromIterator<StmtNode<'a>> for Option<StmtNode<'a>> {
    fn from_iter<T: IntoIterator<Item = StmtNode<'a>>>(iter: T) -> Self {
        iter.into_iter().next()
    }
}

impl<'a> Stmt<'a> {
    pub fn block(stmts: impl Into<Vec<StmtNode<'a>>>) -> Self {
        Self::Block(stmts.into())
    }

    pub fn expression(expr: ExprNode<'a>) -> Self {
        Self::Expression(expr)
    }

    pub fn print(expr: ExprNode<'a>) -> Self {
        Self::Print(expr)
    }

    pub fn var(name: Node<&'a str>, init_expr: Option<ExprNode<'a>>) -> Self {
        Self::Var(name, init_expr)
    }

    pub fn if_(condition: ExprNode<'a>, then_: StmtNode<'a>) -> Self {
        Self::If(condition, then_, None)
    }

    pub fn if_else(condition: ExprNode<'a>, then_: StmtNode<'a>, else_: StmtNode<'a>) -> Self {
        Self::If(condition, then_, Some(else_))
    }

    pub fn while_(condition: ExprNode<'a>, stmt: StmtNode<'a>) -> Self {
        Self::While(condition, stmt)
    }

    pub fn fun(
        name: Node<&'a str>,
        kind: FunctionKind,
        params: Vec<Node<&'a str>>,
        body: StmtNode<'a>,
    ) -> Self {
        Self::Function(FunctionDeclaration {
            name,
            kind,
            params,
            body,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionKind {
    Function,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclaration<'a> {
    name: Node<&'a str>,
    kind: FunctionKind,
    params: Vec<Node<&'a str>>,
    body: StmtNode<'a>,
}
