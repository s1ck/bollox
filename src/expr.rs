use std::{fmt::Display, rc::Rc};

use crate::{node::Node, stmt::FunctionKind, token::Span};

pub type ExprNode<'a> = Node<Box<Expr<'a>>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    Unary {
        op: UnaryOp,
        expr: ExprNode<'a>,
    },
    Binary {
        lhs: ExprNode<'a>,
        op: BinaryOp,
        rhs: ExprNode<'a>,
    },
    Call {
        kind: FunctionKind,
        callee: ExprNode<'a>,
        args: Rc<[ExprNode<'a>]>,
    },
    Logical {
        lhs: ExprNode<'a>,
        op: LogicalOp,
        rhs: ExprNode<'a>,
    },
    Group {
        expr: ExprNode<'a>,
    },
    Literal {
        lit: Literal<'a>,
    },
    Variable {
        name: &'a str,
    },
    Assign {
        name: &'a str,
        expr: ExprNode<'a>,
    },
}

impl<'a> FromIterator<ExprNode<'a>> for Option<ExprNode<'a>> {
    fn from_iter<T: IntoIterator<Item = ExprNode<'a>>>(iter: T) -> Self {
        iter.into_iter().next()
    }
}

impl<'a> Expr<'a> {
    pub fn at(self, span: impl Into<Span>) -> ExprNode<'a> {
        ExprNode::new(Box::new(self), span)
    }
}

impl<'a> Expr<'a> {
    pub fn unary(op: UnaryOp, expr: ExprNode<'a>) -> Self {
        Self::Unary { op, expr }
    }

    pub fn binary(lhs: ExprNode<'a>, op: BinaryOp, rhs: ExprNode<'a>) -> Self {
        Self::Binary { lhs, op, rhs }
    }

    pub fn logical(lhs: ExprNode<'a>, op: LogicalOp, rhs: ExprNode<'a>) -> Self {
        Self::Logical { lhs, op, rhs }
    }

    pub fn group(expr: ExprNode<'a>) -> Self {
        Self::Group { expr }
    }

    pub fn call(kind: FunctionKind, callee: ExprNode<'a>, args: Rc<[ExprNode<'a>]>) -> Self {
        Self::Call { kind, callee, args }
    }

    pub fn literal(lit: Literal<'a>) -> Self {
        Self::Literal { lit }
    }

    pub fn variable(name: &'a str) -> Self {
        Self::Variable { name }
    }

    pub fn assign(name: &'a str, expr: ExprNode<'a>) -> Self {
        Self::Assign { name, expr }
    }

    pub fn string(s: &'a str) -> Self {
        Self::Literal {
            lit: Literal::String(s),
        }
    }

    pub fn number(num: f64) -> Self {
        Self::Literal {
            lit: Literal::Number(num),
        }
    }

    pub fn tru() -> Self {
        Self::Literal { lit: Literal::True }
    }

    pub fn fals() -> Self {
        Self::Literal {
            lit: Literal::False,
        }
    }

    pub fn nil() -> Self {
        Self::Literal { lit: Literal::Nil }
    }

    pub fn neg(expr: ExprNode<'a>) -> Self {
        Self::Unary {
            op: UnaryOp::Neg,
            expr,
        }
    }

    pub fn not(expr: ExprNode<'a>) -> Self {
        Self::Unary {
            op: UnaryOp::Not,
            expr,
        }
    }

    pub fn equals(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Equals,
            rhs,
        }
    }

    pub fn not_equals(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::NotEquals,
            rhs,
        }
    }

    pub fn less_than(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::LessThan,
            rhs,
        }
    }

    pub fn less_than_or_equal(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::LessThanOrEqual,
            rhs,
        }
    }

    pub fn greater_than(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::GreaterThan,
            rhs,
        }
    }

    pub fn greater_than_or_equal(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::GreaterThanOrEqual,
            rhs,
        }
    }

    pub fn add(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Add,
            rhs,
        }
    }

    pub fn sub(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Sub,
            rhs,
        }
    }

    pub fn mul(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Mul,
            rhs,
        }
    }

    pub fn div(lhs: ExprNode<'a>, rhs: ExprNode<'a>) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Div,
            rhs,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Number(f64),
    True,
    False,
    Nil,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg => f.write_str("-"),
            Self::Not => f.write_str("!"),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equals => f.write_str("=="),
            Self::NotEquals => f.write_str("!="),
            Self::LessThan => f.write_str("<"),
            Self::LessThanOrEqual => f.write_str("<="),
            Self::GreaterThan => f.write_str(">"),
            Self::GreaterThanOrEqual => f.write_str(">="),
            Self::Add => f.write_str("+"),
            Self::Sub => f.write_str("-"),
            Self::Mul => f.write_str("*"),
            Self::Div => f.write_str("/"),
        }
    }
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOp::And => f.write_str("and"),
            LogicalOp::Or => f.write_str("or"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
}

pub trait Associates {
    fn associates(&self) -> Associativity;
}

impl Associates for UnaryOp {
    fn associates(&self) -> Associativity {
        Associativity::Right
    }
}

impl Associates for BinaryOp {
    fn associates(&self) -> Associativity {
        Associativity::Left
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrecedenceGroup {
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
}

pub trait Precedence {
    fn precedence(&self) -> PrecedenceGroup;
}

impl Precedence for UnaryOp {
    fn precedence(&self) -> PrecedenceGroup {
        PrecedenceGroup::Unary
    }
}

impl Precedence for BinaryOp {
    fn precedence(&self) -> PrecedenceGroup {
        match self {
            BinaryOp::Equals => PrecedenceGroup::Equality,
            BinaryOp::NotEquals => PrecedenceGroup::Equality,
            BinaryOp::LessThan => PrecedenceGroup::Comparison,
            BinaryOp::LessThanOrEqual => PrecedenceGroup::Comparison,
            BinaryOp::GreaterThan => PrecedenceGroup::Comparison,
            BinaryOp::GreaterThanOrEqual => PrecedenceGroup::Comparison,
            BinaryOp::Add => PrecedenceGroup::Term,
            BinaryOp::Sub => PrecedenceGroup::Term,
            BinaryOp::Mul => PrecedenceGroup::Factor,
            BinaryOp::Div => PrecedenceGroup::Factor,
        }
    }
}
