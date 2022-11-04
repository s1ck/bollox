use std::fmt::Display;

use crate::token::{Range, Span};

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'a> {
    node: Box<Node<'a>>,
    span: Span,
}

impl<'a> Expr<'a> {
    pub fn new(node: Box<Node<'a>>, span: Span) -> Self {
        Self { node, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node<'a, T: Sized = Expr<'a>> {
    Unary { op: UnaryOp, expr: T },
    Binary { lhs: T, op: BinaryOp, rhs: T },
    Group { expr: T },
    Literal { lit: Literal<'a> },
}

impl<'a> Node<'a> {
    pub fn into_expr(self, range: Range) -> Expr<'a> {
        Expr {
            node: Box::new(self),
            span: Span::from(range),
        }
    }
}

impl<'a, T: Sized> Node<'a, T> {
    pub fn unary(op: UnaryOp, expr: T) -> Self {
        Self::Unary { op, expr }
    }

    pub fn binary(lhs: T, op: BinaryOp, rhs: T) -> Self {
        Self::Binary { lhs, op, rhs }
    }

    pub fn group(expr: T) -> Self {
        Self::Group { expr }
    }

    pub fn literal(lit: Literal<'a>) -> Self {
        Self::Literal { lit }
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

    pub fn neg(expr: T) -> Self {
        Self::Unary {
            op: UnaryOp::Neg,
            expr,
        }
    }

    pub fn not(expr: T) -> Self {
        Self::Unary {
            op: UnaryOp::Not,
            expr,
        }
    }

    pub fn equals(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Equals,
            rhs,
        }
    }

    pub fn not_equals(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::NotEquals,
            rhs,
        }
    }

    pub fn less_than(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::LessThan,
            rhs,
        }
    }

    pub fn less_than_or_equal(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::LessThanOrEqual,
            rhs,
        }
    }

    pub fn greater_than(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::GreaterThan,
            rhs,
        }
    }

    pub fn greater_than_or_equal(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::GreaterThanOrEqual,
            rhs,
        }
    }

    pub fn add(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Add,
            rhs,
        }
    }

    pub fn sub(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Sub,
            rhs,
        }
    }

    pub fn mul(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Mul,
            rhs,
        }
    }

    pub fn div(lhs: T, rhs: T) -> Self {
        Self::Binary {
            lhs,
            op: BinaryOp::Div,
            rhs,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Copy, Clone, Debug, PartialEq)]
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

#[cfg(test)]
fn print(expr: Expr, source: &str) -> String {
    fn visit(expr: Expr, source: &str, res: &mut String) {
        match *expr.node {
            Node::Unary { op, expr } => parenthesize(source, op, Some(expr), res),
            Node::Binary { lhs, op, rhs } => parenthesize(source, op, [lhs, rhs], res),
            Node::Group { expr } => parenthesize(source, "group", Some(expr), res),
            Node::Literal { lit: _ } => res.push_str(&source[Range::from(expr.span)]),
        }
    }

    fn parenthesize<'a>(
        source: &'a str,
        name: impl Display,
        exprs: impl IntoIterator<Item = Expr<'a>>,
        res: &mut String,
    ) {
        use std::fmt::Write;

        res.push('(');
        write!(res, "{name}").unwrap();

        for expr in exprs {
            res.push(' ');
            visit(expr, source, res);
        }

        res.push(')');
    }

    let mut res = String::new();
    visit(expr, source, &mut res);
    res
}

// Challenge 5.3: Reverse Polish Notation
#[cfg(test)]
fn print_rpn(expr: Expr, source: &str) -> String {
    fn visit(expr: Expr, source: &str, res: &mut Vec<String>) {
        match *expr.node {
            Node::Unary { op, expr } => {
                res.push(op.to_string());
                visit(expr, source, res);
            }
            Node::Binary { lhs, op, rhs } => {
                visit(lhs, source, res);
                visit(rhs, source, res);
                res.push(op.to_string());
            }
            Node::Group { expr } => visit(expr, source, res),
            Node::Literal { lit: _ } => res.push((&source[Range::from(expr.span)]).to_string()),
        }
    }

    let mut res = Vec::new();
    visit(expr, source, &mut res);
    res.join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print() {
        let input = "-123 * (45.67)";

        let num = Node::number(123_f64).into_expr(1..4);
        let neg = Node::neg(num).into_expr(0..4);
        let num = Node::number(45.67).into_expr(8..13);
        let grp = Node::group(num).into_expr(7..14);
        let ast = Node::mul(neg, grp).into_expr(0..14);

        assert_eq!(print(ast, input), "(* (- 123) (group 45.67))");
    }

    #[test]
    fn test_print_rpn() {
        let input = "(1 + 2) * (4 - 3)";

        let num1 = Node::number(1_f64).into_expr(1..2);
        let num2 = Node::number(2_f64).into_expr(5..6);
        let add = Node::add(num1, num2).into_expr(1..6);
        let grp0 = Node::group(add).into_expr(0..7);

        let num1 = Node::number(4_f64).into_expr(11..12);
        let num2 = Node::number(3_f64).into_expr(15..16);
        let sub = Node::sub(num1, num2).into_expr(11..16);
        let grp1 = Node::group(sub).into_expr(10..17);

        let ast = Node::mul(grp0, grp1).into_expr(0..17);

        assert_eq!(print_rpn(ast, input), "1 2 + 4 3 - *");
    }
}
