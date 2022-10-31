use bollox::token::TokenType::*;
use bollox::{token::Token, Source};

fn run(source: &str, expected: Vec<Token>) {
    let source = Source::new(source);
    let actual = source.scan_all().unwrap();

    // for (a, e) in actual.iter().zip(expected.iter()) {
    //     assert_eq!(a, e);
    // }

    assert_eq!(actual, expected);
}

#[test]
fn test_classes() {
    run(include_str!("classes.bollox"), include!("classes.tokens"));
}

#[test]
fn test_control_flow() {
    run(
        include_str!("control_flow.bollox"),
        include!("control_flow.tokens"),
    );
}

#[test]
fn test_expressions() {
    run(
        include_str!("expressions.bollox"),
        include!("expressions.tokens"),
    );
}

#[test]
fn test_functions() {
    run(
        include_str!("functions.bollox"),
        include!("functions.tokens"),
    );
}

#[test]
fn test_hello_world() {
    run(
        include_str!("hello_world.bollox"),
        include!("hello_world.tokens"),
    );
}

#[test]
fn test_statements() {
    run(
        include_str!("statements.bollox"),
        include!("statements.tokens"),
    );
}

#[test]
fn test_types() {
    run(include_str!("types.bollox"), include!("types.tokens"));
}

#[test]
fn test_variables() {
    run(
        include_str!("variables.bollox"),
        include!("variables.tokens"),
    );
}
