#[test]
fn test_classes() -> bollox::BolloxResult {
    bollox::run(include_str!("classes.crox"))
}

#[test]
fn test_control_flow() -> bollox::BolloxResult {
    bollox::run(include_str!("control_flow.crox"))
}

#[test]
fn test_expressions() -> bollox::BolloxResult {
    bollox::run(include_str!("expressions.crox"))
}

#[test]
fn test_functions() -> bollox::BolloxResult {
    bollox::run(include_str!("functions.crox"))
}

#[test]
fn test_hello_world() -> bollox::BolloxResult {
    bollox::run(include_str!("hello_world.crox"))
}

#[test]
fn test_statements() -> bollox::BolloxResult {
    bollox::run(include_str!("statements.crox"))
}

#[test]
fn test_types() -> bollox::BolloxResult {
    bollox::run(include_str!("types.crox"))
}

#[test]
fn test_variables() -> bollox::BolloxResult {
    bollox::run(include_str!("variables.crox"))
}
