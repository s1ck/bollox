# bollox

A Rust port of an interpreter for the Lox language.
The implementation follows the great book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

## Run

Run REPL

```
cargo run --release
```

Run Lox file

```
cargo run --release -- /path/to/code.lox
```

Run benchmarks

```
cargo +nightly bench
```
