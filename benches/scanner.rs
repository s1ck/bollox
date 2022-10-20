#![feature(test)]

extern crate test;

use bollox::Source;

#[bench]
fn bench_scanner(b: &mut test::Bencher) {
    let input = include_str!("../tests/classes.crox");
    b.bytes = input.len() as u64;
    b.iter(|| {
        let _ = Source::new(input)
            .into_iter()
            .collect::<Result<Vec<_>, _>>();
    });
}
