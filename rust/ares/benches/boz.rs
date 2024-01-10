use ares::jets::bits::jet_rev;
use ares::jets::util::test::init_context;
use ares::noun::{D, T};
use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let ctx = &mut init_context();
    let test = 0x1234567890123u64;
    let sam = T(&mut ctx.stack, &[D(3), D(7), D(test)]);
    c.bench_function("boz 3", |b| b.iter(|| jet_rev(ctx, sam)));

    let a24 = D(0x876543);
    let sam = T(&mut ctx.stack, &[D(0), D(60), a24]);
    c.bench_function("boz 0", |b| b.iter(|| jet_rev(ctx, sam)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
