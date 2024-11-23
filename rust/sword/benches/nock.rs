extern crate criterion;
use criterion::{criterion_group, criterion_main, Criterion};
use sword::hamt::Hamt;
use sword::interpreter::{interpret, Context, Slogger};
use sword::jets::cold::Cold;
use sword::jets::hot::{Hot, URBIT_HOT_STATE};
use sword::jets::warm::Warm;
use sword::mem::{AllocResult, NockStack};
use sword::noun::{Noun, D, T};
use tracing::*;

struct BenchSlogger;

impl Slogger for BenchSlogger {
    fn slog(
        &mut self,
        stack: &mut NockStack,
        pri: u64,
        tank: sword::noun::Noun,
    ) -> AllocResult<()> {
        Ok(())
    }

    fn flog(&mut self, stack: &mut NockStack, cord: sword::noun::Noun) -> AllocResult<()> {
        Ok(())
    }
}

fn bench_dec(c: &mut Criterion) {
    const TO_DEC: Noun = D(1000000);
    let mut context = {
        let mut stack = NockStack::new(1 << 28, 0);
        let mut cold = Cold::new(&mut stack).unwrap();
        let mut hot = Hot::init(&mut stack, URBIT_HOT_STATE).unwrap();
        let warm = Warm::init(&mut stack, &mut cold, &mut hot).unwrap();
        let cache = Hamt::new(&mut stack).unwrap();
        let slogger = Box::pin(BenchSlogger);
        Context {
            stack,
            slogger,
            cold,
            hot,
            warm,
            cache,
            scry_stack: D(0),
            trace_info: None,
        }
    };
    // Decrement formula
    // [8 0 8 f 9 2 0 1]
    // input axis: 7
    // counter axis: 6
    // f: [6 [5 [0 7] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1]
    let dec_nock = {
        let stack = &mut context.stack;
        let const_0 = T(stack, &[D(1), D(0)]).unwrap();
        let axis1 = T(stack, &[D(0), D(1)]).unwrap();
        let axis6 = T(stack, &[D(0), D(6)]).unwrap();
        let axis7 = T(stack, &[D(0), D(7)]).unwrap();
        let inc6 = T(stack, &[D(4), axis6]).unwrap();
        let patch6 = T(stack, &[D(6), inc6]).unwrap();
        let recur = T(stack, &[D(9), D(2), D(10), patch6, axis1]).unwrap();
        let compare = T(stack, &[D(5), axis7, inc6]).unwrap();
        let cond = T(stack, &[D(1), D(6), compare, axis6, recur]).unwrap();
        let _easy_nock = T(stack, &[D(1), D(0), D(1)]).unwrap();
        T(stack, &[D(8), const_0, D(8), cond, D(9), D(2), D(0), D(1)]).unwrap()
    };

    c.bench_function("dec", |bencher| {
        bencher.iter(|| {
            let res = interpret(&mut context, TO_DEC, dec_nock);
            assert!(res.is_ok());
            assert!(unsafe { res.unwrap().raw_equals(D(999999)) });
        });
    });
}

criterion_group!(raw, bench_dec);
criterion_main!(raw);
