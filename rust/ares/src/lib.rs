extern crate num_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate static_assertions;
pub mod codegen;
pub mod hamt;
pub mod interpreter;
pub mod jets;
pub mod mem;
pub mod mug;
pub mod newt;
pub mod noun;
pub mod serf;
//pub mod bytecode;
pub mod load;
pub mod persist;
pub mod serialization;
pub mod trace;
pub mod unifying_equality;

/** Introduce useful functions for debugging
 *
 * The main difficulty with these is that rust wants to strip them out if they're not used in the
 * code.  Even if you get it past the compiler, the linker will get rid of them.  The solution here
 * is to call use_gdb() from main.rs on each module.  This is ugly, but I haven't found another way
 * that keeps these available in the debugger.
 *
 * Thus, every file that touches nouns should include `crate::gdb!();` at the top, and main.rs should
 * call use_gdb on that module.
 */
macro_rules! gdb {
    () => {
        fn pretty_noun(noun: crate::noun::Noun) -> String {
            noun.to_string()
        }

        pub fn use_gdb() {
            pretty_noun(crate::noun::D(0));
        }
    };
}

// Use the allocator from assert_no_alloc.
//
// DO NOT COMMENT THIS OUT
//
// if you need to allow allocations somewhere for debugging, wrap your debug code in
// ```
// permit_alloc( || {
//   your.code.goes.here()
// })
// ```
//
// (see https://docs.rs/assert_no_alloc/latest/assert_no_alloc/#advanced-use)
#[cfg(debug_assertions)]
#[global_allocator]
static A: assert_no_alloc::AllocDisabler = assert_no_alloc::AllocDisabler;

pub(crate) use gdb;

#[cfg(test)]
mod tests {

    #[test]
    fn tas() {
        use ares_macros::tas;
        assert_eq!(tas!(b"cut"), 0x747563);
        assert_eq!(tas!(b"dec"), 0x636564);
        assert_eq!(tas!(b"prop"), 0x706f7270);
    }

    #[test]
    fn test_jam() {
        use crate::mem::NockStack;
        use crate::noun::*;
        use crate::serialization::jam;
        let mut stack = NockStack::new(8 << 10 << 10, 0);
        let head = Atom::new(&mut stack, 0).as_noun();
        let tail = Atom::new(&mut stack, 1).as_noun();
        let cell = Cell::new(&mut stack, head, tail).as_noun();
        let res = jam(&mut stack, cell).as_direct().unwrap().data();
        assert_eq!(res, 201);
    }
}
