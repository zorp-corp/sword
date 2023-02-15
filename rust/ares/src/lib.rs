#[macro_use]
extern crate num_derive;
pub mod interpreter;
pub mod jets;
pub mod mem;
pub mod mug;
pub mod newt;
pub mod noun;
pub mod serf;
pub mod serialization;
pub mod snapshot;

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
}
