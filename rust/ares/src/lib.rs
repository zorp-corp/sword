#[macro_use]
extern crate num_derive;
#[macro_use]
extern crate static_assertions;
pub mod interpreter;
pub mod jets;
pub mod mem;
pub mod mug;
pub mod newt;
pub mod noun;
pub mod serf;
//pub mod bytecode;
pub mod serialization;
pub mod snapshot;
pub mod hamt;

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
