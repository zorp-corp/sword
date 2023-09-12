pub mod bits;
pub mod hash;
pub mod math;
pub mod nock;
pub mod tree;

use crate::jets::bits::*;
use crate::jets::hash::*;
use crate::jets::math::*;
use crate::jets::nock::*;
use crate::jets::tree::*;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{self, Noun, Slots};
use ares_macros::tas;
use std::cmp;

crate::gdb!();

/// Return Err if the computation crashed or should punt to Nock
pub type Result = std::result::Result<Noun, JetErr>;
pub type Jet = fn(&mut NockStack, &mut Option<&mut Newt>, Noun) -> Result;

/**
 * Only return a deterministic error if the Nock would have deterministically
 * crashed.
 */
#[derive(Debug, PartialEq)]
pub enum JetErr {
    Punt,             // Retry with the raw nock
    Deterministic,    // The Nock would have crashed
    NonDeterministic, // Other error
}

impl From<noun::Error> for JetErr {
    fn from(_err: noun::Error) -> Self {
        Self::Deterministic
    }
}

impl From<JetErr> for () {
    fn from(_: JetErr) -> Self {}
}

pub fn get_jet(jet_name: Noun) -> Option<Jet> {
    match jet_name.as_direct().ok()?.data() {
        tas!(b"add") => Some(jet_add),
        tas!(b"dec") => Some(jet_dec),
        tas!(b"div") => Some(jet_div),
        tas!(b"dvr") => Some(jet_dvr),
        tas!(b"gte") => Some(jet_gte),
        tas!(b"gth") => Some(jet_gth),
        tas!(b"lte") => Some(jet_lte),
        tas!(b"lth") => Some(jet_lth),
        tas!(b"mod") => Some(jet_mod),
        tas!(b"mul") => Some(jet_mul),
        tas!(b"sub") => Some(jet_sub),
        //
        tas!(b"cap") => Some(jet_cap),
        tas!(b"mas") => Some(jet_mas),
        //
        tas!(b"bex") => Some(jet_bex),
        tas!(b"can") => Some(jet_can),
        tas!(b"cat") => Some(jet_cat),
        tas!(b"cut") => Some(jet_cut),
        tas!(b"end") => Some(jet_end),
        tas!(b"lsh") => Some(jet_lsh),
        tas!(b"met") => Some(jet_met),
        tas!(b"rap") => Some(jet_rap),
        tas!(b"rep") => Some(jet_rep),
        tas!(b"rev") => Some(jet_rev),
        tas!(b"rip") => Some(jet_rip),
        tas!(b"rsh") => Some(jet_rsh),
        //
        tas!(b"con") => Some(jet_con),
        tas!(b"dis") => Some(jet_dis),
        tas!(b"mix") => Some(jet_mix),
        //
        tas!(b"mug") => Some(jet_mug),
        //
        tas!(b"mink") => Some(jet_mink),
        _ => {
            // eprintln!("Unknown jet: {:?}", jet_name);
            None
        }
    }
}

pub fn get_jet_test_mode(_jet_name: Noun) -> bool {
    /*
    match jet_name.as_direct().unwrap().data() {
        tas!(b"cut") => true,
        _ => false,
    }
    */
    false
}

pub mod util {
    use super::*;
    use crate::noun::Error::NotRepresentable;
    use crate::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun, D};
    use bitvec::prelude::{BitSlice, Lsb0};
    use ibig::UBig;
    use std::result;

    /**
     * Currently, only addresses indexable by the first 48 bits are reachable by
     * modern 64-bit CPUs.
     */
    const MAX_BIT_LENGTH: usize = (1 << 47) - 1;

    /// Performs addition that returns None on Noun size overflow
    pub fn checked_add(a: usize, b: usize) -> result::Result<usize, JetErr> {
        a.checked_add(b)
            .filter(|x| x <= &MAX_BIT_LENGTH)
            .ok_or(JetErr::NonDeterministic)
    }

    /// Performs addition that returns None on Noun size overflow
    pub fn checked_sub(a: usize, b: usize) -> result::Result<usize, JetErr> {
        a.checked_sub(b).ok_or(JetErr::NonDeterministic)
    }

    pub fn checked_left_shift(bloq: usize, a: usize) -> result::Result<usize, JetErr> {
        let res = a << bloq;

        // Catch overflow
        if (res >> bloq) < a || res > MAX_BIT_LENGTH {
            Err(JetErr::NonDeterministic)
        } else {
            Ok(res)
        }
    }

    /// Convert length in bits to length in 64-bit words
    pub fn bits_to_word(a: usize) -> result::Result<usize, JetErr> {
        checked_add(a, 63).map(|x| x >> 6)
    }

    /// Convert length as bite to length in 64-bit words
    pub fn bite_to_word(bloq: usize, step: usize) -> result::Result<usize, JetErr> {
        bits_to_word(checked_left_shift(bloq, step)?)
    }

    pub fn slot(noun: Noun, axis: u64) -> Result {
        noun.slot(axis).map_err(|_e| JetErr::Deterministic)
    }

    /// Extract a bloq and check that it's computable by the current system
    pub fn bloq(a: Noun) -> result::Result<usize, JetErr> {
        let bloq = a.as_direct()?.data() as usize;
        if bloq >= 47 {
            Err(JetErr::NonDeterministic)
        } else {
            Ok(bloq)
        }
    }

    /// Extract the bloq and step from a bite
    pub fn bite(a: Noun) -> result::Result<(usize, usize), JetErr> {
        if let Ok(cell) = a.as_cell() {
            let bloq = bloq(cell.head())?;
            let step = cell.tail().as_direct()?.data() as usize;
            Ok((bloq, step))
        } else {
            bloq(a).map(|x| (x, 1 as usize))
        }
    }

    /** In a bloq space, copy from `from` for a span of `step`, to position `to`.
     *
     * Note: unlike the vere version, this sets the bits instead of XORing
     * them.  If we need the XOR version, we could use ^=.
     */
    pub unsafe fn chop(
        bloq: usize,
        from: usize,
        step: usize,
        to: usize,
        dest: &mut BitSlice<u64, Lsb0>,
        source: &BitSlice<u64, Lsb0>,
    ) -> result::Result<(), JetErr> {
        let from_b = checked_left_shift(bloq, from)?;
        let to_b = checked_left_shift(bloq, to)?;
        let mut step_b = checked_left_shift(bloq, step)?;
        let end_b = checked_add(from_b, step_b)?;

        if from_b >= source.len() {
            return Ok(());
        }

        if end_b > source.len() {
            step_b -= end_b - source.len();
        }

        dest[to_b..to_b + step_b].copy_from_bitslice(&source[from_b..from_b + step_b]);
        Ok(())
    }

    /// Subtraction
    pub fn sub(stack: &mut NockStack, a: Atom, b: Atom) -> noun::Result<Atom> {
        if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
            let a_small = a.data();
            let b_small = b.data();

            if a_small < b_small {
                Err(NotRepresentable)
            } else {
                Ok(Atom::new(stack, a_small - b_small))
            }
        } else {
            let a_big = a.as_ubig(stack);
            let b_big = b.as_ubig(stack);

            if a_big < b_big {
                Err(NotRepresentable)
            } else {
                let a_big = a.as_ubig(stack);
                let b_big = b.as_ubig(stack);
                let res = UBig::sub_stack(stack, a_big, b_big);
                Ok(Atom::from_ubig(stack, &res))
            }
        }
    }

    /// Binary exponent
    pub fn bex(stack: &mut NockStack, arg: usize) -> Atom {
        unsafe {
            if arg < 63 {
                DirectAtom::new_unchecked(1 << arg).as_atom()
            } else {
                let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, (arg + 7) >> 3);
                dest.set(arg, true);
                atom.normalize_as_atom()
            }
        }
    }

    /// Measure the number of bloqs in an atom
    pub fn met(bloq: usize, a: Atom) -> usize {
        if unsafe { a.as_noun().raw_equals(D(0)) } {
            0
        } else if bloq < 6 {
            (a.bit_size() + ((1 << bloq) - 1)) >> bloq
        } else {
            let bloq_word = bloq - 6;
            (a.size() + ((1 << bloq_word) - 1)) >> bloq_word
        }
    }

    pub fn rip(stack: &mut NockStack, bloq: usize, step: usize, atom: Atom) -> Result {
        let len = (met(bloq, atom) + step - 1) / step;
        let mut list = D(0);
        for i in (0..len).rev() {
            let new_atom = unsafe {
                let (mut new_indirect, new_slice) =
                    IndirectAtom::new_raw_mut_bitslice(stack, step << bloq);
                chop(bloq, i * step, step, 0, new_slice, atom.as_bitslice())?;
                new_indirect.normalize_as_atom()
            };
            list = Cell::new(stack, new_atom.as_noun(), list).as_noun();
        }

        Ok(list)
    }

    /// Binary OR
    pub fn con(stack: &mut NockStack, a: Atom, b: Atom) -> Atom {
        let new_size = cmp::max(a.size(), b.size());

        unsafe {
            let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
            let a_bit = a.as_bitslice();
            dest[..a_bit.len()].copy_from_bitslice(a_bit);
            *dest |= b.as_bitslice();
            atom.normalize_as_atom()
        }
    }

    pub mod test {
        use super::*;
        use crate::mem::{unifying_equality, NockStack};
        use crate::noun::{Atom, Noun, D, T};
        use assert_no_alloc::assert_no_alloc;
        use ibig::UBig;

        pub fn init_stack() -> NockStack {
            NockStack::new(8 << 10 << 10, 0)
        }

        #[allow(non_snake_case)]
        pub fn A(stack: &mut NockStack, ubig: &UBig) -> Noun {
            Atom::from_ubig(stack, ubig).as_noun()
        }

        pub fn assert_noun_eq(stack: &mut NockStack, mut a: Noun, mut b: Noun) {
            let eq = unsafe { unifying_equality(stack, &mut a, &mut b) };
            assert!(eq, "got: {}, need: {}", a, b);
        }

        pub fn assert_jet(stack: &mut NockStack, jet: Jet, sam: Noun, res: Noun) {
            let sam = T(stack, &[D(0), sam, D(0)]);
            let jet_res = assert_no_alloc(|| jet(stack, &mut None, sam).unwrap());
            assert_noun_eq(stack, jet_res, res);
        }

        pub fn assert_jet_ubig(stack: &mut NockStack, jet: Jet, sam: Noun, res: UBig) {
            let res = A(stack, &res);
            assert_jet(stack, jet, sam, res);
        }

        pub fn assert_nary_jet_ubig(stack: &mut NockStack, jet: Jet, sam: &[Noun], res: UBig) {
            let sam = T(stack, sam);
            assert_jet_ubig(stack, jet, sam, res);
        }

        pub fn assert_jet_err(stack: &mut NockStack, jet: Jet, sam: Noun, err: JetErr) {
            let sam = T(stack, &[D(0), sam, D(0)]);
            let jet_res = jet(stack, &mut None, sam);
            assert!(
                jet_res.is_err(),
                "with sample: {}, expected err: {:?}, got: {:?}",
                sam,
                err,
                &jet_res
            );
            let jet_err = jet_res.unwrap_err();
            assert_eq!(
                jet_err, err,
                "with sample: {}, expected err: {:?}, got: {:?}",
                sam, err, jet_err
            );
        }
    }

    #[cfg(test)]
    mod tests {
        use super::test::{init_stack, A};
        use super::*;
        use ibig::ubig;

        #[test]
        fn test_met() {
            let s = &mut init_stack();

            let a = A(s, &ubig!(0xdeadbeef12345678fedcba9876543210))
                .as_atom()
                .unwrap();
            assert_eq!(met(0, a), 128);
            assert_eq!(met(1, a), 64);
            assert_eq!(met(2, a), 32);
            assert_eq!(met(3, a), 16);
            assert_eq!(met(4, a), 8);
            assert_eq!(met(5, a), 4);
            assert_eq!(met(6, a), 2);
            assert_eq!(met(7, a), 1);
            assert_eq!(met(8, a), 1);

            let a = D(0x7fffffffffffffff).as_atom().unwrap();
            assert_eq!(met(0, a), 63);
            assert_eq!(met(1, a), 32);
            assert_eq!(met(2, a), 16);
            assert_eq!(met(3, a), 8);
            assert_eq!(met(4, a), 4);
            assert_eq!(met(5, a), 2);
            assert_eq!(met(6, a), 1);
            assert_eq!(met(7, a), 1);
        }
    }
}
