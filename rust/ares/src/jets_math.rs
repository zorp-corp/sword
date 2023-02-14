use crate::interpreter::raw_slot;
use crate::jets::{JetErr, JetErr::*};
use crate::mem::NockStack;
use crate::noun::{Atom, DirectAtom, IndirectAtom, Noun, D, DIRECT_MAX, NO, T, YES};
/** Math jets
 *
 * We use ibig for math operations.  This is a pure rust library, and it is very convenient to use.
 * If they're noticeably, fater, we may want to use gmp or a library wrapping it, such as rug.
 *
 * In any case, it's important to ensure that the library only allocates on the nock stack.  Gmp
 * has mp_set_memory_functions.  I don't know if rug does any allocation on top of that.  ibig does
 * not appear to support custom allocation functions, but we could probably patch it.  If we're
 * patching it, we might even be able to avoid copying the input and output at all, which might
 * give a greater performance advantage than using gmp anyway.
 *
 * Another approach is use a global custom allocator.  This is fairly involved, but it would allow
 * us to use any library without worrying whether it allocates.
 */
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::*;
use ibig::ops::DivRem;
use ibig::UBig;
use num_traits::identities::One;

pub fn jet_dec(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    if let Ok(atom) = arg.as_atom() {
        match atom.as_either() {
            Left(direct) => {
                if direct.data() == 0 {
                    Err(Deterministic)
                } else {
                    Ok(unsafe { DirectAtom::new_unchecked(direct.data() - 1) }.as_noun())
                }
            }
            Right(indirect) => {
                let indirect_slice = indirect.as_bitslice();
                match indirect_slice.first_one() {
                    None => {
                        panic!("Decrementing 0 stored as an indirect atom");
                    }
                    Some(first_one) => {
                        let (mut new_indirect, new_slice) =
                            unsafe { IndirectAtom::new_raw_mut_bitslice(stack, indirect.size()) };
                        if first_one > 0 {
                            new_slice[..first_one].fill(true);
                        }
                        new_slice.set(first_one, false);
                        new_slice[first_one + 1..]
                            .copy_from_bitslice(&indirect_slice[first_one + 1..]);
                        let res = unsafe { new_indirect.normalize_as_atom() };
                        Ok(res.as_noun())
                    }
                }
            }
        }
    } else {
        Err(Deterministic)
    }
}

pub fn jet_cut(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let bloq = raw_slot(arg, 2).as_direct()?.data() as usize;
    if bloq >= 64 {
        return Err(Deterministic);
    }
    let start = raw_slot(arg, 12).as_direct()?.data() as usize;
    let run = raw_slot(arg, 13).as_direct()?.data() as usize;
    let atom = raw_slot(arg, 7).as_atom()?;

    let new_indirect = unsafe {
        let (mut new_indirect, new_slice) =
            IndirectAtom::new_raw_mut_bitslice(stack, ((run << bloq) + 63) >> 6);
        chop(bloq, start, run, 0, new_slice, atom.as_bitslice())?;
        new_indirect.normalize_as_atom()
    };
    Ok(new_indirect.as_noun())
}

pub fn jet_add(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        Ok(Atom::new(stack, a.data() + b.data()).as_noun())
    } else {
        let res = a.as_ubig() + b.as_ubig();
        Ok(Atom::from_ubig(stack, &res).as_noun())
    }
}

pub fn jet_sub(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() < b.data() {
            Err(Deterministic)
        } else {
            Ok(unsafe { DirectAtom::new_unchecked(a.data() - b.data()) }.as_noun())
        }
    } else {
        let a_int = a.as_ubig();
        let b_int = b.as_ubig();
        if a_int < b_int {
            Err(Deterministic)
        } else {
            let res = a_int - b_int;
            Ok(Atom::from_ubig(stack, &res).as_noun())
        }
    }
}

pub fn jet_mul(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        let res = a.data() as u128 * b.data() as u128;
        if res < DIRECT_MAX as u128 {
            Ok(Atom::new(stack, res as u64).as_noun())
        } else {
            Ok(unsafe {
                IndirectAtom::new_raw_bytes(
                    stack,
                    if res < u64::MAX as u128 { 8 } else { 16 },
                    &res.to_le_bytes() as *const u8,
                )
            }
            .as_noun())
        }
    } else {
        let res = a.as_ubig() * b.as_ubig();
        Ok(Atom::from_ubig(stack, &res).as_noun())
    }
}

pub fn jet_div(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    if unsafe { b.as_noun().raw_equals(D(0)) } {
        Err(Deterministic)
    } else {
        if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
            Ok(unsafe { DirectAtom::new_unchecked(a.data() / b.data()) }.as_noun())
        } else {
            let res = a.as_ubig() / b.as_ubig();
            Ok(Atom::from_ubig(stack, &res).as_noun())
        }
    }
}

pub fn jet_mod(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    if unsafe { b.as_noun().raw_equals(D(0)) } {
        Err(Deterministic)
    } else {
        if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
            Ok(unsafe { DirectAtom::new_unchecked(a.data() % b.data()) }.as_noun())
        } else {
            let res = a.as_ubig() % b.as_ubig();
            Ok(Atom::from_ubig(stack, &res).as_noun())
        }
    }
}

pub fn jet_dvr(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    if unsafe { b.as_noun().raw_equals(D(0)) } {
        Err(Deterministic)
    } else {
        let (div, rem) = if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
            let (div, rem) = (a.data() / b.data(), a.data() % b.data());
            unsafe {
                (
                    DirectAtom::new_unchecked(div).as_noun(),
                    DirectAtom::new_unchecked(rem).as_noun(),
                )
            }
        } else {
            let (div, rem) = a.as_ubig().div_rem(&b.as_ubig());
            (
                Atom::from_ubig(stack, &div).as_noun(),
                Atom::from_ubig(stack, &rem).as_noun(),
            )
        };

        Ok(T(stack, &[div, rem]))
    }
}

pub fn jet_lth(_stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() < b.data() {
            YES
        } else {
            NO
        }
    } else {
        if a.bit_size() < b.bit_size() {
            YES
        } else if a.bit_size() > b.bit_size() {
            NO
        } else {
            if a.as_ubig() < b.as_ubig() {
                YES
            } else {
                NO
            }
        }
    })
}

pub fn jet_lte(_stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() <= b.data() {
            YES
        } else {
            NO
        }
    } else {
        if a.bit_size() < b.bit_size() {
            YES
        } else if a.bit_size() > b.bit_size() {
            NO
        } else {
            if a.as_ubig() <= b.as_ubig() {
                YES
            } else {
                NO
            }
        }
    })
}

pub fn jet_gth(_stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() > b.data() {
            YES
        } else {
            NO
        }
    } else {
        if a.bit_size() > b.bit_size() {
            YES
        } else if a.bit_size() < b.bit_size() {
            NO
        } else {
            if a.as_ubig() > b.as_ubig() {
                YES
            } else {
                NO
            }
        }
    })
}

pub fn jet_gte(_stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let a = raw_slot(arg, 2).as_atom()?;
    let b = raw_slot(arg, 3).as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() >= b.data() {
            YES
        } else {
            NO
        }
    } else {
        if a.bit_size() > b.bit_size() {
            YES
        } else if a.bit_size() < b.bit_size() {
            NO
        } else {
            if a.as_ubig() >= b.as_ubig() {
                YES
            } else {
                NO
            }
        }
    })
}

pub fn jet_bex(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6).as_direct()?.data();

    if arg < 63 {
        Ok(unsafe { DirectAtom::new_unchecked(1 << arg) }.as_noun())
    } else {
        let mut res = UBig::one();
        res <<= arg as usize;
        Ok(Atom::from_ubig(stack, &res).as_noun())
    }
}

pub fn jet_lsh(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let (bloq, step) = bite(raw_slot(arg, 2))?;
    let bloq = bloq.as_direct()?.data() as usize;
    if bloq >= 64 {
        return Err(Deterministic);
    }
    let step = step.as_direct()?.data() as usize;
    let a = raw_slot(arg, 3).as_atom()?;

    // TODO: need to assert step << bloq doesn't overflow?
    let len = met(bloq, a);
    let new_size = (a
        .bit_size()
        .checked_add(step << bloq)
        .ok_or(NonDeterministic)?
        .checked_add(63)
        .ok_or(NonDeterministic)?)
        >> 6;

    if unsafe { a.as_noun().raw_equals(D(0)) } {
        Ok(D(0))
    } else {
        unsafe {
            let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
            chop(bloq, 0, len, step, dest, a.as_bitslice())?;
            Ok(atom.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_rsh(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let (bloq, step) = bite(raw_slot(arg, 2))?;
    let bloq = bloq.as_direct()?.data() as usize;
    if bloq >= 64 {
        return Err(Deterministic);
    }
    let step = step.as_direct()?.data() as usize;
    let a = raw_slot(arg, 3).as_atom()?;

    let len = met(bloq, a);
    if step >= len {
        return Ok(D(0));
    }

    // TODO: need to assert step << bloq doesn't overflow?
    let new_size = (a
        .bit_size()
        .checked_sub(step << bloq)
        .ok_or(NonDeterministic)?
        .checked_add(63)
        .ok_or(NonDeterministic)?)
        >> 6;

    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
        chop(bloq, step, len - step, 0, dest, a.as_bitslice())?;
        Ok(atom.normalize_as_atom().as_noun())
    }
}

/** Extract the bloq and step from a bite */
fn bite(a: Noun) -> Result<(Atom, Atom), ()> {
    if let Ok(cell) = a.as_cell() {
        Ok((cell.head().as_atom()?, cell.tail().as_atom()?))
    } else {
        Ok((a.as_atom()?, D(1).as_atom()?))
    }
}

/** In a bloq space, copy from `from` for a span of `step`, to position `to`.
 *
 * Note: unlike the vere version, this sets the bits instead of XORing them.
 */
fn chop(
    bloq: usize,
    from: usize,
    step: usize,
    to: usize,
    dest: &mut BitSlice<u64, Lsb0>,
    source: &BitSlice<u64, Lsb0>,
) -> Result<(), ()> {
    let from_b = from << bloq;
    let to_b = to << bloq;
    let mut step_b = step << bloq;
    let end_b = from_b.checked_add(step_b).ok_or(())?;

    if (from_b >> bloq) != from {
        return Err(());
    }

    if from_b >= source.len() {
        return Ok(());
    }

    if end_b > source.len() {
        step_b -= end_b - source.len();
    }

    dest[to_b..to_b + step_b].copy_from_bitslice(&source[from_b..from_b + step_b]);
    Ok(())
}

/** Measure the number of bloqs in an atom */
pub fn met(bloq: usize, a: Atom) -> usize {
    if unsafe { a.as_noun().raw_equals(D(0)) } {
        0
    } else {
        if bloq < 6 {
            (a.bit_size() + ((1 << bloq) - 1)) >> bloq
        } else {
            let bloq_word = bloq - 6;
            (a.size() + ((1 << bloq_word) - 1)) >> bloq_word
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::Jet;
    use crate::mem::unifying_equality;
    use crate::noun::Atom;
    use ibig::ubig;

    fn init() -> NockStack {
        NockStack::new(8 << 10 << 10, 0)
    }

    fn atoms(s: &mut NockStack) -> (Noun, Noun, Noun, Noun, Noun) {
        (atom_0(s), atom_24(s), atom_63(s), atom_96(s), atom_128(s))
    }

    fn atom_0(_stack: &mut NockStack) -> Noun {
        D(0)
    }

    fn atom_24(_stack: &mut NockStack) -> Noun {
        D(0x876543)
    }

    fn atom_63(_stack: &mut NockStack) -> Noun {
        D(0x7fffffffffffffff)
    }

    fn atom_96(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xfaceb00c15deadbeef123456))
    }

    fn atom_128(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xdeadbeef12345678fedcba9876543210))
    }

    fn atom_128_b(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xdeadbeef12345678fedcba9876540000))
    }

    #[allow(non_snake_case)]
    fn A(stack: &mut NockStack, ubig: &UBig) -> Noun {
        Atom::from_ubig(stack, &ubig).as_noun()
    }

    fn assert_noun_eq(stack: &mut NockStack, mut a: Noun, mut b: Noun) {
        let eq = unsafe { unifying_equality(stack, &mut a, &mut b) };
        assert!(eq, "got: {:?}, need: {:?}", a, b);
    }

    fn assert_jet(stack: &mut NockStack, jet: Jet, sam: Noun, res: Noun) {
        let sam = T(stack, &[D(0), sam, D(0)]);
        let jet_res = jet(stack, sam).unwrap();
        assert_noun_eq(stack, jet_res, res);
    }

    fn assert_jet_ubig(stack: &mut NockStack, jet: Jet, sam: Noun, res: UBig) {
        let res = A(stack, &res);
        assert_jet(stack, jet, sam, res);
    }

    fn assert_nary_jet_ubig(stack: &mut NockStack, jet: Jet, sam: &[Noun], res: UBig) {
        let sam = T(stack, sam);
        assert_jet_ubig(stack, jet, sam, res);
    }

    fn assert_math_jet(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        res: UBig,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        assert_nary_jet_ubig(stack, jet, &sam, res);
    }

    fn assert_math_jet_noun(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        res: Noun,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        let sam = T(stack, &sam);
        assert_jet(stack, jet, sam, res);
    }

    fn assert_jet_err(stack: &mut NockStack, jet: Jet, sam: Noun, err: JetErr) {
        let sam = T(stack, &[D(0), sam, D(0)]);
        let jet_res = jet(stack, sam);
        assert!(
            jet_res.is_err(),
            "with sample: {:?}, expected err: {:?}, got: {:?}",
            sam,
            err,
            &jet_res
        );
        let jet_err = jet_res.unwrap_err();
        assert_eq!(
            jet_err, err,
            "with sample: {:?}, expected err: {:?}, got: {:?}",
            sam, err, jet_err
        );
    }

    fn assert_math_jet_err(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        err: JetErr,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        let sam = T(stack, &sam);
        assert_jet_err(stack, jet, sam, err);
    }

    #[test]
    fn test_met() {
        let ref mut s = init();
        let a = atom_128(s).as_atom().unwrap();
        assert_eq!(met(0, a), 128);
        assert_eq!(met(1, a), 64);
        assert_eq!(met(2, a), 32);
        assert_eq!(met(3, a), 16);
        assert_eq!(met(4, a), 8);
        assert_eq!(met(5, a), 4);
        assert_eq!(met(6, a), 2);
        assert_eq!(met(7, a), 1);
        assert_eq!(met(8, a), 1);

        let a = atom_63(s).as_atom().unwrap();
        assert_eq!(met(0, a), 63);
        assert_eq!(met(1, a), 32);
        assert_eq!(met(2, a), 16);
        assert_eq!(met(3, a), 8);
        assert_eq!(met(4, a), 4);
        assert_eq!(met(5, a), 2);
        assert_eq!(met(6, a), 1);
        assert_eq!(met(7, a), 1);
    }

    #[test]
    fn test_dec() {
        let ref mut s = init();
        let a = atom_128(s);
        assert_jet_ubig(s, jet_dec, a, ubig!(0xdeadbeef12345678fedcba987654320f));
        let a = atom_63(s);
        assert_jet(s, jet_dec, a, D(0x7ffffffffffffffe));
    }

    #[test]
    fn test_add() {
        let ref mut s = init();
        assert_math_jet(
            s,
            jet_add,
            &[atom_128, atom_96],
            ubig!(0xdeadbef00d03068514bb685765666666),
        );
        assert_math_jet(
            s,
            jet_add,
            &[atom_63, atom_96],
            ubig!(0xfaceb00c95deadbeef123455),
        );
        assert_math_jet(s, jet_add, &[atom_63, atom_63], ubig!(0xfffffffffffffffe));
    }

    #[test]
    fn test_sub() {
        let ref mut s = init();
        assert_math_jet(
            s,
            jet_sub,
            &[atom_128, atom_96],
            ubig!(0xdeadbeee1765a66ce8fe0cd98741fdba),
        );
        assert_math_jet(
            s,
            jet_sub,
            &[atom_96, atom_63],
            ubig!(0xfaceb00b95deadbeef123457),
        );
        assert_math_jet(s, jet_sub, &[atom_63, atom_63], ubig!(0));
        assert_math_jet(s, jet_sub, &[atom_128, atom_128], ubig!(0));
        assert_math_jet_err(s, jet_sub, &[atom_63, atom_96], Deterministic);
    }

    #[test]
    fn test_mul() {
        let ref mut s = init();
        assert_math_jet(
            s,
            jet_mul,
            &[atom_128, atom_96],
            ubig!(_0xda297567129704bf42e744f13ff0ea4fc4ac01215b708bc94f941160),
        );
        assert_math_jet(
            s,
            jet_mul,
            &[atom_63, atom_96],
            ubig!(_0x7d6758060aef56de7cba6a1eea21524110edcbaa),
        );
        assert_math_jet(
            s,
            jet_mul,
            &[atom_63, atom_63],
            ubig!(0x3fffffffffffffff0000000000000001),
        );
        assert_math_jet(s, jet_mul, &[atom_24, atom_24], ubig!(0x479bf4b7ef89));
    }

    #[test]
    fn test_div() {
        let ref mut s = init();
        assert_math_jet(s, jet_div, &[atom_128, atom_96], ubig!(0xe349f8f0));
        assert_math_jet(s, jet_div, &[atom_96, atom_63], ubig!(0x1f59d6018));
        assert_math_jet(s, jet_div, &[atom_63, atom_96], ubig!(0));
        assert_math_jet(s, jet_div, &[atom_63, atom_63], ubig!(1));
        assert_math_jet(s, jet_div, &[atom_63, atom_24], ubig!(0xf2044dacfe));
        assert_math_jet(
            s,
            jet_div,
            &[atom_128, atom_24],
            ubig!(0x1a507f98b6fa8605ea3a79e97bf),
        );
        assert_math_jet_err(s, jet_div, &[atom_63, atom_0], Deterministic);
        assert_math_jet_err(s, jet_div, &[atom_0, atom_0], Deterministic);
    }

    #[test]
    fn test_mod() {
        let ref mut s = init();
        assert_math_jet(
            s,
            jet_mod,
            &[atom_128, atom_96],
            ubig!(0xcb0ce564ec598f658409d170),
        );
        assert_math_jet(s, jet_mod, &[atom_96, atom_63], ubig!(0x15deadc0e4af946e));
        assert_math_jet(s, jet_mod, &[atom_63, atom_96], ubig!(0x7fffffffffffffff));
        assert_math_jet(s, jet_mod, &[atom_63, atom_63], ubig!(0));
        assert_math_jet(s, jet_mod, &[atom_63, atom_24], ubig!(0x798385));
        assert_math_jet(s, jet_mod, &[atom_128, atom_24], ubig!(0x3b2013));
        assert_math_jet_err(s, jet_mod, &[atom_63, atom_0], Deterministic);
        assert_math_jet_err(s, jet_mod, &[atom_0, atom_0], Deterministic);
    }

    #[test]
    fn test_dvr() {
        let ref mut s = init();
        let (a0, a24, a63, a96, a128) = atoms(s);

        let sam = T(s, &[a128, a96]);
        let res_a = A(s, &ubig!(0xe349f8f0));
        let res_b = A(s, &ubig!(0xcb0ce564ec598f658409d170));
        let res = T(s, &[res_a, res_b]);
        assert_jet(s, jet_dvr, sam, res);

        let sam = T(s, &[a128, a24]);
        let res_a = A(s, &ubig!(0x1a507f98b6fa8605ea3a79e97bf));
        let res_b = A(s, &ubig!(0x3b2013));
        let res = T(s, &[res_a, res_b]);
        assert_jet(s, jet_dvr, sam, res);

        let sam = T(s, &[a63, a63]);
        let res_a = A(s, &ubig!(1));
        let res_b = A(s, &ubig!(0));
        let res = T(s, &[res_a, res_b]);
        assert_jet(s, jet_dvr, sam, res);

        let sam = T(s, &[a0, a24]);
        let res_a = A(s, &ubig!(0));
        let res_b = A(s, &ubig!(0));
        let res = T(s, &[res_a, res_b]);
        assert_jet(s, jet_dvr, sam, res);

        assert_math_jet_err(s, jet_dvr, &[atom_63, atom_0], Deterministic);
    }

    #[test]
    fn test_lth() {
        let ref mut s = init();
        assert_math_jet_noun(s, jet_lth, &[atom_128, atom_96], NO);
        assert_math_jet_noun(s, jet_lth, &[atom_96, atom_63], NO);
        assert_math_jet_noun(s, jet_lth, &[atom_63, atom_96], YES);
        assert_math_jet_noun(s, jet_lth, &[atom_63, atom_63], NO);
        assert_math_jet_noun(s, jet_lth, &[atom_63, atom_24], NO);
        assert_math_jet_noun(s, jet_lth, &[atom_128, atom_24], NO);
        assert_math_jet_noun(s, jet_lth, &[atom_128, atom_128_b], NO);
        assert_math_jet_noun(s, jet_lth, &[atom_128_b, atom_128], YES);
    }

    #[test]
    fn test_lte() {
        let ref mut s = init();
        assert_math_jet_noun(s, jet_lte, &[atom_128, atom_96], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_96, atom_63], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_63, atom_96], YES);
        assert_math_jet_noun(s, jet_lte, &[atom_63, atom_63], YES);
        assert_math_jet_noun(s, jet_lte, &[atom_63, atom_24], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_128, atom_24], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_128, atom_128_b], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_128_b, atom_128], YES);
    }
}
