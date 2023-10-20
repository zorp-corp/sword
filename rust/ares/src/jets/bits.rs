/** Bit arithmetic & logic jets
 */
use crate::interpreter::Context;
use crate::jets::util::*;
use crate::jets::text::util::lent;
use crate::jets::JetErr::*;
use crate::jets::Result;
use crate::noun::DIRECT_MAX;
use crate::noun::{DirectAtom, IndirectAtom, Noun, D, T};
use std::cmp;

crate::gdb!();

/*
 * Bit arithmetic
 */

pub fn jet_bex(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?.as_direct()?.data() as usize;
    Ok(bex(context.stack, arg).as_noun())
}

pub fn jet_can(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let bloq = bloq(slot(arg, 2)?)?;
    let original_list = slot(arg, 3)?;

    let mut len = 0usize;
    let mut list = original_list;
    loop {
        if unsafe { list.raw_equals(D(0)) } {
            break;
        }

        let cell = list.as_cell()?;
        let item = cell.head().as_cell()?;
        let step = item.head().as_direct()?.data() as usize;

        len = checked_add(len, step)?;
        list = cell.tail();
    }

    if len == 0 {
        Ok(D(0))
    } else {
        unsafe {
            let (mut new_indirect, new_slice) =
                IndirectAtom::new_raw_mut_bitslice(context.stack, bite_to_word(bloq, len)?);
            let mut pos = 0;
            let mut list = original_list;
            loop {
                if list.raw_equals(D(0)) {
                    break;
                }

                let cell = list.as_cell()?;
                let item = cell.head().as_cell()?;
                let step = item.head().as_direct()?.data() as usize;
                let atom = item.tail().as_atom()?;
                chop(bloq, 0, step, pos, new_slice, atom.as_bitslice())?;

                pos += step;
                list = cell.tail();
            }
            Ok(new_indirect.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_cat(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let bloq = bloq(slot(arg, 2)?)?;
    let a = slot(arg, 6)?.as_atom()?;
    let b = slot(arg, 7)?.as_atom()?;

    let len_a = met(bloq, a);
    let len_b = met(bloq, b);
    let new_len = bite_to_word(bloq, checked_add(len_a, len_b)?)?;
    if new_len == 0 {
        Ok(a.as_noun())
    } else {
        unsafe {
            let (mut new_indirect, new_slice) =
                IndirectAtom::new_raw_mut_bitslice(context.stack, new_len);
            chop(bloq, 0, len_a, 0, new_slice, a.as_bitslice())?;
            chop(bloq, 0, len_b, len_a, new_slice, b.as_bitslice())?;
            Ok(new_indirect.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_cut(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let bloq = bloq(slot(arg, 2)?)?;
    let start = slot(arg, 12)?.as_direct()?.data() as usize;
    let run = slot(arg, 13)?.as_direct()?.data() as usize;
    let atom = slot(arg, 7)?.as_atom()?;

    if run == 0 {
        return Ok(D(0));
    }

    let new_indirect = unsafe {
        let (mut new_indirect, new_slice) =
            IndirectAtom::new_raw_mut_bitslice(context.stack, bite_to_word(bloq, run)?);
        chop(bloq, start, run, 0, new_slice, atom.as_bitslice())?;
        new_indirect.normalize_as_atom()
    };
    Ok(new_indirect.as_noun())
}

pub fn jet_end(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let a = slot(arg, 3)?.as_atom()?;

    if step == 0 {
        Ok(D(0))
    } else if step >= met(bloq, a) {
        Ok(a.as_noun())
    } else {
        unsafe {
            let (mut new_indirect, new_slice) =
                IndirectAtom::new_raw_mut_bitslice(context.stack, bite_to_word(bloq, step)?);
            chop(bloq, 0, step, 0, new_slice, a.as_bitslice())?;
            Ok(new_indirect.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_lsh(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let a = slot(arg, 3)?.as_atom()?;

    let len = met(bloq, a);
    if len == 0 {
        return Ok(D(0));
    }

    let new_size = bits_to_word(checked_add(a.bit_size(), checked_left_shift(bloq, step)?)?)?;
    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(context.stack, new_size);
        chop(bloq, 0, len, step, dest, a.as_bitslice())?;
        Ok(atom.normalize_as_atom().as_noun())
    }
}

pub fn jet_met(_context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let bloq = bloq(slot(arg, 2)?)?;
    let a = slot(arg, 3)?.as_atom()?;

    Ok(D(met(bloq, a) as u64))
}

pub fn jet_rap(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let bloq = bloq(slot(arg, 2)?)?;
    let original_list = slot(arg, 3)?;

    let mut len = 0usize;
    let mut list = original_list;
    loop {
        if unsafe { list.raw_equals(D(0)) } {
            break;
        }

        let cell = list.as_cell()?;

        len = checked_add(len, met(bloq, cell.head().as_atom()?))?;
        list = cell.tail();
    }

    if len == 0 {
        Ok(D(0))
    } else {
        unsafe {
            let (mut new_indirect, new_slice) =
                IndirectAtom::new_raw_mut_bitslice(context.stack, bite_to_word(bloq, len)?);
            let mut pos = 0;
            let mut list = original_list;

            loop {
                if list.raw_equals(D(0)) {
                    break;
                }

                let cell = list.as_cell()?;
                let atom = cell.head().as_atom()?;
                let step = met(bloq, atom);
                chop(bloq, 0, step, pos, new_slice, atom.as_bitslice())?;

                pos += step;
                list = cell.tail();
            }

            Ok(new_indirect.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_rep(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let original_list = slot(arg, 3)?;

    let mut len = 0usize;
    let mut list = original_list;
    loop {
        if unsafe { list.raw_equals(D(0)) } {
            break;
        }

        let cell = list.as_cell()?;

        len = checked_add(len, step)?;
        list = cell.tail();
    }

    if len == 0 {
        Ok(D(0))
    } else {
        unsafe {
            let (mut new_indirect, new_slice) =
                IndirectAtom::new_raw_mut_bitslice(context.stack, bite_to_word(bloq, len)?);
            let mut pos = 0;
            let mut list = original_list;
            loop {
                if list.raw_equals(D(0)) {
                    break;
                }

                let cell = list.as_cell()?;
                let atom = cell.head().as_atom()?;
                chop(bloq, 0, step, pos, new_slice, atom.as_bitslice())?;

                pos += step;
                list = cell.tail();
            }
            Ok(new_indirect.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_rev(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let boz = slot(arg, 2)?.as_atom()?.as_direct()?.data();

    if boz >= 64 {
        return Err(NonDeterministic);
    }

    let boz = boz as usize;

    let len = slot(arg, 6)?.as_atom()?.as_direct()?.data();

    let dat = slot(arg, 7)?.as_atom()?;

    let bits = len << boz;

    /* 63 is the maximum number of bits for a direct atom */
    let mut output = if dat.is_direct() && bits < 64 {
        unsafe { DirectAtom::new_unchecked(0).as_atom() }
    } else {
        unsafe { IndirectAtom::new_raw(context.stack, ((bits + 7) / 8) as usize, &0).as_atom() }
    };

    let src = dat.as_bitslice();
    let dest = output.as_bitslice_mut();

    let len = len as usize;
    let total_len = len << boz;

    for (start, end) in (0..len).map(|b| (b << boz, (b + 1) << boz)) {
        dest[start..end].copy_from_bitslice(&src[(total_len - end)..(total_len - start)]);
    }

    Ok(unsafe { output.normalize() }.as_noun())
}

pub fn jet_rip(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let atom = slot(arg, 3)?.as_atom()?;
    rip(context.stack, bloq, step, atom)
}

pub fn jet_rsh(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let a = slot(arg, 3)?.as_atom()?;

    let len = met(bloq, a);
    if step >= len {
        return Ok(D(0));
    }

    let new_size = bits_to_word(checked_sub(a.bit_size(), checked_left_shift(bloq, step)?)?)?;
    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(context.stack, new_size);
        chop(bloq, step, len - step, 0, dest, a.as_bitslice())?;
        Ok(atom.normalize_as_atom().as_noun())
    }
}

/*
 * Bit logic
 */

pub fn jet_con(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    Ok(con(context.stack, a, b).as_noun())
}

pub fn jet_dis(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    let new_size = cmp::max(a.size(), b.size());

    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(context.stack, new_size);
        let a_bit = a.as_bitslice();
        dest[..a_bit.len()].copy_from_bitslice(a_bit);
        *dest &= b.as_bitslice();
        Ok(atom.normalize_as_atom().as_noun())
    }
}

pub fn jet_mix(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    let new_size = cmp::max(a.size(), b.size());

    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(context.stack, new_size);
        let a_bit = a.as_bitslice();
        dest[..a_bit.len()].copy_from_bitslice(a_bit);
        *dest ^= b.as_bitslice();
        Ok(atom.normalize_as_atom().as_noun())
    }
}

pub fn jet_xeb(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let a = slot(sam, 1)?.as_atom()?;

    let syz = met(0, a) as u64;

    unsafe {
        Ok(DirectAtom::new_unchecked(syz).as_atom().as_noun())
    }
}

pub fn jet_flop(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let src = slot(sam, 1)?;

    if unsafe { src.raw_equals(D(0)) } {
        return Ok(D(0));
    }

    flop(context.stack, src)
}

pub fn jet_swp(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let bloq = slot(sam, 2)?.as_atom()?.as_direct()?.data() as usize;
    let atom = slot(sam, 3)?.as_atom()?;

    let ripper = rip(context.stack, bloq, 1, atom)?;
    let flopper = flop(context.stack, ripper)?;
    let sample = T(context.stack, &[D(1), flopper]);
    jet_rep(context, sample)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, assert_jet_ubig, init_stack, A};
    use crate::mem::NockStack;
    use crate::noun::{Noun, D, T};
    use ibig::ubig;

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

    /*
     * Bit arithmetic
     */

    #[test]
    fn test_bex() {
        let s = &mut init_stack();
        assert_jet(s, jet_bex, D(0), D(1));
        assert_jet(s, jet_bex, D(5), D(32));
        assert_jet(s, jet_bex, D(62), D(0x4000000000000000));
        assert_jet_ubig(
            s,
            jet_bex,
            D(256),
            ubig!(_0x10000000000000000000000000000000000000000000000000000000000000000),
        );
    }

    #[test]
    fn test_can() {
        let s = &mut init_stack();
        let (a0, _a24, _a63, _a96, a128) = atoms(s);
        let bloq0 = D(0);
        let bloq3 = D(3);
        let bloq4 = D(4);
        let sam = T(s, &[bloq0, D(0)]);
        assert_jet(s, jet_can, sam, D(0));
        let sam = T(s, &[bloq3, D(0)]);
        assert_jet(s, jet_can, sam, D(0));
        let run1 = T(s, &[D(0), a0]);
        let run2 = T(s, &[D(1), a0]);
        let run3 = T(s, &[D(2), a0]);
        let sam = T(s, &[bloq0, run1, run2, run3, D(0)]);
        assert_jet(s, jet_can, sam, D(0));
        let sam = T(s, &[bloq3, run1, run2, run3, D(0)]);
        assert_jet(s, jet_can, sam, D(0));
        let run1 = T(s, &[D(1), a128]);
        let run2 = T(s, &[D(3), a0]);
        let sam = T(s, &[bloq3, run1, run2, D(0)]);
        assert_jet(s, jet_can, sam, D(0x10));
        let run1 = T(s, &[D(3), a0]);
        let run2 = T(s, &[D(1), a128]);
        let sam = T(s, &[bloq3, run1, run2, D(0)]);
        assert_jet(s, jet_can, sam, D(0x10000000));
        let run1 = T(s, &[D(8), D(0xfe)]);
        let run2 = T(s, &[D(4), D(0xa)]);
        let run3 = T(s, &[D(0), D(0xbbbb)]);
        let run4 = T(s, &[D(1), D(0)]);
        let run5 = T(s, &[D(1), D(0)]);
        let run6 = T(s, &[D(1), D(1)]);
        let run7 = T(s, &[D(1), D(1)]);
        let sam = T(s, &[bloq0, run1, run2, run3, run4, run5, run6, run7, D(0)]);
        assert_jet(s, jet_can, sam, D(0xcafe));
        let run1 = T(s, &[D(1), D(0xfe)]);
        let run2 = T(s, &[D(1), D(0xca)]);
        let sam = T(s, &[bloq4, run1, run2, D(0)]);
        assert_jet(s, jet_can, sam, D(0xca00fe));
    }

    #[test]
    fn test_cat() {
        let s = &mut init_stack();
        let (a0, a24, _a63, _a96, a128) = atoms(s);
        let bloq0 = D(0);
        let bloq3 = D(3);
        let bloq4 = D(4);
        let sam = T(s, &[bloq0, a0, a0]);
        assert_jet(s, jet_cat, sam, D(0));
        let sam = T(s, &[bloq3, a0, a0]);
        assert_jet(s, jet_cat, sam, D(0));
        let sam = T(s, &[bloq0, a24, a128]);
        let res = A(s, &ubig!(_0xdeadbeef12345678fedcba9876543210876543));
        assert_jet(s, jet_cat, sam, res);
        let sam = T(s, &[bloq3, a24, a128]);
        let res = A(s, &ubig!(_0xdeadbeef12345678fedcba9876543210876543));
        assert_jet(s, jet_cat, sam, res);
        let sam = T(s, &[bloq4, a24, a128]);
        let res = A(s, &ubig!(_0xdeadbeef12345678fedcba987654321000876543));
        assert_jet(s, jet_cat, sam, res);
    }

    #[test]
    fn test_cut() {
        let s = &mut init_stack();
        let (_a0, a24, _a63, a96, a128) = atoms(s);
        let run = T(s, &[D(0), D(0)]);
        let sam = T(s, &[D(0), run, a24]);
        assert_jet(s, jet_cut, sam, D(0));
        let run = T(s, &[D(0), D(5)]);
        let sam = T(s, &[D(0), run, a24]);
        assert_jet(s, jet_cut, sam, D(0x3));
        let run = T(s, &[D(4), D(6)]);
        let sam = T(s, &[D(3), run, a96]);
        assert_jet(s, jet_cut, sam, D(0xb00c15deadbe));
        let run = T(s, &[D(4), D(1)]);
        let sam = T(s, &[D(4), run, a24]);
        assert_jet(s, jet_cut, sam, D(0));
        let run = T(s, &[D(2), D(10)]);
        let sam = T(s, &[D(4), run, a128]);
        let res = A(s, &ubig!(0xdeadbeef12345678fedcba98));
        assert_jet(s, jet_cut, sam, res);
    }

    #[test]
    fn test_end() {
        let s = &mut init_stack();
        let (a0, a24, _a63, a96, a128) = atoms(s);
        let sam = T(s, &[a0, a24]);
        assert_jet(s, jet_end, sam, D(0x1));
        let sam = T(s, &[D(3), a24]);
        assert_jet(s, jet_end, sam, D(0x43));
        let sam = T(s, &[D(7), a24]);
        assert_jet(s, jet_end, sam, a24);
        let sam = T(s, &[D(6), a128]);
        let res = A(s, &ubig!(0xfedcba9876543210));
        assert_jet(s, jet_end, sam, res);

        let bit = T(s, &[D(0), D(5)]);
        let sam = T(s, &[bit, a24]);
        assert_jet(s, jet_end, sam, D(0x3));
        let bit = T(s, &[D(4), D(6)]);
        let sam = T(s, &[bit, a96]);
        assert_jet(s, jet_end, sam, a96);
    }

    #[test]
    fn test_lsh() {
        let s = &mut init_stack();
        let (a0, a24, _a63, a96, a128) = atoms(s);
        let sam = T(s, &[a0, a24]);
        assert_jet(s, jet_lsh, sam, D(0x10eca86));
        let sam = T(s, &[D(3), a24]);
        assert_jet(s, jet_lsh, sam, D(0x87654300));
        let sam = T(s, &[D(7), a24]);
        let res = A(s, &ubig!(_0x87654300000000000000000000000000000000));
        assert_jet(s, jet_lsh, sam, res);
        let sam = T(s, &[D(6), a128]);
        let res = A(
            s,
            &ubig!(_0xdeadbeef12345678fedcba98765432100000000000000000),
        );
        assert_jet(s, jet_lsh, sam, res);

        let bit = T(s, &[D(0), D(5)]);
        let sam = T(s, &[bit, a24]);
        assert_jet(s, jet_lsh, sam, D(0x10eca860));
        let bit = T(s, &[D(4), D(6)]);
        let sam = T(s, &[bit, a96]);
        let res = A(
            s,
            &ubig!(_0xfaceb00c15deadbeef123456000000000000000000000000),
        );
        assert_jet(s, jet_lsh, sam, res);
    }

    #[test]
    fn test_met() {
        let s = &mut init_stack();
        let (a0, a24, _a63, _a96, a128) = atoms(s);
        let sam = T(s, &[a0, a0]);
        assert_jet(s, jet_met, sam, D(0));
        let sam = T(s, &[a0, a24]);
        assert_jet(s, jet_met, sam, D(24));
        let sam = T(s, &[D(3), a24]);
        assert_jet(s, jet_met, sam, D(3));
        let sam = T(s, &[D(1), a128]);
        assert_jet(s, jet_met, sam, D(64));
    }

    #[test]
    fn test_rap() {
        let s = &mut init_stack();
        let bloq0 = D(0);
        let bloq2 = D(2);
        let bloq3 = D(3);
        let empty_list = D(0);
        let zero_list = T(s, &[D(0), D(0), D(0)]);
        let test_list = T(s, &[D(0xe), D(0xf), D(0xa), D(0xc), D(0)]);
        let wide_list = T(s, &[D(0xafe), D(0xc), D(0)]);
        let sam = T(s, &[bloq0, empty_list]);
        assert_jet(s, jet_rap, sam, D(0));
        let sam = T(s, &[bloq0, zero_list]);
        assert_jet(s, jet_rap, sam, D(0));
        let sam = T(s, &[bloq3, zero_list]);
        assert_jet(s, jet_rap, sam, D(0));
        let sam = T(s, &[bloq0, test_list]);
        assert_jet(s, jet_rap, sam, D(0xcafe));
        let sam = T(s, &[bloq2, test_list]);
        assert_jet(s, jet_rap, sam, D(0xcafe));
        let sam = T(s, &[bloq2, wide_list]);
        assert_jet(s, jet_rap, sam, D(0xcafe));
        let sam = T(s, &[bloq3, test_list]);
        let res = A(s, &ubig!(0xc0a0f0e));
        assert_jet(s, jet_rap, sam, res);
        let sam = T(s, &[bloq3, wide_list]);
        assert_jet(s, jet_rap, sam, D(0xc0afe));
    }

    #[test]
    fn test_rep() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        let sam = T(s, &[D(0), D(0)]);
        assert_jet(s, jet_rep, sam, D(0));
        let bit = T(s, &[D(3), D(2)]);
        let sam = T(s, &[bit, a0, a24, a63, a96, a128, D(0)]);
        let res = A(s, &ubig!(0x32103456ffff65430000));
        assert_jet(s, jet_rep, sam, res);
    }

    #[test]
    fn test_rev() {
        let s = &mut init_stack();
        let (_a0, a24, _a63, _a96, _a128) = atoms(s);
        let sam = T(s, &[D(0), D(60), a24]);
        assert_jet(s, jet_rev, sam, D(0xc2a6e1000000000));
        let test = 0x1234567890123u64;
        let sam = T(s, &[D(3), D(7), D(test)]);
        assert_jet(s, jet_rev, sam, D(test.swap_bytes() >> 8));
    }

    #[test]
    fn test_rip() {
        let s = &mut init_stack();
        let (_a0, _a24, _a63, _a96, a128) = atoms(s);
        let sam = T(s, &[D(0), D(0)]);
        assert_jet(s, jet_rip, sam, D(0));
        let bit = T(s, &[D(1), D(2)]);
        let sam = T(s, &[bit, a128]);
        #[rustfmt::skip]
        let res = T(
            s,
            &[
                D(0x0), D(0x1), D(0x2), D(0x3), D(0x4), D(0x5), D(0x6), D(0x7),
                D(0x8), D(0x9), D(0xa), D(0xb), D(0xc), D(0xd), D(0xe), D(0xf),
                D(0x8), D(0x7), D(0x6), D(0x5), D(0x4), D(0x3), D(0x2), D(0x1),
                D(0xf), D(0xe), D(0xe), D(0xb), D(0xd), D(0xa), D(0xe), D(0xd),
                D(0x0),
            ],
        );

        assert_jet(s, jet_rip, sam, res);
    }

    #[test]
    fn test_rsh() {
        let s = &mut init_stack();
        let (a0, a24, _a63, a96, a128) = atoms(s);
        let sam = T(s, &[a0, a24]);
        assert_jet(s, jet_rsh, sam, D(0x43b2a1));
        let sam = T(s, &[D(3), a24]);
        assert_jet(s, jet_rsh, sam, D(0x8765));
        let sam = T(s, &[D(7), a24]);
        assert_jet(s, jet_rsh, sam, D(0));
        let sam = T(s, &[D(2), a128]);
        let res = A(s, &ubig!(0xdeadbeef12345678fedcba987654321));
        assert_jet(s, jet_rsh, sam, res);
        let sam = T(s, &[D(6), a128]);
        let res = A(s, &ubig!(0xdeadbeef12345678));
        assert_jet(s, jet_rsh, sam, res);

        let bit = T(s, &[D(0), D(5)]);
        let sam = T(s, &[bit, a24]);
        assert_jet(s, jet_rsh, sam, D(0x43b2a));
        let bit = T(s, &[D(4), D(6)]);
        let sam = T(s, &[bit, a96]);
        assert_jet(s, jet_rsh, sam, D(0));
    }

    /*
     * Bit logic
     */

    #[test]
    fn test_con() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        let sam = T(s, &[a0, a0]);
        assert_jet(s, jet_con, sam, D(0));
        let sam = T(s, &[a24, a96]);
        let res = A(s, &ubig!(0xfaceb00c15deadbeef977557));
        assert_jet(s, jet_con, sam, res);
        let sam = T(s, &[a96, a128]);
        let res = A(s, &ubig!(0xdeadbeeffafef67cffdebfbeff563656));
        assert_jet(s, jet_con, sam, res);
        let sam = T(s, &[a24, a63]);
        assert_jet(s, jet_con, sam, a63);
        let sam = T(s, &[a0, a128]);
        assert_jet(s, jet_con, sam, a128);
        let sam = T(s, &[a128, a0]);
        assert_jet(s, jet_con, sam, a128);
    }

    #[test]
    fn test_dis() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        let sam = T(s, &[a0, a0]);
        assert_jet(s, jet_dis, sam, D(0));
        let sam = T(s, &[a24, a96]);
        assert_jet(s, jet_dis, sam, D(0x22442));
        let sam = T(s, &[a96, a128]);
        let res = A(s, &ubig!(0x1204100814dca89866103010));
        assert_jet(s, jet_dis, sam, res);
        let sam = T(s, &[a24, a63]);
        assert_jet(s, jet_dis, sam, a24);
        let sam = T(s, &[a0, a128]);
        assert_jet(s, jet_dis, sam, a0);
        let sam = T(s, &[a128, a0]);
        assert_jet(s, jet_dis, sam, a0);
    }

    #[test]
    fn test_mix() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        let sam = T(s, &[a0, a0]);
        assert_jet(s, jet_mix, sam, D(0));
        let sam = T(s, &[a24, a96]);
        let res = A(s, &ubig!(0xfaceb00c15deadbeef955115));
        assert_jet(s, jet_mix, sam, res);
        let sam = T(s, &[a96, a128]);
        let res = A(s, &ubig!(0xdeadbeefe8fae674eb02172699460646));
        assert_jet(s, jet_mix, sam, res);
        let sam = T(s, &[a24, a63]);
        let res = A(s, &ubig!(0x7fffffffff789abc));
        assert_jet(s, jet_mix, sam, res);
        let sam = T(s, &[a0, a128]);
        assert_jet(s, jet_mix, sam, a128);
        let sam = T(s, &[a128, a0]);
        assert_jet(s, jet_mix, sam, a128);
    }

    #[test]
    fn test_xeb() {
        let s = &mut init_stack();
        assert_jet(s, jet_xeb, D(0), D(0));
        assert_jet(s, jet_xeb, D(1), D(1));
        assert_jet(s, jet_xeb, D(31), D(5));
        assert_jet(s, jet_xeb, D(32), D(6));
        assert_jet(s, jet_xeb, D(0xfff), D(12));
        assert_jet(s, jet_xeb, D(0xffff), D(16));
        assert_jet(s, jet_xeb, D(0x3fffffffffffffff), D(62));
        assert_jet(s, jet_xeb, D(0x4000000000000000), D(63));
    }

    #[test]
    fn test_swp() {
        let s = &mut init_stack();
        let sam = T(s, &[D(0), D(0x18)]);
        assert_jet(s, jet_swp, sam, D(0x3));
        let sam = T(s, &[D(0), D(0x80)]);
        assert_jet(s, jet_swp, sam, D(0x1));
        let sam = T(s, &[D(3), D(0x636261)]);
        assert_jet(s, jet_swp, sam, D(0x616263));
    }

    #[test]
    fn test_flop() {
        let s = &mut init_stack();
        let sam = T(s, &[D(1), D(2), D(3), D(0)]);
        let res = T(s, &[D(3), D(2), D(1), D(0)]);
        assert_jet(s, jet_flop, sam, res);

        let sam = T(
            s,
            &[
                D(0xd), D(0xe), D(0xa), D(0xd), D(0xb), D(0xe), D(0xe), D(0xf),
                D(0x1), D(0x2), D(0x3), D(0x4), D(0x5), D(0x6), D(0x7), D(0x8),
                D(0xf), D(0xe), D(0xd), D(0xc), D(0xb), D(0xa), D(0x9), D(0x8),
                D(0x7), D(0x6), D(0x5), D(0x4), D(0x3), D(0x2), D(0x1), D(0x0),
                D(0x0),
            ],
        );
        #[rustfmt::skip]
        let res = T(
            s,
            &[
                D(0x0), D(0x1), D(0x2), D(0x3), D(0x4), D(0x5), D(0x6), D(0x7),
                D(0x8), D(0x9), D(0xa), D(0xb), D(0xc), D(0xd), D(0xe), D(0xf),
                D(0x8), D(0x7), D(0x6), D(0x5), D(0x4), D(0x3), D(0x2), D(0x1),
                D(0xf), D(0xe), D(0xe), D(0xb), D(0xd), D(0xa), D(0xe), D(0xd),
                D(0x0),
            ],
        );
        assert_jet(s, jet_flop, sam, res);
    }

}
