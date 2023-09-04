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
use crate::jets;
use crate::jets::JetErr::*;
use crate::jets::util::*;
use crate::mem::NockStack;
use crate::mug::mug;
use crate::newt::Newt;
use crate::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun, D, DIRECT_MAX, NO, T, YES};
use either::{Left, Right};
use ibig::ops::DivRem;
use ibig::UBig;
use std::cmp;

crate::gdb!();

pub fn jet_dec(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
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

pub fn jet_add(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        Ok(Atom::new(stack, a.data() + b.data()).as_noun())
    } else {
        let a_big = a.as_ubig(stack);
        let b_big = b.as_ubig(stack);
        let res = UBig::add_stack(stack, a_big, b_big);
        Ok(Atom::from_ubig(stack, &res).as_noun())
    }
}

pub fn jet_sub(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    Ok(sub(stack, a, b)?.as_noun())
}

pub fn jet_mul(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        let res = a.data() as u128 * b.data() as u128;
        if res < DIRECT_MAX as u128 {
            Ok(Atom::new(stack, res as u64).as_noun())
        } else {
            Ok(unsafe {
                IndirectAtom::new_raw_bytes(
                    stack,
                    if res < u64::MAX as u128 { 8 } else { 16 },
                    &res as *const u128 as *const u8,
                )
            }
            .as_noun())
        }
    } else {
        let a_big = a.as_ubig(stack);
        let b_big = b.as_ubig(stack);
        let res = UBig::mul_stack(stack, a_big, b_big);
        Ok(Atom::from_ubig(stack, &res).as_noun())
    }
}

pub fn jet_div(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    if unsafe { b.as_noun().raw_equals(D(0)) } {
        Err(Deterministic)
    } else if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        Ok(unsafe { DirectAtom::new_unchecked(a.data() / b.data()) }.as_noun())
    } else {
        let a_big = a.as_ubig(stack);
        let b_big = b.as_ubig(stack);
        let res = UBig::div_stack(stack, a_big, b_big);
        Ok(Atom::from_ubig(stack, &res).as_noun())
    }
}

pub fn jet_mod(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    if unsafe { b.as_noun().raw_equals(D(0)) } {
        Err(Deterministic)
    } else if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        Ok(unsafe { DirectAtom::new_unchecked(a.data() % b.data()) }.as_noun())
    } else {
        let res = a.as_ubig(stack) % b.as_ubig(stack);
        Ok(Atom::from_ubig(stack, &res).as_noun())
    }
}

pub fn jet_dvr(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

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
            let (div, rem) = a.as_ubig(stack).div_rem(b.as_ubig(stack));
            (
                Atom::from_ubig(stack, &div).as_noun(),
                Atom::from_ubig(stack, &rem).as_noun(),
            )
        };

        Ok(T(stack, &[div, rem]))
    }
}

pub fn jet_lth(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() < b.data() {
            YES
        } else {
            NO
        }
    } else if a.bit_size() < b.bit_size() {
        YES
    } else if a.bit_size() > b.bit_size() {
        NO
    } else if a.as_ubig(stack) < b.as_ubig(stack) {
        YES
    } else {
        NO
    })
}

pub fn jet_lte(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() <= b.data() {
            YES
        } else {
            NO
        }
    } else if a.bit_size() < b.bit_size() {
        YES
    } else if a.bit_size() > b.bit_size() {
        NO
    } else if a.as_ubig(stack) <= b.as_ubig(stack) {
        YES
    } else {
        NO
    })
}

pub fn jet_gth(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() > b.data() {
            YES
        } else {
            NO
        }
    } else if a.bit_size() > b.bit_size() {
        YES
    } else if a.bit_size() < b.bit_size() {
        NO
    } else if a.as_ubig(stack) > b.as_ubig(stack) {
        YES
    } else {
        NO
    })
}

pub fn jet_gte(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    Ok(if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() >= b.data() {
            YES
        } else {
            NO
        }
    } else if a.bit_size() > b.bit_size() {
        YES
    } else if a.bit_size() < b.bit_size() {
        NO
    } else if a.as_ubig(stack) >= b.as_ubig(stack) {
        YES
    } else {
        NO
    })
}

pub fn jet_bex(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?.as_direct()?.data() as usize;
    Ok(bex(stack, arg).as_noun())
}

pub fn jet_lsh(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let a = slot(arg, 3)?.as_atom()?;

    let len = met(bloq, a);
    if len == 0 {
        return Ok(D(0));
    }
    
    let new_size = bits_to_word(checked_add(a.bit_size(), checked_left_shift(bloq, step)?)?)?;
    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
        chop(bloq, 0, len, step, dest, a.as_bitslice())?;
        Ok(atom.normalize_as_atom().as_noun())
    }
}

pub fn jet_rsh(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let a = slot(arg, 3)?.as_atom()?;

    let len = met(bloq, a);
    if step >= len {
        return Ok(D(0));
    }

    let new_size = bits_to_word(checked_sub(a.bit_size(), checked_left_shift(bloq, step)?)?)?;
    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
        chop(bloq, step, len - step, 0, dest, a.as_bitslice())?;
        Ok(atom.normalize_as_atom().as_noun())
    }
}

pub fn jet_con(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    Ok(con(stack, a, b).as_noun())
}

pub fn jet_dis(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    let new_size = cmp::max(a.size(), b.size());

    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
        let a_bit = a.as_bitslice();
        dest[..a_bit.len()].copy_from_bitslice(a_bit);
        *dest &= b.as_bitslice();
        Ok(atom.normalize_as_atom().as_noun())
    }
}

pub fn jet_mix(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    let new_size = cmp::max(a.size(), b.size());

    unsafe {
        let (mut atom, dest) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
        let a_bit = a.as_bitslice();
        dest[..a_bit.len()].copy_from_bitslice(a_bit);
        *dest ^= b.as_bitslice();
        Ok(atom.normalize_as_atom().as_noun())
    }
}

pub fn jet_end(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let a = slot(arg, 3)?.as_atom()?;

    if step == 0 {
        Ok(D(0))
    } else if step >= met(bloq, a) {
        Ok(a.as_noun())
    } else {
        unsafe {
            let (mut new_indirect, new_slice) = IndirectAtom::new_raw_mut_bitslice(stack, bite_to_word(bloq, step)?);
            chop(bloq, 0, step, 0, new_slice, a.as_bitslice())?;
            Ok(new_indirect.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_cat(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let bloq = slot(arg, 2)?.as_direct()?.data() as usize;
    if bloq >= 64 {
        return Err(NonDeterministic);
    }
    let a = slot(arg, 6)?.as_atom()?;
    let b = slot(arg, 7)?.as_atom()?;

    let new_size = a.size() + b.size();
    if new_size == 0 {
        Ok(a.as_noun())
    } else {
        unsafe {
            let len_a = met(bloq, a);
            let len_b = met(bloq, b);
            let (mut new_indirect, new_slice) = IndirectAtom::new_raw_mut_bitslice(stack, new_size);
            chop(bloq, 0, len_a, 0, new_slice, a.as_bitslice())?;
            chop(bloq, 0, len_b, len_a, new_slice, b.as_bitslice())?;
            Ok(new_indirect.normalize_as_atom().as_noun())
        }
    }
}

pub fn jet_can(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let bloq = slot(arg, 2)?.as_direct()?.data() as usize;
    if bloq >= 64 {
        return Err(NonDeterministic);
    }
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
            let (mut new_indirect, new_slice) = IndirectAtom::new_raw_mut_bitslice(stack, bite_to_word(bloq, len)?);
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

pub fn jet_rep(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
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
            let (mut new_indirect, new_slice) = IndirectAtom::new_raw_mut_bitslice(stack, bite_to_word(bloq, len)?);
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

pub fn jet_cut(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let bloq = slot(arg, 2)?.as_direct()?.data() as usize;
    if bloq >= 64 {
        return Err(NonDeterministic);
    }
    let start = slot(arg, 12)?.as_direct()?.data() as usize;
    let run = slot(arg, 13)?.as_direct()?.data() as usize;
    let atom = slot(arg, 7)?.as_atom()?;

    let new_indirect = unsafe {
        let (mut new_indirect, new_slice) = IndirectAtom::new_raw_mut_bitslice(stack, bite_to_word(bloq, run)?);
        chop(bloq, start, run, 0, new_slice, atom.as_bitslice())?;
        new_indirect.normalize_as_atom()
    };
    Ok(new_indirect.as_noun())
}

pub fn jet_rip(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let (bloq, step) = bite(slot(arg, 2)?)?;
    let atom = slot(arg, 3)?.as_atom()?;
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

pub fn jet_met(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let bloq = slot(arg, 2)?.as_direct()?.data() as usize;
    if bloq >= 64 {
        return Err(NonDeterministic);
    }
    let a = slot(arg, 3)?.as_atom()?;

    Ok(D(met(bloq, a) as u64))
}

pub fn jet_mug(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    Ok(mug(stack, arg).as_noun())
}

pub fn jet_rev(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
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
        unsafe { IndirectAtom::new_raw(stack, ((bits + 7) / 8) as usize, &0).as_atom() }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::{Jet, JetErr};
    use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_ubig, assert_nary_jet_ubig, init_stack, A};
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

    fn atom_128_b(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xdeadbeef12345678fedcba9876540000))
    }

    fn atom_264(stack: &mut NockStack) -> Noun {
        A(
            stack,
            &ubig!(_0xdeadbeef12345678fedcba9876540000deadbeef12345678fedcba9876540000ff),
        )
    }

    fn atom_528(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(_0xdeadbeef12345678fedcba9876540000deadbeef12345678fedcba9876540000ffdeadbeef12345678fedcba9876540000deadbeef12345678fedcba9876540001ff))
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
    fn test_dec() {
        let s = &mut init_stack();
        let (a0, _a24, a63, _a96, a128) = atoms(s);
        assert_jet_ubig(s, jet_dec, a128, ubig!(0xdeadbeef12345678fedcba987654320f));
        assert_jet(s, jet_dec, a63, D(0x7ffffffffffffffe));
        assert_jet_err(s, jet_dec, a0, Deterministic);
    }

    #[test]
    fn test_add() {
        let s = &mut init_stack();
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
        let s = &mut init_stack();
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
        let s = &mut init_stack();
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
        let s = &mut init_stack();
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
        let res = ubig!(
            _0x00000000000001000000000000000000000000000000000000000000000000000000000000000001
        );
        assert_math_jet(s, jet_div, &[atom_528, atom_264], res);
        assert_math_jet_err(s, jet_div, &[atom_63, atom_0], Deterministic);
        assert_math_jet_err(s, jet_div, &[atom_0, atom_0], Deterministic);
    }

    #[test]
    fn test_mod() {
        let s = &mut init_stack();
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
        assert_math_jet(s, jet_mod, &[atom_528, atom_264], ubig!(0x100));
        assert_math_jet_err(s, jet_mod, &[atom_63, atom_0], Deterministic);
        assert_math_jet_err(s, jet_mod, &[atom_0, atom_0], Deterministic);
    }

    #[test]
    fn test_dvr() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        let a264 = atom_264(s);
        let a528 = atom_528(s);

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

        let sam = T(s, &[a528, a264]);
        let res_a = A(
            s,
            &ubig!(
                _0x00000000000001000000000000000000000000000000000000000000000000000000000000000001
            ),
        );
        let res_b = A(s, &ubig!(0x100));
        let res = T(s, &[res_a, res_b]);
        assert_jet(s, jet_dvr, sam, res);

        assert_math_jet_err(s, jet_dvr, &[atom_63, atom_0], Deterministic);
    }

    #[test]
    fn test_lth() {
        let s = &mut init_stack();
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
        let s = &mut init_stack();
        assert_math_jet_noun(s, jet_lte, &[atom_128, atom_96], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_96, atom_63], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_63, atom_96], YES);
        assert_math_jet_noun(s, jet_lte, &[atom_63, atom_63], YES);
        assert_math_jet_noun(s, jet_lte, &[atom_63, atom_24], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_128, atom_24], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_128, atom_128_b], NO);
        assert_math_jet_noun(s, jet_lte, &[atom_128_b, atom_128], YES);
    }

    #[test]
    fn test_gth() {
        let s = &mut init_stack();
        assert_math_jet_noun(s, jet_gth, &[atom_128, atom_96], YES);
        assert_math_jet_noun(s, jet_gth, &[atom_96, atom_63], YES);
        assert_math_jet_noun(s, jet_gth, &[atom_63, atom_96], NO);
        assert_math_jet_noun(s, jet_gth, &[atom_63, atom_63], NO);
        assert_math_jet_noun(s, jet_gth, &[atom_63, atom_24], YES);
        assert_math_jet_noun(s, jet_gth, &[atom_128, atom_24], YES);
        assert_math_jet_noun(s, jet_gth, &[atom_128, atom_128_b], YES);
        assert_math_jet_noun(s, jet_gth, &[atom_128_b, atom_128], NO);
    }

    #[test]
    fn test_gte() {
        let s = &mut init_stack();
        assert_math_jet_noun(s, jet_gte, &[atom_128, atom_96], YES);
        assert_math_jet_noun(s, jet_gte, &[atom_96, atom_63], YES);
        assert_math_jet_noun(s, jet_gte, &[atom_63, atom_96], NO);
        assert_math_jet_noun(s, jet_gte, &[atom_63, atom_63], YES);
        assert_math_jet_noun(s, jet_gte, &[atom_63, atom_24], YES);
        assert_math_jet_noun(s, jet_gte, &[atom_128, atom_24], YES);
        assert_math_jet_noun(s, jet_gte, &[atom_128, atom_128_b], YES);
        assert_math_jet_noun(s, jet_gte, &[atom_128_b, atom_128], NO);
    }

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

    #[test]
    fn test_con() {
        let s = &mut init_stack();
        let (_a0, _a24, a63, _a96, a128) = atoms(s);
        assert_math_jet(s, jet_con, &[atom_0, atom_0], ubig!(0));
        assert_math_jet(
            s,
            jet_con,
            &[atom_24, atom_96],
            ubig!(0xfaceb00c15deadbeef977557),
        );
        assert_math_jet(
            s,
            jet_con,
            &[atom_96, atom_128],
            ubig!(0xdeadbeeffafef67cffdebfbeff563656),
        );
        assert_math_jet_noun(s, jet_con, &[atom_24, atom_63], a63);
        assert_math_jet_noun(s, jet_con, &[atom_0, atom_128], a128);
        assert_math_jet_noun(s, jet_con, &[atom_128, atom_0], a128);
    }

    #[test]
    fn test_dis() {
        let s = &mut init_stack();
        let (a0, a24, _a63, _a96, _a128) = atoms(s);
        assert_math_jet(s, jet_dis, &[atom_0, atom_0], ubig!(0));
        assert_math_jet(s, jet_dis, &[atom_24, atom_96], ubig!(0x22442));
        assert_math_jet(
            s,
            jet_dis,
            &[atom_96, atom_128],
            ubig!(0x1204100814dca89866103010),
        );
        assert_math_jet_noun(s, jet_dis, &[atom_24, atom_63], a24);
        assert_math_jet_noun(s, jet_dis, &[atom_0, atom_128], a0);
        assert_math_jet_noun(s, jet_dis, &[atom_128, atom_0], a0);
    }

    #[test]
    fn test_mix() {
        let s = &mut init_stack();
        let (_a0, _a24, _a63, _a96, a128) = atoms(s);
        assert_math_jet(s, jet_mix, &[atom_0, atom_0], ubig!(0));
        assert_math_jet(
            s,
            jet_mix,
            &[atom_24, atom_96],
            ubig!(0xfaceb00c15deadbeef955115),
        );
        assert_math_jet(
            s,
            jet_mix,
            &[atom_96, atom_128],
            ubig!(0xdeadbeefe8fae674eb02172699460646),
        );
        assert_math_jet(s, jet_mix, &[atom_24, atom_63], ubig!(0x7fffffffff789abc));
        assert_math_jet_noun(s, jet_mix, &[atom_0, atom_128], a128);
        assert_math_jet_noun(s, jet_mix, &[atom_128, atom_0], a128);
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
    fn test_cat() {
        let s = &mut init_stack();
        let (a0, a24, a63, _a96, a128) = atoms(s);
        let sam = T(s, &[a0, a0, a0]);
        assert_jet(s, jet_cat, sam, D(0));
        let sam = T(s, &[a0, a24, a128]);
        let res = A(s, &ubig!(_0xdeadbeef12345678fedcba9876543210876543));
        assert_jet(s, jet_cat, sam, res);
        let sam = T(s, &[a0, a63, a24]);
        let res = A(s, &ubig!(0x43b2a1ffffffffffffffff));
        assert_jet(s, jet_cat, sam, res);
        let sam = T(s, &[D(1), a63, a24]);
        let res = A(s, &ubig!(0x8765437fffffffffffffff));
        assert_jet(s, jet_cat, sam, res);
        let sam = T(s, &[D(4), a24, a128]);
        let res = A(s, &ubig!(_0xdeadbeef12345678fedcba987654321000876543));
        assert_jet(s, jet_cat, sam, res);
    }

    #[test]
    fn test_cut() {
        let s = &mut init_stack();
        let (_a0, a24, _a63, a96, a128) = atoms(s);
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
    fn test_can() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        let sam = T(s, &[D(0), D(0)]);
        assert_jet(s, jet_can, sam, D(0));
        let run1 = T(s, &[D(0), a0]);
        let run2 = T(s, &[D(3), a24]);
        let run3 = T(s, &[D(2), a63]);
        let run4 = T(s, &[D(16), a96]);
        let run5 = T(s, &[D(8), a128]);
        let sam = T(s, &[D(3), run1, run2, run3, run4, run5, D(0)]);
        let res = A(
            s,
            &ubig!(_0xfedcba987654321000000000faceb00c15deadbeef123456ffff876543),
        );
        assert_jet(s, jet_can, sam, res);
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
    fn test_mug() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        assert_jet(s, jet_mug, a0, D(0x79ff04e8));
        assert_jet(s, jet_mug, a24, D(0x69d59d90));
        assert_jet(s, jet_mug, a63, D(0x7a9f252e));
        assert_jet(s, jet_mug, a96, D(0x2aa4c8fb));
        assert_jet(s, jet_mug, a128, D(0x44fb2c0c));
        let sam = T(s, &[a128, a128]);
        assert_jet(s, jet_mug, sam, D(0x61c0ea5c));
        let sam = T(s, &[a96, a128]);
        assert_jet(s, jet_mug, sam, D(0x20fb143f));
        let sam = T(s, &[a0, a0]);
        assert_jet(s, jet_mug, sam, D(0x192f5588));
        let sam = T(s, &[a0, a24, a63, a96, a128]);
        let sam = T(s, &[sam, a0, a24, a63, a96, a128]);
        let sam = T(s, &[sam, a0, a24, a63, a96, a128]);
        assert_jet(s, jet_mug, sam, D(0x7543cac7));
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
}
