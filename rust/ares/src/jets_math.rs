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
use crate::interpreter::raw_slot;
use crate::jets::{JetErr, JetErr::*};
use crate::mem::NockStack;
use crate::noun::{Atom, DirectAtom, IndirectAtom, Noun, D, DIRECT_MAX, NO, T, YES};
use either::Either::*;
use ibig::ops::DivRem;

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
    let bloq = raw_slot(arg, 2).as_direct()?.data();
    let start = raw_slot(arg, 12).as_direct()?.data();
    let run = raw_slot(arg, 13).as_direct()?.data();
    let atom = raw_slot(arg, 7).as_atom()?;
    let slice = atom.as_bitslice();
    let unit = 1 << bloq;
    let new_indirect = unsafe {
        let (mut new_indirect, new_slice) =
            IndirectAtom::new_raw_mut_bitslice(stack, ((run * unit + 63) >> 6) as usize);
        new_slice[..(unit * run) as usize]
            .copy_from_bitslice(&slice[(unit * start) as usize..(unit * (start + run)) as usize]);
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
