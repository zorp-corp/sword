use crate::interpreter::raw_slot;
use crate::mem::NockStack;
use crate::noun::{DirectAtom, IndirectAtom, Noun};
use ares_macros::tas;
use either::Either::*;

pub fn get_jet(jet_name: Noun) -> Result<fn(&mut NockStack, Noun) -> Noun, ()> {
    match jet_name.as_direct()?.data() {
        tas!(b"dec") => Ok(jet_dec),
        tas!(b"cut") => Ok(jet_cut),
        _ => Err(()),
    }
}

fn jet_dec(stack: &mut NockStack, subject: Noun) -> Noun {
    let arg = raw_slot(subject, 6);
    if let Ok(atom) = arg.as_atom() {
        match atom.as_either() {
            Left(direct) => {
                if direct.data() == 0 {
                    panic!("Decrementing 0");
                } else {
                    unsafe { DirectAtom::new_unchecked(direct.data() - 1) }.as_noun()
                }
            }
            Right(indirect) => {
                let indirect_slice = indirect.as_bitslice();
                match indirect_slice.first_one() {
                    None => {
                        panic!("Decrementing 0");
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
                        res.as_noun()
                    }
                }
            }
        }
    } else {
        panic!("Argument to dec must be atom");
    }
}

fn jet_cut(stack: &mut NockStack, subject: Noun) -> Noun {
    let arg = raw_slot(subject, 6);
    let bloq = raw_slot(arg, 2).as_direct().unwrap().data();
    let start = raw_slot(arg, 12).as_direct().unwrap().data();
    let run = raw_slot(arg, 13).as_direct().unwrap().data();
    let atom = raw_slot(arg, 7).as_atom().unwrap();
    let slice = atom.as_bitslice();
    let unit = 1 << bloq;
    let (mut new_indirect, new_slice) =
        unsafe { IndirectAtom::new_raw_mut_bitslice(stack, ((run * unit + 63) >> 6) as usize) };
    new_slice[..(unit * run) as usize]
        .copy_from_bitslice(&slice[(unit * start) as usize..(unit * (start + run)) as usize]);
    unsafe { new_indirect.normalize_as_atom().as_noun() }
}
