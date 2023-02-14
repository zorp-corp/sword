use crate::interpreter::raw_slot;
use crate::mem::NockStack;
use crate::mug::mug;
use crate::noun::{DirectAtom, IndirectAtom, Noun};
use ares_macros::tas;
use either::Either::*;

/// Return Err if the computation crashed or should punt to Nock
pub type Jet = fn(&mut NockStack, Noun) -> Result<Noun, JetErr>;

/// Only return a deterministic error if the Nock would have deterministically crashed.
pub enum JetErr {
    Punt,             // Retry with the raw nock
    Deterministic,    // The Nock would have crashed
    NonDeterministic, // Other error
}

impl From<()> for JetErr {
    fn from(_: ()) -> Self {
        JetErr::NonDeterministic
    }
}

impl From<JetErr> for () {
    fn from(_: JetErr) -> Self {}
}

use JetErr::*;

pub fn get_jet(jet_name: Noun) -> Result<Jet, ()> {
    match jet_name.as_direct()?.data() {
        tas!(b"dec") => Ok(jet_dec),
        tas!(b"cut") => Ok(jet_cut),
        tas!(b"mug") => Ok(jet_mug),
        _ => {
            // eprintln!("Unknown jet: {:?}", jet_name);
            Err(())
        }
    }
}

pub fn get_jet_test_mode(jet_name: Noun) -> bool {
    match jet_name.as_direct().unwrap().data() {
        tas!(b"dec") => true,
        tas!(b"cut") => true,
        _ => false,
    }
}

fn jet_dec(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
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

fn jet_cut(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
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

fn jet_mug(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    Ok(mug(stack, arg).as_noun())
}
