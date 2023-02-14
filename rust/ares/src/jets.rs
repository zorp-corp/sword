use crate::interpreter::raw_slot;
use crate::jets_math::*;
use crate::mem::NockStack;
use crate::mug::mug;
use crate::noun::Noun;
use ares_macros::tas;

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

pub fn get_jet(jet_name: Noun) -> Result<Jet, ()> {
    match jet_name.as_direct()?.data() {
        tas!(b"dec") => Ok(jet_dec),
        tas!(b"add") => Ok(jet_add),
        tas!(b"sub") => Ok(jet_sub),
        tas!(b"mul") => Ok(jet_mul),
        tas!(b"div") => Ok(jet_div),
        tas!(b"dvr") => Ok(jet_dvr),
        tas!(b"mod") => Ok(jet_mod),
        tas!(b"lth") => Ok(jet_lth),
        tas!(b"lte") => Ok(jet_lte),
        tas!(b"gth") => Ok(jet_gth),
        tas!(b"gte") => Ok(jet_gte),
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
        tas!(b"cut") => true,
        tas!(b"dvr") => true,
        tas!(b"mod") => true,
        _ => false,
    }
}

fn jet_mug(stack: &mut NockStack, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    Ok(mug(stack, arg).as_noun())
}
