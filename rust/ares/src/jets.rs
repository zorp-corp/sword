mod math;

use crate::jets::math::*;
use crate::mem::NockStack;
use crate::noun::Noun;
use ares_macros::tas;

/// Return Err if the computation crashed or should punt to Nock
pub type Jet = fn(&mut NockStack, Noun) -> Result<Noun, JetErr>;

/// Only return a deterministic error if the Nock would have deterministically crashed.
#[derive(Debug, PartialEq)]
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
        tas!(b"mod") => Ok(jet_mod),
        tas!(b"dvr") => Ok(jet_dvr),
        tas!(b"lth") => Ok(jet_lth),
        tas!(b"lte") => Ok(jet_lte),
        tas!(b"gth") => Ok(jet_gth),
        tas!(b"gte") => Ok(jet_gte),
        tas!(b"bex") => Ok(jet_bex),
        tas!(b"lsh") => Ok(jet_lsh),
        tas!(b"rsh") => Ok(jet_rsh),
        tas!(b"con") => Ok(jet_con),
        tas!(b"dis") => Ok(jet_dis),
        tas!(b"mix") => Ok(jet_mix),
        tas!(b"end") => Ok(jet_end),
        tas!(b"cat") => Ok(jet_cat),
        tas!(b"cut") => Ok(jet_cut),
        tas!(b"can") => Ok(jet_can),
        tas!(b"rep") => Ok(jet_rep),
        tas!(b"met") => Ok(jet_met),
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
        tas!(b"rsh") => true,
        _ => false,
    }
}
