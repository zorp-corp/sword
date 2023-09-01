pub mod math;
pub mod mink;
pub mod tree;

use crate::jets::math::*;
use crate::jets::mink::*;
use crate::jets::tree::*;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{self, Noun, Slots};
use ares_macros::tas;

crate::gdb!();

/// Return Err if the computation crashed or should punt to Nock
pub type Result = std::result::Result<Noun, JetErr>;
pub type Jet = fn(&mut NockStack, &mut Option<&mut Newt>, Noun) -> Result;

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

impl From<noun::Error> for JetErr {
    fn from(_err: noun::Error) -> Self {
        Self::NonDeterministic
    }
}

impl From<JetErr> for () {
    fn from(_: JetErr) -> Self {}
}

pub fn get_jet(jet_name: Noun) -> Option<Jet> {
    match jet_name.as_direct().ok()?.data() {
        tas!(b"dec") => Some(jet_dec),
        tas!(b"add") => Some(jet_add),
        tas!(b"sub") => Some(jet_sub),
        tas!(b"mul") => Some(jet_mul),
        tas!(b"div") => Some(jet_div),
        tas!(b"mod") => Some(jet_mod),
        tas!(b"dvr") => Some(jet_dvr),
        tas!(b"lth") => Some(jet_lth),
        tas!(b"lte") => Some(jet_lte),
        tas!(b"gth") => Some(jet_gth),
        tas!(b"gte") => Some(jet_gte),
        tas!(b"bex") => Some(jet_bex),
        tas!(b"lsh") => Some(jet_lsh),
        tas!(b"rsh") => Some(jet_rsh),
        tas!(b"con") => Some(jet_con),
        tas!(b"dis") => Some(jet_dis),
        tas!(b"mix") => Some(jet_mix),
        tas!(b"end") => Some(jet_end),
        tas!(b"cat") => Some(jet_cat),
        tas!(b"cut") => Some(jet_cut),
        tas!(b"can") => Some(jet_can),
        tas!(b"rep") => Some(jet_rep),
        tas!(b"rip") => Some(jet_rip),
        tas!(b"met") => Some(jet_met),
        tas!(b"mug") => Some(jet_mug),
        tas!(b"rev") => Some(jet_rev),
        //
        tas!(b"cap") => Some(jet_cap),
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

pub fn slot(noun: Noun, axis: u64) -> Result {
    noun.slot(axis).map_err(|_e| JetErr::Deterministic)
}
