pub mod math;

use crate::jets::math::*;
use crate::mem::NockStack;
use crate::mem::Preserve;
use crate::noun::{self, Noun, Cell, Atom};
use crate::hamt::Hamt;
use ares_macros::tas;
use std::mem::size_of;

crate::gdb!();

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

#[repr(packed)]
pub struct Cold {
    path_to_batteries: Hamt<Noun>,
    battery_to_paths: Hamt<Noun>,
}

impl Preserve for Cold {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        self.path_to_batteries.preserve(stack);
        self.battery_to_paths.preserve(stack);
    }
}

impl Cold {
    /** For snapshotting, dump the cold state as a list of path X battery hierarchy pairs */
    fn as_noun(&mut self, stack: &mut NockStack) -> Noun {
        todo!()
    }

    /** For snapshotting, restore the cold state from a list of path X battery hierarchy pairs */
    fn from_noun(noun: &mut Noun, stack: &mut NockStack) -> Self {
        todo!()
    }

    /** For import from a portable snapshot, restore the cold state from a cued portable snapshot
     */
    fn from_portable_snapshot(snapshot: &mut Noun, stack: &mut NockStack) -> Self {
        todo!()
    }

    /** Register a core */
    fn register(&mut self, stack: &mut NockStack, core: &mut Noun, chum: &mut Noun, parent: &mut Atom) -> Warm {
        todo!()
    }

    /** Regenerate warm state */
    fn warm(&mut self, stack: &mut NockStack) -> Warm {
        todo!()
    }
}


#[repr(packed)]
#[derive(Copy,Clone)]
struct WarmEntry {
    jet: Jet,
    batteries: Noun,
    next: Option<*mut WarmEntry>
}

impl Preserve for WarmEntry {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        let mut dest = stack.struct_alloc_in_previous_frame(size_of::<WarmEntry>());
        loop {
            let mut batt_tmp = self.batteries;
            batt_tmp.preserve(stack);
            // no need to preserve jet as its not allocated on the nockstack
            self.batteries = batt_tmp;
            *dest = *self;
            if let Some(next_ptr) = self.next {
                let new_dest = stack.struct_alloc_in_previous_frame(size_of::<WarmEntry>());
                (*dest).next = Some(new_dest);
                dest = new_dest;
            } else {
                break;
            }
        }
    }
}

#[repr(packed)]
pub struct Warm {
    jets: Hamt<WarmEntry>,
}

impl Preserve for Warm {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        self.jets.preserve(stack);
    }
}

impl Warm {
    pub fn get_jet(&mut self, stack: &mut NockStack, formula: &mut Noun, subject: &mut Noun) -> Option<Jet> {
        todo!()
    }
}

//TODO move this somewhere else
#[derive(Copy, Clone)]
pub struct HList {
    head: Option<Cell>,
}

impl From<Noun> for HList {
    fn from(n: Noun) -> Self {
        if n.is_cell() {
            HList::from(n.as_cell().unwrap())
        } else {
            HList { head: None }
        }
    }
}

impl From<Cell> for HList {
    fn from(c: Cell) -> Self {
        Self { head: Some(c) }
    }
}

#[derive(Copy, Clone)]
pub struct HListIntoIter {
    next: Option<Cell>,
}

impl IntoIterator for HList {
    type Item = Noun;
    type IntoIter = HListIntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        HListIntoIter { next: self.head }
    }
}

impl Iterator for HListIntoIter {
    type Item = Noun;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|cell| {
            let tail = cell.tail();
            self.next = if tail.is_cell() {
                Some(tail.as_cell().unwrap())
            } else {
                None
            };
            cell.head()
        })
    }
}
