pub mod math;

use crate::interpreter::{slot, raw_slot};
use crate::jets::math::*;
use crate::mem::NockStack;
use crate::mem::Preserve;
use crate::mug::mug_u32;
use crate::noun::{self, Noun, Cell, Atom, D, T};
use crate::hamt::Hamt;
use ares_macros::tas;
use assert_no_alloc::{assert_no_alloc, permit_alloc};
use std::mem::size_of;

use colored::*;

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

/**
    +=  location    $:  pattern=(each static dynamic)
                        name=term
                        hooks=(map term axis)
                    ==
    +=  static      (each payload=* parent=location)
    +=  dynamic     [where=axis parent=location]
    ::
    +=  registry    [roots=(map * location) parents=(list parent)]
    +=  parent      (pair axis (map location location))
    ::
    +=  activation  $:  hot-index=@ud
                        drivers=(map axis @ud)
                        label=path
                        jit=* :: FIXME: should probably be (map battery *)
                              :: since there can be multiple batteries per location
                    ==
    +=  hot-info    $:  reg=registry
                        hot-index=@ud
                        drivers=(map axis @ud)
                        label=path
                    ==
    +=  bash        @  :: battery hash (sha-256 based)
    ::
    +=  hot         (map bash hot-info)
    +=  cold        (map battery=^ (pair bash registry))
    +=  warm        (map location activation)
*/

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
    pub fn new() -> Self {
        Self {
            path_to_batteries: Hamt::new(),
            battery_to_paths: Hamt::new(),
        }
    }

    //TODO use From trait?
    /** For snapshotting, dump the cold state as a list of path X battery hierarchy pairs */
    fn as_noun(&mut self, stack: &mut NockStack) -> Noun {
        todo!()
    }

    //TODO use From trait?
    /** For snapshotting, restore the cold state from a list of path X battery hierarchy pairs */
    fn from_noun(noun: &mut Noun, stack: &mut NockStack) -> Self {
        //  +=  cold        (map battery=^ (pair bash registry))
        // let mut cold = Cold { path_to_batteries: Hamt::new(), battery_to_paths: Hamt::new() };
        // let list = HList::from(*noun).into_iter();
        // for entry in list {
        //     let mut bat = entry.as_cell().unwrap().head();
        //     let mut pat = entry.as_cell().unwrap().tail();
        //     cold.path_to_batteries = cold.path_to_batteries.insert(stack, &mut pat, bat);
        //     cold.battery_to_paths = cold.battery_to_paths.insert(stack, &mut bat, pat);
        // };
        // cold
        todo!()
    }

    /** For import from a portable snapshot, restore the cold state from a cued portable snapshot
     */
    fn from_portable_snapshot(snapshot: &mut Noun, stack: &mut NockStack) -> Self {
        todo!()
    }

    /** Register a core */
    pub fn register(&mut self, stack: &mut NockStack, core: &mut Noun, chum: &mut Noun, parent_axis: &Atom, hot: &Hot, warm: &mut Warm) {
        println!("{} {:?}", "register".red(), chum);
        let mut battery = raw_slot(*core, 2);
        let path_frag = T(stack, &[parent_axis.as_noun(), *chum]);

        if parent_axis.is_direct() {
            if parent_axis.as_direct().unwrap().data() == 0 {
                // root
                let mut path = T(stack, &[path_frag, D(0)]);
                println!("root path: {:?}", path);
                self.battery_to_paths = self.battery_to_paths.insert(stack, &mut battery, path);
                self.path_to_batteries = self.path_to_batteries.insert(stack, &mut path, battery);
                //TODO do return
                return ()
            }
        };

        let parent_core = slot(*core, parent_axis.as_bitslice());
        let mut parent_battery = raw_slot(parent_core, 2);
        let Some(parent_core_paths) = self.battery_to_paths.lookup(stack, &mut parent_battery) else {
            println!("{}", "missing parent core path".yellow());
            //TODO do return
            return ()
        };
        let paths_list = HList::from(parent_core_paths);
        let mut paths = D(0);
        for path in paths_list.into_iter() {
            let new_path = T(stack, &[path_frag, path]);
            if paths.is_direct() {
                // It is only direct when first initialized, since all path elements are cells of axis and path
                paths = new_path;
            } else {
                paths = T(stack, &[new_path, paths]);
            }
        }

        println!("{} {:?}", "paths".yellow(), paths);
        println!("{} {:?}", "battery".yellow(), battery);
        self.battery_to_paths = self.battery_to_paths.insert(stack, &mut battery, paths);
        let paths_list_2 = HList::from(paths);
        for mut path in paths_list_2.into_iter() {
            self.path_to_batteries = self.path_to_batteries.insert(stack, &mut path, battery);
            self.warm_add(stack, hot, &mut path, core, warm);
        }
    }

    fn warm_add(&self, stack: &mut NockStack, hot: &Hot, path: &mut Noun, core: &mut Noun, warm: &mut Warm) {
        if let Some(hot_entry) = hot.jets.lookup(stack, path) {
            let mut battery_vec: Vec<Noun> = vec![];
            let mut path = *path;

            // construct formula
            let mut formula = slot(*core, hot_entry.axis.as_bitslice());

            // construct nested batteries
            let mut battery: Noun;
            while path.is_cell() {
                battery = self.path_to_batteries.lookup(stack, &mut path).unwrap();
                let axis = path.as_cell().unwrap().head().as_cell().unwrap().head();
                println!("axis: {:?}", axis);
                let axis_battery = T(stack, &[axis, battery]);
                battery_vec.push(axis_battery);
                path = path.as_cell().unwrap().tail();
            }

            let mut batteries: Noun = D(0);
            for bat in battery_vec.into_iter().rev() {
                batteries = T(stack, &[bat, batteries]);
            }
            //TODO I think we don't care about the last battery, since if its a gate, this is just
            // the formula itself, which is already matched against
            if batteries.is_cell() {
                batteries = batteries.as_cell().unwrap().tail();
            }
            println!("batteries: {:?}", batteries);

            let warm_entry = WarmEntry {
                jet: hot_entry.jet,
                batteries,
                next: None
            };

            warm.jets = warm.jets.insert(stack, &mut formula, warm_entry);
        }
    }

    /** Regenerate warm state */
    fn warm(&mut self, stack: &mut NockStack, hot: Hot) -> Warm {
        todo!()
    }
}

//#[repr(packed)]
#[derive(Copy,Clone)]
struct HotEntry {
    axis: Atom,
    jet: Jet,
}

#[repr(packed)]
pub struct Hot {
    jets: Hamt<HotEntry>,
}

impl Hot {
    pub fn new(stack: &mut NockStack) -> Self {
        let k_139 = T(stack, &[D(0), D(tas!(b"k")), D(139)]);
        let one = T(stack, &[D(3), D(tas!(b"one"))]);

        let dec = T(stack, &[D(7), D(tas!(b"dec"))]);
        let mut dec_path = T(stack, &[dec, one, k_139, D(0)]);
        let dec_jet = HotEntry {
            axis: D(2).as_atom().unwrap(),
            jet: jet_dec,
        };

        let add = T(stack, &[D(7), D(tas!(b"add"))]);
        let mut add_path = T(stack, &[add, one, k_139, D(0)]);
        let add_jet = HotEntry {
            axis: D(2).as_atom().unwrap(),
            jet: jet_add,
        };

        let div = T(stack, &[D(7), D(tas!(b"div"))]);
        let mut div_path = T(stack, &[div, one, k_139, D(0)]);
        let div_jet = HotEntry {
            axis: D(2).as_atom().unwrap(),
            jet: jet_div,
        };

        let dvr = T(stack, &[D(7), D(tas!(b"dvr"))]);
        let mut dvr_path = T(stack, &[dvr, one, k_139, D(0)]);
        let dvr_jet = HotEntry {
            axis: D(2).as_atom().unwrap(),
            jet: jet_dvr,
        };

        let gte = T(stack, &[D(7), D(tas!(b"gte"))]);
        let mut gte_path = T(stack, &[gte, one, k_139, D(0)]);
        let gte_jet = HotEntry {
            axis: D(2).as_atom().unwrap(),
            jet: jet_gte,
        };

        let gth = T(stack, &[D(7), D(tas!(b"gth"))]);
        let mut gth_path = T(stack, &[gth, one, k_139, D(0)]);
        let gth_jet = HotEntry {
            axis: D(2).as_atom().unwrap(),
            jet: jet_gth,
        };

        let mut jets: Hamt<HotEntry> = Hamt::new();
        jets = jets.insert(stack, &mut dec_path, dec_jet);
        jets = jets.insert(stack, &mut add_path, add_jet);
        jets = jets.insert(stack, &mut div_path, div_jet);
        jets = jets.insert(stack, &mut dvr_path, dvr_jet);
        jets = jets.insert(stack, &mut gte_path, gte_jet);
        jets = jets.insert(stack, &mut gth_path, gth_jet);

        Self { jets }
    }
}

//#[repr(packed)]
#[derive(Copy,Clone)]
pub struct WarmEntry {
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
    pub jets: Hamt<WarmEntry>,
}

impl Preserve for Warm {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        self.jets.preserve(stack);
    }
}

impl Warm {
    pub fn new() -> Self {
        Self { jets: Hamt::new() }
    }

    pub fn get_jet(&mut self, stack: &mut NockStack, formula: &mut Noun, subject: Noun) -> Option<Jet> {
        if let Some(warm_entry) = self.jets.lookup(stack, formula) {
            println!("{}", "MATCH!".red());

            let mut our_subject = subject;
            let mut try_batteries = warm_entry.batteries;

            while try_batteries.is_cell() {
                println!("{}", "loop".red());
                let try_ab = try_batteries.as_cell().unwrap().head().as_cell().unwrap();
                let try_axis = try_ab.head();
                let try_battery = try_ab.tail();
                println!("try_axis: {:?}", try_axis);
                println!("try_battery: {:?}", try_battery);
                if try_axis.is_direct() {
                    if try_axis.as_direct().unwrap().data() == 0 {
                        //TODO this needs to check that the root matches somehwow
                        let our_battery = raw_slot(our_subject, 3);
                        println!("our_battery: {:?}", our_battery);
                        println!("root");
                        return Some(warm_entry.jet);
                    }
                }
                let parent_core = slot(our_subject, try_axis.as_atom().unwrap().as_bitslice());
                let our_battery = raw_slot(parent_core, 2);
                println!("our_battery: {:?}", our_battery);

                if mug_u32(stack, try_battery) == mug_u32(stack, our_battery) {
                    println!("{}", "mug match".red());
                } else {
                    println!("jet mismatch");
                    return None
                }

                our_subject = slot(our_subject, try_axis.as_atom().unwrap().as_bitslice());
                println!("our_subject: {:?}", raw_slot(our_subject, 3));
                try_batteries = try_batteries.as_cell().unwrap().tail();
            }
        }
        None
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
