use crate::jets::*;
use crate::noun::{Atom, DirectAtom, Noun, D, T};
use ares_macros::tas;
use either::Either::{self, Left, Right};
use std::ptr::null_mut;

const A_50: Either<u64, (u64, u64)> = Right((tas!(b"a"), 50));
const K_139: Either<u64, (u64, u64)> = Right((tas!(b"k"), 139));

// This is the const state all in one spot as literals
#[allow(clippy::complexity)]
const SHAM_HOT_STATE: &[(&[Either<u64, (u64, u64)>], u64, Jet)] = &[
    (&[A_50, Left(tas!(b"add"))], 1, jet_add),
    (&[A_50, Left(tas!(b"dec"))], 1, jet_dec),
    (&[A_50, Left(tas!(b"div"))], 1, jet_div),
    (&[A_50, Left(tas!(b"dvr"))], 1, jet_dvr),
    (&[A_50, Left(tas!(b"gth"))], 1, jet_gth),
    (&[A_50, Left(tas!(b"gte"))], 1, jet_gte),
    (&[A_50, Left(tas!(b"lte"))], 1, jet_lte),
    (&[A_50, Left(tas!(b"lth"))], 1, jet_lth),
    (&[A_50, Left(tas!(b"mod"))], 1, jet_mod),
    (&[A_50, Left(tas!(b"mul"))], 1, jet_mul),
    (&[A_50, Left(tas!(b"sub"))], 1, jet_sub),
    //
    (&[A_50, Left(tas!(b"cap"))], 1, jet_cap),
    (&[A_50, Left(tas!(b"mas"))], 1, jet_mas),
    //
    (&[A_50, Left(tas!(b"lent"))], 1, jet_lent),
    (&[A_50, Left(tas!(b"flop"))], 1, jet_flop),
    //
    (&[A_50, Left(tas!(b"bex"))], 1, jet_bex),
    (&[A_50, Left(tas!(b"can"))], 1, jet_can),
    (&[A_50, Left(tas!(b"cat"))], 1, jet_cat),
    (&[A_50, Left(tas!(b"cut"))], 1, jet_cut),
    (&[A_50, Left(tas!(b"end"))], 1, jet_end),
    (&[A_50, Left(tas!(b"lsh"))], 1, jet_lsh),
    (&[A_50, Left(tas!(b"met"))], 1, jet_met),
    (&[A_50, Left(tas!(b"rap"))], 1, jet_rap),
    (&[A_50, Left(tas!(b"rep"))], 1, jet_rep),
    (&[A_50, Left(tas!(b"rev"))], 1, jet_rev),
    (&[A_50, Left(tas!(b"rip"))], 1, jet_rip),
    (&[A_50, Left(tas!(b"rsh"))], 1, jet_rsh),
    (&[A_50, Left(tas!(b"xeb"))], 1, jet_xeb),
    //
    (&[A_50, Left(tas!(b"con"))], 1, jet_con),
    (&[A_50, Left(tas!(b"dis"))], 1, jet_dis),
    (&[A_50, Left(tas!(b"mix"))], 1, jet_mix),
    //
    (&[A_50, Left(tas!(b"mug"))], 1, jet_mug),
    //
    (&[A_50, Left(tas!(b"dor"))], 1, jet_dor),
    (&[A_50, Left(tas!(b"gor"))], 1, jet_gor),
    (&[A_50, Left(tas!(b"mor"))], 1, jet_mor),
    //
    (&[A_50, Left(tas!(b"scow"))], 1, jet_scow),
    //
    (&[A_50, Left(tas!(b"mink"))], 1, jet_mink),
];

#[allow(clippy::complexity)]
const TRUE_HOT_STATE: &[(&[Either<u64, (u64, u64)>], u64, Jet)] = &[
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"add"))], 1, jet_add),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"dec"))], 1, jet_dec),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"div"))], 1, jet_div),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"dvr"))], 1, jet_dvr),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"gte"))], 1, jet_gte),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"gth"))], 1, jet_gth),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"lte"))], 1, jet_lte),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"lth"))], 1, jet_lth),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"max"))], 1, jet_max),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"min"))], 1, jet_min),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"mod"))], 1, jet_mod),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"mul"))], 1, jet_mul),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"sub"))], 1, jet_sub),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"cap"))], 1, jet_cap),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"mas"))], 1, jet_mas),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"peg"))], 1, jet_peg),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"flop"))], 1, jet_flop),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"lent"))], 1, jet_lent),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"zing"))], 1, jet_zing),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"bex"))], 1, jet_bex),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"can"))], 1, jet_can),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"cat"))], 1, jet_cat),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"cut"))], 1, jet_cut),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"end"))], 1, jet_end),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"lsh"))], 1, jet_lsh),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"met"))], 1, jet_met),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"rap"))], 1, jet_rap),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"rep"))], 1, jet_rep),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"rev"))], 1, jet_rev),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"rip"))], 1, jet_rip),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"rsh"))], 1, jet_rsh),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"xeb"))], 1, jet_xeb),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"con"))], 1, jet_con),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"dis"))], 1, jet_dis),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"mix"))], 1, jet_mix),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"mug"))], 1, jet_mug),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"dor"))], 1, jet_dor),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"gor"))], 1, jet_gor),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"mor"))], 1, jet_mor),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"cue"))], 1, jet_cue),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"jam"))], 1, jet_jam),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"scow"))], 1, jet_scow),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"mink"))], 1, jet_mink),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"mole"))], 1, jet_mole),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"mule"))], 1, jet_mule),
    //
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"pen")), Left(tas!(b"ut")), Left(tas!(b"crop"))], 1, jet_ut_crop),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"pen")), Left(tas!(b"ut")), Left(tas!(b"fish"))], 1, jet_ut_fish),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"pen")), Left(tas!(b"ut")), Left(tas!(b"fuse"))], 1, jet_ut_fuse),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"pen")), Left(tas!(b"ut")), Left(tas!(b"mint"))], 1, jet_ut_mint),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"pen")), Left(tas!(b"ut")), Left(tas!(b"mull"))], 1, jet_ut_mull),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"pen")), Left(tas!(b"ut")), Left(tas!(b"nest"))], Left(tas!(b"nest-in")), Left(tas!(b"nest-dext")), 1, jet_ut_nest_dext),
    (&[K_139, Left(tas!(b"one")), Left(tas!(b"two")), Left(tas!(b"tri")), Left(tas!(b"qua")), Left(tas!(b"pen")), Left(tas!(b"ut")), Left(tas!(b"rest"))], 1, jet_ut_rest),
];

#[derive(Copy, Clone)]
pub struct Hot(*mut HotMem);

impl Hot {
    pub fn init(stack: &mut NockStack) -> Self {
        unsafe {
            let mut next = Hot(null_mut());
            for (htap, axe, jet) in HOT_STATE {
                let mut a_path = D(0);
                for i in *htap {
                    match i {
                        Left(tas) => {
                            a_path = T(
                                stack,
                                &[DirectAtom::new_panic(*tas).as_atom().as_noun(), a_path],
                            );
                        }
                        Right((tas, ver)) => {
                            let chum = T(
                                stack,
                                &[
                                    DirectAtom::new_panic(*tas).as_atom().as_noun(),
                                    DirectAtom::new_panic(*ver).as_atom().as_noun(),
                                ],
                            );
                            a_path = T(stack, &[chum, a_path]);
                        }
                    };
                }
                let axis = DirectAtom::new_panic(*axe).as_atom();
                let hot_mem_ptr: *mut HotMem = stack.struct_alloc(1);
                *hot_mem_ptr = HotMem {
                    a_path,
                    axis,
                    jet: *jet,
                    next,
                };
                next = Hot(hot_mem_ptr);
            }
            next
        }
    }
}

impl Iterator for Hot {
    type Item = (Noun, Atom, Jet); // path,axis,jet
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            return None;
        }
        unsafe {
            let res = ((*(self.0)).a_path, (*(self.0)).axis, (*(self.0)).jet);
            *self = (*(self.0)).next;
            Some(res)
        }
    }
}

struct HotMem {
    a_path: Noun,
    axis: Atom, // Axis of jetted formula in *battery*;
    jet: Jet,
    next: Hot,
}
