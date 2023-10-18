use crate::jets::*;
use crate::noun::{Atom, DirectAtom, Noun, D, T};
use ares_macros::tas;
use either::Either::{self, Left, Right};
use std::ptr::null_mut;

const A_50: Either<u64, (u64, u64)> = Right((tas!(b"a"), 50));

// This is the const state all in one spot as literals
#[allow(clippy::complexity)]
const HOT_STATE: &[(&[Either<u64, (u64, u64)>], u64, Jet)] = &[
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
