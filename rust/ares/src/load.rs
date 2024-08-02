use crate::jets::util::slot;
use crate::mem::NockStack;
use crate::mug::mug_u32;
use crate::noun::{Noun, D};
use crate::serialization::cue_bytes;
use ares_macros::tas;
use std::include_bytes;

// formula, subject
pub fn load_cg(stack: &mut NockStack) -> (Noun, Noun) {
    let cg_bytes = include_bytes!("../bin/cg.jam");
    let cg_noun = cue_bytes(stack, cg_bytes);
    let _cg_mug = mug_u32(stack, cg_noun);
    // eprintln!("codegen mug: {:x}", cg_mug);
    assert!(unsafe { slot(cg_noun, 2).unwrap().raw_equals(D(tas!(b"cg"))) });
    (slot(cg_noun, 6).unwrap(), slot(cg_noun, 7).unwrap())
}
