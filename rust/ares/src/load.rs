use crate::interpreter::Error;
use crate::jets::util::slot;
use crate::mem::NockStack;
use crate::noun::{Noun, D};
use crate::serialization::cue_bytes;
use ares_macros::tas;
use assert_no_alloc::permit_alloc;
use crate::mug::mug_u32;
use std::include_bytes;

/// Return a (formula, trap) that can be kicked to get the codegen core
pub fn load_cg_trap(stack: &mut NockStack) -> Result<(Noun, Noun), Error> {
    let cg_bytes = include_bytes!("../bin/cg.jam");
    let cg_noun = cue_bytes(stack, cg_bytes);

    let cg_mug = mug_u32(stack, cg_noun);

    permit_alloc(|| {
        eprintln!("cg_noun mug: {:#x}", cg_mug);
    });

    if !unsafe { slot(cg_noun, 2)?.raw_equals(D(tas!(b"cg"))) } {
        Err(Error::Deterministic(D(0)))
    } else {
        Ok((slot(cg_noun, 6)?, slot(cg_noun, 7)?))
    }
}
