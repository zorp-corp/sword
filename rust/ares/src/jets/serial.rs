use crate::interpreter::Context;
use crate::jets::util::*;

use crate::jets::Result;
use crate::noun::{Noun};

use crate::serialization::{cue, jam};

crate::gdb!();

pub fn jet_cue(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    let atom = sam.as_atom()?;

    Ok(cue(stack, atom))
}

pub fn jet_jam(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    Ok(jam(stack, sam).as_noun())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{
        assert_jet, assert_jet_err, assert_jet_ubig, assert_nary_jet_ubig, init_stack, A,
    };
    use crate::jets::{Jet, JetErr};
    use crate::mem::NockStack;
    use crate::noun::{Noun, D, NO, T, YES};

    #[test]
    fn test_jam() {
        let s = &mut init_stack();
        assert_jet(s, jet_jam, D(0x0), D(0x2));
        assert_jet(s, jet_jam, D(0x1), D(0xc));
        let sam = T(s, &[D(0x0), D(0x0)]);
        assert_jet(s, jet_jam, sam, D(0x29));
        let sam = T(s, &[D(0x1), D(0x2), D(0x3), D(0x0)]);
        assert_jet(s, jet_jam, sam, D(0x2d0c871));
    }

    #[test]
    fn test_cue() {
        let s = &mut init_stack();
        assert_jet(s, jet_cue, D(0x2), D(0x0));
        assert_jet(s, jet_cue, D(0xc), D(0x1));
        let res = T(s, &[D(0x0), D(0x0)]);
        assert_jet(s, jet_cue, D(0x29), res);
        let res = T(s, &[D(0x1), D(0x2), D(0x3), D(0x0)]);
        assert_jet(s, jet_cue, D(0x2d0c871), res);
    }
}