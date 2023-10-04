use crate::jets::util::*;
/** Hash jets
 */
use crate::jets::Result;
use crate::mem::NockStack;
use crate::mug::mug;
use crate::newt::Newt;
use crate::noun::Noun;

crate::gdb!();

pub fn jet_mug(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    Ok(mug(stack, arg).as_noun())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, init_stack, A};
    use crate::mem::NockStack;
    use crate::noun::{Noun, D, T};
    use ibig::ubig;

    fn atoms(s: &mut NockStack) -> (Noun, Noun, Noun, Noun, Noun) {
        (atom_0(s), atom_24(s), atom_63(s), atom_96(s), atom_128(s))
    }

    fn atom_0(_stack: &mut NockStack) -> Noun {
        D(0)
    }

    fn atom_24(_stack: &mut NockStack) -> Noun {
        D(0x876543)
    }

    fn atom_63(_stack: &mut NockStack) -> Noun {
        D(0x7fffffffffffffff)
    }

    fn atom_96(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xfaceb00c15deadbeef123456))
    }

    fn atom_128(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xdeadbeef12345678fedcba9876543210))
    }

    #[test]
    fn test_mug() {
        let s = &mut init_stack();
        let (a0, a24, a63, a96, a128) = atoms(s);
        assert_jet(s, jet_mug, a0, D(0x79ff04e8));
        assert_jet(s, jet_mug, a24, D(0x69d59d90));
        assert_jet(s, jet_mug, a63, D(0x7a9f252e));
        assert_jet(s, jet_mug, a96, D(0x2aa4c8fb));
        assert_jet(s, jet_mug, a128, D(0x44fb2c0c));
        let sam = T(s, &[a128, a128]);
        assert_jet(s, jet_mug, sam, D(0x61c0ea5c));
        let sam = T(s, &[a96, a128]);
        assert_jet(s, jet_mug, sam, D(0x20fb143f));
        let sam = T(s, &[a0, a0]);
        assert_jet(s, jet_mug, sam, D(0x192f5588));
        let sam = T(s, &[a0, a24, a63, a96, a128]);
        let sam = T(s, &[sam, a0, a24, a63, a96, a128]);
        let sam = T(s, &[sam, a0, a24, a63, a96, a128]);
        assert_jet(s, jet_mug, sam, D(0x7543cac7));
    }
}
