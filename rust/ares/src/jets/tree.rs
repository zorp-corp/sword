/** Tree jets
 */
use crate::interpreter::{Context, Error};
use crate::jets;
use crate::jets::bits;
use crate::jets::util::*;
use crate::jets::{JetErr, Result};
use crate::noun::{Noun, D};

crate::gdb!();

pub fn jet_cap(_context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let tom = arg.as_atom()?;
    let met = met(0, tom);

    unsafe {
        if met < 2 {
            Err(JetErr::Fail(Error::Deterministic(D(0))))
        } else if *(tom.as_bitslice().get_unchecked(met - 2)) {
            Ok(D(3))
        } else {
            Ok(D(2))
        }
    }
}

pub fn jet_mas(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let arg = slot(subject, 6)?;
    let tom = arg.as_atom()?;
    let met = met(0, tom);

    if met < 2 {
        Err(JetErr::Fail(Error::Deterministic(D(0))))
    } else {
        let c = bex(stack, met - 1);
        let d = bex(stack, met - 2);
        let e = sub(stack, tom, c)?;

        Ok(con(stack, e, d).as_noun())
    }
}

pub fn jet_peg(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?;
    let b = slot(arg, 3)?;

    unsafe {
        if a.raw_equals(D(0)) {
            return Err(JetErr::Fail(Error::Deterministic(D(0))));
        }
        //  XX: Import jet mistmatch from Vere
        if b.raw_equals(D(0)) {
            return Err(JetErr::Fail(Error::Deterministic(D(0))));
        }
    };

    let c = met(0, b.as_atom()?);
    let d = c - 1;
    let e = bits::util::lsh(stack, 0, d, D(1).as_atom()?)?;
    let f = jets::util::sub(stack, b.as_atom()?, e)?;
    let g = bits::util::lsh(stack, 0, d, a.as_atom()?)?;
    Ok(jets::util::add(stack, f, g).as_noun())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::Error;
    use crate::jets::util::test::*;
    use crate::jets::JetErr;
    use crate::mem::NockStack;
    use crate::noun::{Noun, D, DIRECT_MAX};
    use ibig::ubig;

    fn atom_0(_stack: &mut NockStack) -> Noun {
        D(0x0)
    }

    fn atom_1(_stack: &mut NockStack) -> Noun {
        D(0x1)
    }

    fn atom_62(_stack: &mut NockStack) -> Noun {
        D(0x3fffffffffffffff)
    }

    fn atom_63(_stack: &mut NockStack) -> Noun {
        D(0x4000000000000000)
    }

    fn atom_65(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(_0x10000000000000000))
    }

    fn pos_2(_stack: &mut NockStack) -> Noun {
        D(2)
    }

    fn pos_3(_stack: &mut NockStack) -> Noun {
        D(3)
    }

    fn pos_4(_stack: &mut NockStack) -> Noun {
        D(4)
    }

    #[test]
    fn test_cap() {
        let c = &mut init_context();

        assert_jet_err(c, jet_cap, D(0), JetErr::Fail(Error::Deterministic(D(0))));
        assert_jet_err(c, jet_cap, D(1), JetErr::Fail(Error::Deterministic(D(0))));

        assert_jet(c, jet_cap, D(2), D(2));
        assert_jet(c, jet_cap, D(3), D(3));
        assert_jet(c, jet_cap, D(4), D(2));
        assert_jet(c, jet_cap, D(5), D(2));
        assert_jet(c, jet_cap, D(6), D(3));
        assert_jet(c, jet_cap, D(7), D(3));
        assert_jet(c, jet_cap, D(8), D(2));
    }

    #[test]
    fn test_mas() {
        let c = &mut init_context();

        assert_jet_err(c, jet_mas, D(0), JetErr::Fail(Error::Deterministic(D(0))));
        assert_jet_err(c, jet_mas, D(1), JetErr::Fail(Error::Deterministic(D(0))));

        assert_jet(c, jet_mas, D(2), D(1));
        assert_jet(c, jet_mas, D(3), D(1));
        assert_jet(c, jet_mas, D(4), D(2));
        assert_jet(c, jet_mas, D(5), D(3));
        assert_jet(c, jet_mas, D(6), D(2));
        assert_jet(c, jet_mas, D(7), D(3));
        assert_jet(c, jet_mas, D(8), D(4));
    }

    #[test]
    fn test_peg() {
        let c = &mut init_context();

        assert_common_jet_err(
            c,
            jet_peg,
            &[atom_0, atom_1],
            JetErr::Fail(Error::Deterministic(D(0))),
        );
        assert_common_jet_err(
            c,
            jet_peg,
            &[atom_1, atom_0],
            JetErr::Fail(Error::Deterministic(D(0))),
        );

        // Test direct
        assert_common_jet_noun(c, jet_peg, &[pos_2, pos_3], D(5));
        assert_common_jet_noun(c, jet_peg, &[pos_4, pos_4], D(16));

        // Test direct with overflow.
        assert_common_jet_noun(c, jet_peg, &[atom_62, pos_3], D(DIRECT_MAX));
        assert_common_jet(c, jet_peg, &[pos_2, atom_63], ubig!(0x8000000000000000));

        // Test indirect.
        assert_common_jet(c, jet_peg, &[atom_65, atom_1], ubig!(_0x10000000000000000));
        assert_common_jet(c, jet_peg, &[atom_1, atom_65], ubig!(_0x10000000000000000));
        assert_common_jet(
            c,
            jet_peg,
            &[atom_65, atom_65],
            ubig!(_0x100000000000000000000000000000000),
        );
    }
}
