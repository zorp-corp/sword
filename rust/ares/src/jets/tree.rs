/** Tree jets
 */
use crate::interpreter::Context;
use crate::jets::util::*;
use crate::jets::JetErr::*;
use crate::jets::Result;
use crate::noun::{Noun, D};

crate::gdb!();

pub fn jet_cap(_context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let tom = arg.as_atom()?;
    let met = met(0, tom);

    unsafe {
        if met < 2 {
            Err(Deterministic)
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
        Err(Deterministic)
    } else {
        let c = bex(stack, met - 1);
        let d = bex(stack, met - 2);
        let e = sub(stack, tom, c)?;

        Ok(con(stack, e, d).as_noun())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_context};
    use crate::jets::JetErr;
    use crate::noun::D;

    #[test]
    fn test_cap() {
        let c = &mut init_context();

        assert_jet_err(c, jet_cap, D(0), JetErr::Deterministic);
        assert_jet_err(c, jet_cap, D(1), JetErr::Deterministic);

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

        assert_jet_err(c, jet_mas, D(0), JetErr::Deterministic);
        assert_jet_err(c, jet_mas, D(1), JetErr::Deterministic);

        assert_jet(c, jet_mas, D(2), D(1));
        assert_jet(c, jet_mas, D(3), D(1));
        assert_jet(c, jet_mas, D(4), D(2));
        assert_jet(c, jet_mas, D(5), D(3));
        assert_jet(c, jet_mas, D(6), D(2));
        assert_jet(c, jet_mas, D(7), D(3));
        assert_jet(c, jet_mas, D(8), D(4));
    }
}
