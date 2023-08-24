/** Tree jets
 */
use crate::jets;
use crate::jets::JetErr::*;
use crate::jets::util::{met, slot};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Noun, D};

crate::gdb!();

pub fn jet_cap(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::JetErr;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_stack};
    use crate::noun::D;

    #[test]
    fn test_cap() {
        let s = &mut init_stack();

        assert_jet_err(s, jet_cap, D(0), JetErr::Deterministic);
        assert_jet_err(s, jet_cap, D(1), JetErr::Deterministic);

        assert_jet(s, jet_cap, D(2), D(2));
        assert_jet(s, jet_cap, D(3), D(3));
        assert_jet(s, jet_cap, D(4), D(2));
        assert_jet(s, jet_cap, D(5), D(2));
        assert_jet(s, jet_cap, D(6), D(3));
        assert_jet(s, jet_cap, D(7), D(3));
        assert_jet(s, jet_cap, D(8), D(2));
    }
}
