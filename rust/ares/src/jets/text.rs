/** Text processing jets
 */
use crate::interpreter::Context;
use crate::jets::util::slot;
use crate::jets::Result;
use crate::noun::{Noun, D};

crate::gdb!();

pub fn jet_lent(_context: &mut Context, subject: Noun) -> Result {
    let tape = slot(subject, 6)?;
    util::lent(tape).map(|x| D(x as u64))
}

pub mod util {
    use crate::interpreter::Failure;
    use crate::jets::JetErr;
    use crate::noun::Noun;

    pub fn lent(tape: Noun) -> Result<usize, JetErr> {
        let mut len = 0usize;
        let mut list = tape;
        loop {
            if let Some(atom) = list.atom() {
                if atom.as_bitslice().first_one().is_none() {
                    break;
                } else {
                    return Err(JetErr::Fail(Failure::Deterministic));
                }
            }
            let cell = list.as_cell()?;
            // don't need checked_add or indirect atom result: 2^63-1 atoms would be 64 ebibytes
            len += 1;
            list = cell.tail();
        }
        Ok(len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::Failure;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_context};
    use crate::jets::JetErr;
    use crate::noun::{D, T};

    #[test]
    fn test_lent() {
        let c = &mut init_context();

        assert_jet(c, jet_lent, D(0), D(0));
        let sam = T(&mut c.stack, &[D(1), D(2), D(3), D(0)]);
        assert_jet(c, jet_lent, sam, D(3));
        let sam = T(&mut c.stack, &[D(3), D(2), D(1), D(0)]);
        assert_jet(c, jet_lent, sam, D(3));
        assert_jet_err(c, jet_lent, D(1), JetErr::Fail(Failure::Deterministic));
        let sam = T(&mut c.stack, &[D(3), D(2), D(1)]);
        assert_jet_err(c, jet_lent, sam, JetErr::Fail(Failure::Deterministic));
    }
}
