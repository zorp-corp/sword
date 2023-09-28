use crate::jets::util::slot;
/** Text processing jets
 */
use crate::jets::Result;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Noun, D};

crate::gdb!();

pub fn jet_lent(_stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let tape = slot(subject, 6)?;
    util::lent(tape).map(|x| D(x as u64))
}

pub fn jet_trip(_stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let atom = slot(subject, 6)?.as_atom()?;
    util::trip(_stack, atom)
}

pub mod util {
    use crate::jets::util::rip;
    use crate::jets::JetErr;
    use crate::mem::NockStack;
    use crate::noun::{Atom, Noun};

    pub fn lent(tape: Noun) -> Result<usize, JetErr> {
        let mut len = 0usize;
        let mut list = tape;
        loop {
            if let Some(atom) = list.atom() {
                if atom.as_bitslice().first_one().is_none() {
                    break;
                } else {
                    return Err(JetErr::Deterministic);
                }
            }
            let cell = list.as_cell()?;
            // don't need checked_add or indirect atom result: 2^63-1 atoms would be 64 ebibytes
            len += 1;
            list = cell.tail();
        }
        Ok(len)
    }

    pub fn trip(_stack: &mut NockStack, atom: Atom) -> Result<Noun, JetErr> {
        rip(_stack, 3, 1, atom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_neq, init_stack, A};
    use crate::jets::JetErr;
    use crate::noun::{tape, D, T};
    use ibig::ubig;

    #[test]
    fn test_lent() {
        let s = &mut init_stack();
        assert_jet(s, jet_lent, D(0), D(0));
        let sam = T(s, &[D(1), D(2), D(3), D(0)]);
        assert_jet(s, jet_lent, sam, D(3));
        let sam = T(s, &[D(3), D(2), D(1), D(0)]);
        assert_jet(s, jet_lent, sam, D(3));
        assert_jet_err(s, jet_lent, D(1), JetErr::Deterministic);
        let sam = T(s, &[D(3), D(2), D(1)]);
        assert_jet_err(s, jet_lent, sam, JetErr::Deterministic);
    }

    #[test]
    fn test_trip() {
        let s = &mut init_stack();
        assert_jet(s, jet_trip, D(0), D(0));
        let sam = A(
            s,
            &ubig!(_0x74756f2064656b6f6f6c207375616c7365636e655720676e694b20646f6f47),
        );
        let ret = tape(s, "Good King Wenceslaus looked out");
        assert_jet(s, jet_trip, sam, ret);
        let sam = A(s, &ubig!(_0x6e65687065745320666f20747361654620656874206e4f));
        let ret = tape(s, "On the Feast of Stephen");
        assert_jet(s, jet_trip, sam, ret);
        let sam = A(
            s,
            &ubig!(_0x74756f626120646e756f722079616c20776f6e7320656874206e656857),
        );
        let ret = tape(s, "When the snow lay round about");
        assert_jet(s, jet_trip, sam, ret);
        let sam = T(s, &[D(1), D(2), D(3), D(0)]);
        assert_jet_err(s, jet_trip, sam, JetErr::Deterministic);
        let sam = D(97); //  (trip 97) -> "a"
        let ret = tape(s, "b");
        assert_jet_neq(s, jet_trip, sam, ret);
    }
}
