/** Text processing jets
 */
use crate::interpreter::Context;
use crate::jets::util::slot;
use crate::jets::Result;
use crate::noun::{Noun, D};

crate::gdb!();

pub fn jet_flop(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let src = slot(sam, 1)?;

    if unsafe { src.raw_equals(D(0)) } {
        return Ok(D(0));
    }

    // util::flop(&mut context.stack, src).map_err(|x| x.into())
    Ok(util::flop(&mut context.stack, src)?)
}

pub fn jet_lent(_context: &mut Context, subject: Noun) -> Result {
    let tape = slot(subject, 6)?;
    util::lent(tape).map(|x| D(x as u64))
}

pub mod util {
    use crate::interpreter::Error;
    use crate::jets::JetErr;
    use crate::mem::NockStack;
    use crate::noun::{Cell, Noun, D};
    use std::result::Result;

    /// Reverse order of list
    pub fn flop(stack: &mut NockStack, noun: Noun) -> Result<Noun, Error> {
        let mut list = noun;
        let mut tsil = D(0);
        loop {
            if let Some(list) = list.atom() {
                if list.as_bitslice().first_one().is_none() {
                    break;
                } else {
                    return Err(Error::Deterministic(D(0)));
                }
            }
            let cell = list.as_cell()?;
            tsil = Cell::new(stack, cell.head(), tsil).as_noun();
            list = cell.tail();
        }

        Ok(tsil)
    }

    pub fn lent(tape: Noun) -> Result<usize, JetErr> {
        let mut len = 0usize;
        let mut list = tape;
        loop {
            if let Some(atom) = list.atom() {
                if atom.as_bitslice().first_one().is_none() {
                    break;
                } else {
                    return Err(JetErr::Fail(Error::Deterministic(D(0))));
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
    use crate::interpreter::Error;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_context};
    use crate::jets::JetErr;
    use crate::noun::{D, T};

    #[test]
    fn test_flop() {
        let c = &mut init_context();

        let sam = T(&mut c.stack, &[D(1), D(2), D(3), D(0)]);
        let res = T(&mut c.stack, &[D(3), D(2), D(1), D(0)]);
        assert_jet(c, jet_flop, sam, res);

        #[rustfmt::skip]
        let sam = T(
            &mut c.stack,
            &[
                D(0xd), D(0xe), D(0xa), D(0xd), D(0xb), D(0xe), D(0xe), D(0xf),
                D(0x1), D(0x2), D(0x3), D(0x4), D(0x5), D(0x6), D(0x7), D(0x8),
                D(0xf), D(0xe), D(0xd), D(0xc), D(0xb), D(0xa), D(0x9), D(0x8),
                D(0x7), D(0x6), D(0x5), D(0x4), D(0x3), D(0x2), D(0x1), D(0x0),
                D(0x0),
            ],
        );
        #[rustfmt::skip]
        let res = T(
            &mut c.stack,
            &[
                D(0x0), D(0x1), D(0x2), D(0x3), D(0x4), D(0x5), D(0x6), D(0x7),
                D(0x8), D(0x9), D(0xa), D(0xb), D(0xc), D(0xd), D(0xe), D(0xf),
                D(0x8), D(0x7), D(0x6), D(0x5), D(0x4), D(0x3), D(0x2), D(0x1),
                D(0xf), D(0xe), D(0xe), D(0xb), D(0xd), D(0xa), D(0xe), D(0xd),
                D(0x0),
            ],
        );
        assert_jet(c, jet_flop, sam, res);
    }

    #[test]
    fn test_lent() {
        let c = &mut init_context();

        assert_jet(c, jet_lent, D(0), D(0));
        let sam = T(&mut c.stack, &[D(1), D(2), D(3), D(0)]);
        assert_jet(c, jet_lent, sam, D(3));
        let sam = T(&mut c.stack, &[D(3), D(2), D(1), D(0)]);
        assert_jet(c, jet_lent, sam, D(3));
        assert_jet_err(c, jet_lent, D(1), JetErr::Fail(Error::Deterministic(D(0))));
        let sam = T(&mut c.stack, &[D(3), D(2), D(1)]);
        assert_jet_err(c, jet_lent, sam, JetErr::Fail(Error::Deterministic(D(0))));
    }
}
