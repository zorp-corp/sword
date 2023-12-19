/** Text processing jets
 */
use crate::interpreter::Context;
use crate::jets::util::slot;
use crate::jets::Result;
use crate::noun::{Noun, D};

crate::gdb!();

pub fn jet_flop(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    util::flop(&mut context.stack, sam)
}

pub fn jet_lent(_context: &mut Context, subject: Noun) -> Result {
    let list = slot(subject, 6)?;
    util::lent(list).map(|x| D(x as u64))
}

pub fn jet_zing(context: &mut Context, subject: Noun) -> Result {
    let list = slot(subject, 6)?;
    let stack = &mut context.stack;

    util::zing(stack, list)
}

pub mod util {
    use crate::interpreter::Error;
    use crate::jets::{JetErr, Result};
    use crate::mem::NockStack;
    use crate::noun::{Cell, Noun, D, T};
    use std::result;

    /// Reverse order of list
    pub fn flop(stack: &mut NockStack, noun: Noun) -> Result {
        let mut list = noun;
        let mut tsil = D(0);
        loop {
            if unsafe { list.raw_equals(D(0)) } {
                break;
            }

            let cell = list.as_cell()?;
            tsil = T(stack, &[cell.head(), tsil]);
            list = cell.tail();
        }

        Ok(tsil)
    }

    pub fn lent(tape: Noun) -> result::Result<usize, JetErr> {
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

    pub fn zing(stack: &mut NockStack, mut list: Noun) -> Result {
        unsafe {
            let mut res: Noun = D(0);
            let mut dest = &mut res as *mut Noun;

            while !list.raw_equals(D(0)) {
                let pair = list.as_cell()?;
                let mut sublist = pair.head();
                list = pair.tail();

                while !sublist.raw_equals(D(0)) {
                    if sublist.is_atom()?;
                    let it = sublist.as_cell()?;
                    let i = it.head();
                    sublist = it.tail();

                    let (new_cell, new_memory) = Cell::new_raw_mut(stack);
                    (*new_memory).head = i;
                    *dest = new_cell.as_noun();
                    dest = &mut (*new_memory).tail;
                }
            }

            *dest = D(0);
            Ok(res)
        }
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

        assert_jet_err(c, jet_flop, D(1), JetErr::Fail(Error::Deterministic(D(0))));
        let sam = T(&mut c.stack, &[D(1), D(2), D(3)]);
        assert_jet_err(c, jet_flop, sam, JetErr::Fail(Error::Deterministic(D(0))));
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

    #[test]
    fn test_zing() {
        let c = &mut init_context();

        let list_0 = T(&mut c.stack, &[D(0), D(0), D(0), D(0)]);
        let list_1 = T(&mut c.stack, &[D(1), D(2), D(3), D(0)]);
        let list_2 = T(&mut c.stack, &[D(4), D(5), D(6), D(0)]);
        let list_3 = T(&mut c.stack, &[D(1), D(2), D(3), D(4), D(5), D(6), D(0)]);

        assert_jet(c, jet_zing, D(0), D(0));
        assert_jet(c, jet_zing, list_0, D(0));
        let sam = T(&mut c.stack, &[list_0, D(0)]);
        assert_jet(c, jet_zing, sam, list_0);
        let sam = T(&mut c.stack, &[list_1, list_2, D(0)]);
        assert_jet(c, jet_zing, sam, list_3);
    }
}
