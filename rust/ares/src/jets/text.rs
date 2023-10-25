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

pub fn jet_weld(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;
    util::weld(context.stack, a, b)
}

pub mod util {
    use crate::jets::JetErr;
    use crate::noun::{Noun, Cell, D, T};
    use crate::mem::NockStack;

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

/*
let len = (met(bloq, atom) + step - 1) / step;
let mut list = D(0);
for i in (0..len).rev() {
    let new_atom = unsafe {
        let (mut new_indirect, new_slice) =
            IndirectAtom::new_raw_mut_bitslice(stack, step << bloq);
        chop(bloq, i * step, step, 0, new_slice, atom.as_bitslice())?;
        new_indirect.normalize_as_atom()
    };
    list = Cell::new(stack, new_atom.as_noun(), list).as_noun();
}
*/

    // combine two lists into one
    pub fn weld(stack: &mut NockStack, mut left: Noun, mut right: Noun) -> Result<Noun, JetErr> {
        let mut result = D(0x0);

        // process to the null terminator at tail of left
        loop {
            if let Some(atom) = left.atom() {
                if atom.as_bitslice().first_one().is_none() {
                    break;
                } else {
                    return Err(JetErr::Deterministic);
                }
            }
            let cell = left.as_cell()?;
            eprintln!("weld: result: {:?}", result);
            eprintln!("weld: cell.head: {:?}", cell.head());
            result = T(stack, &[result, cell.head()]);
            //result = Cell::new(stack, result, cell.head()).as_noun();
            left = cell.tail();
        }
        
        // now we have a tuple of left w/o a null terminator
        // so append right
        loop {
            if let Some(atom) = right.atom() {
                if atom.as_bitslice().first_one().is_none() {
                    break;
                } else {
                    return Err(JetErr::Deterministic);
                }
            }
            let cell = right.as_cell()?;
            result = Cell::new(stack, result, cell.head()).as_noun();
            right = cell.tail();
        }
        
        result = Cell::new(stack, result, D(0x0)).as_noun();
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_stack};
    use crate::jets::JetErr;
    use crate::noun::{D, T};

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
    fn test_weld() {
        let s = &mut init_stack();

        let a = T(s, &[D(1), D(2), D(3), D(0)]);
        let b = T(s, &[D(4), D(5), D(6), D(0)]);
        let sam = T(s, &[a, b]);
        let res = T(s, &[D(1), D(2), D(3), D(4), D(5), D(6), D(0)]);
        assert_jet(s, jet_weld, sam, res);
    }
}
