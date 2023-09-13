/** Formatting jets
 */
use crate::jets::Result;
use crate::jets::util::slot;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::Noun;

crate::gdb!();

pub fn jet_scow(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> Result {
    let aura = slot(subject, 12)?.as_direct()?;
    let atom = slot(subject, 13)?.as_atom()?;
    util::scow(stack, aura, atom)
}

pub mod util {
    use crate::jets;
    use crate::jets::JetErr;
    use crate::jets::util::rip;
    use crate::jets::text::util::lent;
    use crate::mem::NockStack;
    use crate::noun::{Atom, DirectAtom, Cell, CellMemory, D, T};
    use ares_macros::tas;
    use std::result;

    fn num_to_ascii(num: u64) -> result::Result<u64, JetErr> {
        if num > 9 {
            Err(JetErr::Deterministic)
        } else {
            Ok(num + 48)
        }
    }

    pub fn scow(
        stack: &mut NockStack,
        aura: DirectAtom,       // XX: technically this should be Atom?
        atom: Atom
    ) -> jets::Result {
        match aura.data() {
            tas!(b"ud") => {
                eprintln!("1");
                let tape = rip(stack, 3, 1, atom)?;
                let mut lent = lent(tape)?;
    
                eprintln!("2");
                if lent == 0 {
                    assert!(tape.is_atom());
                    return Ok(T(stack, &[D(num_to_ascii(0)?), D(0)]));
                }
    
                eprintln!("3");
                unsafe {
                    let mut list = tape.as_cell()?;
                    let (root, mut memory) = Cell::new_raw_mut(stack);
                    (*memory).head = D(num_to_ascii(list.head().as_direct()?.data())?);
                    lent -= 1;
                    
                eprintln!("4");
                    let mut new_cell: Cell;
                    let mut new_memory: *mut CellMemory;
                    while lent > 0 {
                        list = list.tail().as_cell()?;
                        
                        if lent % 3 == 0 {
                            (new_cell, new_memory) = Cell::new_raw_mut(stack);
                            (*memory).tail = new_cell.as_noun();
                            memory = new_memory;
                            (*memory).head = D(46); // '.'
                        }
                        
                        (new_cell, new_memory) = Cell::new_raw_mut(stack);
                        (*memory).tail = new_cell.as_noun();
                        memory = new_memory;
                        (*memory).head = D(num_to_ascii(tape.as_cell()?.head().as_direct()?.data())?);
                        lent -= 1;
                    }
                    eprintln!("5");
                    (*memory).tail = D(0);
    
                    // 1            1   1
                    // 10           2   2
                    // 100          3   0
                    // 1.000        4   1
                    // 10.000       5   2
                    // 100.000      6   0
                    // 1.000.000    7   1

                    eprintln!("6");
                    Ok(root.as_noun())
                }
            }
            _ => Err(JetErr::Punt)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::JetErr;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_stack, A};
    use crate::noun::{D, T};
    use ares_macros::tas;
    use ibig::ubig;

    #[test]
    fn test_scow() {
        let s = &mut init_stack();
        let aura = D(tas!(b"ud"));
        let sam = T(s, &[aura, D(0)]);
        let res = T(s, &[D(48), D(0)]);
        assert_jet(s, jet_scow, sam, res);
        let sam = T(s, &[aura, D(100)]);
        let res = T(s, &[D(49), D(48), D(48), D(0)]);
        assert_jet(s, jet_scow, sam, res);
        let big = A(s, &ubig!(100));
        let sam = T(s, &[aura, big]);
        let res = T(s, &[D(49), D(48), D(48), D(0)]);
        assert_jet(s, jet_scow, sam, res);
        let sam = T(s, &[aura, D(1000)]);
        let res = T(s, &[D(49), D(46), D(48), D(48), D(48), D(0)]);
        assert_jet(s, jet_scow, sam, res);
        let big = A(s, &ubig!(1000));
        let sam = T(s, &[aura, big]);
        let res = T(s, &[D(49), D(46), D(48), D(48), D(48), D(0)]);
        assert_jet(s, jet_scow, sam, res);
        let bad_aura = D(tas!(b"ux"));
        let sam = T(s, &[bad_aura, D(0)]);
        assert_jet_err(s, jet_scow, sam, JetErr::Punt);
    }
}
