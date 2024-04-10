/** Tree jets
 */
// use crate::interpreter::Context;
// use crate::jets::bits::util::*;
// use crate::jets::util::*;
// use crate::jets::Result;
// use crate::noun::{IndirectAtom, Noun, D};

use self::util::*;

crate::gdb!();

// XX TODO actual jets

pub mod util {
    use crate::mug::mug_u32;
    use crate::unifying_equality::unifying_equality;
    use crate::mem::NockStack;
    use crate::jets::math::util::lth_b;
    use crate::jets::util::slot;
    use crate::noun::{Noun, D};
    use either::Either::*;
    use crate::jets::JetErr;

    pub fn dor_b(stack: &mut NockStack, a: &mut Noun, b: &mut Noun) -> bool {
        let mut ap = a as *mut Noun;
        let mut bp = a as *mut Noun;

        unsafe {
            loop {
                if unifying_equality(stack, ap, bp) {
                    break true;
                } else {
                    match (*ap).as_either_atom_cell() {
                        Left(a_atom) => {
                            if let Ok(b_atom) = (*bp).as_atom() {
                                break lth_b(stack, a_atom, b_atom);
                            } else {
                                break true;
                            }
                        },
                        Right(a_cell) => {
                            if let Ok(b_cell) = (*bp).as_cell() {
                                if unifying_equality(stack, a_cell.head_as_mut(), b_cell.head_as_mut()) {
                                    ap = a_cell.tail_as_mut();
                                    bp = b_cell.tail_as_mut();
                                    continue;
                                } else {
                                    ap = a_cell.head_as_mut();
                                    bp = b_cell.head_as_mut();
                                    continue;
                                }
                            } else {
                                break false;
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn gor_b(stack: &mut NockStack, a: &mut Noun, b: &mut Noun) -> bool {
        let c = mug_u32(stack, *a);
        let d = mug_u32(stack, *b);
        if c == d {
            dor_b(stack, a, b)
        } else {
            c < d
        }
    }

    pub fn get_by(stack: &mut NockStack, a: &mut Noun, b: &mut Noun) -> Result<Option<Noun>, JetErr> {
        let mut ap = a as *mut Noun;
        let bp = b as *mut Noun;
        unsafe {
            loop {
                if (*ap).raw_equals(D(0)) {
                    break Ok(None);
                }
                let na = slot(*ap, 2)?; // n.a
                let mut pna = slot(na, 2)?; // p.n.a
                if unifying_equality(stack, bp, &mut pna) {
                    break Ok(Some(slot(na, 3)?)); // q.n.a
                }
                let lr_cell = slot((*ap), 3)?.as_cell()?;

                ap = if gor_b(stack, &mut (*bp), &mut pna) {
                    lr_cell.head_as_mut()
                } else {
                    lr_cell.tail_as_mut()
                };
            }
        }
    }
}
