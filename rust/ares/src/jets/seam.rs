/** Map/set jets
 */
use crate::interpreter::Context;
use crate::jets::util::slot;
use crate::jets::Result;
use crate::noun::{Noun, D, T};
use crate::site::{site_slam, Site};

crate::gdb!();

fn by_rep(context: &mut Context, tree: Noun, site: &Site, out: &mut Noun) {
    if unsafe { tree.raw_equals(D(0)) } {
    } else if let Ok(node) = slot(tree, 2) {
        let acc = T(&mut context.stack, &[node, *out]);
        *out = site_slam(context, site, acc);

        if let Ok(left) = slot(tree, 6) {
            by_rep(context, left, site, out);
        }

        if let Ok(rite) = slot(tree, 7) {
            by_rep(context, rite, site, out);
        }
    }
}

pub fn jet_by_rep(context: &mut Context, subject: Noun) -> Result {
    let tree = slot(subject, 30)?;
    let mut gate = slot(subject, 6)?;
    let mut pro = slot(gate, 13)?;

    let site = Site::new(context, &mut gate);
    by_rep(context, tree, &site, &mut pro);
    Ok(pro)
}

pub mod util {
    use crate::jets::math::util::lth_b;
    use crate::jets::util::slot;
    use crate::jets::JetErr;
    use crate::mem::NockStack;
    use crate::mug::mug_u32;
    use crate::noun::{Noun, D};
    use crate::unifying_equality::unifying_equality;
    use either::Either::*;

    pub fn dor_b(stack: &mut NockStack, a: &mut Noun, b: &mut Noun) -> bool {
        let mut ap = a as *mut Noun;
        let mut bp = b as *mut Noun;

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
                        }
                        Right(a_cell) => {
                            if let Ok(b_cell) = (*bp).as_cell() {
                                if unifying_equality(
                                    stack,
                                    a_cell.head_as_mut(),
                                    b_cell.head_as_mut(),
                                ) {
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

    pub fn get_by(
        stack: &mut NockStack,
        a: &mut Noun,
        b: &mut Noun,
    ) -> Result<Option<Noun>, JetErr> {
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
                let lr_cell = slot(*ap, 3)?.as_cell()?;

                ap = if gor_b(stack, &mut (*bp), &mut pna) {
                    lr_cell.head_as_mut()
                } else {
                    lr_cell.tail_as_mut()
                };
            }
        }
    }
}
