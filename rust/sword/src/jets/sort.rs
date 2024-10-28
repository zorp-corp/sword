/** Sorting jets
 */
use crate::interpreter::Context;
use crate::jets;
use crate::jets::util::slot;
use crate::mug::mug;
use crate::noun::{Noun, NO, YES};
use std::cmp::Ordering;

crate::gdb!();

pub fn jet_dor(context: &mut Context, subject: Noun) -> jets::Result<Noun> {
    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;

    Ok(util::dor(&mut context.stack, a, b)?)
}

pub fn jet_gor(context: &mut Context, subject: Noun) -> jets::Result<Noun> {
    let stack = &mut context.stack;

    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;

    let c = mug(stack, a);
    let d = mug(stack, b);

    match c.data().cmp(&d.data()) {
        Ordering::Greater => Ok(NO),
        Ordering::Less => Ok(YES),
        Ordering::Equal => Ok(util::dor(stack, a, b)?),
    }
}

pub fn jet_mor(context: &mut Context, subject: Noun) -> jets::Result<Noun> {
    let stack = &mut context.stack;

    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;

    let c = mug(stack, a);
    let d = mug(stack, b);

    let e = mug(stack, c.as_noun());
    let f = mug(stack, d.as_noun());

    match e.data().cmp(&f.data()) {
        Ordering::Greater => Ok(NO),
        Ordering::Less => Ok(YES),
        Ordering::Equal => Ok(util::dor(stack, a, b)?),
    }
}

pub mod util {
    use crate::jets::math::util::lth;
    use crate::jets::util::slot;
    use crate::mem::{AllocResult, NockStack};
    use crate::noun::{Noun, NO, YES};
    use either::{Left, Right};

    pub fn dor(stack: &mut NockStack, a: Noun, b: Noun) -> AllocResult<Noun> {
        let res = if unsafe { a.raw_equals(b) } {
            YES
        } else {
            match (a.as_either_atom_cell(), b.as_either_atom_cell()) {
                (Left(atom_a), Left(atom_b)) => lth(stack, atom_a, atom_b)?,
                (Left(_), Right(_)) => YES,
                (Right(_), Left(_)) => NO,
                (Right(cell_a), Right(cell_b)) => {
                    let a_head = match slot(cell_a.as_noun(), 2) {
                        Ok(n) => n,
                        Err(_) => return Ok(NO),
                    };
                    let b_head = slot(cell_b.as_noun(), 2).unwrap();
                    let a_tail = slot(cell_a.as_noun(), 3).unwrap();
                    let b_tail = slot(cell_b.as_noun(), 3).unwrap();
                    if unsafe { a_head.raw_equals(b_head) } {
                        dor(stack, a_tail, b_tail)?
                    } else {
                        dor(stack, a_head, b_head)?
                    }
                }
            }
        };
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, init_context};
    use crate::noun::D;
    use ibig::ubig;
    // Override with the panicky variant
    use crate::test_fns::{A, T};

    #[test]
    fn test_dor() {
        let c = &mut init_context().unwrap();

        let sam = T(&mut c.stack, &[D(1), D(1)]);
        assert_jet(c, jet_dor, sam, YES);

        let a = A(&mut c.stack, &ubig!(_0x3fffffffffffffff));
        let sam = T(&mut c.stack, &[a, D(1)]);
        assert_jet(c, jet_dor, sam, NO);

        let a = A(&mut c.stack, &ubig!(_0x3fffffffffffffff));
        let sam = T(&mut c.stack, &[a, a]);
        assert_jet(c, jet_dor, sam, YES);
    }

    #[test]
    fn test_gor() {
        let c = &mut init_context().unwrap();

        let sam = T(&mut c.stack, &[D(1), D(1)]);
        assert_jet(c, jet_gor, sam, YES);

        let a = A(&mut c.stack, &ubig!(_0x3fffffffffffffff));
        let sam = T(&mut c.stack, &[a, a]);
        assert_jet(c, jet_gor, sam, YES);
    }

    #[test]
    fn test_mor() {
        let c = &mut init_context().unwrap();

        let sam = T(&mut c.stack, &[D(1), D(1)]);
        assert_jet(c, jet_mor, sam, YES);

        let a = A(&mut c.stack, &ubig!(_0x3fffffffffffffff));
        let sam = T(&mut c.stack, &[a, a]);
        assert_jet(c, jet_mor, sam, YES);
    }
}
