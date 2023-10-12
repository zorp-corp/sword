/** Sorting jets
 */
use crate::jets;
use crate::jets::util::slot;
use crate::mem::NockStack;
use crate::mug::mug;
use crate::newt::Newt;
use crate::noun::{Noun, NO, YES};

crate::gdb!();

pub fn jet_mor(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;

    let c = mug(stack, a);
    let d = mug(stack, b);

    let e = mug(stack, c.as_noun());
    let f = mug(stack, d.as_noun());

    if e.data() == f.data() {
        Ok(util::dor(a, b))
    } else {
        if e.data() < f.data() {
            Ok(YES)
        } else {
            Ok(NO)
        }
    }
}

pub fn jet_gor(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;

    let c = mug(stack, a);
    let d = mug(stack, b);

    if c.data() == d.data() {
        Ok(util::dor(a, b))
    } else {
        if c.data() < d.data() {
            Ok(YES)
        } else {
            Ok(NO)
        }
    }
}

pub fn jet_dor(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;

    Ok(util::dor(a, b))
}

pub mod util {
    use either::{Left, Right};
    use crate::jets::util::slot;
    use crate::jets::math::util::lth;
    use crate::noun::{Noun, YES, NO};

    pub fn dor(a: Noun, b: Noun) -> Noun {
        if unsafe { a.raw_equals(b) } {
            YES
        } else {
            match (a.as_either_atom_cell(), b.as_either_atom_cell()) {
                (Left(atom_a), Left(atom_b)) => lth(atom_a, atom_b),
                (Left(_), Right(_)) => YES,
                (Right(_), Left(_)) => NO,
                (Right(cell_a), Right(cell_b)) => {
                    let a_head = match slot(cell_a.as_noun(), 2) {
                        Ok(n) => n,
                        Err(_) => return NO,
                    };
                    let b_head = slot(cell_b.as_noun(), 2).unwrap();
                    let a_tail = slot(cell_a.as_noun(), 3).unwrap();
                    let b_tail = slot(cell_b.as_noun(), 3).unwrap();
                    if unsafe { a_head.raw_equals(b_head) } {
                        dor(a_tail, b_tail)
                    } else {
                        dor(a_head, b_head)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ibig::ubig;
    use crate::jets::util::test::{A, assert_jet, init_stack};
    use crate::noun::{D, T};

    #[test]
    fn test_dor() {
        let s = &mut init_stack();
        let sam = T(s, &[D(1), D(1)]);
        assert_jet(s, jet_dor, sam, YES);

        let a = A(s, &ubig!(_0x3fffffffffffffff));
        let sam = T(s, &[a, D(1)]);
        assert_jet(s, jet_dor, sam, NO);

        let a = A(s, &ubig!(_0x3fffffffffffffff));
        let sam = T(s, &[a, a]);
        assert_jet(s, jet_dor, sam, YES);
    }

    #[test]
    fn test_gor() {
        let s = &mut init_stack();
        let sam = T(s, &[D(1), D(1)]);
        assert_jet(s, jet_gor, sam, YES);

        let a = A(s, &ubig!(_0x3fffffffffffffff));
        let sam = T(s, &[a, a]);
        assert_jet(s, jet_gor, sam, YES);
    }

    #[test]
    fn test_mor() {
        let s = &mut init_stack();
        let sam = T(s, &[D(1), D(1)]);
        assert_jet(s, jet_mor, sam, YES);

        let a = A(s, &ubig!(_0x3fffffffffffffff));
        let sam = T(s, &[a, a]);
        assert_jet(s, jet_mor, sam, YES);
    }
}
