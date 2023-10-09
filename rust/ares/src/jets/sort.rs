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
        util::dor(stack, a, b)
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
        util::dor(stack, a, b)
    } else {
        if c.data() < d.data() {
            Ok(YES)
        } else {
            Ok(NO)
        }
    }
}

pub fn jet_dor(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let sam = slot(subject, 6)?;
    let a = slot(sam, 2)?;
    let b = slot(sam, 3)?;

    util::dor(stack, a, b)
}

pub mod util {
    use crate::jets::util::slot;
    use crate::jets::math::util::lth;
    use crate::jets::Result;
    use crate::mem::NockStack;
    use crate::noun::{Noun, YES, NO};

    pub fn dor(stack: &mut NockStack, a: Noun, b: Noun) -> Result {
        if unsafe { a.raw_equals(b) } {
            Ok(YES)
        } else {
            if a.is_atom() {
                if b.is_atom() {
                    lth(stack, a.as_atom()?, b.as_atom()?)
                } else {
                    Ok(YES)
                }
            } else {
                if b.is_atom() {
                    Ok(NO)
                } else {
                    let a_head = slot(a, 2)?;
                    let b_head = slot(b, 2)?;
                    let a_tail = slot(a, 3)?;
                    let b_tail = slot(b, 3)?;
                    if unsafe { a_head.raw_equals(b_head) } {
                        Ok(dor(stack, a_tail, b_tail)?)
                    } else {
                        Ok(dor(stack, a_head, b_head)?)
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
