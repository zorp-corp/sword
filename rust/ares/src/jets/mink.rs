/** Virtualization jets
 */
use crate::interpreter::{interpret, raw_slot, NockErr};
use crate::jets::JetErr;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Noun, D, T};

crate::gdb!();

// TODO: interpret should accept optional scry function and potentially produce blocked
// TODO: what do we do with the trace, if it's on the stack?
pub fn jet_mink(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let v_subject = raw_slot(arg, 4);
    let v_formula = raw_slot(arg, 5);
    let _scry = raw_slot(arg, 3);

    match interpret(stack, newt, v_subject, v_formula) {
        Ok(res) => Ok(T(stack, &[D(0), res])),
        Err(err) => match err {
            NockErr::Blocked(block) => Ok(T(stack, &[D(1), block])),
            NockErr::Error(error) => Ok(T(stack, &[D(2), error])),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::Jet;
    use crate::mem::unifying_equality;
    use assert_no_alloc::assert_no_alloc;

    fn init() -> NockStack {
        NockStack::new(8 << 10 << 10, 0)
    }

    fn assert_noun_eq(stack: &mut NockStack, mut a: Noun, mut b: Noun) {
        let eq = unsafe { unifying_equality(stack, &mut a, &mut b) };
        assert!(eq, "got: {}, need: {}", a, b);
    }

    fn assert_jet(stack: &mut NockStack, jet: Jet, sam: Noun, res: Noun) {
        let sam = T(stack, &[D(0), sam, D(0)]);
        let jet_res = assert_no_alloc(|| jet(stack, &mut None, sam).unwrap());
        assert_noun_eq(stack, jet_res, res);
    }

    #[test]
    fn test_mink_success() {
        let s = &mut init();
        let n = T(s, &[D(0), D(1), D(53)]);
        let sam = T(s, &[n, D(0)]);
        let res = T(s, &[D(0), D(53)]);
        assert_jet(s, jet_mink, sam, res);
    }

    #[test]
    fn test_mink_zapzap() {
        let s = &mut init();
        let n = T(s, &[D(0), D(0), D(0)]);
        let sam = T(s, &[n, D(0)]);
        let res = T(s, &[D(2), D(1)]);
        assert_jet(s, jet_mink, sam, res);
    }
}
