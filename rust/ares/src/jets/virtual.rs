/** Virtualization jets
 */
use crate::interpreter::raw_slot;
use crate::jets::{JetErr, JetErr::*};
use crate::mem::NockStack;
use crate::mug::mug;
use crate::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun, D, DIRECT_MAX, NO, T, YES};
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::*;
use std::{cmp, convert::TryFrom};

crate::gdb!();

// can we reuse stack like this?
// interpret should accept optional scry function and potentially produce blocked
// what do we do with the trace, if it's on the stack?
pub fn jet_mink(stack: &mut NockStack, newt: &mut Option<&mut Newt>, subject: Noun) -> Result<Noun, JetErr> {
    let arg = raw_slot(subject, 6);
    let v_subject = raw_slot(arg, 4);
    let v_formula = raw_slot(arg, 5);
    let scry = raw_slot(arg, 3);

    match interpret(stack, newt, v_subject, v_formula) {
        Ok(res) => Ok(T(stack, &[D(0), res])),
        Err(err) => match err {
            Blocked(block) => Ok(T(stack, &[D(1), block])),
            Error(error) => Ok(T(stack, &[D(2), error])),
        }
    }
}
