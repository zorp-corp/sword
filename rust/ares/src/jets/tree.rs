/** Tree jets
 */
use crate::jets;
use crate::jets::JetErr::*;
use crate::jets::util::*;
use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_ubig, assert_nary_jet_ubig, init_stack, A};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, Noun, D};
//use crate::noun::{Atom, DirectAtom, IndirectAtom, Noun, D, DIRECT_MAX, NO, T, YES};
use ibig::{UBig, ubig};

crate::gdb!();

pub fn jet_cap(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let tom = arg.as_atom()?;
    let met = met(0, tom);

    unsafe {
        if met < 2 {
            Err(Deterministic)
        } else if *(tom.as_bitslice().get_unchecked(met - 2)) {
            Ok(D(3))
        } else {
            Ok(D(2))
        }
    }
}

pub fn jet_mas(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let tom = arg.as_atom()?;
    let met = met(0, tom);
    
    if met < 2 {
        Err(Deterministic)
    } else {
        let c = bex(stack, met - 1);
        let d = bex(stack, met - 2);
        let e = sub(stack, tom, c)?;
        
        Ok(con(stack, e, d).as_noun())
    }
}

/*
++  peg
  ~/  %peg
  |=  [a=@ b=@]
  ?<  =(0 a)
  ^-  @
  ?-  b
    %1  a
    %2  (mul a 2)
    %3  +((mul a 2))
    *   (add (mod b 2) (mul $(b (div b 2)) 2))
  ==
*/
pub fn jet_peg(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.as_direct()?.data() == 0 {
            return Err(Deterministic);
        }
        //  XX JET MISMATCH IMPORTED FROM VERE
        if b.as_direct()?.data() == 0 {
            return Err(Deterministic);
        }

        let c = met(0, b);
        let d = c - 1;
        let e = d << 1;
        let f = b.as_direct()?.data() - e;  // left or right child
        let g = safe_shl(d, a);
        let h = safe_add(f, g);

        //  XX MAY RETURN INDIRECT ATOM
        D(h)
    } else {
        let a_big = a.as_ubig(stack)?;
        let b_big = b.as_ubig(stack)?;

        if a_big == 0 {
            return Err(Deterministic);
        }
        //  XX JET MISMATCH IMPORTED FROM VERE
        if b_big == 0 {
            return Err(Deterministic);
        }

        let c = met(0, b);
        let d = c - 1;
        let e = safe_shl(d, 1);
        let f = b_big - e;  // left or right child
        let g = safe_shl(d, a);
        let h = safe_add(f, g);

        Ok(Atom::from_ubig(stack, &h).as_noun())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::JetErr;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_stack};
    use crate::noun::D;

    #[test]
    fn test_cap() {
        let s = &mut init_stack();

        assert_jet_err(s, jet_cap, D(0), JetErr::Deterministic);
        assert_jet_err(s, jet_cap, D(1), JetErr::Deterministic);

        assert_jet(s, jet_cap, D(2), D(2));
        assert_jet(s, jet_cap, D(3), D(3));
        assert_jet(s, jet_cap, D(4), D(2));
        assert_jet(s, jet_cap, D(5), D(2));
        assert_jet(s, jet_cap, D(6), D(3));
        assert_jet(s, jet_cap, D(7), D(3));
        assert_jet(s, jet_cap, D(8), D(2));
    }

    #[test]
    fn test_mas() {
        let s = &mut init_stack();

        assert_jet_err(s, jet_mas, D(0), JetErr::Deterministic);
        assert_jet_err(s, jet_mas, D(1), JetErr::Deterministic);

        assert_jet(s, jet_mas, D(2), D(1));
        assert_jet(s, jet_mas, D(3), D(1));
        assert_jet(s, jet_mas, D(4), D(2));
        assert_jet(s, jet_mas, D(5), D(3));
        assert_jet(s, jet_mas, D(6), D(2));
        assert_jet(s, jet_mas, D(7), D(3));
        assert_jet(s, jet_mas, D(8), D(4));
    }
}
