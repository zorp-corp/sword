/** Tree jets
 */
use crate::jets;
use crate::jets::JetErr::*;
use crate::jets::util::*;
use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_ubig, assert_nary_jet_ubig, init_stack, A};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, Noun, D, T};
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
        if a.data() == 0 {
            return Err(Deterministic);
        }
        //  XX JET MISMATCH IMPORTED FROM VERE
        if b.data() == 0 {
            return Err(Deterministic);
        }

        let c = met(0, b.as_atom()) as u8;
        let d = c - 1;
        let e = d << 1;
        let f = b.data() - e as u64;  // left or right child
        let g = ubig!(d) << a.data() as usize;
        let _h = f + g;

        Ok(A(stack, &ubig!(h)))
    } else {
        let a_big = a.as_ubig(stack);
        let b_big = b.as_ubig(stack);

        if a_big == ubig!(0) {
            return Err(Deterministic);
        }
        //  XX JET MISMATCH IMPORTED FROM VERE
        if b_big == ubig!(0) {
            return Err(Deterministic);
        }

        let c = met(0, b);
        let d = c - 1;
        let e = d << 1;
        let f = b_big - e;  // left or right child
        //  shl on ubig not defined, so crash if a_big is too big
        if a_big.bit_len() > 64 {
            return Err(Deterministic);
        } else {
            // downcast a_big to u64
            let g = d << a.as_direct()?.data();
            let h = f + g;

            Ok(Atom::from_ubig(stack, &h).as_noun())
        }
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

    fn assert_math_jet(
        stack: &mut NockStack,
        jet: jets::Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        res: UBig,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        assert_nary_jet_ubig(stack, jet, &sam, res);
    }

    fn assert_math_jet_err(
        stack: &mut NockStack,
        jet: jets::Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        err: JetErr,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        let sam = T(stack, &sam);
        assert_jet_err(stack, jet, sam, err);
    }

    fn atom_0(_stack: &mut NockStack) -> Noun {
        print!("{:x}", 0);
        D(0x0)
    }

    fn atom_1(_stack: &mut NockStack) -> Noun {
        print!("{:x}", 1);
        D(0x1)
    }

    fn atom_2(_stack: &mut NockStack) -> Noun {
        print!("{:x}", 2);
        D(0x2)
    }

    fn atom_3(_stack: &mut NockStack) -> Noun {
        print!("{:x}", 3);
        D(0x3)
    }

    fn atom_4(_stack: &mut NockStack) -> Noun {
        D(0x4)
    }

    fn atom_5(_stack: &mut NockStack) -> Noun {
        D(0x5)
    }

    fn atom_10(_stack: &mut NockStack) -> Noun {
        D(0x10)
    }

    fn atom_7f(_stack: &mut NockStack) -> Noun {
        D(0x7fffffffffffffff)
    }

    fn atom_100(stack: &mut NockStack) -> Noun {
        let shl1_6 = UBig::from_str_radix("10000000000000000", 16);
        A(stack, &ubig!(shl1_6))
    }

    fn atom_1000(stack: &mut NockStack) -> Noun {
        let shl1_7 = UBig::from_str_radix("100000000000000000000000000000000", 16);
        A(stack, &ubig!(shl1_7))
    }

    fn atom_2000(stack: &mut NockStack) -> Noun {
        let shl2_7 = UBig::from_str_radix("200000000000000000000000000000000", 16);
        A(stack, &ubig!(shl2_7))
    }

    #[test]
    fn test_peg() {
        let s = &mut init_stack();

        assert_math_jet_err(s, jet_peg, &[atom_0, atom_1], JetErr::Deterministic);

        // Test direct
        assert_math_jet(s, jet_peg, &[atom_2, atom_3], ubig!(5));
        assert_math_jet(s, jet_peg, &[atom_10,atom_10], ubig!(82));
        assert_math_jet(s, jet_peg, &[atom_7f, atom_2], ubig!(0xfffffffffffffffe));
        assert_math_jet(s, jet_peg, &[atom_7f, atom_3], ubig!(0xffffffffffffffff));

        // Test direct with overflow.
        assert_math_jet(s, jet_peg, &[atom_7f, atom_4], ubig!(0x1fffffffffffffffc));
        assert_math_jet(s, jet_peg, &[atom_7f, atom_5], ubig!(0x1fffffffffffffffd));

        // Test indirect.
        let val_1000 = UBig::from_str_radix("100000000000000000000000000000000", 16);
        let val_2000 = UBig::from_str_radix("200000000000000000000000000000000", 16);
        assert_math_jet(s, jet_peg, &[atom_1000, atom_2], ubig!(val_2000));
        assert_math_jet(s, jet_peg, &[atom_2, atom_1000], ubig!(val_2000));
        assert_math_jet(s, jet_peg, &[atom_100, atom_100], ubig!(val_1000));
    }
}
