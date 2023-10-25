/** Tree jets
 */
use crate::interpreter::{Context, Error};
use crate::jets::util::*;
use crate::jets::{JetErr, Result};
use crate::noun::{Noun, Atom, D};

crate::gdb!();

pub fn jet_cap(_context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    let tom = arg.as_atom()?;
    let met = met(0, tom);

    unsafe {
        if met < 2 {
            Err(JetErr::Fail(Error::Deterministic(D(0))))
        } else if *(tom.as_bitslice().get_unchecked(met - 2)) {
            Ok(D(3))
        } else {
            Ok(D(2))
        }
    }
}

pub fn jet_mas(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let arg = slot(subject, 6)?;
    let tom = arg.as_atom()?;
    let met = met(0, tom);

    if met < 2 {
        Err(JetErr::Fail(Error::Deterministic(D(0))))
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
pub fn jet_peg(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?;
    let b = slot(arg, 3)?.as_atom()?;

    if let (Ok(a), Ok(b)) = (a.as_direct(), b.as_direct()) {
        if a.data() == 0 {
            return Err(JetErr::Fail(Error::Deterministic(D(0))));
        }
        //  XX JET MISMATCH IMPORTED FROM VERE
        if b.data() == 0 {
            return Err(JetErr::Fail(Error::Deterministic(D(0))));
        }

        let c = met(0, b.as_atom()) as u8;
        let d = c - 1;
        let e = d << 1;
        let f = b.data() - e as u64;  // left or right child
        let g = d << a.data();
        let h = f + g as u64;

        Ok(Atom::new(stack, h).as_noun())
    } else {
        // Don't need 0 checks here since it's presumed that any UBig is
        // greater than or equal to 2^63 (i.e. that indirect atoms are
        // normalized before being returned).
        let a_big = a.as_ubig(stack);
        let b_big = b.as_ubig(stack);

        let c = met(0, b);
        let d = c - 1;
        let e = d << 1;
        let f = b_big - e;  // left or right child
        //  shl on ubig not defined, so crash if a_big is too big
        if a_big.bit_len() > 64 {
            return Err(JetErr::Fail(Error::Deterministic(D(0))));
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
    use crate::interpreter::{Context, Error};
    use crate::jets::util::test::*;
    use crate::jets::{Jet, JetErr};
    use crate::mem::NockStack;
    use crate::noun::{Noun, D, T};
    use ibig::UBig;
    use ibig::ubig;

    #[test]
    fn test_cap() {
        let c = &mut init_context();

        assert_jet_err(c, jet_cap, D(0), JetErr::Fail(Error::Deterministic(D(0))));
        assert_jet_err(c, jet_cap, D(1), JetErr::Fail(Error::Deterministic(D(0))));

        assert_jet(c, jet_cap, D(2), D(2));
        assert_jet(c, jet_cap, D(3), D(3));
        assert_jet(c, jet_cap, D(4), D(2));
        assert_jet(c, jet_cap, D(5), D(2));
        assert_jet(c, jet_cap, D(6), D(3));
        assert_jet(c, jet_cap, D(7), D(3));
        assert_jet(c, jet_cap, D(8), D(2));
    }

    #[test]
    fn test_mas() {
        let c = &mut init_context();

        assert_jet_err(c, jet_mas, D(0), JetErr::Fail(Error::Deterministic(D(0))));
        assert_jet_err(c, jet_mas, D(1), JetErr::Fail(Error::Deterministic(D(0))));

        assert_jet(c, jet_mas, D(2), D(1));
        assert_jet(c, jet_mas, D(3), D(1));
        assert_jet(c, jet_mas, D(4), D(2));
        assert_jet(c, jet_mas, D(5), D(3));
        assert_jet(c, jet_mas, D(6), D(2));
        assert_jet(c, jet_mas, D(7), D(3));
        assert_jet(c, jet_mas, D(8), D(4));
    }

    fn assert_math_jet(
        context: &mut Context,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        res: UBig,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(&mut context.stack)).collect();
        assert_nary_jet_ubig(context, jet, &sam, res);
    }

    fn assert_math_jet_noun(
        context: &mut Context,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        res: Noun,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(&mut context.stack)).collect();
        let sam = T(&mut context.stack, &sam);
        assert_jet(context, jet, sam, res);
    }

    fn assert_math_jet_err(
        context: &mut Context,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        err: JetErr,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(&mut context.stack)).collect();
        let sam = T(&mut context.stack, &sam);
        assert_jet_err(context, jet, sam, err);
    }

    fn atom_0(_stack: &mut NockStack) -> Noun {
        D(0x0)
    }

    fn atom_1(_stack: &mut NockStack) -> Noun {
        D(0x1)
    }

    fn atom_2(_stack: &mut NockStack) -> Noun {
        D(0x2)
    }

    fn atom_3(_stack: &mut NockStack) -> Noun {
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
        A(stack, &ubig!(_0x10000000000000000))
    }

    fn atom_1000(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(_0x100000000000000000000000000000000))
    }

    #[test]
    fn test_peg() {
        let c = &mut init_context();

        assert_math_jet_err(c, jet_peg, &[atom_0, atom_1], JetErr::Fail(Error::Deterministic(D(0))));

        // Test direct
        assert_math_jet_noun(c, jet_peg, &[atom_2, atom_3], D(5));
        assert_math_jet_noun(c, jet_peg, &[atom_10, atom_10], D(82));
        // assert_math_jet_noun(c, jet_peg, &[atom_7f, atom_2], D(0xfffffffffffffffe));
        // assert_math_jet(c, jet_peg, &[atom_7f, atom_3], ubig!(0xffffffffffffffff));

        // // Test direct with overflow.
        // assert_math_jet(c, jet_peg, &[atom_7f, atom_4], ubig!(0x1fffffffffffffffc));
        // assert_math_jet(c, jet_peg, &[atom_7f, atom_5], ubig!(0x1fffffffffffffffd));

        // // Test indirect.
        // let val_1000 = A(&mut c.stack, &ubig!(_0x100000000000000000000000000000000));
        // let val_2000 = A(&mut c.stack, &ubig!(_0x200000000000000000000000000000000));
        // assert_math_jet_noun(c, jet_peg, &[atom_1000, atom_2], val_2000);
        // assert_math_jet_noun(c, jet_peg, &[atom_2, atom_1000], val_2000);
        // assert_math_jet_noun(c, jet_peg, &[atom_100, atom_100], val_1000);
    }
}
