/** Floating-point jets
 */
use crate::jets;
use crate::jets::JetErr::*;
use crate::jets::util::*;
use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_ubig, assert_nary_jet_ubig, init_stack, A};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, IndirectAtom, Noun, D, T};
//use crate::noun::{Atom, DirectAtom, IndirectAtom, Noun, D, DIRECT_MAX, NO, T, YES};
use ibig::{UBig, ubig};
use softfloat_sys::*;

crate::gdb!();

const SINGNAN: u32 = 0x7fc00000;
const SINGZERO: u32 = 0x00000000;

#[inline(always)]
fn _nan_test(
    a: float32_t
) -> bool {
    unsafe {
        !f32_eq(a, a)
    }
}

#[inline(always)]
fn _nan_unify(
    a: float32_t
) -> float32_t {
    unsafe {
        if _nan_test(a) {
            return ui32_to_f32(SINGNAN);
        }
        a
    }
}

#[inline(always)]
fn _set_rounding(
    r: char
) -> u8 {
    match r {
        'n' => softfloat_round_near_even,
        'z' => softfloat_round_minMag,
        'u' => softfloat_round_max,
        'd' => softfloat_round_min,
        // formal fallthrough, should never happen
        _      => softfloat_round_near_even,
    }
}

pub fn jet_rs_add(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?.as_direct()?;
    let b = slot(arg, 3)?.as_atom()?.as_direct()?;
    let r = slot(arg, 30)?.as_atom()?.as_direct()?;

    unsafe {
        let dat_a: float32_t = ui32_to_f32(a.data() as u32);
        let dat_b: float32_t = ui32_to_f32(b.data() as u32);
        let mod_r = _set_rounding(r.data() as u8 as char);

        if f32_eq(dat_a, ui32_to_f32(SINGZERO)) {
            return Ok(b.as_noun());
        }
        if f32_eq(dat_b, ui32_to_f32(SINGZERO)) {
            return Ok(a.as_noun());
        }

        let c = _nan_unify(f32_add(dat_a, dat_b));

        Ok(D(f32_to_ui32(c, mod_r, true)))
    }
}

pub fn jet_rs_sub(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?.as_direct()?;
    let b = slot(arg, 3)?.as_atom()?.as_direct()?;
    let r = slot(arg, 30)?.as_atom()?.as_direct()?;

    unsafe {
        let dat_a: float32_t = ui32_to_f32(a.data() as u32);
        let dat_b: float32_t = ui32_to_f32(b.data() as u32);
        let mod_r = _set_rounding(r.data() as u8 as char);

        if f32_eq(dat_b, ui32_to_f32(SINGZERO)) {
            return Ok(a.as_noun());
        }

        let c = _nan_unify(f32_sub(dat_a, dat_b));

        Ok(D(f32_to_ui32(c, mod_r, true)))
    }
}

pub fn jet_rs_mul(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?.as_direct()?;
    let b = slot(arg, 3)?.as_atom()?.as_direct()?;
    let r = slot(arg, 30)?.as_atom()?.as_direct()?;

    unsafe {
        let dat_a: float32_t = ui32_to_f32(a.data() as u32);
        let dat_b: float32_t = ui32_to_f32(b.data() as u32);
        let mod_r = _set_rounding(r.data() as u8 as char);

        if f32_eq(dat_a, ui32_to_f32(SINGZERO)) {
            return Ok(D(SINGZERO as u64));
        }
        if f32_eq(dat_b, ui32_to_f32(SINGZERO)) {
            return Ok(D(SINGZERO as u64));
        }

        let c = _nan_unify(f32_mul(dat_a, dat_b));

        Ok(D(f32_to_ui32(c, mod_r, true)))
    }
}

pub fn jet_rs_div(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?.as_direct()?;
    let b = slot(arg, 3)?.as_atom()?.as_direct()?;
    let r = slot(arg, 30)?.as_atom()?.as_direct()?;

    unsafe {
        let dat_a: float32_t = ui32_to_f32(a.data() as u32);
        let dat_b: float32_t = ui32_to_f32(b.data() as u32);
        let mod_r = _set_rounding(r.data() as u8 as char);

        if f32_eq(dat_a, ui32_to_f32(SINGZERO)) {
            return Ok(D(SINGZERO as u64));
        }

        let c = _nan_unify(f32_div(dat_a, dat_b));

        Ok(D(f32_to_ui32(c, mod_r, true)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::JetErr;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_stack};
    use crate::noun::D;

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
        D(0x00000000)
    }

    fn atom_1(_stack: &mut NockStack) -> Noun {
        print!("{:x}", 1);
        D(0x3f800000)
    }

    fn atom_2(_stack: &mut NockStack) -> Noun {
        print!("{:x}", 2);
        D(0x40000000)
    }

    fn atom_3(_stack: &mut NockStack) -> Noun {
        print!("{:x}", 3);
        D(0x40400000)
    }

    fn atom_1_5(_stack: &mut NockStack) -> Noun {
        D(0x3fc00000)
    }

    // for test (1.1 - 0.8) - 0.3
    fn atom_0_(_stack: &mut NockStack) -> Noun {
        D(0x24800000)
    }

    fn atom_1_1(_stack: &mut NockStack) -> Noun {
        D(0x3f8ccccd)
    }

    fn atom_0_8(_stack: &mut NockStack) -> Noun {
        D(0x3f4ccccd)
    }

    fn atom_0_3(_stack: &mut NockStack) -> Noun {
        D(0x3e99999a)
    }

    fn r(_stack : &mut NockStack) -> Noun {
        D('n' as u64)
    }

    #[test]
    fn test_rs_add() {
        let s = &mut init_stack();

        assert_math_jet(s, jet_rs_add, &[atom_0, atom_0, r], ubig!(0x00000000));
        assert_math_jet(s, jet_rs_add, &[atom_0, atom_1, r], ubig!(0x3f800000));
        assert_math_jet(s, jet_rs_add, &[atom_1, atom_1, r], ubig!(0x40000000));
        assert_math_jet(s, jet_rs_add, &[atom_1, atom_2, r], ubig!(0x40400000));
        assert_math_jet(s, jet_rs_add, &[atom_2, atom_1, r], ubig!(0x40400000));
        assert_math_jet(s, jet_rs_add, &[atom_0_8, atom_0_3, r], ubig!(0x3f8ccccd));
    }
}
