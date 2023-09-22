/** Floating-point jets
 */
use crate::jets;
use crate::jets::JetErr::*;
use crate::jets::util::*;
use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_ubig, assert_nary_jet_ubig, init_stack, A};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, IndirectAtom, Noun, D, T, Cell, NO, YES};
use ibig::{UBig, ubig};
use softfloat_sys::*;
use std::io::Write;

crate::gdb!();

const HALFNAN: u16  = 0x7e00;
const HALFINF: u16  = 0x7c00;
const HALFZERO: u16 = 0x0000;

#[inline(always)]
fn _nan_test(
    a: float16_t
) -> bool {
    unsafe {
        !f16_eq(a, a)
    }
}

#[inline(always)]
fn _nan_unify(
    a: float16_t
) -> float16_t {
    unsafe {
        if _nan_test(a) {
            return softfloat_sys::float16_t { v: HALFNAN};
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

pub fn jet_rh_add(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f16_add(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rh_sub(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f16_sub(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rh_mul(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f16_mul(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rh_div(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f16_div(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rh_sqt(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        eprintln!("»sam: {:?}", sam);
        let a = sam.as_atom()?.as_direct()?;
        eprintln!("»a: {:?}", a.as_noun());
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f16_sqrt(dat_a));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rh_fma(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 6)?.as_atom()?.as_direct()?;
        let c = slot(sam, 7)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let dat_c = softfloat_sys::float16_t { v: c.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f16_mulAdd(dat_a, dat_b, dat_c));
        
        Ok(D(x.v as u64))
    }
}

pub fn jet_rh_lth(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f16_lt(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rh_lte(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f16_le(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rh_equ(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f16_eq(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rh_gte(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f16_le(dat_b, dat_a);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rh_gth(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float16_t { v: a.data() as u16};
        let dat_b = softfloat_sys::float16_t { v: b.data() as u16};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f16_lt(dat_b, dat_a);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::{Jet, JetErr};
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_stack};
    use crate::noun::D;
    use crate::jets::util::test::{assert_noun_eq};
    use assert_no_alloc::assert_no_alloc;

    pub fn assert_jet_in_door(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],  // regular sample
        ctx: &[fn(&mut NockStack) -> Noun],  // door sample as context
        res: Noun) {
        unsafe {
            let mut sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
            let mut ctx: Vec<Noun> = ctx.iter().map(|f| f(stack)).collect();
            let sam = if(sam.len() > 1) { T(stack, &sam) } else { sam[0] };
            eprintln!("sam: {:?}", sam);
            let ctx = if(ctx.len() > 1) { T(stack, &ctx) } else { ctx[0] };
            eprintln!("ctx: {:?}", ctx);
            let pay = Cell::new(stack, sam, ctx).as_noun();
            eprintln!("pay: {:?}", pay);
            let sbj = Cell::new(stack, D(0), pay).as_noun();
            eprintln!("sbj: {:?}", sbj);
            std::io::stderr().flush().unwrap();
            let jet_res = jet(stack, &mut None, sbj).unwrap();
            eprintln!("jet: {:x}\n", jet_res.as_atom().expect("").as_direct().expect("").data());
            std::io::stderr().flush().unwrap();
            assert_noun_eq(stack, jet_res, res);
        }
    }

    fn atom_0(_stack: &mut NockStack) -> Noun {
        D(0x0000)
    }

    fn atom_1(_stack: &mut NockStack) -> Noun {
        D(0x3c00)
    }

    fn atom_2(_stack: &mut NockStack) -> Noun {
        D(0x4000)
    }

    fn atom_3(_stack: &mut NockStack) -> Noun {
        D(0x4200)
    }

    fn atom_1_5(_stack: &mut NockStack) -> Noun {
        D(0x3e00)
    }

    fn atom_1_1(_stack: &mut NockStack) -> Noun {
        D(0x3c66)
    }

    fn atom_0_8(_stack: &mut NockStack) -> Noun {
        D(0x3a66)
    }

    fn atom_0_3(_stack: &mut NockStack) -> Noun {
        D(0x34cd)
    }

    fn r_sample_n(_stack: &mut NockStack) -> Noun {
        D('n' as u8 as u64)
    }

    fn r_sample_z(_stack: &mut NockStack) -> Noun {
        D('z' as u8 as u64)
    }

    fn r_sample_u(_stack: &mut NockStack) -> Noun {
        D('u' as u8 as u64)
    }

    fn r_sample_d(_stack: &mut NockStack) -> Noun {
        D('d' as u8 as u64)
    }

    #[test]
    fn test_rh_add() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_add, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_add, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3c00));
        assert_jet_in_door(s, jet_rh_add, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x4000));
        assert_jet_in_door(s, jet_rh_add, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0x4200));
        assert_jet_in_door(s, jet_rh_add, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x4200));
        assert_jet_in_door(s, jet_rh_add, &[atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3c66));
    }

    #[test]
    fn test_rh_sub() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_sub, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_sub, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0xbc00));
        assert_jet_in_door(s, jet_rh_sub, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_sub, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0xbc00));
        assert_jet_in_door(s, jet_rh_sub, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3c00));
        assert_jet_in_door(s, jet_rh_sub, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], D(0x34cc));
        assert_jet_in_door(s, jet_rh_sub, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3a66));
}

    #[test]
    fn test_rh_mul() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_mul, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_mul, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_mul, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3c00));
        assert_jet_in_door(s, jet_rh_mul, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0x4000));
        assert_jet_in_door(s, jet_rh_mul, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], D(0x4400));
        assert_jet_in_door(s, jet_rh_mul, &[atom_1_5, atom_2], &[atom_0, r_sample_n, atom_0], D(0x4200));
        assert_jet_in_door(s, jet_rh_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_z, atom_0], D(0x3b09));
        assert_jet_in_door(s, jet_rh_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_u, atom_0], D(0x3b0a));
        assert_jet_in_door(s, jet_rh_mul, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3547));
    }

    #[test]
    fn test_rh_div() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_div, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], D(HALFINF as u64));
        // XX test 0/0
        assert_jet_in_door(s, jet_rh_div, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_div, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3c00));
        assert_jet_in_door(s, jet_rh_div, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0x3800));
        assert_jet_in_door(s, jet_rh_div, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], D(0x3c00));
        assert_jet_in_door(s, jet_rh_div, &[atom_1_1, atom_0_8], &[atom_0, r_sample_z, atom_0], D(0x3d7f));
        assert_jet_in_door(s, jet_rh_div, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], D(0x3d80));
        assert_jet_in_door(s, jet_rh_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x4354));
        assert_jet_in_door(s, jet_rh_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_u, atom_0], D(0x4355));
    }

    #[test]
    fn test_rh_sqt() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_sqt, &[atom_0], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_sqt, &[atom_1], &[atom_0, r_sample_n, atom_0], D(0x3c00));
        assert_jet_in_door(s, jet_rh_sqt, &[atom_2], &[atom_0, r_sample_n, atom_0], D(0x3da8));
        assert_jet_in_door(s, jet_rh_sqt, &[atom_3], &[atom_0, r_sample_n, atom_0], D(0x3eee));
        assert_jet_in_door(s, jet_rh_sqt, &[atom_3], &[atom_0, r_sample_z, atom_0], D(0x3eed));
        assert_jet_in_door(s, jet_rh_sqt, &[atom_1_1], &[atom_0, r_sample_n, atom_0], D(0x3c32));
        assert_jet_in_door(s, jet_rh_sqt, &[atom_1_1], &[atom_0, r_sample_z, atom_0], D(0x3c31));
    }

    #[test]
    fn test_rh_fma() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_fma, &[atom_1, atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x0000));
        assert_jet_in_door(s, jet_rh_fma, &[atom_0, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3c00));
        assert_jet_in_door(s, jet_rh_fma, &[atom_1, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x4000));
        assert_jet_in_door(s, jet_rh_fma, &[atom_1, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x4200));
        assert_jet_in_door(s, jet_rh_fma, &[atom_2, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x4500));
        assert_jet_in_door(s, jet_rh_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3cb8));
        assert_jet_in_door(s, jet_rh_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_z, atom_0], D(0x3cb7));
    }

    #[test]
    fn test_rh_lth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_lth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rh_lth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rh_lth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }

    #[test]
    fn test_rh_lte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_lte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rh_lte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rh_lte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rh_equ() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_equ, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rh_equ, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rh_equ, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rh_gte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_gte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rh_gte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rh_gte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rh_gth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rh_gth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rh_gth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rh_gth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }



}
