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

const SINGNAN: u32  = 0x7fc00000;
const SINGINF: u32  = 0x7f800000;
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
            return softfloat_sys::float32_t { v: SINGNAN };
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
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f32_add(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rs_sub(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f32_sub(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rs_mul(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f32_mul(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rs_div(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f32_div(dat_a, dat_b));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rs_sqt(
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

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f32_sqrt(dat_a));

        Ok(D(x.v as u64))
    }
}

pub fn jet_rs_fma(
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

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let dat_c = softfloat_sys::float32_t { v: c.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f32_mulAdd(dat_a, dat_b, dat_c));
        
        Ok(D(x.v as u64))
    }
}

pub fn jet_rs_lth(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f32_lt(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rs_lte(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f32_le(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rs_equ(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f32_eq(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rs_gte(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f32_le(dat_b, dat_a);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rs_gth(
    _stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?.as_direct()?;
        let b = slot(sam, 3)?.as_atom()?.as_direct()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float32_t { v: a.data() as u32};
        let dat_b = softfloat_sys::float32_t { v: b.data() as u32};
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f32_lt(dat_b, dat_a);

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
        D(0x00000000)
    }

    fn atom_1(_stack: &mut NockStack) -> Noun {
        D(0x3f800000)
    }

    fn atom_2(_stack: &mut NockStack) -> Noun {
        D(0x40000000)
    }

    fn atom_3(_stack: &mut NockStack) -> Noun {
        D(0x40400000)
    }

    fn atom_1_5(_stack: &mut NockStack) -> Noun {
        D(0x3fc00000)
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
    fn test_rs_add() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_add, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_add, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3f800000));
        assert_jet_in_door(s, jet_rs_add, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x40000000));
        assert_jet_in_door(s, jet_rs_add, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0x40400000));
        assert_jet_in_door(s, jet_rs_add, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x40400000));
        assert_jet_in_door(s, jet_rs_add, &[atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3f8ccccd));
    }

    #[test]
    fn test_rs_sub() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_sub, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_sub, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0xbf800000));
        assert_jet_in_door(s, jet_rs_sub, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_sub, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0xbf800000));
        assert_jet_in_door(s, jet_rs_sub, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3f800000));
        assert_jet_in_door(s, jet_rs_sub, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], D(0x3e99999a));
        assert_jet_in_door(s, jet_rs_sub, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3f4ccccd));
}

    #[test]
    fn test_rs_mul() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_mul, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_mul, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_mul, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3f800000));
        assert_jet_in_door(s, jet_rs_mul, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0x40000000));
        assert_jet_in_door(s, jet_rs_mul, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], D(0x40800000));
        assert_jet_in_door(s, jet_rs_mul, &[atom_1_5, atom_2], &[atom_0, r_sample_n, atom_0], D(0x40400000));
        assert_jet_in_door(s, jet_rs_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_z, atom_0], D(0x3f6147ae));
        assert_jet_in_door(s, jet_rs_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], D(0x3f6147af));
        assert_jet_in_door(s, jet_rs_mul, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3ea8f5c3));
    }

    #[test]
    fn test_rs_div() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_div, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], D(SINGINF as u64));
        // XX test 0/0
        assert_jet_in_door(s, jet_rs_div, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_div, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3f800000));
        assert_jet_in_door(s, jet_rs_div, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], D(0x3f000000));
        assert_jet_in_door(s, jet_rs_div, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], D(0x3f800000));
        assert_jet_in_door(s, jet_rs_div, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], D(0x3fb00000));
        assert_jet_in_door(s, jet_rs_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x406aaaaa));
        assert_jet_in_door(s, jet_rs_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_u, atom_0], D(0x406aaaab));
    }

    #[test]
    fn test_rs_sqt() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_sqt, &[atom_0], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_sqt, &[atom_1], &[atom_0, r_sample_n, atom_0], D(0x3f800000));
        assert_jet_in_door(s, jet_rs_sqt, &[atom_2], &[atom_0, r_sample_n, atom_0], D(0x3fb504f3));
        assert_jet_in_door(s, jet_rs_sqt, &[atom_3], &[atom_0, r_sample_n, atom_0], D(0x3fddb3d7));
        assert_jet_in_door(s, jet_rs_sqt, &[atom_1_1], &[atom_0, r_sample_n, atom_0], D(0x3f863f5e));
    }

    #[test]
    fn test_rs_fma() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_fma, &[atom_1, atom_0, atom_0], &[atom_0, r_sample_n, atom_0], D(0x00000000));
        assert_jet_in_door(s, jet_rs_fma, &[atom_0, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x3f800000));
        assert_jet_in_door(s, jet_rs_fma, &[atom_1, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], D(0x40000000));
        assert_jet_in_door(s, jet_rs_fma, &[atom_1, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x40400000));
        assert_jet_in_door(s, jet_rs_fma, &[atom_2, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], D(0x40a00000));
        assert_jet_in_door(s, jet_rs_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], D(0x3f970a3e));
        assert_jet_in_door(s, jet_rs_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_z, atom_0], D(0x3f970a3d));
    }

    #[test]
    fn test_rs_lth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_lth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rs_lth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rs_lth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }

    #[test]
    fn test_rs_lte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_lte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rs_lte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rs_lte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rs_equ() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_equ, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rs_equ, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rs_equ, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rs_gte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_gte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rs_gte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rs_gte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rs_gth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rs_gth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rs_gth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rs_gth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }
}
