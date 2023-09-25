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

const DOUBNAN: u64  = 0x7ff8000000000000;
const DOUBINF: u64  = 0x7ff0000000000000;
const DOUBZERO: u64 = 0x0000000000000000;

#[inline(always)]
fn _nan_test(
    a: float64_t
) -> bool {
    unsafe {
        !f64_eq(a, a)
    }
}

#[inline(always)]
fn _nan_unify(
    a: float64_t
) -> float64_t {
    unsafe {
        if _nan_test(a) {
            return softfloat_sys::float64_t { v: DOUBNAN };
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

pub fn jet_rd_add(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rd MAY be indirect but they are 64 bits
        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f64_add(dat_a, dat_b));

        let x_msb = ((x.v >> 63) & 1) != 0;
        let x_2sb = ((x.v >> 62) & 1) != 0;

        if x_msb || x_2sb {
            Ok(A(stack, &UBig::from(x.v as u64)))
        } else {
            Ok(D(x.v as u64))
        }
    }
}

pub fn jet_rd_sub(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rd MAY be indirect but they are 64 bits
        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f64_sub(dat_a, dat_b));

        let x_msb = ((x.v >> 63) & 1) != 0;
        let x_2sb = ((x.v >> 62) & 1) != 0;

        if x_msb || x_2sb {
            Ok(A(stack, &UBig::from(x.v as u64)))
        } else {
            Ok(D(x.v as u64))
        }
    }
}

pub fn jet_rd_mul(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rd MAY be indirect but they are 64 bits
        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f64_mul(dat_a, dat_b));

        let x_msb = ((x.v >> 63) & 1) != 0;
        let x_2sb = ((x.v >> 62) & 1) != 0;

        if x_msb || x_2sb {
            Ok(A(stack, &UBig::from(x.v as u64)))
        } else {
            Ok(D(x.v as u64))
        }
    }
}

pub fn jet_rd_div(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rd MAY be indirect but they are 64 bits
        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f64_div(dat_a, dat_b));

        let x_msb = ((x.v >> 63) & 1) != 0;
        let x_2sb = ((x.v >> 62) & 1) != 0;

        if x_msb || x_2sb {
            Ok(A(stack, &UBig::from(x.v as u64)))
        } else {
            Ok(D(x.v as u64))
        }
    }
}

pub fn jet_rd_sqt(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = sam.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f64_sqrt(dat_a));

        let x_msb = ((x.v >> 63) & 1) != 0;
        let x_2sb = ((x.v >> 62) & 1) != 0;

        if x_msb || x_2sb {
            Ok(A(stack, &UBig::from(x.v as u64)))
        } else {
            Ok(D(x.v as u64))
        }
    }
}

pub fn jet_rd_fma(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 6)?.as_atom()?;
        let c = slot(sam, 7)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let dat_c = softfloat_sys::float64_t { v: c.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f64_mulAdd(dat_a, dat_b, dat_c));

        let x_msb = ((x.v >> 63) & 1) != 0;
        let x_2sb = ((x.v >> 62) & 1) != 0;

        if x_msb || x_2sb {
            Ok(A(stack, &UBig::from(x.v as u64)))
        } else {
            Ok(D(x.v as u64))
        }
    }
}

pub fn jet_rd_lth(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f64_lt(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rd_lte(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f64_le(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rd_equ(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f64_eq(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rd_gte(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f64_le(dat_b, dat_a);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rd_gth(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float64_t { v: a.as_u64()? };
        let dat_b = softfloat_sys::float64_t { v: b.as_u64()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f64_lt(dat_b, dat_a);

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
    use libc::SS;

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
            //eprintln!("jet: {:x}\n", jet_res.as_atom().expect("").as_direct().expect("").data());
            std::io::stderr().flush().unwrap();
            assert_noun_eq(stack, jet_res, res);
        }
    }

    fn atom_0(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x0000000000000000))
    }

    fn atom_1(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3ff0000000000000))
    }

    fn atom_2(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x4000000000000000))
    }

    fn atom_3(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x4008000000000000))
    }

    fn atom_1_5(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3ff8000000000000))
    }

    fn atom_1_1(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3ff199999999999a))
    }

    fn atom_0_8(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3fe999999999999a))
    }

    fn atom_0_3(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3fd3333333333333))
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
    fn test_rd_add() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rd_add, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_add, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0000000000000)));
        assert_jet_in_door(s, jet_rd_add, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4000000000000000)));
        assert_jet_in_door(s, jet_rd_add, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4008000000000000)));
        assert_jet_in_door(s, jet_rd_add, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4008000000000000)));
        assert_jet_in_door(s, jet_rd_add, &[atom_0_8, atom_0_3], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3ff1999999999999)));
        assert_jet_in_door(s, jet_rd_add, &[atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff199999999999a)));
    }

    #[test]
    fn test_rd_sub() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rd_sub, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_sub, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0xbff0000000000000)));
        assert_jet_in_door(s, jet_rd_sub, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_sub, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0xbff0000000000000)));
        assert_jet_in_door(s, jet_rd_sub, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0000000000000)));
        assert_jet_in_door(s, jet_rd_sub, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fd3333333333334)));
        assert_jet_in_door(s, jet_rd_sub, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fe999999999999a)));
}

    #[test]
    fn test_rd_mul() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rd_mul, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0000000000000)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4000000000000000)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4010000000000000)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_1_5, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4008000000000000)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3fec28f5c28f5c2a)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fec28f5c28f5c2a)));
        assert_jet_in_door(s, jet_rd_mul, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fd51eb851eb851f)));
    }

    #[test]
    fn test_rd_div() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rd_div, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &UBig::from(DOUBINF as u64)));
        // XX test 0/0
        assert_jet_in_door(s, jet_rd_div, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_div, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0000000000000)));
        assert_jet_in_door(s, jet_rd_div, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fe0000000000000)));
        assert_jet_in_door(s, jet_rd_div, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0000000000000)));
        assert_jet_in_door(s, jet_rd_div, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff6000000000000)));
        assert_jet_in_door(s, jet_rd_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x400d555555555556)));
        assert_jet_in_door(s, jet_rd_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x400d555555555556)));
    }

    #[test]
    fn test_rd_sqt() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rd_sqt, &[atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_sqt, &[atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0000000000000)));
        assert_jet_in_door(s, jet_rd_sqt, &[atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff6a09e667f3bcd)));
        assert_jet_in_door(s, jet_rd_sqt, &[atom_2], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3ff6a09e667f3bcc)));
        assert_jet_in_door(s, jet_rd_sqt, &[atom_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ffbb67ae8584caa)));
        assert_jet_in_door(s, jet_rd_sqt, &[atom_1_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0c7ebc96a56f6)));
        assert_jet_in_door(s, jet_rd_sqt, &[atom_1_1], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3ff0c7ebc96a56f5)));
    }

    #[test]
    fn test_rd_fma() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rd_fma, &[atom_1, atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x0000000000000000)));
        assert_jet_in_door(s, jet_rd_fma, &[atom_0, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff0000000000000)));
        assert_jet_in_door(s, jet_rd_fma, &[atom_1, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4000000000000000)));
        assert_jet_in_door(s, jet_rd_fma, &[atom_1, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4008000000000000)));
        assert_jet_in_door(s, jet_rd_fma, &[atom_2, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4014000000000000)));
        assert_jet_in_door(s, jet_rd_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ff2e147ae147ae2)));
        assert_jet_in_door(s, jet_rd_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3ff2e147ae147ae1)));
    }

    #[test]
    fn test_rd_lth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rd_lth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rd_lth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rd_lth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }

    #[test]
    fn test_rd_lte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rd_lte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rd_lte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rd_lte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rd_equ() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rd_equ, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rd_equ, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rd_equ, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rd_gte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rd_gte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rd_gte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rd_gte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rd_gth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rd_gth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rd_gth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rd_gth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }
}
