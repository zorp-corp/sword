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

const QUADNAN: [u64; 2]  = [0x0000000000000000, 0x7fff800000000000];
const QUADINF: [u64; 2]  = [0x0000000000000000, 0x7fff000000000000];
const QUADZERO: [u64; 2] = [0x0000000000000000, 0x0000000000000000];

#[inline(always)]
fn _nan_test(
    a: float128_t
) -> bool {
    unsafe {
        !f128_eq(a, a)
    }
}

#[inline(always)]
fn _nan_unify(
    a: float128_t
) -> float128_t {
    unsafe {
        if _nan_test(a) {
        return softfloat_sys::float128_t { v: QUADNAN };
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

pub fn jet_rq_add(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rq MAY be indirect but they are 128 bits
        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f128_add(dat_a, dat_b));

        let x1_msb = (x.v[1] | 0) != 0;
        let x2_msb = ((x.v[0] >> 63) & 1) != 0;
        let x2_2sb = ((x.v[0] >> 62) & 1) != 0;

        if x1_msb || x2_msb || x2_2sb {
            let hi = (x.v[1] as u128) << 64;
            let lo = x.v[0] as u128;
            Ok(A(stack, &UBig::from(hi | lo)))
        } else {
            Ok(D(x.v[0]))
        }
    }
}

pub fn jet_rq_sub(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rq MAY be indirect but they are 128 bits
        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f128_sub(dat_a, dat_b));

        let x1_msb = (x.v[1] | 0) != 0;
        let x2_msb = ((x.v[0] >> 63) & 1) != 0;
        let x2_2sb = ((x.v[0] >> 62) & 1) != 0;

        if x1_msb || x2_msb || x2_2sb {
            let hi = (x.v[1] as u128) << 64;
            let lo = x.v[0] as u128;
            Ok(A(stack, &UBig::from(hi | lo)))
        } else {
            Ok(D(x.v[0]))
        }
    }
}

pub fn jet_rq_mul(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rq MAY be indirect but they are 128 bits
        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f128_mul(dat_a, dat_b));

        let x1_msb = (x.v[1] | 0) != 0;
        let x2_msb = ((x.v[0] >> 63) & 1) != 0;
        let x2_2sb = ((x.v[0] >> 62) & 1) != 0;

        if x1_msb || x2_msb || x2_2sb {
            let hi = (x.v[1] as u128) << 64;
            let lo = x.v[0] as u128;
            Ok(A(stack, &UBig::from(hi | lo)))
        } else {
            Ok(D(x.v[0]))
        }
    }
}

pub fn jet_rq_div(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe {
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        //  @rq MAY be indirect but they are 128 bits
        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f128_div(dat_a, dat_b));

        let x1_msb = (x.v[1] | 0) != 0;
        let x2_msb = ((x.v[0] >> 63) & 1) != 0;
        let x2_2sb = ((x.v[0] >> 62) & 1) != 0;

        if x1_msb || x2_msb || x2_2sb {
            let hi = (x.v[1] as u128) << 64;
            let lo = x.v[0] as u128;
            Ok(A(stack, &UBig::from(hi | lo)))
        } else {
            Ok(D(x.v[0]))
        }
    }
}

pub fn jet_rq_sqt(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = sam.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f128_sqrt(dat_a));

        let x1_msb = (x.v[1] | 0) != 0;
        let x2_msb = ((x.v[0] >> 63) & 1) != 0;
        let x2_2sb = ((x.v[0] >> 62) & 1) != 0;

        if x1_msb || x2_msb || x2_2sb {
            let hi = (x.v[1] as u128) << 64;
            let lo = x.v[0] as u128;
            Ok(A(stack, &UBig::from(hi | lo)))
        } else {
            Ok(D(x.v[0]))
        }
    }
}

pub fn jet_rq_fma(
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

        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let dat_c = softfloat_sys::float128_t { v: c.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let x = _nan_unify(f128_mulAdd(dat_a, dat_b, dat_c));

        let x1_msb = (x.v[1] | 0) != 0;
        let x2_msb = ((x.v[0] >> 63) & 1) != 0;
        let x2_2sb = ((x.v[0] >> 62) & 1) != 0;

        if x1_msb || x2_msb || x2_2sb {
            let hi = (x.v[1] as u128) << 64;
            let lo = x.v[0] as u128;
            Ok(A(stack, &UBig::from(hi | lo)))
        } else {
            Ok(D(x.v[0]))
        }
    }
}

pub fn jet_rq_lth(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f128_lt(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rq_lte(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f128_le(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rq_equ(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f128_eq(dat_a, dat_b);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rq_gte(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f128_le(dat_b, dat_a);

        if t { Ok(YES) } else { Ok(NO) }
    }
}

pub fn jet_rq_gth(
    stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> jets::Result {
    unsafe{
        let sam = slot(subject, 6)?;
        let a = slot(sam, 2)?.as_atom()?;
        let b = slot(sam, 3)?.as_atom()?;
        let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;

        let dat_a = softfloat_sys::float128_t { v: a.as_u128_pair()? };
        let dat_b = softfloat_sys::float128_t { v: b.as_u128_pair()? };
        let mod_r = _set_rounding(r);

        softfloat_roundingMode_write_helper(mod_r);
        let t = f128_lt(dat_b, dat_a);

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
        A(stack, &ubig!(0x00000000000000000000000000000000))
    }

    fn atom_1(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3fff0000000000000000000000000000))
    }

    fn atom_2(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x40000000000000000000000000000000))
    }

    fn atom_3(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x40008000000000000000000000000000))
    }

    fn atom_1_5(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3fff8000000000000000000000000000))
    }

    fn atom_1_1(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3fff199999999999999999999999999a))
    }

    fn atom_0_8(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3ffe999999999999999999999999999a))
    }

    fn atom_0_3(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0x3ffd3333333333333333333333333333))
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
    fn test_rq_add() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rq_add, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_add, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_add, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_add, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40008000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_add, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40008000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_add, &[atom_0_8, atom_0_3], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3fff1999999999999999999999999999)));
        assert_jet_in_door(s, jet_rq_add, &[atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff199999999999999999999999999a)));
    }

    #[test]
    fn test_rq_sub() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rq_sub, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_sub, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0xbfff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_sub, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_sub, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0xbfff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_sub, &[atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_sub, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ffd3333333333333333333333333334)));
        assert_jet_in_door(s, jet_rq_sub, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ffe999999999999999999999999999a)));
}

    #[test]
    fn test_rq_mul() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rq_mul, &[atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40010000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_1_5, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40008000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3ffec28f5c28f5c28f5c28f5c28f5c2a)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ffec28f5c28f5c28f5c28f5c28f5c2a)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ffd51eb851eb851eb851eb851eb851f)));
        assert_jet_in_door(s, jet_rq_mul, &[atom_1_1, atom_0_3], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3ffd51eb851eb851eb851eb851eb851e)));
    }

    #[test]
    fn test_rq_div() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rq_div, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &UBig::from((QUADINF[1] as u128) << 64)));
        // XX test 0/0
        assert_jet_in_door(s, jet_rq_div, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_div, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_div, &[atom_1, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3ffe0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_div, &[atom_2, atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_div, &[atom_1_1, atom_0_8], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff6000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x4000d555555555555555555555555556)));
        assert_jet_in_door(s, jet_rq_div, &[atom_1_1, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x4000d555555555555555555555555556)));
    }

    #[test]
    fn test_rq_sqt() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rq_sqt, &[atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_sqt, &[atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_sqt, &[atom_2], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff6a09e667f3bcc908b2fb1366ea95)));
        assert_jet_in_door(s, jet_rq_sqt, &[atom_2], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3fff6a09e667f3bcc908b2fb1366ea95)));
        assert_jet_in_door(s, jet_rq_sqt, &[atom_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fffbb67ae8584caa73b25742d7078b8)));
        assert_jet_in_door(s, jet_rq_sqt, &[atom_1_1], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3fff0c7ebc96a56f59f61213380ea638)));
        assert_jet_in_door(s, jet_rq_sqt, &[atom_1_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0c7ebc96a56f59f61213380ea638)));
    }

    #[test]
    fn test_rq_fma() {
        let s = &mut init_stack();
        let q = &mut init_stack();

        assert_jet_in_door(s, jet_rq_fma, &[atom_1, atom_0, atom_0], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x00000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_fma, &[atom_0, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff0000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_fma, &[atom_1, atom_1, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40000000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_fma, &[atom_1, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40008000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_fma, &[atom_2, atom_2, atom_1], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x40014000000000000000000000000000)));
        assert_jet_in_door(s, jet_rq_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_n, atom_0], A(q, &ubig!(0x3fff2e147ae147ae147ae147ae147ae2)));
        assert_jet_in_door(s, jet_rq_fma, &[atom_1_1, atom_0_8, atom_0_3], &[atom_0, r_sample_z, atom_0], A(q, &ubig!(0x3fff2e147ae147ae147ae147ae147ae1)));
    }

    #[test]
    fn test_rq_lth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rq_lth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rq_lth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rq_lth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }

    #[test]
    fn test_rq_lte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rq_lte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rq_lte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rq_lte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rq_equ() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rq_equ, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rq_equ, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rq_equ, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rq_gte() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rq_gte, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rq_gte, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rq_gte, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], YES);
    }

    #[test]
    fn test_rq_gth() {
        let s = &mut init_stack();

        assert_jet_in_door(s, jet_rq_gth, &[atom_1, atom_0], &[atom_0, r_sample_n, atom_0], YES);
        assert_jet_in_door(s, jet_rq_gth, &[atom_0, atom_1], &[atom_0, r_sample_n, atom_0], NO);
        assert_jet_in_door(s, jet_rq_gth, &[atom_1, atom_1], &[atom_0, r_sample_n, atom_0], NO);
    }
}
