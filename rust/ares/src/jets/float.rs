/** Floating-point jets
 */
use crate::jets;
use crate::jets::JetErr::*;
use crate::jets::util::*;
use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_ubig, assert_nary_jet_ubig, init_stack, A};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, IndirectAtom, Noun, D, T};
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
    unsafe{
    let arg = slot(subject, 6)?;
    let a = slot(arg, 2)?.as_atom()?.as_direct()?;
    eprintln!("a: {}", a.data());
    //let b = slot(arg, 3)?.as_atom()?.as_direct()?;
    //eprintln!("b: {}", b.data());
    //let r = slot(subject, 30)?.as_atom()?.as_direct()?.data() as u8 as char;
    //eprintln!("r: {}", r);
    //let r = 'n';

    return Ok(D(0x0));

   /* unsafe {
        let dat_a: float32_t = ui32_to_f32(a.data() as u32);
        let dat_b: float32_t = ui32_to_f32(b.data() as u32);
        let mod_r = _set_rounding(r);

        if f32_eq(dat_a, ui32_to_f32(SINGZERO)) {
            return Ok(b.as_noun());
        }
        if f32_eq(dat_b, ui32_to_f32(SINGZERO)) {
            return Ok(a.as_noun());
        }

        let c = _nan_unify(f32_add(dat_a, dat_b));

        Ok(D(f32_to_ui32(c, mod_r, true)))*/
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
    use crate::jets::{Jet, JetErr};
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_stack};
    use crate::noun::D;
    use crate::jets::util::test::{assert_noun_eq};
    use assert_no_alloc::assert_no_alloc;

    pub fn assert_door_jet(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        ctx: &mut Vec<Noun>,
        res: Noun) {
            unsafe {
        let mut sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        sam.insert(0, D(0));  // prepend ~ for battery bunt
        sam.append(ctx);        // append context for tail, so at +7 already
        eprintln!("sam: {:?}", sam);
        let pay = T(stack, &sam);
        eprintln!("pay: {:?}", pay);
        let jet_res = jet(stack, &mut None, pay).unwrap();
        eprintln!("jet: {:?}", jet_res);
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

    // at +7 already, so door sample at +30 should be at +6
    fn context(stack: &mut NockStack, value: Noun) -> Vec<Noun> {
        [D(0), value, D(0)].to_vec()
    }

    #[test]
    fn test_rs_add() {
        let s = &mut init_stack();
        let mut c: Vec<Noun> = context(s, D('n' as u64));

        eprintln!("atom_0 {}\n", atom_0(s).as_atom().expect("REASON").as_direct().unwrap().data());
        assert_door_jet(s, jet_rs_add, &[atom_0, atom_0], &mut c, D(0x00000000));
        //assert_door_jet(s, jet_rs_add, &[atom_0, atom_1], c, D(0x3f800000));
        //assert_door_jet(s, jet_rs_add, &[atom_1, atom_1], c, D(0x40000000));
        //assert_door_jet(s, jet_rs_add, &[atom_1, atom_2], c, D(0x40400000));
        //assert_door_jet(s, jet_rs_add, &[atom_2, atom_1], c, D(0x40400000));
        //assert_door_jet(s, jet_rs_add, &[atom_0_8, atom_0_3], c, D(0x3f8ccccd));
    }
}
