use either::Either::{Left, Right};

/** Parsing jets
 */
use crate::interpreter::{Context, Error};
use crate::jets::util::{kick, slot};
use crate::jets::{Result, JetErr};
use crate::noun::{Noun, D, T};

crate::gdb!();

pub fn jet_pose(context: &mut Context, subject: Noun) -> Result {
    let vex = slot(subject, 12)?;
    let sab = slot(subject, 13)?;

    match vex.as_either_atom_cell() {
        Left(_a) => {
            assert!(unsafe { vex.raw_equals(D(0)) });

            //let fol = slam_gate_fol(&mut context.stack);
            let roq = kick(context, sab, D(2))?;

            let arg = &[
                util::last(context, vex.as_cell()?.head(), roq.as_cell()?.head())?,
                roq.as_cell()?.tail(),
            ];
            Ok(T(
                &mut context.stack,
                arg,
            ))
        }
        Right(_c) => Ok(vex),
    }
}

pub fn jet_last(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let zyc = slot(sam, 2)?;
    let naz = slot(sam, 3)?;

    util::last(context, zyc, naz)
}

pub fn jet_just(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let daf = slot(sam, 2)?;
    let tub = slot(sam, 3)?;

    let p_tub = tub.as_cell()?.head();
    let q_tub = tub.as_cell()?.tail();

    if !q_tub.is_cell() {
        return util::fail(context, tub);
    }

    let iq_tub = q_tub.as_cell()?.head();

    if unsafe { daf.raw_equals(iq_tub) } {
        util::next(context, tub)
    } else {
        util::fail(context, tub)
    }
}

pub mod util {
    use crate::interpreter::{Context, Error};
    use crate::jets::Result;
    use crate::jets::math::jet_add;
    use crate::noun::{Noun, D, T};

    pub fn last(context: &mut Context, zyc: Noun, naz: Noun) -> Result {
        let zyl = zyc.as_cell()?;
        let nal = naz.as_cell()?;

        assert!(zyl.head().is_direct());
        assert!(zyl.tail().is_direct());
        assert!(nal.head().is_direct());
        assert!(nal.tail().is_direct());

        unsafe {
            if zyl.head().raw_equals(nal.head()) {
                if zyl.tail().as_atom()?.as_u64()? > nal.tail().as_atom()?.as_u64()? {
                    Ok(zyc)
                } else {
                    Ok(naz)
                }
            } else {
                if zyl.head().as_atom()?.as_u64()? > nal.head().as_atom()?.as_u64()? {
                    Ok(zyc)
                } else {
                    Ok(naz)
                }
            }
        }
    }

    pub fn next(context: &mut Context, tub: Noun) -> Result {
        let p_tub = tub.as_cell()?.head();
        let q_tub = tub.as_cell()?.tail();

        if !q_tub.is_cell() {
            return fail(context, tub);
        }

        let iq_tub = q_tub.as_cell()?.head();
        let tq_tub = q_tub.as_cell()?.tail();

        let zac = slip(context, iq_tub, p_tub)?;

        Ok(T(&mut context.stack, &[zac, D(0x0), iq_tub, zac, tq_tub]))
    }

    pub fn slip(context: &mut Context, weq: Noun, naz: Noun) -> Result {
        let p_naz = naz.as_cell()?.head();
        let q_naz = naz.as_cell()?.tail();

        if unsafe { weq.raw_equals(D(10)) } {
            let arg = T(&mut context.stack, &[p_naz, D(1)]);
            let p_arg = jet_add(context, arg)?;
            Ok(T(&mut context.stack, &[p_arg, D(1)]))
        } else {
            let arg = T(&mut context.stack, &[q_naz, D(1)]);
            let q_arg = jet_add(context, arg)?;
            Ok(T(&mut context.stack, &[p_naz, q_arg]))
        }
    }

    pub fn fail(context: &mut Context, tub: Noun) -> Result {
        let p_tub = tub.as_cell()?.head();
        let q_tub = tub.as_cell()?.tail();

        Ok(T(&mut context.stack, &[p_tub, D(0)]))
    }
}

pub mod test {}
