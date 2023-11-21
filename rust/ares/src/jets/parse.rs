use either::Either::{Left, Right};

/** Parsing jets
 */
use crate::interpreter::Context;
use crate::jets::util::{kick, slot};
use crate::jets::Result;
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

            Ok(T(
                &mut context.stack,
                &[
                    util::last(vex.as_cell()?.head(), roq.as_cell()?.head())?,
                    roq.as_cell()?.tail(),
                ],
            ))
        }
        Right(_c) => Ok(vex),
    }
}

pub fn jet_last(_context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let zyc = slot(sam, 2)?;
    let naz = slot(sam, 3)?;

    util::last(zyc, naz)
}

pub mod util {
    use crate::jets::Result;
    use crate::noun::Noun;

    pub fn last(zyc: Noun, naz: Noun) -> Result {
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
}

pub mod test {}
