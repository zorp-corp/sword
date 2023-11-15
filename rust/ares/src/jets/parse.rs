use either::Either::{Left, Right};

/** Parsing jets
 */
use crate::interpreter::{Context, interpret};
use crate::jets::util::{slot, kick};
use crate::jets::Result;
use crate::noun::{Noun, D, T};
use crate::jets::nock::util::slam_gate_fol;

crate::gdb!();

/*
u3_noun roq = u3x_good(u3n_kick_on(u3k(sab)));
u3_noun p_roq, q_roq;
u3_noun ret;

u3x_cell(roq, &p_roq, &q_roq);
ret = u3nc(_last(p_vex, p_roq),
            u3k(q_roq));

u3z(roq);
return ret;
*/
pub fn jet_pose(context: &mut Context, subject: Noun) -> Result {
    let vex = slot(subject, 12)?;
    let sab = slot(subject, 13)?;

    match vex.as_either_atom_cell() {
        Left(_a) => {
            assert!(vex.raw_equals(D(0)));

            //let fol = slam_gate_fol(&mut context.stack);
            let roq = kick(context, sab, D(2))?;

            Ok(T(&mut context.stack,
                &[util::last(vex.as_cell()?.head(),
                                 roq.as_cell()?.head())?,
                      roq.as_cell()?.tail()]))
        },
        Right(_c) => {
            Ok(vex)
        }
    }
}

pub mod util {
    use crate::interpreter::{Context, interpret};
    use crate::jets::util::slot;
    use crate::jets::Result;
    use crate::noun::{Noun, D};
    
    pub fn last(zyc: Noun, naz:Noun) -> Result {
        let zyl = zyc.as_cell()?;
        let nal = naz.as_cell()?;

        assert!(zyl.head().is_direct());
        assert!(zyl.tail().is_direct());
        assert!(nal.head().is_direct());
        assert!(nal.tail().is_direct());

        if zyl.head().raw_equals(nal.head()) {
            if zyl.tail().as_direct().as_u64() > nal.tail().as_direct().as_u64() {
                Ok(zyc)
            } else {
                Ok(naz)
            }
        } else {
            if zyl.head().as_direct().as_u64() > nal.head().as_direct().as_u64() {
                Ok(zyc)
            } else {
                Ok(naz)
            }
        }
    }
}

pub mod test {

}
