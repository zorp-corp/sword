/** Parsing jets
 */
use crate::interpreter::{Context, interpret};
use crate::jets::util::slot;
use crate::jets::Result;
use crate::noun::{Noun, D};

crate::gdb!();

/*
++  pose                                                ::  first or second
  ~/  %pose
  |*  [vex=edge sab=rule]
  ?~  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq]
  vex
::
      u3_noun roq = u3x_good(u3n_kick_on(u3k(sab)));
      u3_noun p_roq, q_roq;
      u3_noun ret;

      u3x_cell(roq, &p_roq, &q_roq);
      ret = u3nc(_last(p_vex, p_roq),
                 u3k(q_roq));

      u3z(roq);
      return ret;
*/
pub fn jet_pose(_context: &mut Context, subject: Noun) -> Result {
    let vex = slot(subject, 12)?;
    let sab = slot(subject, 13)?;

    let vel = vex.as_cell();
    match vel {
        Ok(c) => Ok(vex),
        Error => {
            let
        }
    }
}

pub mod util {
    pub fn last() {}
}

pub mod test {

}
