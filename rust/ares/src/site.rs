/** Call site of a kick (Nock 9), used to cache call targets. */
use bitvec::order::Lsb0;
use bitvec::slice::BitSlice;

use crate::interpreter::Context;
use crate::jets::util::slot;
use crate::jets::{Jet, Result};
use crate::noun::{D, Noun};

pub struct Site {
    pub jet: Jet,
    pub path: Noun,
}

impl Site {
    /// Prepare a locally cached gate to call repeatedly.
    pub fn new(ctx: &mut Context, gate: &mut Noun) -> Option<Site> {
        let mut gate_battery = slot(*gate, 2).unwrap();
        if let Some((jet, path)) = ctx
            .warm
            .find_jet(&mut ctx.stack, gate, &mut gate_battery)
            .filter(|(_jet, mut path)| {
                // check that 7 is a prefix of the parent battery axis,
                // to ensure that the sample (axis 6) is not part of the jet match.
                //
                // XX TODO this check is pessimized since there could be multiple ways to match the
                // jet and we only actually match one of them, but we check all of them and run
                // unjetted if any have an axis outside 7.
                let axis_7_bits: &BitSlice<u64, Lsb0> = BitSlice::from_element(&7u64);
                let batteries_list = ctx.cold.find(&mut ctx.stack, &mut path);
                let mut ret = true;
                for mut batteries in batteries_list {
                    if let Some((_battery, parent_axis)) = batteries.next() {
                        let parent_axis_prefix_bits = &parent_axis.as_bitslice()[0..3];
                        if parent_axis_prefix_bits == axis_7_bits {
                            continue;
                        } else {
                            ret = false;
                            break;
                        }
                    } else {
                        ret = false;
                        break;
                    }
                }
                ret
            })
        {
            return Some(Site { jet, path } );
        }
        None
    }

    /// Kick a core with a cached `Site`.
    pub fn kick() -> Result {
        Ok(D(0))
    }
}
