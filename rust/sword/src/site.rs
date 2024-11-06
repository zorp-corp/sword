/** Call site of a kick (Nock 9), used to cache call targets. */
use bitvec::order::Lsb0;
use bitvec::slice::BitSlice;

use crate::interpreter::{interpret, Context};
use crate::jets::util::slot;
use crate::jets::{Jet, JetErr};
use crate::mem::AllocResult;
use crate::noun::{Noun, D, T};

/// Return Err if the computation crashed or should punt to Nock
pub type Result = std::result::Result<Noun, JetErr>;

pub struct Site {
    pub battery: Noun,    // battery
    pub context: Noun,    // context
    pub jet: Option<Jet>, // jet driver
    pub path: Noun,       // label
}

impl Site {
    /// Prepare a locally cached gate to call repeatedly.
    pub fn new(ctx: &mut Context, core: &mut Noun) -> AllocResult<Site> {
        let mut battery = slot(*core, 2).unwrap();
        let context = slot(*core, 7).unwrap();

        let warm_result = ctx
            .warm
            .find_jet(&mut ctx.stack, core, &mut battery)?
            .filter(|(_jet, mut path)| {
                // check that 7 is a prefix of the parent battery axis,
                // to ensure that the sample (axis 6) is not part of the jet match.
                //
                // XX TODO this check is pessimized since there could be multiple ways to match the
                // jet and we only actually match one of them, but we check all of them and run
                // unjetted if any have an axis outside 7.
                let axis_7_bits: &BitSlice<u64, Lsb0> = BitSlice::from_element(&7u64);
                // TODO: This panic is ugly but the code is awkward.
                let batteries_list = ctx.cold.find(&mut ctx.stack, &mut path).ok().expect("OOM'd on ctx.cold.find in Site::new");
                let mut ret = true;
                for mut batteries in batteries_list {
                    if let Some((_, parent_axis)) = batteries.next() {
                        // let parent_axis = unsafe { (*battery.0).parent_axis };
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
        });
        Ok(Site {
            battery,
            context,
            jet: warm_result.map(|(jet, _)| jet),
            path: warm_result.map(|(_, path)| path).unwrap_or(D(0)),
        })
    }
}

/// Slam a cached call site.
pub fn site_slam(ctx: &mut Context, site: &Site, sample: Noun) -> Result {
    let subject = T(&mut ctx.stack, &[site.battery, sample, site.context])?;
    if site.jet.is_some() {
        let jet = site.jet.unwrap();
        jet(ctx, subject)
    } else {
        Ok(interpret(ctx, subject, site.battery)?)
    }
}
