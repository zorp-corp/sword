use std::result;

/** Parsing jets
 */
use crate::interpreter::{Context, Error};
use crate::jets::util::{kick, slam, slot};
use crate::jets::Result;
use crate::noun::{Noun, D, T};

use super::JetErr;

crate::gdb!();

//
//  Tracing
//

pub fn jet_last(_context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let zyc = slot(sam, 2)?;
    let naz = slot(sam, 3)?;

    util::last(zyc, naz)
}

//
//  Combinators
//

pub fn jet_bend(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?.as_cell()?;
    let sab = slot(sam, 3)?;
    let van = slot(subject, 7)?;
    let raq = slot(van, 6)?;

    let p_vex = vex.head();
    let q_vex = vex.tail();

    if unsafe { q_vex.raw_equals(D(0)) } {
        return Ok(vex.as_noun());
    }

    let uq_vex = q_vex.as_cell()?.tail().as_cell()?;
    let puq_vex = uq_vex.head();
    let quq_vex = uq_vex.tail();

    let yit = slam(context, sab, quq_vex)?.as_cell()?;
    let p_yit = yit.head();
    let q_yit = yit.tail();

    let yur = util::last(p_vex, p_yit)?;

    if unsafe { q_yit.raw_equals(D(0)) } {
        Ok(T(&mut context.stack, &[yur, q_vex]))
    } else {
        let uq_yit = q_yit.as_cell()?.tail().as_cell()?;
        let puq_yit = uq_yit.head();
        let quq_yit = uq_yit.tail();

        let arg = T(&mut context.stack, &[puq_vex, puq_yit]);
        let vux = slam(context, raq, arg)?;

        if unsafe { vux.raw_equals(D(0)) } {
            Ok(T(&mut context.stack, &[yur, q_vex]))
        } else {
            let q_vux = vux.as_cell()?.tail();
            Ok(T(&mut context.stack, &[yur, D(0), q_vux, quq_yit]))
        }
    }
}

pub fn jet_comp(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?.as_cell()?;
    let sab = slot(sam, 3)?;
    let van = slot(subject, 7)?;
    let raq = slot(van, 6)?;

    let p_vex = vex.head();
    let q_vex = vex.tail();

    if unsafe { q_vex.raw_equals(D(0)) } {
        return Ok(vex.as_noun());
    }

    let uq_vex = q_vex.as_cell()?.tail().as_cell()?;
    let puq_vex = uq_vex.head();
    let quq_vex = uq_vex.tail();

    let yit = slam(context, sab, quq_vex)?.as_cell()?;
    let p_yit = yit.head();
    let q_yit = yit.tail();

    let yur = util::last(p_vex, p_yit)?;

    if unsafe { q_yit.raw_equals(D(0)) } {
        Ok(T(&mut context.stack, &[yur, D(0)]))
    } else {
        let uq_yit = q_yit.as_cell()?.tail().as_cell()?;
        let puq_yit = uq_yit.head();
        let quq_yit = uq_yit.tail();

        let arg = T(&mut context.stack, &[puq_vex, puq_yit]);
        let vux = slam(context, raq, arg)?;
        Ok(T(&mut context.stack, &[yur, D(0), vux, quq_yit]))
    }
}

pub fn jet_glue(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?.as_cell()?;
    let sab = slot(sam, 3)?;
    let van = slot(subject, 7)?;
    let bus = slot(van, 6)?;

    let p_vex = vex.head();
    let q_vex = vex.tail();

    if unsafe { q_vex.raw_equals(D(0)) } {
        return Ok(vex.as_noun());
    }

    let uq_vex = q_vex.as_cell()?.tail().as_cell()?;
    let puq_vex = uq_vex.head();
    let quq_vex = uq_vex.tail();

    let yit = slam(context, bus, quq_vex)?.as_cell()?;
    let p_yit = yit.head();
    let q_yit = yit.tail();

    let yur = util::last(p_vex, p_yit)?;

    if unsafe { q_yit.raw_equals(D(0)) } {
        Ok(T(&mut context.stack, &[yur, D(0)]))
    } else {
        let uq_yit = q_yit.as_cell()?.tail().as_cell()?;
        let quq_yit = uq_yit.tail();

        let wam = slam(context, sab, quq_yit)?.as_cell()?;
        let p_wam = wam.head();
        let q_wam = wam.tail();

        let goy = util::last(yur, p_wam)?;

        if unsafe { q_wam.raw_equals(D(0)) } {
            Ok(T(&mut context.stack, &[goy, D(0)]))
        } else {
            let uq_wam = q_wam.as_cell()?.tail().as_cell()?;
            let puq_wam = uq_wam.head();
            let quq_wam = uq_wam.tail();

            let puq_arg = T(&mut context.stack, &[puq_vex, puq_wam]);
            Ok(T(&mut context.stack, &[goy, D(0x0), puq_arg, quq_wam]))
        }
    }
}

pub fn jet_pfix(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?.as_cell()?;
    let sab = slot(sam, 3)?;

    let p_vex = vex.head();
    let q_vex = vex.tail();

    if unsafe { q_vex.raw_equals(D(0)) } {
        return Ok(vex.as_noun());
    }

    let uq_vex = q_vex.as_cell()?.tail().as_cell()?;
    let quq_vex = uq_vex.tail();

    let yit = slam(context, sab, quq_vex)?.as_cell()?;

    let p_yit = yit.head();
    let q_yit = yit.tail();

    //  XX: Why don't we just return yit? When would p_vex ever be the later of the two?
    let arg = util::last(p_vex, p_yit)?;
    Ok(T(&mut context.stack, &[arg, q_yit]))
}

pub fn jet_pose(context: &mut Context, subject: Noun) -> Result {
    let vex = slot(subject, 12)?.as_cell()?;
    let sab = slot(subject, 13)?;

    let p_vex = vex.head();
    let q_vex = vex.tail();

    if unsafe { !q_vex.raw_equals(D(0)) } {
        return Ok(vex.as_noun());
    }

    let roq = kick(context, sab, D(2))?.as_cell()?;
    let yur = util::last(p_vex, roq.head())?;
    Ok(T(&mut context.stack, &[yur, roq.tail()]))
}

pub fn jet_sfix(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?.as_cell()?;
    let sab = slot(sam, 3)?;

    let p_vex = vex.head();
    let q_vex = vex.tail();

    if unsafe { q_vex.raw_equals(D(0)) } {
        return Ok(vex.as_noun());
    }

    let uq_vex = q_vex.as_cell()?.tail().as_cell()?;
    let puq_vex = uq_vex.head();
    let quq_vex = uq_vex.tail();

    let yit = slam(context, sab, quq_vex)?.as_cell()?;

    let p_yit = yit.head();
    let q_yit = yit.tail();
    let yur = util::last(p_vex, p_yit)?;

    if unsafe { q_yit.raw_equals(D(0)) } {
        Ok(T(&mut context.stack, &[yur, D(0)]))
    } else {
        let uq_yit = q_yit.as_cell()?.tail().as_cell()?;
        let quq_yit = uq_yit.tail();

        Ok(T(&mut context.stack, &[yur, D(0), puq_vex, quq_yit]))
    }
}

// +$  edge  [p=hair q=(unit [p=* q=nail])]
#[derive(Copy, Clone)]
struct StirPair {
    pub har: Noun,
    pub res: Noun,
}

pub fn jet_stir(context: &mut Context, subject: Noun) -> Result {
    unsafe {
        context.with_stack_frame(0, |context| {
            let mut tub = slot(subject, 6)?;
            let van = slot(subject, 7)?;
            let rud = slot(van, 12)?;
            let raq = slot(van, 26)?;
            let fel = slot(van, 27)?;

            // initial accumulator (deconstructed)
            let mut p_wag: Noun;
            let mut puq_wag: Noun;
            let quq_wag: Noun;

            // push incremental, succesful [fel] parse results onto stack
            {
                let vex = slam(context, fel, tub)?.as_cell()?;
                let mut p_vex = vex.head();
                let mut q_vex = vex.tail();
                eprintln!("stir: got vex\r\n");
                let i = 0;
                while !q_vex.raw_equals(D(0)) {
                    eprintln!("stir: starting vex loop {}\r\n", i);
                    let puq_vex = q_vex.as_cell()?.head();
                    let quq_vex = q_vex.as_cell()?.tail();

                    *(context.stack.push::<StirPair>()) = StirPair {
                        har: p_vex,
                        res: puq_vex,
                    };

                    tub = quq_vex;

                    let vex = slam(context, fel, tub)?.as_cell()?;
                    p_vex = vex.head();
                    q_vex = vex.tail();
                }

                p_wag = p_vex;
                puq_wag = rud;
                quq_wag = tub;
            }

            // unwind the stack, folding parse results into [wag] by way of [raq]
            let mut j = 0;
            while !context.stack.stack_is_empty() {
                eprintln!("stir: unwinding stack loop {}\r\n", j);
                let par_u = *(context.stack.top::<StirPair>());
                p_wag = util::last(par_u.har, p_wag)?;
                let sam = T(&mut context.stack, &[par_u.res, puq_wag]);
                puq_wag = slam(context, raq, sam)?;
                context.stack.pop::<StirPair>();
                j += 1;
            }

            let res = T(&mut context.stack, &[p_wag, D(0), puq_wag, quq_wag]);

            eprintln!("stir: done!\r\n");

            Ok(res)
        })
    }
}

fn stew_wor(ort: Noun, wan: Noun) -> result::Result<bool, JetErr> {
    eprintln!("stew_wor\r");
    if !ort.as_atom()?.is_direct() {
        return Err(JetErr::Fail(Error::Deterministic(D(0))));
    } else {
        if !wan.is_cell() {
            if !wan.as_atom()?.is_direct() {
                return Err(JetErr::Fail(Error::Deterministic(D(0))));
            } else {
                let ort_dat = ort.as_direct()?.data();
                let wan_dat = wan.as_direct()?.data();
                return Ok(ort_dat < wan_dat);
            }
        } else {
            let h_wan = wan.as_cell()?.head();

            if !h_wan.as_atom()?.is_direct() {
                return Err(JetErr::Fail(Error::Deterministic(D(0))));
            } else {
                let ort_dat = ort.as_direct()?.data();
                let h_wan_dat = h_wan.as_direct()?.data();
                return Ok(ort_dat < h_wan_dat);
            }
        }
    }
}

pub fn jet_stew(context: &mut Context, subject: Noun) -> Result {
    eprintln!("jet_stew\r");
    let tub = slot(subject, 6)?;
    let con = slot(subject, 7)?;
    let mut hel = slot(con, 2)?;

    let q_tub = tub.as_cell()?.tail();
    if !q_tub.is_cell() {
        return util::fail(context, tub);
    } else {
        let iq_tub = q_tub.as_cell()?.head();
        if !iq_tub.as_atom()?.is_direct() {
            return util::fail(context, tub);
        } else {
            loop {
                if !hel.is_cell() {
                    return util::fail(context, tub);
                } else {
                    let n_hel = slot(hel, 2)?;
                    let l_hel = slot(hel, 6)?;
                    let r_hel = slot(hel, 7)?;
                    let pn_hel = n_hel.as_cell()?.head();
                    let qn_hel = n_hel.as_cell()?.tail();
                    let bit;

                    if !pn_hel.is_cell() {
                        let iq_tub_dat = iq_tub.as_direct()?.data();
                        let pn_hel_dat = pn_hel.as_direct()?.data();
                        bit = iq_tub_dat == pn_hel_dat;
                    } else {
                        let hpn_hel = pn_hel.as_cell()?.head();
                        let tpn_hel = pn_hel.as_cell()?.tail();

                        if !hpn_hel.as_atom()?.is_direct() || !tpn_hel.as_atom()?.is_direct() {
                            return util::fail(context, tub);
                        } else {
                            let iq_tub_dat = iq_tub.as_direct()?.data();
                            let hpn_hel_dat = hpn_hel.as_direct()?.data();
                            let tpn_hel_dat = tpn_hel.as_direct()?.data();
                            bit = (iq_tub_dat >= hpn_hel_dat) && (iq_tub_dat <= tpn_hel_dat);
                        }
                    }

                    if bit {
                        return slam(context, qn_hel, tub);
                    } else {
                        if stew_wor(iq_tub, pn_hel)? {
                            hel = l_hel;
                        } else {
                            hel = r_hel;
                        }
                    }
                }
            }
        }
    }
}

//
//  Rule Builders
//

pub fn jet_easy(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let huf = slot(van, 6)?;

    Ok(T(
        &mut context.stack,
        &[tub.as_cell()?.head(), D(0), huf, tub],
    ))
}

pub fn jet_here(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let hez = slot(van, 12)?;
    let sef = slot(van, 13)?;

    let p_tub = tub.as_cell()?.head();

    let vex = slam(context, sef, tub)?.as_cell()?;
    let p_vex = vex.head();
    let q_vex = vex.tail();

    // XX fixes Vere's jet mismatch with Hoon 139.
    if unsafe { q_vex.raw_equals(D(0)) } {
        return Ok(vex.as_noun());
    }

    let uq_vex = q_vex.as_cell()?.tail().as_cell()?;
    let puq_vex = uq_vex.head();
    let quq_vex = uq_vex.tail();
    let pquq_vex = quq_vex.as_cell()?.head();

    let inner_gud = T(&mut context.stack, &[p_tub, pquq_vex]);
    let gud = T(&mut context.stack, &[inner_gud, puq_vex]);
    let wag = slam(context, hez, gud)?;

    Ok(T(&mut context.stack, &[p_vex, D(0), wag, quq_vex]))
}

pub fn jet_just(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let daf = slot(van, 6)?;

    let p_tub = tub.as_cell()?.head();
    let q_tub = tub.as_cell()?.tail();

    if unsafe { q_tub.raw_equals(D(0)) || !daf.raw_equals(q_tub.as_cell()?.head()) } {
        util::fail(context, p_tub)
    } else {
        util::next(context, tub)
    }
}

pub fn jet_mask(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let mut bud = slot(van, 6)?;

    let p_tub = tub.as_cell()?.head();
    let q_tub = tub.as_cell()?.tail();

    if unsafe { q_tub.raw_equals(D(0)) } {
        return util::fail(context, p_tub);
    }

    let iq_tub = q_tub.as_cell()?.head();
    while unsafe { !bud.raw_equals(D(0)) } {
        let cell = bud.as_cell()?;
        if unsafe { cell.head().raw_equals(iq_tub) } {
            return util::next(context, tub);
        }
        bud = cell.tail();
    }
    util::fail(context, p_tub)
}

pub fn jet_stag(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let gob = slot(van, 12)?;
    let sef = slot(van, 13)?;

    let vex = slam(context, sef, tub)?.as_cell()?;
    let p_vex = vex.head();
    let q_vex = vex.tail();

    if unsafe { q_vex.raw_equals(D(0)) } {
        Ok(vex.as_noun())
    } else {
        let uq_vex = q_vex.as_cell()?.tail().as_cell()?;
        let puq_vex = uq_vex.head();
        let quq_vex = uq_vex.tail();

        let wag = T(&mut context.stack, &[gob, puq_vex]);
        Ok(T(&mut context.stack, &[p_vex, D(0), wag, quq_vex]))
    }
}

pub mod util {
    use crate::interpreter::{inc, Context};
    use crate::jets::Result;
    use crate::noun::{Noun, D, T};
    use std::cmp::Ordering;

    pub fn last(zyc: Noun, naz: Noun) -> Result {
        let zyl = zyc.as_cell()?;
        let nal = naz.as_cell()?;

        let zyll = zyl.head().as_direct()?.data();
        let zylc = zyl.tail().as_direct()?.data();
        let nall = nal.head().as_direct()?.data();
        let nalc = nal.tail().as_direct()?.data();

        match zyll.cmp(&nall) {
            Ordering::Equal => {
                if zylc > nalc {
                    Ok(zyc)
                } else {
                    Ok(naz)
                }
            }
            Ordering::Greater => Ok(zyc),
            Ordering::Less => Ok(naz),
        }
    }

    // Passing Noun and doing Cell check inside next is best to keep jet semantics in sync w/ Hoon.
    pub fn next(context: &mut Context, tub: Noun) -> Result {
        let p_tub = tub.as_cell()?.head();
        let q_tub = tub.as_cell()?.tail();

        if unsafe { q_tub.raw_equals(D(0)) } {
            return fail(context, p_tub);
        }

        let iq_tub = q_tub.as_cell()?.head();
        let tq_tub = q_tub.as_cell()?.tail();

        let zac = lust(context, iq_tub, p_tub)?;
        Ok(T(&mut context.stack, &[zac, D(0), iq_tub, zac, tq_tub]))
    }

    // Passing Noun and doing Cell check inside next is best to keep jet semantics in sync w/ Hoon.
    pub fn lust(context: &mut Context, weq: Noun, naz: Noun) -> Result {
        let p_naz = naz.as_cell()?.head().as_atom()?;
        let q_naz = naz.as_cell()?.tail().as_atom()?;

        if unsafe { weq.raw_equals(D(10)) } {
            let arg = inc(&mut context.stack, p_naz).as_noun();
            Ok(T(&mut context.stack, &[arg, D(1)]))
        } else {
            let arg = inc(&mut context.stack, q_naz).as_noun();
            Ok(T(&mut context.stack, &[p_naz.as_noun(), arg]))
        }
    }

    pub fn fail(context: &mut Context, hair: Noun) -> Result {
        Ok(T(&mut context.stack, &[hair, D(0)]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::*;
    use crate::noun::{D, T};
    use crate::serialization::cue;
    use ibig::ubig;

    //  XX: need unit tests for:
    //      +last
    //      +bend
    //      +comp
    //      +glue
    //      +pfix
    //      +pose
    //      +sfix
    //      +here
    //      +just
    //      +mask
    //      +stag

    #[test]
    fn test_easy() {
        let c = &mut init_context();

        // ((easy 'a') [[1 1] "abc"])
        //  [[1 1] "abc"]
        let sam_jam = A(&mut c.stack, &ubig!(3205468216717221061))
            .as_atom()
            .unwrap();
        let sam = cue(&mut c.stack, sam_jam);
        //  [p=[p=1 q=1] q=[~ [p='a' q=[p=[p=1 q=1] q="abc"]]]]
        let ans_jam = A(&mut c.stack, &ubig!(1720922644868600060465749189))
            .as_atom()
            .unwrap();
        let ans = cue(&mut c.stack, ans_jam);
        let ctx = T(&mut c.stack, &[D(0), D(97), D(0)]);
        assert_jet_door(c, jet_easy, sam, ctx, ans);

        // ((easy %foo) [[1 1] "abc"])
        //  [[1 1] "abc"]
        let sam_jam = A(&mut c.stack, &ubig!(3205468216717221061))
            .as_atom()
            .unwrap();
        let sam = cue(&mut c.stack, sam_jam);
        //  [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="abc"]]]]
        let ans_jam = A(&mut c.stack, &ubig!(3609036366588910247778413036281029))
            .as_atom()
            .unwrap();
        let ans = cue(&mut c.stack, ans_jam);
        let ctx = T(&mut c.stack, &[D(0), D(0x6f6f66), D(0)]);
        assert_jet_door(c, jet_easy, sam, ctx, ans);
    }
}
