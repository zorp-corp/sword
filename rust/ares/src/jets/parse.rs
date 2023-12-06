/** Parsing jets
 */
use crate::interpreter::Context;
use crate::jets::util::{kick, slam, slot};
use crate::jets::Result;
use crate::noun::{Noun, D, T};

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

/*
    u3x_cell(tub, &p_tub, &q_tub);
    return u3nq(u3k(p_tub),
                u3_nul,
                u3k(huf),
                u3k(tub));

*/

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
    use crate::jets::{Jet, Result};
    use crate::noun::{Noun, D, T, Cell};
    use crate::jets::util::test::*;
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

    pub fn assert_jet_in_door(
        context: &mut Context,
        jet: Jet,
        sam: Noun, //&[fn(&mut Context) -> Noun],  // regular sample
        ctx: Noun, //&[fn(&mut Context) -> Noun],  // door sample as context
        res: Noun) {
        unsafe {
            // let mut sam: Vec<Noun> = sam.iter().map(|f| f(&mut c.stack)).collect();
            // let mut ctx: Vec<Noun> = ctx.iter().map(|f| f(&mut c.stack)).collect();
            // let sam = if(sam.len() > 1) { T(&mut c.stack, &sam) } else { sam[0] };
            eprintln!("sam: {:?}", sam);
            // let ctx = if(ctx.len() > 1) { T(&mut c.stack, &ctx) } else { ctx[0] };
            eprintln!("ctx: {:?}", ctx);
            let pay = Cell::new(&mut context.stack, sam, ctx).as_noun();
            eprintln!("pay: {:?}", pay);
            let sbj = Cell::new(&mut context.stack, D(0), pay).as_noun();
            eprintln!("sbj: {:?}", sbj);
            let jet_res = jet(context, sbj).unwrap();
            assert_noun_eq(&mut context.stack, jet_res, res);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::*;
    use crate::mem::NockStack;
    use crate::noun::{Noun, D, T};
    use crate::serialization::{cue, jam};

    use ibig::ubig;

    #[test]
    fn test_easy() {
        let c = &mut init_context();

        /*
          'a' at +6 of easy
          [[1 1] "abc"] at +6 of (easy 'a')
         */

        /// ((easy 'a') [[1 1] "abc"])
        //  [[1 1] "abc"]
        let sam_jam = A(&mut c.stack, &ubig!(3205468216717221061)).as_atom().unwrap();
        let sam = cue(&mut c.stack, sam_jam);
        //  [p=[p=1 q=1] q=[~ [p='a' q=[p=[p=1 q=1] q="abc"]]]]
        let ans_jam = A(&mut c.stack, &ubig!(1720922644868600060465749189)).as_atom().unwrap();
        let ans = cue(&mut c.stack, ans_jam);
        let ctx = T(&mut c.stack, &[D(0), D(97), D(0)]);
        util::assert_jet_in_door(c, jet_easy, sam, ctx, ans);

        /// ((easy %foo) [[1 1] "abc"])
        //  [[1 1] "abc"]
        let sam_jam = A(&mut c.stack, &ubig!(3205468216717221061)).as_atom().unwrap();
        let sam = cue(&mut c.stack, sam_jam);
        //  [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="abc"]]]]
        let ans_jam = A(&mut c.stack, &ubig!(3609036366588910247778413036281029)).as_atom().unwrap();
        let ans = cue(&mut c.stack, ans_jam);
        let ctx = T(&mut c.stack, &[D(0), D(0x6f6f66), D(0)]);
        util::assert_jet_in_door(c, jet_easy, sam, ctx, ans);
    }
}
