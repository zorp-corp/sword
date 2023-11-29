use either::Either::{Left, Right};
use libc::QIF_USAGE;

/** Parsing jets
 */
use crate::interpreter::{interpret, Context, Error};
use crate::jets::util::{kick, slam, slot};
use crate::jets::{Result, JetErr};
use crate::noun::{Noun, D, T};

crate::gdb!();

pub fn jet_bend(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?;
    let sab = slot(sam, 3)?;
    let van = slot(subject, 7)?;
    let raq = slot(van, 6)?;
    
    let p_vex = vex.as_cell()?.head();
    let q_vex = vex.as_cell()?.tail();

    if !q_vex.is_cell() {
        return Ok(vex);
    }

    let uq_vex = q_vex.as_cell()?.tail();
    let puq_vex = uq_vex.as_cell()?.head();
    let quq_vex = uq_vex.as_cell()?.tail();

    let yit = slam(context, sab, quq_vex)?;

    let p_yit = yit.as_cell()?.head();
    let q_yit = yit.as_cell()?.tail();

    let yur = util::last(context, p_vex, p_yit)?;

    if !q_yit.is_cell() {
        Ok(T(&mut context.stack, &[yur, vex]))
    } else {
        let uq_yit = q_yit.as_cell()?.tail();
        let puq_yit = uq_yit.as_cell()?.head();
        let quq_yit = uq_yit.as_cell()?.tail();

        let arg = T(&mut context.stack, &[puq_vex, puq_yit]);
        let vux = slam(context, raq, arg)?;

        if unsafe { vux.raw_equals(D(0x0)) } {
            Ok(T(&mut context.stack, &[yur, q_vex]))
        } else {
            let q_vux = vux.as_cell()?.tail();
            Ok(T(&mut context.stack, &[yur, D(0x0), q_vux, quq_yit]))
        }
    }
}

pub fn jet_comp(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?;
    let sab = slot(sam, 3)?;
    let van = slot(subject, 7)?;
    let raq = slot(van, 6)?;

    let p_vex = vex.as_cell()?.head();
    let q_vex = vex.as_cell()?.tail();

    if !q_vex.is_cell() {
        return Ok(vex);
    } else {
        let uq_vex = q_vex.as_cell()?.tail();
        let puq_vex = uq_vex.as_cell()?.head();
        let quq_vex = uq_vex.as_cell()?.tail();

        let yit = slam(context, sab, quq_vex)?;

        let p_yit = yit.as_cell()?.head();
        let q_yit = yit.as_cell()?.tail();

        let yur = util::last(context, p_vex, p_yit)?;

        if !q_yit.is_cell() {
            Ok(T(&mut context.stack, &[yur, q_vex]))
        } else {
            let uq_yit = q_yit.as_cell()?.tail();
            let puq_yit = uq_yit.as_cell()?.head();
            let quq_yit = uq_yit.as_cell()?.tail();

            let arg = T(&mut context.stack, &[puq_vex, puq_yit]);
            let puq_arg = slam(context, raq, puq_yit)?;
            Ok(T(&mut context.stack, &[yur, D(0x0), puq_arg, quq_yit]))
        }
    }
}    

pub fn jet_easy(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let huf = slot(van, 6)?;

    let p_tub = tub.as_cell()?.head();
    let q_tub = tub.as_cell()?.tail();

    Ok(T(&mut context.stack, &[p_tub, D(0x0), huf, tub],))
}

/*
/* glue
*/
  static u3_noun
  _cqe_glue_fun(u3_noun bus,
                u3_noun vex,
                u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3n == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3x_good(u3n_slam_on(u3k(bus), u3k(quq_vex)));

      u3x_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( c3n == u3du(q_yit) ) {
        ret = u3nc(yur, u3_nul);
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;
        u3_noun wam, p_wam, q_wam, goy;

        u3x_cell(uq_yit, &puq_yit, &quq_yit);
        wam = u3x_good(u3n_slam_on(u3k(sab), u3k(quq_yit)));

        u3x_cell(wam, &p_wam, &q_wam);
        goy = _last(yur, p_wam);
        u3z(yur);

        if ( c3n == u3du(q_wam) ) {
          ret = u3nc(goy, u3_nul);
        } else {
          u3_noun uq_wam = u3t(q_wam);
          u3_noun puq_wam, quq_wam;

          u3x_cell(uq_wam, &puq_wam, &quq_wam);
          ret = u3nq(goy,
                     u3_nul,
                     u3nc(u3k(puq_vex),
                          u3k(puq_wam)),
                     u3k(quq_wam));
        }
        u3z(wam);
      }
      u3z(yit);
      return ret;
    }
  }

  u3_noun
 u3we_glue_fun(u3_noun cor)
  {
    u3_noun van, bus, vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex,
                                u3x_sam_3, &sab,
                                u3x_con, &van, 0)) ||
         (u3_none == (bus = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_glue_fun(bus, vex, sab);
    }
  }
 */

pub fn jet_glue(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?;
    let sab = slot(sam, 3)?;
    let van = slot(subject, 7)?;
    let bus = slot(van, 6)?;

    let p_vex = vex.as_cell()?.head();
    let q_vex = vex.as_cell()?.tail();

    if !q_vex.is_cell() {
        Ok(vex)
    } else {
        let uq_vex = q_vex.as_cell()?.tail();
        let puq_vex = uq_vex.as_cell()?.head();
        let quq_vex = uq_vex.as_cell()?.tail();

        let yit = slam(context, bus, quq_vex)?;
        let p_yit = yit.as_cell()?.head();
        let q_yit = yit.as_cell()?.tail();

        let yur = util::last(context, p_vex, p_yit)?;

        if !q_yit.is_cell() {
            Ok(T(&mut context.stack, &[yur, D(0x0)]))
        } else{
            let uq_yit = q_yit.as_cell()?.tail();
            let puq_yit = uq_yit.as_cell()?.head();
            let quq_yit = uq_yit.as_cell()?.tail();

            let wam = slam(context, sab, quq_yit)?;
            let p_wam = wam.as_cell()?.head();
            let q_wam = wam.as_cell()?.tail();

            let goy = util::last(context, yur, p_wam)?;

            if !q_wam.is_cell() {
                Ok(T(&mut context.stack, &[goy, D(0x0)]))
            } else {
                let uq_wam = q_wam.as_cell()?.tail();
                let puq_wam = uq_wam.as_cell()?.head();
                let quq_wam = uq_wam.as_cell()?.tail();

                let puq_arg = T(&mut context.stack, &[puq_vex, puq_wam]);
                Ok(T(&mut context.stack, &[goy, D(0x0), puq_arg, quq_wam]))
            }

        }
    }
}

pub fn jet_here(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let hez = slot(van, 2)?;
    let sef = slot(van, 3)?;

    let p_tub = tub.as_cell()?.head();

    let vex = slam(context, sef, tub)?;
    let p_vex = vex.as_cell()?.head();
    let q_vex = vex.as_cell()?.tail();

    if !q_vex.is_cell() {
        Ok(vex)
    } else {
        let uq_vex = q_vex.as_cell()?.tail();
        let puq_vex = uq_vex.as_cell()?.head();
        let quq_vex = uq_vex.as_cell()?.tail();
        let pquq_vex = quq_vex.as_cell()?.head();

        let gud = T(&mut context.stack, &[p_tub, pquq_vex, puq_vex]);
        let wag = slam(context, hez, gud)?;
        Ok(T(&mut context.stack, &[p_vex, D(0x0), wag, quq_vex]))
    }
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

pub fn jet_last(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let zyc = slot(sam, 2)?;
    let naz = slot(sam, 3)?;

    util::last(context, zyc, naz)
}

pub fn jet_mask(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let bud = &mut slot(van, 6)?;

    let p_tub = tub.as_cell()?.head();
    let q_tub = tub.as_cell()?.tail();

    if !q_tub.is_cell() {
        return util::fail(context, tub);
    } else {
        let iq_tub = q_tub.as_cell()?.head();

        while bud.is_cell() {
            if unsafe { bud.as_cell()?.head().raw_equals(iq_tub) } {
                return util::next(context, tub);
            }
            let bud = &mut bud.as_cell()?.tail();
        }
        return util::fail(context, tub);
    }
}

pub fn jet_pfix(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?;
    let sab = slot(sam, 3)?;

    let p_vex = vex.as_cell()?.head();
    let q_vex = vex.as_cell()?.tail();

    if !q_vex.is_cell() {
        return Ok(vex);
    }

    let uq_vex = q_vex.as_cell()?.tail();
    let puq_vex = uq_vex.as_cell()?.head();
    let quq_vex = uq_vex.as_cell()?.tail();

    let yit = slam(context, sab, quq_vex)?;

    let p_yit = yit.as_cell()?.head();
    let q_yit = yit.as_cell()?.tail();

    let arg = util::last(context, p_vex, p_yit)?;
    Ok(T(&mut context.stack, &[arg, q_yit]))
}

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

pub fn jet_sfix(context: &mut Context, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let vex = slot(sam, 2)?;
    let sab = slot(sam, 3)?;

    let p_vex = vex.as_cell()?.head();
    let q_vex = vex.as_cell()?.tail();

    if !q_vex.is_cell() {
        return Ok(vex);
    }

    let uq_vex = q_vex.as_cell()?.tail();
    let puq_vex = uq_vex.as_cell()?.head();
    let quq_vex = uq_vex.as_cell()?.tail();

    let yit = slam(context, sab, quq_vex)?;

    let p_yit = yit.as_cell()?.head();
    let q_yit = yit.as_cell()?.tail();

    let yur = util::last(context, p_vex, p_yit)?;

    if !q_yit.is_cell() {
        Ok(T(&mut context.stack, &[yur, D(0x0)]))
    } else {
        let uq_yit = q_yit.as_cell()?.tail();
        let puq_yit = uq_yit.as_cell()?.head();
        let quq_yit = uq_yit.as_cell()?.tail();

        Ok(T(&mut context.stack, &[yur, D(0x0), puq_vex, quq_yit]))
    }
}

pub fn jet_stag(context: &mut Context, subject: Noun) -> Result {
    let tub = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let gob = slot(van, 14)?;
    let sef = slot(van, 15)?;

    let vex = slam(context, sef, tub)?;
    let p_vex = vex.as_cell()?.head();
    let q_vex = vex.as_cell()?.tail();

    if !q_vex.is_cell() {
        Ok(vex)
    } else {

        let uq_vex = q_vex.as_cell()?.tail();
        let puq_vex = uq_vex.as_cell()?.head();
        let quq_vex = uq_vex.as_cell()?.tail();

        let wag = T(&mut context.stack, &[gob, puq_vex]);
        Ok(T(&mut context.stack, &[p_vex, D(0x0), wag, quq_vex]))
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
