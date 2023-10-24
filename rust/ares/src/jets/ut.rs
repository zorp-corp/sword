use crate::interpreter::{Context, Error, interpret};
use crate::jets::util::*;
use crate::jets::{JetErr, Result};
use crate::noun::{Atom, DirectAtom, IndirectAtom, Noun, D, DIRECT_MAX, NO, T, YES};

crate::gdb!();

pub fn jet_mint_ut(
    context: &mut Context,
    subject: Noun,
) -> Result {
    let sam = slot(subject, 6)?;
    let gol = slot(sam, 2);
    let gen = slot(sam, 3);
    let ctx = slot(subject, 7)?;
    let bat = slot(subject, 2)?;
    let pay = slot(sam, 3)?;
    let sut = sam;  // XX refactor out

    // XX unlike Vere, we don't have runtime caching yet
    // so we just call the nock interpreter directly
    match interpret(context, pay, bat) {
        Ok(res) => Ok(res),
        Err(err) => Err(JetErr::Fail(err))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::{Context, Error};
    use crate::mem::NockStack;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_context, A, assert_noun_eq};
    use crate::jets::{Jet, JetErr};
    use crate::noun::{Noun, Cell, D, T};
    use ares_macros::tas;
    use ibig::ubig;

    pub fn assert_jet_in_door(
        context: &mut Context,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],  // regular sample
        ctx: &[fn(&mut NockStack) -> Noun],  // door sample as context
        res: Noun) {
        unsafe {
            let mut sam: Vec<Noun> = sam.iter().map(|f| f(&mut context.stack)).collect();
            let mut ctx: Vec<Noun> = ctx.iter().map(|f| f(&mut context.stack)).collect();
            let sam = if sam.len() > 1 { T(&mut context.stack, &sam) } else { sam[0] };
            eprintln!("sam: {:?}", sam);
            let ctx = if ctx.len() > 1 { T(&mut context.stack, &ctx) } else { ctx[0] };
            eprintln!("ctx: {:?}", ctx);
            let pay = Cell::new(&mut context.stack, sam, ctx).as_noun();
            eprintln!("pay: {:?}", pay);
            let sbj = Cell::new(&mut context.stack, D(0), pay).as_noun();
            eprintln!("sbj: {:?}", sbj);
            let jet_res = jet(context, sbj).unwrap();
            eprintln!("jet: {:x}\n", jet_res.as_atom().expect("").as_direct().expect("").data());
            assert_noun_eq(&mut context.stack, jet_res, res);
        }
    }

    fn atom_0(_stack: &mut NockStack) -> Noun {
        D(0x00000000)
    }

    //  %rock
    fn atom_rock(_stack: &mut NockStack) -> Noun {
        D(0x6b636f72)
    }

    //  %tas
    fn atom_tas(_stack: &mut NockStack) -> Noun {
        D(0x736174)
    }

    //  %hello
    fn atom_hello(_stack: &mut NockStack) -> Noun {
        D(0x6f6c6c6568)
    }

    //  %atom
    fn atom_atom(_stack: &mut NockStack) -> Noun {
        D(0x6d6f7461)
    }

    //  %noun
    fn atom_noun(_stack: &mut NockStack) -> Noun {
        D(0x6e756f6e)
    }

    #[test]
    fn test_mint_ut() {
        let c = &mut init_context();

        // (~(mint ut %noun) %noun (ream '%hello'))
        // [[%atom %tas ~ 478.560.413.032] %1 478.560.413.032]
        let text = D(0x6f6c6c6568);  // XX what is idiomatic way to do a cord?
        let res_typ = T(&mut c.stack, &[D(0x6d6f7461), D(0x736174), D(0x0), D(0x6f6c6c6568)]);
        let res_val = T(&mut c.stack, &[D(0x1), D(0x6f6c6c6568)]);
        let res = T(&mut c.stack, &[res_typ, res_val]);
        assert_jet_in_door(c, jet_mint_ut,
            &[atom_rock, atom_tas, atom_hello],
            &[atom_0, atom_noun, atom_0],
            res
        );
    }
}
/*
u3_noun
u3wfu_mint(u3_noun cor)
{
  u3_noun bat, sut, gol, gen, van;

  if (  (c3n == u3r_mean(cor, u3x_sam_2, &gol,
                              u3x_sam_3, &gen,
                              u3x_con, &van, 0))
     || (u3_none == (bat = u3r_at(u3x_bat, van)))
     || (u3_none == (sut = u3r_at(u3x_sam, van))) )
  {
    return u3m_bail(c3__fail);
  }
  else {
    c3_m  fun_m = 141 + c3__mint;
    u3_noun vet = u3r_at(u3qfu_van_vet, van);
    u3_noun key = u3z_key_5(fun_m, vet, sut, gol, gen, bat);
    u3_weak pro = u3z_find(key);

    if ( u3_none != pro ) {
      u3z(key);
      return pro;
    }
    else {
      pro = u3n_nock_on(u3k(cor), u3k(u3x_at(u3x_bat, cor)));
      return u3z_save(key, pro);
    }
  }
}

*/