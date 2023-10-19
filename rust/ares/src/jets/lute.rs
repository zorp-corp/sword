/** ++ut jets (compiler backend and pretty-printer)
 */
use ares_macros::tas;
use crate::interpreter::{Context, interpret};
use crate::jets::util::*;
use crate::jets::JetErr::*;
use crate::jets::Result;
use crate::noun::{Noun, D, T};

crate::gdb!();

/*
u3_noun
u3wfu_fuse(u3_noun cor)
{
  u3_noun bat, sut, ref, van;

  if (  (c3n == u3r_mean(cor, u3x_sam, &ref, u3x_con, &van, 0))
     || (u3_none == (bat = u3r_at(u3x_bat, van)))
     || (u3_none == (sut = u3r_at(u3x_sam, van))) )
  {
    return u3m_bail(c3__fail);
  }
  else {
    u3_weak vet = u3r_at(u3qfu_van_vet, van);
    c3_m  fun_m = 141 + c3__fuse + ((!!vet) << 8);
    u3_noun key = u3z_key_3(fun_m, sut, ref, bat);
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
pub fn jet_fuse(context: &mut Context, subject: Noun) -> Result {
    let ref_ = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let bat = slot(van , 2)?;
    let sut = slot(van , 6)?; // why does vere bail:fail if any of these are none?

    let vet = slot(van, 59)?;
    let flag = if unsafe { vet.raw_equals(D(0)) } { 0 } else { 1 };
    let fun = (141 + tas!(b"fuse")) + (flag << 8);
    let mut key = T(context.stack, &[D(fun), sut, ref_, bat]);
    let pro = context.cache.lookup(context.stack, &mut key);

    match pro {
        Some(pro) => Ok(pro),
        None => {
            let res = interpret(context, subject, slot(subject, 2)?);
            match res {
                Err(_) => {
                    Err(Deterministic)
                },
                Ok(pro) => {
                    context.cache.insert(context.stack, &mut key, pro);
                    Ok(pro)
                }
            }
        }
    }
}

/*
u3_noun
u3wfu_fish(u3_noun cor)
{
  u3_noun bat, sut, axe, van;

  if (  (c3n == u3r_mean(cor, u3x_sam, &axe, u3x_con, &van, 0))
     || (c3n == u3ud(axe))
     || (u3_none == (bat = u3r_at(u3x_bat, van)))
     || (u3_none == (sut = u3r_at(u3x_sam, van))) )
  {
    return u3m_bail(c3__fail);
  }
  else {
    u3_weak vet = u3r_at(u3qfu_van_vet, van);
    c3_m  fun_m = 141 + c3__fish + ((!!vet) << 8);
    u3_noun key = u3z_key_3(fun_m, sut, axe, bat);
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
pub fn jet_fish(context: &mut Context, subject: Noun) -> Result {
    let axe = slot(subject, 6)?.as_atom()?;
    let van = slot(subject, 7)?;
    let bat = slot(van , 2)?;
    let sut = slot(van , 6)?; // why does vere bail:fail if any of these are none?

    let vet = slot(van, 59)?;
    let flag = if unsafe { vet.raw_equals(D(0)) } { 0 } else { 1 };
    let fun = (141 + tas!(b"fish")) + (flag << 8);
    let mut key = T(context.stack, &[D(fun), sut, axe.as_noun(), bat]);
    let pro = context.cache.lookup(context.stack, &mut key);

    match pro {
        Some(pro) => Ok(pro),
        None => {
            let res = interpret(context, subject, slot(subject, 2)?);
            match res {
                Err(_) => {
                    Err(Deterministic)
                },
                Ok(pro) => {
                    context.cache.insert(context.stack, &mut key, pro);
                    Ok(pro)
                }
            }
        }
    }
}

/*
u3_noun
u3qf_fine(u3_noun fuv,
          u3_noun lup,
          u3_noun mar)
{
  if ( (c3__void == lup) || (c3__void == mar) ) {
    return c3__void;
  } else {
    return u3nq(c3__fine,
                u3k(fuv),
                u3k(lup),
                u3k(mar));
  }
}
*/
pub fn jet_fine(context: &mut Context, subject: Noun) -> Result {
    let fuv = slot(subject, 12)?;
    let lup = slot(subject, 26)?.as_direct()?;
    let mar = slot(subject, 27)?.as_direct()?;

    if tas!(b"void") == lup.data() || tas!(b"void") == mar.data() {
        Ok(D(tas!(b"void")))
    } else {
        Ok(T(context.stack, &[D(tas!(b"fine")), fuv, lup.as_noun(), mar.as_noun()]))
    }
}

/*
u3_noun
u3wfu_crop(u3_noun cor)
{
  u3_noun bat, sut, ref, van;

  if (  (c3n == u3r_mean(cor, u3x_sam, &ref, u3x_con, &van, 0))
     || (u3_none == (bat = u3r_at(u3x_bat, van)))
     || (u3_none == (sut = u3r_at(u3x_sam, van))) )
  {
    return u3m_bail(c3__fail);
  }
  else {
    u3_weak vet = u3r_at(u3qfu_van_vet, van);
    c3_m  fun_m = 141 + c3__crop + ((!!vet) << 8);
    u3_noun key = u3z_key_3(fun_m, sut, ref, bat);
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
pub fn jet_crop(context: &mut Context, subject: Noun) -> Result {
    let ref_ = slot(subject, 6)?;
    let van = slot(subject, 7)?;
    let bat = slot(van , 2)?;
    let sut = slot(van , 6)?; // why does vere bail:fail if any of these are none?

    let vet = slot(van, 59)?;
    let flag = if unsafe { vet.raw_equals(D(0)) } { 0 } else { 1 };
    let fun = (141 + tas!(b"crop")) + (flag << 8);
    let mut key = T(context.stack, &[D(fun), sut, ref_, bat]);
    let pro = context.cache.lookup(context.stack, &mut key);

    match pro {
        Some(pro) => Ok(pro),
        None => {
            let res = interpret(context, subject, slot(subject, 2)?);
            match res {
                Err(_) => {
                    Err(Deterministic)
                },
                Ok(pro) => {
                    context.cache.insert(context.stack, &mut key, pro);
                    Ok(pro)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, assert_jet_ubig, init_stack, A};
    use crate::mem::NockStack;
    use crate::noun::{Noun, D, T};
    use ibig::ubig;

    fn atoms(s: &mut NockStack) -> (Noun, Noun, Noun, Noun, Noun) {
        (atom_0(s), atom_24(s), atom_63(s), atom_96(s), atom_128(s))
    }

    fn atom_0(_stack: &mut NockStack) -> Noun {
        D(0)
    }

    fn atom_24(_stack: &mut NockStack) -> Noun {
        D(0x876543)
    }

    fn atom_63(_stack: &mut NockStack) -> Noun {
        D(0x7fffffffffffffff)
    }

    fn atom_96(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xfaceb00c15deadbeef123456))
    }

    fn atom_128(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xdeadbeef12345678fedcba9876543210))
    }

    #[test]
    fn test_crop() {
        let s = &mut init_stack();
    }
}
