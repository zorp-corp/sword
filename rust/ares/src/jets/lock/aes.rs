use crate::jets::util::{met, slot};
use crate::jets::{JetErr, Result};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{IndirectAtom, Noun, YES, NO};
use urcrypt_sys::*;

crate::gdb!();

/*
// the siv* hoon doesn't explicitly check keysizes, but all of these functions
// have fixed maximum keysizes, so we will punt if we get a key that is too
// large.

static u3_noun
_cqea_siva_en(u3_atom key,
              u3_noun ads,
              u3_atom txt)
{
  if ( u3r_met(3, key) > 32 ) {
    return u3_none;
  }
  else {
    c3_y key_y[32];
    u3r_bytes(0, 32, key_y, key);
    return _cqea_siv_en(key_y, 32, ads, txt, &urcrypt_aes_siva_en);
  }
}
*/

pub fn jet_siva_en(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let txt = slot(subject, 6)?.as_atom();
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if (met(3, key) as usize) > 32 {
        // vere punts; we should do the same in the future
        return Err(JetErr::NonDeterministic);
    }

    unsafe {
        let key_bytes = key.as_bytes();
        let (mut _kee_ida, kee) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        for i in 0..kee.len() {
            kee[i] = key_bytes[i];
        }

    }

    Err(JetErr::Deterministic)
}

mod util {
    use crate::noun::{D, Noun};
    use std::result::Result;
    use crate::jets::JetErr;
    use crate::jets::util::met;

    /// Returns a tuple of (soc, mat, dat):
    /// * soc: number of items in the list of atoms
    /// * mat: size in bytes of the associative array
    /// * dat: size of allocation (array + atom storage)
    ///
    /// # Arguments
    ///
    /// * `ads` - the list of atoms to measure
    ///
    pub fn measure_ads(ads: Noun) -> Result<(usize, usize, usize), JetErr> {
        let mut soc = 0;
        let mut mat = 0;
        let mut dat = 0;

        let mut t = ads;
        unsafe {
            while !t.raw_equals(D(0)) {
                let (i, tt) = match t.as_cell() {
                    Ok(cell) => (cell.head(), cell.tail()),
                    Err(_) => panic!("measure_ads: not a cell"),
                };
                let i = match i.as_atom() {
                    Ok(a) => a,
                    Err(_) => panic!("measure_ads: not an atom"),
                };

            }

            (soc, mat, dat)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ibig::ubig;
    use crate::noun::{D, T};
    use crate::jets::util::test::{A, assert_jet, init_stack, assert_jet_err};

    #[test]
    fn test_siva_en() {
        let s = &mut init_stack();
    }
}
