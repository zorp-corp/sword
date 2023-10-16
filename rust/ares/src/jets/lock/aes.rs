use crate::jets::util::{met, slot};
use crate::jets::{JetErr, Result};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{IndirectAtom, Noun, YES, NO};
use urcrypt_sys::*;

crate::gdb!();

type urcrypt_siv = fn(&mut [u8],
    usize,
    urcrypt_aes_siv_data,
    usize,
    &mut [u8],
    [u8; 16],
    &mut u8
);

pub fn jet_siva_en(stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> Result {
    let txt = slot(subject, 6)?.as_atom();
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if (met(3, key) as usize) > 32 {
        // XX vere punts; we should do the same in the future
        Err(JetErr::NonDeterministic)
    } else {
        unsafe {
            let key_bytes = key.as_bytes();
            let (mut _kee_ida, kee) = IndirectAtom::new_raw_mut_bytes(stack, 32);
            kee[0..key_bytes.len()].copy_from_slice(key_bytes);

            Ok(util::siv_en(stack,
                kee.as_mut_ptr(),
                32,
                ads,
                txt,
                urcrypt_aes_siva_en
            ))
        }
    }
}

mod util {
    use crate::noun::{D, IndirectAtom, Noun};
    use crate::jets::util::met;
    use urcrypt_sys::urcrypt_aes_siv_data;
    use crate::jets::Result;

    /// Returns a tuple of (soc, mat, dat):
    /// * soc: number of items in the list of atoms
    /// * mat: size in bytes of the associative array
    /// * dat: size of allocation (array + atom storage)
    ///
    /// # Arguments
    ///
    /// * `ads` - the list of atoms to measure
    ///
    pub fn measure_ads(ads: Noun) -> (usize, usize, usize) {
        let mut soc = 0;
        let mut mat = 0;
        let mut dat = 0;

        let mut tail = ads;
        let mut a = 0;
        let mut b = 0;
        unsafe {
            while !tail.raw_equals(D(0)) {
                let (head, ttail) = match tail.as_cell() {
                    Ok(cell) => (cell.head(), cell.tail()),
                    Err(_) => panic!("measure_ads: not a cell"),
                };
                let head = match head.as_atom() {
                    Ok(a) => a,
                    Err(_) => panic!("measure_ads: head not an atom"),
                };
                tail = ttail;

                let tmp = b;
                b += met(3, head);
                //  XX why this check?
                if b < tmp {
                    panic!("measure_ads: overflow");
                }
                a += 1;
            }

            // check for size overflows
            let tmp = a * std::mem::size_of::<urcrypt_aes_siv_data>();
            if (tmp / a) != std::mem::size_of::<urcrypt_aes_siv_data>() {
                panic!("measure_ads: wrong size")
            } else if (tmp + b) < tmp {
                dat = tmp + b;
                panic!("measure_ads: overflow")
            } else {
                soc = a;
                mat = tmp;
            }

            (soc, mat, dat)
        }
    }

    /// Encodes the list of atoms. Assumes ads is a
    /// valid list of atoms, as it's already been measured.
    ///
    ///
    /// # Arguments
    ///
    /// * `ads` - the list of atoms to allocate
    /// * `mat` - the encoding size
    /// * `dat` - the data allocation
    ///
    /*
static void
_cqea_encode_ads(u3_noun ads,
                 c3_w mat_w,
                 urcrypt_aes_siv_data *dat_u)
{
  c3_w met_w;
  u3_noun i, t;
  urcrypt_aes_siv_data *cur_u;
  c3_y *dat_y = ((c3_y*) dat_u) + mat_w;

  for ( cur_u = dat_u, t = ads; u3_nul != t; t = u3t(t), ++cur_u ) {
    i = u3h(t);
    met_w = u3r_met(3, i);
    u3r_bytes(0, met_w, dat_y, i);
    cur_u->length = met_w;
    cur_u->bytes = dat_y;
    dat_y += met_w;
  }
}
*/
    pub fn encode_ads(
        ads: Noun,
        mat: usize,
        dat: *mut urcrypt_aes_siv_data,
    ) -> Result {
        let mut current = unsafe { *(dat) };
        let mut data_bytes: &mut [u8] = unsafe {
            std::slice::from_raw_parts_mut(current.bytes, current.length + mat)
        };

        let mut head: Noun;
        let mut tail: Noun = ads;
        unsafe {
            while !tail.raw_equals(D(0)) {
                let head = tail.as_cell()?.head().as_atom()?;
                let num_bytes = met(3, head);
                current.length = num_bytes;
                current.bytes = data_bytes.as_mut_ptr();
                data_bytes[0..num_bytes].copy_from_slice(head.as_bytes());
                data_bytes = &mut data_bytes[num_bytes..];
                let tail = tail.as_cell()?.tail();
                return Ok(ads);
            }
        }
        Ok(ads)
    }

    pub fn ads_alloc(ads: Noun, soc: &mut usize) -> urcrypt_aes_siv_data {
        if !ads {
            soc = 0;
            D(0x0)
        } else {
            let (soc, mat, dat) = measure_ads(ads);
            let dat = IndirectAtom::new_raw_mut_bytes(stack, dat);
            let dat = dat.as_mut_ptr() as *mut urcrypt_aes_siv_data;
            encode_ads(ads, mat, dat);
            dat
        }
    }

    #[allow(non_snake_case)]
    pub fn siv_en(stack: &mut NockStack,
        &[u8]: key_y,
        usize: key_w,
        Noun: ads,
        Atom: txt,
        urcrypt_siv: low_f
    ) -> Noun {
        let txt_w: usize;
        let soc_w: usize;
        let txt_y: &[u8];
        let out_y: &[u8];
        let iv_y: [u8; 16];
        let dat_u: &[urcrypt_aes_siv_data];

        dat_u = ads_alloc(ads, &soc_w);
        txt_y = IndirectAtom::new_raw_mut_bytes(stack, dat);
        txt_y[0..txt_w].copy_from_slice(txt);
        let dat = dat.as_mut_ptr() as *mut urcrypt_aes_siv_data;
        let out = IndirectAtom::new_raw_mut_bytes(stack, txt_w);

        let ret = match (*low_f)(txt_y, txt_w, dat_u, soc_w, key_y, iv_y, out_y) {
            0 => T(stack, &[iv_y, txt_w[0], txt_w]),
                 // ^ not correct yet
            _ => D(0x0)
        };

        ret
    }

    pub fn siv_de(stack: &mut NockStack,
        &[u8]: key_y,
        usize: key_w,
        Noun: ads,
        Atom: iv,
        Atom: len,
        Atom: txt,
        urcrypt_siv: low_f
    ) -> Noun {

    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ibig::ubig;
    use crate::noun::{D, T};
    use crate::jets::util::test::{A, assert_jet, init_stack, assert_jet_err};

    fn string_to_ubig(text: &str) -> UBig {
        let bytes = text.as_bytes();
        let mut lsb_value = ubig!(0x0);

        for (i, &byte) in bytes.iter().enumerate() {
            lsb_value += ubig!(byte) << (i * 8);
        }

        lsb_value
    }

    fn atom_hello_mars(stack: &mut NockStack) -> Noun {
        A(stack, string_to_ubig("Hello Mars"))
    }

    #[test]
    fn test_siva_en() {
        let s = &mut init_stack();
        /*
        > (~(en siva:aes:crypto 0x0 ~))
        [p=0xb0f7.a0df.be76.c85b.5e29.bb31.aaec.fc77 q=0 r=0x0]
         */
        assert_jet(
            s,
            jet_siva_en,
            A(s, &ubig!(0x0)),
            T(s, &[A(s, &ubig!(0xb0f7a0dfbe76c85b5e29bb31aaecfc77)), D(0x0), D(0x0)]),
        );
        /*
        > (~(en siva:aes:crypto (shax 0x0) ~))
        [p=0x9e60.092f.8061.fcad.e893.1c80.dc36.f050 q=0 r=0x0]
        */
        let (mut ida_0, out) = IndirectAtom::new_raw_mut_bytes(stack, 512);
        urcrypt_shay(D(0x0).as_bytes().as_ptr(), len, out.as_mut_ptr());
        Ok(ida_0.normalize_as_atom().as_noun())
        assert_jet(
            s,
            jet_siva_en,
            ida_0,
            T(s, &[A(s, &ubig!(0x9e60092f8061fcade8931c80dc36f050)), D(0x0), D(0x0)]),
        );
        /*
        > (~(en siva:aes:crypto (shax 'Hello Mars') ~))
        [p=0x3bd4.b5d5.652f.3ee0.9898.0b0d.c564.2498 q=0 r=0x0]
        */
        assert_jet(
            s,
            jet_siva_en,
            A(s, "Hello Mars"),
            T(s, &[A(s, &ubig!(0x3bd4b5d5652f3ee098980b0dc5642498)), D(0x0), D(0x0)]),
        );
        /*
        > (~(en siva:aes:crypto (shax 0x0) ~[1 2 3]))
        [p=0x2464.c22e.919a.df29.ab6a.d55b.885a.1cb2 q=0 r=0x0]
        */
        assert_jet(
            s,
            jet_siva_en,
            T(s, &[D(1), D(2), D(3)]),
            T(s, &[A(s, &ubig!(0x2464c22e919adf29ab6ad55b885a1cb2)), D(0x0), D(0x0)]),
        );
        /*
        > (~(en siva:aes:crypto (shax 'Hello Mars') ~[1 2 3]))
        [p=0x1114.18f8.b82e.ca32.d9f3.41f9.1c23.f555 q=0 r=0x0]
        */
        assert_jet(
            s,
            jet_siva_en,
            T(s, &[D(1), D(2), D(3)]),
            T(s, &[A(s, &ubig!(0x111418f8b82eca32d9f341f91c23f555)), D(0x0), D(0x0)]),
        );
    }


    fn atom_63(_stack: &mut NockStack) -> Noun {
        D(0x7fffffffffffffff)
    }


    fn atom_128(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xdeadbeef12345678fedcba9876543210))
    }

    fn atom_128_b(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(0xdeadbeef12345678fedcba9876540000))
    }

    fn atom_264(stack: &mut NockStack) -> Noun {
        A(
            stack,
            &ubig!(_0xdeadbeef12345678fedcba9876540000deadbeef12345678fedcba9876540000ff),
        )
    }

    fn atom_528(stack: &mut NockStack) -> Noun {
        A(stack, &ubig!(_0xdeadbeef12345678fedcba9876540000deadbeef12345678fedcba9876540000ffdeadbeef12345678fedcba9876540000deadbeef12345678fedcba9876540001ff))
    }

    fn assert_math_jet(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        res: UBig,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        assert_nary_jet_ubig(stack, jet, &sam, res);
    }

    fn assert_math_jet_noun(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        res: Noun,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        let sam = T(stack, &sam);
        assert_jet(stack, jet, sam, res);
    }

    fn assert_math_jet_err(
        stack: &mut NockStack,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun],
        err: JetErr,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(stack)).collect();
        let sam = T(stack, &sam);
        assert_jet_err(stack, jet, sam, err);
    }

    #[test]
    fn test_add() {
        let s = &mut init_stack();
        assert_math_jet(
            s,
            jet_add,
            &[atom_128, atom_96],
            ubig!(0xdeadbef00d03068514bb685765666666),
        );
        assert_math_jet(
            s,
            jet_add,
            &[atom_63, atom_96],
            ubig!(0xfaceb00c95deadbeef123455),
        );
        assert_math_jet(s, jet_add, &[atom_63, atom_63], ubig!(0xfffffffffffffffe));
    }

}
