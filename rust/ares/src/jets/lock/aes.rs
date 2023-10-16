use crate::jets::util::{met, slot};
use crate::jets::{JetErr, Result};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{IndirectAtom, Noun};
use urcrypt_sys::*;

crate::gdb!();

pub fn jet_siva_en(stack: &mut NockStack,
    _newt: &mut Option<&mut Newt>,
    subject: Noun
) -> Result {
    let txt = slot(subject, 6)?.as_atom();
    let key = slot(subject, 60)?.as_atom()?;
    let atoms = slot(subject, 61)?;

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
                atoms,
                txt,
                urcrypt_aes_siva_en
            ))
        }
    }
}

mod util {
    use crate::mem::NockStack;
    use crate::noun::{Atom, D, Noun, IndirectAtom};
    use crate::jets::util::met;
    use urcrypt_sys::urcrypt_aes_siv_data;

    type UrcryptSiv = fn(&mut [u8],
        &[urcrypt_aes_siv_data],
        &mut [u8],
        [u8; 16],
        &mut [u8]
    );

    /// Returns a tuple of (length, bytes, size):
    /// * length: number of items in the list of atoms
    /// * bytes: size in bytes of the associative array
    /// * size: size of allocation (array + atom storage)
    ///
    /// # Arguments
    ///
    /// * `atoms` - the list of atoms to measure
    ///
    pub fn _measure_atoms(atoms: Noun) -> (usize, usize, usize) {
        let mut length = 0;
        let mut bytes = 0;
        let mut size = 0;

        let mut tail = atoms;
        let mut a = 0;
        let mut b = 0;
        unsafe {
            while !tail.raw_equals(D(0)) {
                let (head, ttail) = match tail.as_cell() {
                    Ok(cell) => (cell.head(), cell.tail()),
                    Err(_) => panic!("measure_atoms: not a cell"),
                };
                let head = match head.as_atom() {
                    Ok(a) => a,
                    Err(_) => panic!("measure_atoms: head not an atom"),
                };
                tail = ttail;

                let tmp = b;
                b += met(3, head);
                //  could be just asserting that met returns more than 0
                if b < tmp {
                    panic!("measure_atoms: overflow");
                }
                a += 1;
            }

            // check for size overflows
            let tmp = a * std::mem::size_of::<urcrypt_aes_siv_data>();
            size = tmp + b;
            if (tmp / a) != std::mem::size_of::<urcrypt_aes_siv_data>() {
                panic!("measure_atoms: wrong size")
            } else if size < tmp {
                panic!("measure_atoms: overflow")
            } else {
                length = a;
                bytes = tmp;
            }

            (length, bytes, size)
        }
    }

    /// Encodes the list of atoms. Assumes atoms is a
    /// valid list of atoms, as it's already been measured.
    ///
    /// # Arguments
    ///
    /// * `atoms` - the list of atoms to allocate
    /// * `bytes` - the encoding size
    /// * `data` - the data allocation
    ///
    pub fn _encode_atoms(atoms: Noun, bytes: usize, data: &mut [urcrypt_aes_siv_data]) {
        // iterate through the list of atoms
        let mut t = atoms;
        let mut i = 0;
        unsafe {
            while !t.raw_equals(D(0)) {
                let (head, tail) = match t.as_cell() {
                    Ok(cell) => (cell.head(), cell.tail()),
                    Err(_) => panic!("_encode_atoms: not a cell"),
                };
                let head = match head.as_atom() {
                    Ok(a) => a,
                    Err(_) => panic!("_encode_atoms: head not an atom"),
                };
                t = tail;
                let head_bytes = head.as_bytes();
                data[i].length = head_bytes.len();
                for j in 0..head_bytes.len() {
                    let byte_ptr = data[i+j].bytes;
                    (*(byte_ptr)) = head_bytes[j];
                }
                i += data[i].length;
            }
        }
    }

    pub fn _allocate_atoms(atoms: Noun, length: usize) -> &'static mut [urcrypt_aes_siv_data] {
        let (length, bytes, size) = _measure_atoms(atoms);
        let siv_data: &mut [urcrypt_aes_siv_data];
        _encode_atoms(atoms, bytes, siv_data);
        siv_data
    }

    /*
static u3_noun
_cqea_siv_en(c3_y*   key_y,
             c3_w    key_w,
             u3_noun ads,
             u3_atom txt,
             urcrypt_siv low_f)
{
  u3_noun ret;
  c3_w txt_w, soc_w;
  c3_y *txt_y, *out_y, iv_y[16];
  urcrypt_aes_siv_data *dat_u;

  dat_u = _cqea_ads_alloc(ads, &soc_w);
  txt_y = u3r_bytes_all(&txt_w, txt);
  out_y = u3a_malloc(txt_w);

  ret = ( 0 != (*low_f)(txt_y, txt_w, dat_u, soc_w, key_y, iv_y, out_y) )
      ? u3_none
      : u3nt(u3i_bytes(16, iv_y),
             u3i_words(1, &txt_w),
             u3i_bytes(txt_w, out_y));

  u3a_free(txt_y);
  u3a_free(out_y);
  _cqea_ads_free(dat_u);
  return ret;
}
     */
    pub fn siv_en(stack: &mut NockStack, key: &mut [u8], atoms: Noun, text: Atom, func: UrcryptSiv) -> Noun {
        let siv_data = _allocate_atoms(atoms, 0);
        let (txt_ida, txt_bytes) = unsafe { IndirectAtom::new_raw_mut_bytes(stack, siv_data.len()) };
        txt_bytes.copy_from_slice(text.as_bytes());
        let mut iv = [0u8; 16];
        let mut out: &[u8];
        let mut ret = 0;
        unsafe {
            func(txt_bytes, siv_data, key, iv, &mut out);
        }
        atoms
    }

    // pub fn siv_de(stack: &mut NockStack,
    //     &[u8]: key_y,
    //     usize: key_w,
    //     Noun: atoms,
    //     Atom: iv,
    //     Atom: len,
    //     Atom: txt,
    //     urcrypt_siv: low_f
    // ) -> Noun {
    // }
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
