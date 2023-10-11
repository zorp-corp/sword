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

            siv_en(stack,
                kee.as_mut_ptr(),
                32,
                ads,
                txt,
                urcrypt_aes_siva_en
            )
        }
    }
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

        let mut tail = ads;
        let a = 0;
        let b = 0;
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
            } else if (dat = tmp + b) < tmp {
                panic!("measure_ads: overflow")
            } else {
                soc = a;
                mat = tmp;
            }

            (soc, mat, dat)
        }
    }

    /// # Arguments
    /// 
    /// * `ads` - the list of atoms to allocate
    /// * `mat` - the encoding size
    /// * `dat` - the data allocation
    ///
    pub fn encode_ads(
        ads: Noun,
        mat: usize,
        dat: &mut [urcrypt_aes_siv_data]
    ) {
        let current: &mut urcrypt_aes_siv_data;
        let data_bytes: &mut [u8] = &mut dat[mat..];

        let mut tail = ads;
        while !tail.raw_equals(D(0)) {
            let head = tail.head();
            let head = match head.as_atom() {
                Ok(head) => head,
                Err(_) => panic!("measure_ads: head not an atom"),
            };

            let length = met(3, head);
            data_bytes = IndirectAtom::new_raw_mut_bytes(stack, length);
            data_bytes[..length].copy_from_slice(head.as_bytes());

            current.length = length;
            current.bytes = data_bytes.as_mut_ptr();
            data_bytes = &mut data_bytes[length..];
            current = &mut dat[dat.length() - data_bytes.length()];
            tail = tail.tail();
        }
    }

    pub fn ads_alloc(
        ads: Noun,
        soc: &mut usize
    ) -> urcrypt_aes_siv_data {
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

    pub fn siv_en(stack: &mut NockStack,
        &[u8]: key_y,
        usize: key_w,
        Noun: ads,
        Atom: txt,
        urcrypt_siv: low_f    
    ) -> Noun {
        let ret: Noun;
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
