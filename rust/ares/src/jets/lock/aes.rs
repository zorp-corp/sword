use crate::interpreter::Context;
use crate::jets::bits::util::met;
use crate::jets::util::slot;
use crate::jets::{JetErr, Result};
use crate::noun::{IndirectAtom, Noun};
use urcrypt_sys::*;

crate::gdb!();

pub fn jet_siva_en(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let txt = slot(subject, 6)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let atoms = slot(subject, 61)?;

    if met(3, key) > 32 {
        Err(JetErr::Punt)
    } else {
        unsafe {
            let (mut _key_ida, key_bytes) = IndirectAtom::new_raw_mut_bytes(stack, 32);
            key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

            Ok(util::_siv_en(
                stack,
                key_bytes,
                atoms,
                txt,
                urcrypt_aes_siva_en,
            ))
        }
    }
}

pub fn jet_siva_de(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let iv = slot(subject, 12)?.as_atom()?;
    let len = slot(subject, 26)?.as_atom()?;
    let txt = slot(subject, 27)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if met(3, key) > 64 {
        return Err(JetErr::Punt);
    }

    unsafe {
        let (mut _key_ida, key_bytes) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_de(
            stack,
            key_bytes,
            ads,
            iv,
            len,
            txt,
            urcrypt_aes_siva_de,
        )
    }
}

pub fn jet_sivb_en(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let txt = slot(subject, 6)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let atoms = slot(subject, 61)?;

    if met(3, key) > 48 {
        Err(JetErr::Punt)
    } else {
        unsafe {
            let (mut _key_ida, key_bytes) = IndirectAtom::new_raw_mut_bytes(stack, 48);
            key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

            Ok(util::_siv_en(
                stack,
                key_bytes,
                atoms,
                txt,
                urcrypt_aes_sivb_en,
            ))
        }
    }
}

pub fn jet_sivb_de(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let iv = slot(subject, 12)?.as_atom()?;
    let len = slot(subject, 26)?.as_atom()?;
    let txt = slot(subject, 27)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if met(3, key) > 48 {
        return Err(JetErr::Punt);
    }

    unsafe {
        let (mut _key_ida, key_bytes) = IndirectAtom::new_raw_mut_bytes(stack, 48);
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_de(
            stack,
            key_bytes,
            ads,
            iv,
            len,
            txt,
            urcrypt_aes_sivb_de,
        )
    }
}

pub fn jet_sivc_en(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let txt = slot(subject, 6)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let atoms = slot(subject, 61)?;

    if met(3, key) > 64 {
        Err(JetErr::Punt)
    } else {
        unsafe {
            let (mut _key_ida, key_bytes) = IndirectAtom::new_raw_mut_bytes(stack, 64);
            key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

            Ok(util::_siv_en(
                stack,
                key_bytes,
                atoms,
                txt,
                urcrypt_aes_sivc_en,
            ))
        }
    }
}

pub fn jet_sivc_de(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let iv = slot(subject, 12)?.as_atom()?;
    let len = slot(subject, 26)?.as_atom()?;
    let txt = slot(subject, 27)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if met(3, key) > 64 {
        return Err(JetErr::Punt);
    }

    unsafe {
        let (mut _key_ida, key_bytes) = IndirectAtom::new_raw_mut_bytes(stack, 64);
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_de(
            stack,
            key_bytes,
            ads,
            iv,
            len,
            txt,
            urcrypt_aes_sivc_de,
        )
    }
}

mod util {
    use crate::jets::JetErr;
    use crate::jets::bits::util::met;
    use crate::interpreter::Error;
    use crate::mem::NockStack;
    use crate::noun::{Atom, IndirectAtom, Noun, D, T};
    use std::ptr::null_mut;
    use urcrypt_sys::urcrypt_aes_siv_data;

    type UrcryptSiv = unsafe extern "C" fn(
        *mut u8,
        usize,
        *mut urcrypt_aes_siv_data,
        usize,
        *mut u8,
        *mut u8,
        *mut u8,
    ) -> i32;

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
        let length;
        let bytes;
        let size;

        let mut tail = atoms;
        let mut a = 0;
        let mut b = 0;
        unsafe {
            loop {
                if tail.raw_equals(D(0)) {
                    break;
                }
                let (head, ttail) = match tail.as_cell() {
                    Ok(cell) => (cell.head(), cell.tail()),
                    Err(_) => panic!("_measure_atoms: not a cell"),
                };
                let head = match head.as_atom() {
                    Ok(a) => a,
                    Err(_) => panic!("_measure_atoms: head not an atom"),
                };
                tail = ttail;

                let tmp = b;
                b += met(3, head);
                //  could be just asserting that met returns more than 0
                if b < tmp {
                    panic!("_measure_atoms: overflow");
                }
                a += 1;
            }

            // check for size overflows
            let tmp = a * std::mem::size_of::<urcrypt_aes_siv_data>();
            size = tmp + b;
            if (tmp / a) != std::mem::size_of::<urcrypt_aes_siv_data>() {
                panic!("_measure_atoms: wrong size")
            } else if size < tmp {
                panic!("_measure_atoms: overflow")
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
    pub fn _encode_atoms(atoms: Noun, data: &mut [urcrypt_aes_siv_data]) {
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
                // allocate enough bytes at data[i].bytes
                let ptr = std::alloc::alloc(
                    std::alloc::Layout::from_size_align(head_bytes.len(), 8).unwrap(),
                );
                // copy the bytes from head_bytes into the buffer pointed to by data[i].bytes (which is a *mut u8)
                let data_bytes: &mut [u8] = std::slice::from_raw_parts_mut(ptr, head_bytes.len());
                data_bytes.copy_from_slice(head_bytes);
                data[i].bytes = data_bytes.as_mut_ptr();
                i += 1;
            }
        }
    }

    pub fn _allocate_atoms(atoms: Noun) -> &'static mut [urcrypt_aes_siv_data] {
        if unsafe { atoms.raw_equals(D(0)) } {
            return &mut [];
        }
        let (length, _, size) = _measure_atoms(atoms);
        let siv_data: &mut [urcrypt_aes_siv_data] = unsafe {
            let ptr = std::alloc::alloc(std::alloc::Layout::from_size_align(size, 8).unwrap());
            std::slice::from_raw_parts_mut(ptr as *mut urcrypt_aes_siv_data, length)
        };
        _encode_atoms(atoms, siv_data);
        siv_data
    }

    pub fn _siv_en(
        stack: &mut NockStack,
        key: &mut [u8],
        ads: Noun,
        txt: Atom,
        fun: UrcryptSiv,
    ) -> Noun {
        let siv_data = _allocate_atoms(ads);

        let txt_len = met(3, txt);
        let (_txt_atom, txt_bytes) = match txt_len {
            0 => (D(0), &mut [] as &mut [u8]),
            _ => {
                let (mut txt_ida, txt_bytes) =
                    unsafe { IndirectAtom::new_raw_mut_bytes(stack, txt_len) };
                txt_bytes[0..txt_len].copy_from_slice(&(txt.as_bytes()[0..txt_len]));
                (unsafe { txt_ida.normalize_as_atom().as_noun() }, txt_bytes)
            }
        };

        let (mut iv, iv_bytes) = unsafe { IndirectAtom::new_raw_mut_bytes(stack, 16) };
        let (out_atom, out_bytes) = match txt_len {
            0 => (D(0), &mut [] as &mut [u8]),
            _ => unsafe {
                let (out_ida, out_bytes) = IndirectAtom::new_raw_mut_bytes(stack, txt_len);
                (out_ida.as_noun(), out_bytes)
            },
        };

        unsafe {
            fun(
                if txt_len == 0 {
                    null_mut::<u8>()
                } else {
                    txt_bytes.as_mut_ptr()
                },
                txt_len,
                siv_data.as_mut_ptr(),
                siv_data.len(),
                key.as_mut_ptr(),
                iv_bytes.as_mut_ptr(),
                out_bytes.as_mut_ptr(),
            );
        }

        T(
            stack,
            &[
                unsafe { iv.normalize_as_atom().as_noun() },
                D(txt_len as u64),
                out_atom,
            ],
        )
    }

    pub fn _siv_de(
        stack: &mut NockStack,
        key: &mut [u8],
        ads: Noun,
        iv: Atom,
        len: Atom,
        txt: Atom,
        fun: UrcryptSiv,
    ) -> Result<Noun, JetErr> {
        let siv_data = _allocate_atoms(ads);

        let (_iv_ida, iv_bytes) = unsafe { IndirectAtom::new_raw_mut_bytes(stack, 16) };
        iv_bytes[0..16].copy_from_slice(&(iv.as_bytes()[0..16]));

        let txt_len = match len.as_direct() {
            Ok(direct) => direct.data() as usize,
            Err(_) => return Err(JetErr::Fail(Error::NonDeterministic(D(0)))),
        };
        let (_txt_ida, txt_bytes) = unsafe { IndirectAtom::new_raw_mut_bytes(stack, txt_len) };
        txt_bytes[0..txt_len].copy_from_slice(&(txt.as_bytes()[0..txt_len]));

        let (out_atom, out_bytes) = match txt_len {
            0 => (D(0), &mut [] as &mut [u8]),
            _ => unsafe {
                let (out_ida, out_bytes) = IndirectAtom::new_raw_mut_bytes(stack, txt_len);
                (out_ida.as_noun(), out_bytes)
            },
        };

        unsafe {
            fun(
                if txt_len == 0 {
                    null_mut::<u8>()
                } else {
                    txt_bytes.as_mut_ptr()
                },
                txt_len,
                siv_data.as_mut_ptr(),
                siv_data.len(),
                key.as_mut_ptr(),
                iv_bytes.as_mut_ptr(),
                out_bytes.as_mut_ptr(),
            );
        }

        Ok(T(stack, &[D(0), out_atom]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_noun_eq, init_context, A};
    use crate::jets::Jet;
    use crate::mem::NockStack;
    use crate::noun::{Cell, D, T};
    use ibig::ubig;

    pub fn assert_jet_in_door(
        c: &mut Context,
        jet: Jet,
        sam: &[fn(&mut NockStack) -> Noun], // regular sample
        ctx: &[fn(&mut NockStack) -> Noun], // door sample as context
        res: Noun,
    ) {
        let sam: Vec<Noun> = sam.iter().map(|f| f(&mut c.stack)).collect();
        let ctx: Vec<Noun> = ctx.iter().map(|f| f(&mut c.stack)).collect();
        let sam = if sam.len() > 1 {
            T(&mut c.stack, &sam)
        } else {
            sam[0]
        };
        let ctx = if ctx.len() > 1 {
            T(&mut c.stack, &ctx)
        } else {
            ctx[0]
        };
        let pay = Cell::new(&mut c.stack, sam, ctx).as_noun();
        let sbj = Cell::new(&mut c.stack, D(0), pay).as_noun();
        // std::io::stderr().flush().unwrap();
        let jet_res = jet(c, sbj).unwrap();
        // std::io::stderr().flush().unwrap();
        assert_noun_eq(&mut c.stack, jet_res, res);
    }

    #[test]
    pub fn test_siva_en() {
        let c = &mut init_context();
        /*
        > (~(en siva:aes:crypto [key=0x0 vec=~]) txt=0x0)
        [p=0xb0f7.a0df.be76.c85b.5e29.bb31.aaec.fc77 q=0 r=0x0]
         */
        fn sample(_s: &mut NockStack) -> Noun {
            D(0)
        }
        fn context(s: &mut NockStack) -> Noun {
            let sample = T(s, &[D(0), D(0)]);
            T(s, &[D(0), sample, D(0)])
        }

        let siv = A(&mut c.stack, &ubig!(0xb0f7a0dfbe76c85b5e29bb31aaecfc77));
        let res = T(&mut c.stack, &[siv, D(0), D(0x0)]);
        assert_jet_in_door(c, jet_siva_en, &[sample], &[context], res);

        /* RFC 5297
         * https://datatracker.ietf.org/doc/html/rfc5297#appendix-A
         */
        fn gate_sample(s: &mut NockStack) -> Noun {
            A(s, &ubig!(0x112233445566778899aabbccddee))
        }
        fn gate_context(s: &mut NockStack) -> Noun {
            let key = A(
                s,
                &ubig!(_0xfffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff),
            );
            let a = A(
                s,
                &ubig!(_0x101112131415161718191a1b1c1d1e1f2021222324252627),
            );
            let vec = T(s, &[a, D(0)]);
            let sample = T(s, &[key, vec]);
            T(s, &[D(0), sample, D(0)])
        }
        let iv = A(&mut c.stack, &ubig!(0x85632d07c6e8f37f950acd320a2ecc93));
        let len = D(14);
        let cyp = A(&mut c.stack, &ubig!(0x40c02b9690c4dc04daef7f6afe5c));
        let res = T(&mut c.stack, &[iv, len, cyp]);
        assert_jet_in_door(c, jet_siva_en, &[gate_sample], &[gate_context], res);
    }

    #[test]
    pub fn test_sivb_en() {
        let c = &mut init_context();

        /* RFC 5297
         * https://datatracker.ietf.org/doc/html/rfc5297#appendix-A
         */
        fn gate_sample(s: &mut NockStack) -> Noun {
            A(s, &ubig!(0x112233445566778899aabbccddee))
        }
        fn gate_context(s: &mut NockStack) -> Noun {
            let key = A(s, &ubig!(_0xfffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff00112233445566778899aabbccddeeff));
            let a = A(
                s,
                &ubig!(_0x101112131415161718191a1b1c1d1e1f2021222324252627),
            );
            let vec = T(s, &[a, D(0)]);
            let sample = T(s, &[key, vec]);
            T(s, &[D(0), sample, D(0)])
        }
        let iv = A(&mut c.stack, &ubig!(0x89e869b93256785154f0963962fe0740));
        let len = D(14);
        let cyp = A(&mut c.stack, &ubig!(0xf313e667b56478a032b9913e923c));
        let res = T(&mut c.stack, &[iv, len, cyp]);
        assert_jet_in_door(c, jet_sivb_en, &[gate_sample], &[gate_context], res);
    }

    #[test]
    pub fn test_sivc_en() {
        let c = &mut init_context();

        /* RFC 5297
         * https://datatracker.ietf.org/doc/html/rfc5297#appendix-A
         */
        fn gate_sample(s: &mut NockStack) -> Noun {
            A(s, &ubig!(0x112233445566778899aabbccddee))
        }
        fn gate_context(s: &mut NockStack) -> Noun {
            let key = A(s, &ubig!(_0xfffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff));
            let a = A(
                s,
                &ubig!(_0x101112131415161718191a1b1c1d1e1f2021222324252627),
            );
            let vec = T(s, &[a, D(0)]);
            let sample = T(s, &[key, vec]);
            T(s, &[D(0), sample, D(0)])
        }
        let iv = A(&mut c.stack, &ubig!(0x724dfb2eaf94dbb19b0ba3a299a0801e));
        let len = D(14);
        let cyp = A(&mut c.stack, &ubig!(0x1206291a35ad3db0212773440fd0));
        let res = T(&mut c.stack, &[iv, len, cyp]);
        assert_jet_in_door(c, jet_sivc_en, &[gate_sample], &[gate_context], res);
    }

    #[test]
    pub fn test_siva_de() {
        let c = &mut init_context();

        /* RFC 5297
         * https://datatracker.ietf.org/doc/html/rfc5297#appendix-A
         */
        fn gate_sample(s: &mut NockStack) -> Noun {
            let iv = A(s, &ubig!(0x85632d07c6e8f37f950acd320a2ecc93));
            let len = D(14);
            let cyp = A(s, &ubig!(0x40c02b9690c4dc04daef7f6afe5c));
            T(s, &[iv, len, cyp])
        }
        fn gate_context(s: &mut NockStack) -> Noun {
            let key = A(
                s,
                &ubig!(_0xfffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff),
            );
            let a = A(
                s,
                &ubig!(_0x101112131415161718191a1b1c1d1e1f2021222324252627),
            );
            let vec = T(s, &[a, D(0)]);
            let sample = T(s, &[key, vec]);
            T(s, &[D(0), sample, D(0)])
        }
        let txt = A(&mut c.stack, &ubig!(0x112233445566778899aabbccddee));
        let res = T(&mut c.stack, &[D(0), txt]);
        assert_jet_in_door(c, jet_siva_de, &[gate_sample], &[gate_context], res);
    }

    #[test]
    pub fn test_sivb_de() {
        let c = &mut init_context();

        /* RFC 5297
         * https://datatracker.ietf.org/doc/html/rfc5297#appendix-A
         */
        fn gate_sample(s: &mut NockStack) -> Noun {
            let iv = A(s, &ubig!(0x89e869b93256785154f0963962fe0740));
            let len = D(14);
            let cyp = A(s, &ubig!(0xf313e667b56478a032b9913e923c));
            T(s, &[iv, len, cyp])
        }
        fn gate_context(s: &mut NockStack) -> Noun {
            let key = A(s, &ubig!(_0xfffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff00112233445566778899aabbccddeeff));
            let a = A(
                s,
                &ubig!(_0x101112131415161718191a1b1c1d1e1f2021222324252627),
            );
            let vec = T(s, &[a, D(0)]);
            let sample = T(s, &[key, vec]);
            T(s, &[D(0), sample, D(0)])
        }
        let txt = A(&mut c.stack, &ubig!(0x112233445566778899aabbccddee));
        let res = T(&mut c.stack, &[D(0), txt]);
        assert_jet_in_door(c, jet_sivb_de, &[gate_sample], &[gate_context], res);
    }

    #[test]
    pub fn test_sivc_de() {
        let c = &mut init_context();

        /* RFC 5297
         * https://datatracker.ietf.org/doc/html/rfc5297#appendix-A
         */
        fn gate_sample(s: &mut NockStack) -> Noun {
            let iv = A(s, &ubig!(0x724dfb2eaf94dbb19b0ba3a299a0801e));
            let len = D(14);
            let cyp = A(s, &ubig!(0x1206291a35ad3db0212773440fd0));
            T(s, &[iv, len, cyp])
        }
        fn gate_context(s: &mut NockStack) -> Noun {
            let key = A(s, &ubig!(_0xfffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff));
            let a = A(
                s,
                &ubig!(_0x101112131415161718191a1b1c1d1e1f2021222324252627),
            );
            let vec = T(s, &[a, D(0)]);
            let sample = T(s, &[key, vec]);
            T(s, &[D(0), sample, D(0)])
        }
        let txt = A(&mut c.stack, &ubig!(0x112233445566778899aabbccddee));
        let res = T(&mut c.stack, &[D(0), txt]);
        assert_jet_in_door(c, jet_sivc_de, &[gate_sample], &[gate_context], res);
    }
}
