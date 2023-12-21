use crate::interpreter::Context;
use crate::jets::bits::util::met;
use crate::jets::util::slot;
use crate::jets::{JetErr, Result};
use crate::noun::Noun;
use ares_crypto::aes_siv::{
    ac_aes_siva_de, ac_aes_siva_en, ac_aes_sivb_de, ac_aes_sivb_en, ac_aes_sivc_de, ac_aes_sivc_en,
};

crate::gdb!();

//  Note:   The Hoon code for these functions doesn't explicitly check key
//          sizes. However, the implementations of these functions in ares_crypto
//          have fixed maximum key sizes, therefore we must punt if the key is
//          too large.

pub fn jet_siva_en(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let txt = slot(subject, 6)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if met(3, key) > 32 {
        Err(JetErr::Punt)
    } else {
        let key_bytes = &mut [0u8; 32];
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_en(stack, key_bytes, ads, txt, ac_aes_siva_en)
    }
}

pub fn jet_siva_de(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let iv = slot(subject, 12)?.as_atom()?;
    let len = slot(subject, 26)?.as_atom()?;
    let txt = slot(subject, 27)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if met(3, key) > 32 {
        Err(JetErr::Punt)
    } else {
        let key_bytes = &mut [0u8; 32];
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_de(stack, key_bytes, ads, iv, len, txt, ac_aes_siva_de)
    }
}

pub fn jet_sivb_en(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let txt = slot(subject, 6)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if met(3, key) > 48 {
        Err(JetErr::Punt)
    } else {
        let key_bytes = &mut [0u8; 48];
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_en(stack, key_bytes, ads, txt, ac_aes_sivb_en)
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
        Err(JetErr::Punt)
    } else {
        let key_bytes = &mut [0u8; 48];
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_de(stack, key_bytes, ads, iv, len, txt, ac_aes_sivb_de)
    }
}

pub fn jet_sivc_en(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let txt = slot(subject, 6)?.as_atom()?;
    let key = slot(subject, 60)?.as_atom()?;
    let ads = slot(subject, 61)?;

    if met(3, key) > 64 {
        Err(JetErr::Punt)
    } else {
        let key_bytes = &mut [0u8; 64];
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_en(stack, key_bytes, ads, txt, ac_aes_sivc_en)
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
        Err(JetErr::Punt)
    } else {
        let key_bytes = &mut [0u8; 64];
        key_bytes[0..key.as_bytes().len()].copy_from_slice(key.as_bytes());

        util::_siv_de(stack, key_bytes, ads, iv, len, txt, ac_aes_sivc_de)
    }
}

mod util {
    use crate::interpreter::Error;
    use crate::jets::bits::util::met;
    use crate::jets::list;
    use crate::jets::{JetErr, Result};
    use crate::mem::NockStack;
    use crate::noun::{Atom, IndirectAtom, Noun, D, T};
    use std::result;
    use ares_crypto::aes_siv;

    /// Associated data for AES-SIV functions.
    struct AcAesSivData {
        bytes: *mut u8,
        length: usize,
    }

    type AcAesSiv = fn(
        key: &mut [u8],
        message: &mut [u8],
        data: &mut [&mut [u8]],
        iv: &mut [u8],
        out: &mut [u8],
    ) -> result::Result<(), aes_siv::Error>;

    /// Allocates a noun list as an array of AesSivData structs on the NockStack
    /// for use as associated data in AES-SIV functions.
    fn _allocate_ads(
        stack: &mut NockStack,
        mut ads: Noun,
    ) -> result::Result<&'static mut [AcAesSivData], JetErr> {
        if unsafe { ads.raw_equals(D(0)) } {
            return Ok(&mut []);
        }

        let length = list::util::lent(ads)?;

        let siv_data: &mut [AcAesSivData] = unsafe {
            let ptr = stack.struct_alloc::<AcAesSivData>(length);
            std::slice::from_raw_parts_mut(ptr, length)
        };

        unsafe {
            for item in siv_data.iter_mut().take(length) {
                let cell = ads.as_cell()?;
                let head = cell.head().as_atom()?;
                let bytes = head.as_bytes();
                let len = met(3, head);

                let (mut atom, buffer) = IndirectAtom::new_raw_mut_bytes(stack, bytes.len());
                buffer[0..len].copy_from_slice(&(bytes[0..len]));

                item.length = bytes.len();
                item.bytes = atom.data_pointer_mut() as *mut u8;

                ads = cell.tail();
            }
        }

        Ok(siv_data)
    }

    pub fn _siv_en(
        stack: &mut NockStack,
        key: &mut [u8],
        ads: Noun,
        mut txt: Atom,
        fun: AcAesSiv,
    ) -> Result {
        unsafe {
            let ac_siv_data = _allocate_ads(stack, ads)?;
            let siv_data: &mut [&mut [u8]] = std::slice::from_raw_parts_mut(
                ac_siv_data.as_mut_ptr() as *mut &mut [u8],
                ac_siv_data.len(),
            );

            let txt_len = met(3, txt);
            let txt_bytes = &mut (txt.as_mut_bytes()[0..txt_len]);

            let (mut iv, iv_bytes) = IndirectAtom::new_raw_mut_bytes(stack, 16);

            match txt_len {
                0 => {
                    fun(
                        key,
                        txt_bytes,
                        siv_data,
                        iv_bytes,
                        &mut [0u8; 0],
                    ).unwrap();
                    Ok(T(stack, &[iv.normalize_as_atom().as_noun(), D(0), D(0)]))
                }
                _ => {
                    let (mut out_atom, out_bytes) = IndirectAtom::new_raw_mut_bytes(stack, txt_len);
                    fun(
                        key,
                        txt_bytes,
                        siv_data,
                        iv_bytes,
                        out_bytes
                    ).unwrap();
                    Ok(T(
                        stack,
                        &[
                            iv.normalize_as_atom().as_noun(),
                            D(txt_len as u64),
                            out_atom.normalize_as_atom().as_noun(),
                        ],
                    ))
                }
            }
        }
    }

    pub fn _siv_de(
        stack: &mut NockStack,
        key: &mut [u8],
        ads: Noun,
        mut iv: Atom,
        len: Atom,
        mut txt: Atom,
        fun: AcAesSiv
    ) -> Result {
        unsafe {
            let txt_len = match len.as_direct() {
                Ok(direct) => direct.data() as usize,
                Err(_) => return Err(JetErr::Fail(Error::NonDeterministic(D(0)))),
            };
            let txt_bytes = &mut (txt.as_mut_bytes()[0..txt_len]);

            let iv_bytes = &mut (iv.as_mut_bytes()[0..16]);

            let ac_siv_data = _allocate_ads(stack, ads)?;
            let siv_data: &mut [&mut [u8]] = std::slice::from_raw_parts_mut(
                ac_siv_data.as_mut_ptr() as *mut &mut [u8],
                ac_siv_data.len(),
            );

            let (mut out_atom, out_bytes) = IndirectAtom::new_raw_mut_bytes(stack, txt_len);
            fun(
                key,
                txt_bytes,
                siv_data,
                iv_bytes,
                out_bytes
            ).unwrap();
            Ok(T(stack, &[D(0), out_atom.normalize_as_atom().as_noun()]))
        }
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
