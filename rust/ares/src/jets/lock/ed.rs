use crate::interpreter::{Context, Error};
use crate::jets::bits::util::met;
use crate::jets::util::slot;
use crate::jets::{JetErr, Result};
use crate::noun::{IndirectAtom, Noun, D, NO, YES};
use urcrypt_sys::*;

crate::gdb!();

pub fn jet_puck(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sed = slot(subject, 6)?.as_direct()?;

    unsafe {
        let (mut _seed_ida, seed) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        let sed_bytes = sed.data().to_le_bytes();
        seed[0..sed_bytes.len()].copy_from_slice(&sed_bytes[..]);

        let (mut pub_ida, pub_key) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        urcrypt_ed_puck(seed.as_ptr(), pub_key.as_mut_ptr());

        Ok(pub_ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_shar(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let pub_key = slot(subject, 12)?.as_direct()?;
    let sec_key = slot(subject, 13)?.as_direct()?;

    unsafe {
        let (_, public) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        let (_, secret) = IndirectAtom::new_raw_mut_bytes(stack, 32);

        let pub_bytes = pub_key.data().to_le_bytes();
        let sec_bytes = sec_key.data().to_le_bytes();

        public[0..pub_bytes.len()].copy_from_slice(&pub_bytes[..]);
        secret[0..sec_bytes.len()].copy_from_slice(&sec_bytes[..]);

        let (mut shar_ida, shar) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        urcrypt_ed_shar(public.as_ptr(), secret.as_ptr(), shar.as_mut_ptr());

        Ok(shar_ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_sign(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    let msg = slot(sam, 2)?.as_atom()?;
    let sed = slot(sam, 3)?.as_atom()?;

    unsafe {
        let sed_bytes = sed.as_bytes();
        if sed_bytes.len() > 32 {
            return Err(JetErr::Fail(Error::Deterministic(D(0))));
        };

        let msg_bytes = &(msg.as_bytes())[0..met(3, msg)]; // drop trailing zeros

        let (mut _seed_ida, seed) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        seed.copy_from_slice(sed_bytes);

        let (mut sig_ida, sig) = IndirectAtom::new_raw_mut_bytes(stack, 64);
        urcrypt_ed_sign(
            msg_bytes.as_ptr(),
            msg_bytes.len(),
            seed.as_ptr(),
            sig.as_mut_ptr(),
        );
        sig.reverse(); // LSB first

        Ok(sig_ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_veri(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sig = slot(subject, 12)?.as_atom()?;
    let msg = slot(subject, 26)?.as_atom()?;
    let puk = slot(subject, 27)?.as_atom()?;

    unsafe {
        let sig_bytes = sig.as_bytes();
        if sig_bytes.len() > 64 {
            return Err(JetErr::Punt);
        };

        let pub_bytes = puk.as_bytes();
        if pub_bytes.len() > 32 {
            return Err(JetErr::Punt);
        };

        let (mut _sig_ida, signature) = IndirectAtom::new_raw_mut_bytes(stack, 64);
        signature.copy_from_slice(sig_bytes);
        let (mut _pub_ida, public_key) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        public_key.copy_from_slice(pub_bytes);

        let message = &(msg.as_bytes())[0..met(3, msg)]; // drop trailing zeros

        let valid = urcrypt_ed_veri(
            message.as_ptr(),
            message.len(),
            public_key.as_ptr(),
            signature.as_ptr(),
        );

        Ok(if valid { YES } else { NO })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, assert_jet_err, init_context, A};
    use crate::noun::{D, T};
    use ibig::ubig;

    #[test]
    fn test_puck() {
        let c = &mut init_context();

        let ret = A(
            &mut c.stack,
            &ubig!(_0xfb099b0acc4d1ce37f9982a2ed331245e0cdfdf6979364b7676a142b8233e53b),
        );
        assert_jet(c, jet_puck, D(32), ret);

        let sam = A(
            &mut c.stack,
            &ubig!(_0xfb099b0acc4d1ce37f9982a2ed331245e0cdfdf6979364b7676a142b8233e53b),
        );
        assert_jet_err(c, jet_puck, sam, JetErr::Fail(Error::Deterministic(D(0))));
    }

    #[test]
    fn test_shar() {
        let c = &mut init_context();

        let sam = T(&mut c.stack, &[D(234), D(234)]);
        let ret = A(
            &mut c.stack,
            &ubig!(_0x6ecd5779a47841207a2cd0c9d085796aa646842885a332adac540027d768c1c5),
        );
        assert_jet(c, jet_shar, sam, ret);

        let sam = A(
            &mut c.stack,
            &ubig!(_0xfb099b0acc4d1ce37f9982a2ed331245e0cdfdf6979364b7676a142b8233e53b),
        );
        assert_jet_err(c, jet_shar, sam, JetErr::Fail(Error::Deterministic(D(0))));
    }

    #[test]
    fn test_sign() {
        let c = &mut init_context();

        unsafe {
            let message = D(0x72);

            let sed_ubig =
                ubig!(_0x4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb);
            let sed_bytes = sed_ubig.to_be_bytes();
            let seed =
                IndirectAtom::new_raw_bytes(&mut c.stack, sed_bytes.len(), sed_bytes.as_ptr())
                    .as_noun();

            let sam = T(&mut c.stack, &[message, seed]);
            let ret = A(&mut c.stack, &ubig!(_0x92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00));
            assert_jet(c, jet_sign, sam, ret);

            let msg_ubig = ubig!(_0xaf82);
            let msg_bytes = msg_ubig.to_be_bytes();
            let message =
                IndirectAtom::new_raw_bytes(&mut c.stack, msg_bytes.len(), msg_bytes.as_ptr())
                    .as_noun();

            let sed_ubig =
                ubig!(_0xc5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7);
            let sed_bytes = sed_ubig.to_be_bytes();
            let seed =
                IndirectAtom::new_raw_bytes(&mut c.stack, sed_bytes.len(), sed_bytes.as_ptr())
                    .as_noun();

            let sam = T(&mut c.stack, &[message, seed]);
            let ret = A(&mut c.stack, &ubig!(_0x6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a));
            assert_jet(c, jet_sign, sam, ret);
        }
    }

    #[test]
    fn test_veri() {
        let c = &mut init_context();

        unsafe {
            let sig_ubig = ubig!(_0x92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00);
            let sig_bytes = sig_ubig.to_be_bytes();
            let signature =
                IndirectAtom::new_raw_bytes(&mut c.stack, sig_bytes.len(), sig_bytes.as_ptr())
                    .as_noun();

            let message = D(0x72);

            let pub_ubig =
                ubig!(_0x3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c);
            let pub_bytes = pub_ubig.to_be_bytes();
            let public_key =
                IndirectAtom::new_raw_bytes(&mut c.stack, pub_bytes.len(), pub_bytes.as_ptr())
                    .as_noun();

            let sam = T(&mut c.stack, &[signature, message, public_key]);
            assert_jet(c, jet_veri, sam, YES);

            let sig_ubig = ubig!(_0x6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a);
            let sig_bytes = sig_ubig.to_be_bytes();
            let signature =
                IndirectAtom::new_raw_bytes(&mut c.stack, sig_bytes.len(), sig_bytes.as_ptr())
                    .as_noun();

            let msg_ubig = ubig!(0xaf82);
            let msg_bytes = msg_ubig.to_be_bytes();
            let message =
                IndirectAtom::new_raw_bytes(&mut c.stack, msg_bytes.len(), msg_bytes.as_ptr())
                    .as_noun();

            let pub_ubig =
                ubig!(_0xfc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025);
            let pub_bytes = pub_ubig.to_be_bytes();
            let public_key =
                IndirectAtom::new_raw_bytes(&mut c.stack, pub_bytes.len(), pub_bytes.as_ptr())
                    .as_noun();

            let sam = T(&mut c.stack, &[signature, message, public_key]);
            assert_jet(c, jet_veri, sam, YES);
        }
    }
}
