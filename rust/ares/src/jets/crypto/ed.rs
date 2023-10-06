use crate::jets::util::slot;
use crate::jets::{JetErr, Result};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{IndirectAtom, Noun};
use urcrypt_sys::*;

crate::gdb!();

pub fn jet_shar(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let pub_key = slot(sam, 2)?.as_direct()?;
    let sec_key = slot(sam, 3)?.as_direct()?;

    if pub_key.bit_size() > 32 || sec_key.bit_size() > 32 {
        return Err(JetErr::Deterministic); // right?
    }

    unsafe {
        let (_, public) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        let (_, secret) = IndirectAtom::new_raw_mut_bytes(stack, 32);

        let pub_bytes = pub_key.data().to_le_bytes();
        let sec_bytes = sec_key.data().to_le_bytes();

        for i in 0..pub_bytes.len() {
            public[i] = pub_bytes[i];
        }
        for i in 0..sec_bytes.len() {
            secret[i] = sec_bytes[i];
        }

        let (mut shar_ida, shar) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        urcrypt_ed_shar(public.as_ptr(), secret.as_ptr(), shar.as_mut_ptr());
        Ok(shar_ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_puck(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let sam = slot(subject, 6)?.as_direct()?;

    if sam.bit_size() > 32 {
        return Err(JetErr::Deterministic); // right?
    }

    unsafe {
        let (mut _seed_ida, seed) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        let sam_bytes = sam.data().to_le_bytes();
        // copy sam_bytes into seed one by one
        for i in 0..sam_bytes.len() {
            seed[i] = sam_bytes[i];
        }

        let (mut pub_ida, pub_key) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        urcrypt_ed_puck(seed.as_ptr(), pub_key.as_mut_ptr());
        Ok(pub_ida.normalize_as_atom().as_noun())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ibig::ubig;
    use crate::noun::{D, T};
    use crate::jets::util::test::{A, assert_jet, init_stack, assert_jet_err, assert_jet_ubig};
    // use crate::jets::JetErr;

    #[test]
    fn test_puck() {
        let s = &mut init_stack();

        let ret = A(s, &ubig!(_0xfb099b0acc4d1ce37f9982a2ed331245e0cdfdf6979364b7676a142b8233e53b));
        assert_jet(s, jet_puck, D(32), ret);
    }

    #[test]
    fn test_shar() {
        let s = &mut init_stack();

        let sam = T(s, &[D(234), D(234)]);
        let ret = A(s, &ubig!(_0x6ecd5779a47841207a2cd0c9d085796aa646842885a332adac540027d768c1c5));
        assert_jet(s, jet_shar, sam, ret);
    }

}
