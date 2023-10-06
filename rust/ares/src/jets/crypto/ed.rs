use crate::jets::util::slot;
use crate::jets::{JetErr, Result};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{IndirectAtom, Noun};
use urcrypt_sys::*;

crate::gdb!();

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
    // use crate::noun::{D, T, DIRECT_MAX};
    use crate::noun::D;
    use crate::jets::util::test::{A, assert_jet, init_stack, assert_jet_err, assert_jet_ubig};
    // use crate::jets::JetErr;

    #[test]
    fn test_puck() {
        let s = &mut init_stack();

        let ret = A(s, &ubig!(_0xfb099b0acc4d1ce37f9982a2ed331245e0cdfdf6979364b7676a142b8233e53b));
        assert_jet(s, jet_puck, D(32), ret);
    }

}
