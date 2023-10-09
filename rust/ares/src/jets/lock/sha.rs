use either::{Left, Right};
use crate::jets::util::{met, slot};
use crate::jets::{JetErr, Result};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{DirectAtom, IndirectAtom, Noun};
use urcrypt_sys::*;

crate::gdb!();

pub fn jet_sha1(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let wid = slot(sam, 2)?.as_atom()?;
    let dat = slot(sam, 3)?.as_atom()?;

    let mut wdc: DirectAtom;
    let width = match wid.as_either() {
        Left(direct) => {
            wdc = direct.clone();
            wdc.as_byteslice_mut()
        }
        Right(_) => {
            return Err(JetErr::NonDeterministic);
        }
    };

    let mut dat_direct_clone: DirectAtom;
    let mut dat_indirect_clone: IndirectAtom;
    let message = match dat.as_either() {
        Left(direct) => {
            dat_direct_clone = direct.clone();
            dat_direct_clone.as_byteslice_mut()
        }
        Right(indirect) => {
            dat_indirect_clone = indirect.clone();
            dat_indirect_clone.as_mut_bytes()
        }
    };

    unsafe {
        let (mut ida, out) = IndirectAtom::new_raw_mut_bytes(stack, 20);
        urcrypt_sha1(message.as_mut_ptr(), width.len(), out.as_mut_ptr());
        Ok(ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_shal(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let wid = slot(sam, 2)?.as_atom()?;
    let dat = slot(sam, 3)?.as_atom()?;

    let mut wdc: DirectAtom;
    let width = match wid.as_either() {
        Left(direct) => {
            wdc = direct.clone();
            wdc.as_byteslice_mut()
        }
        Right(_) => {
            return Err(JetErr::NonDeterministic);
        }
    };

    let message = dat.as_bytes();

    unsafe {
        let (mut ida, out) = IndirectAtom::new_raw_mut_bytes(stack, 64);
        urcrypt_shal(message.as_ptr(), width.len(), out.as_mut_ptr());
        Ok(ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_shas(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let sal = slot(sam, 2)?.as_atom()?;
    let ruz= slot(sam, 3)?.as_atom()?;

    let mut sdc: DirectAtom;
    let mut sic: IndirectAtom;
    let salt = match sal.as_either() {
        Left(direct) => {
            sdc = direct.clone();
            sdc.as_byteslice_mut()
        }
        Right(indirect) => {
            sic = indirect.clone();
            sic.as_mut_bytes()
        }
    };

    let mdc: DirectAtom;
    let mic: IndirectAtom;
    let message = match ruz.as_either() {
        Left(direct) => {
            mdc = direct.clone();
            mdc.as_byteslice()
        }
        Right(indirect) => {
            mic = indirect.clone();
            mic.as_bytes()
        }
    };

    unsafe {
        let (mut ida, out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        urcrypt_shas(salt.as_mut_ptr(), salt.len(), message.as_ptr(), message.len(), out.as_mut_ptr());
        Ok(ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_shax(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let a = sam.as_atom()?;
    let len = met(3, a);

    match a.as_either() {
        Left(direct) => {
            unsafe {
                let (mut ida, out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
                urcrypt_shay(direct.as_byteslice().as_ptr(), len, out.as_mut_ptr());
                Ok(ida.normalize_as_atom().as_noun())
            }
        }
        Right(indirect) => {
            unsafe {
                let (mut ida, out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
                urcrypt_shay(indirect.as_bytes().as_ptr(), len, out.as_mut_ptr());
                Ok(ida.normalize_as_atom().as_noun())
            }
        }
    }
}

pub fn jet_shay(stack: &mut NockStack, _newt: &mut Option<&mut Newt>, subject: Noun) -> Result {
    let sam = slot(subject, 6)?;
    let wid = slot(sam, 2)?.as_atom()?;
    let dat = slot(sam, 3)?.as_atom()?;

    if let Ok(len) = wid.as_direct() {
        match dat.as_either() {
            Left(direct) => {
                unsafe {
                    let msg = direct.as_byteslice();
                    let (mut ida, out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
                    urcrypt_shay(msg.as_ptr(), len.data() as usize, out.as_mut_ptr());
                    Ok(ida.normalize_as_atom().as_noun())
                }
            }
            Right(indirect) => {
                unsafe {
                    let msg = indirect.as_bytes();
                    let (mut ida, out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
                    urcrypt_shay(msg.as_ptr(), len.data() as usize, out.as_mut_ptr());
                    Ok(ida.normalize_as_atom().as_noun())
                }
            }
        }
    } else {
        return Err(JetErr::NonDeterministic);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ibig::ubig;
    use crate::noun::{D, T, DIRECT_MAX};
    use crate::jets::util::test::{A, assert_jet, init_stack, assert_jet_err, assert_jet_ubig};
    use crate::jets::JetErr;

    #[test]
    fn test_shay() {
        let s = &mut init_stack();

        let sam = T(s, &[D(1), D(0)]);
        let ret = A(s, &ubig!(_0x1da0af1706a31185763837b33f1d90782c0a78bbe644a59c987ab3ff9c0b346e));
        assert_jet(s, jet_shay, sam, ret);

        let sam = T(s, &[D(0), D(1)]);
        let ret = A(s, &ubig!(_0x55b852781b9995a44c939b64e441ae2724b96f99c8f4fb9a141cfc9842c4b0e3));
        assert_jet(s, jet_shay, sam, ret);

        let sam = T(s, &[D(1), D(478560413032)]); // [1 'hello']
        let ret = A(s, &ubig!(_0x23b14de6713b28aadf8f95026636eb6ab63e99c952bceb401fa4f1642640a9aa));
        assert_jet(s, jet_shay, sam, ret);

        let sam = T(s, &[D(2), D(478560413032)]); // [2 'hello']
        let ret = A(s, &ubig!(_0xde1e7ee30cecf453f1d77c08a125fdc6a4bbac72c01dd7a1e21cd0d22f7e2f37));
        assert_jet(s, jet_shay, sam, ret);

        let big = DIRECT_MAX + 1;
        let ida = unsafe { IndirectAtom::new_raw_bytes(s, 8, &big as *const u64 as *const u8) };
        let sam = T(s, &[ida.as_noun(), D(478560413032)]);
        assert_jet_err(s, jet_shay, sam, JetErr::NonDeterministic);

        let big: u128 = (DIRECT_MAX as u128) << 64;
        let ida = unsafe { IndirectAtom::new_raw_bytes(s, 8, &big as *const u128 as *const u8) };
        let sam = T(s, &[ida.as_noun(), D(478560413032)]);
        assert_jet_err(s, jet_shay, sam, JetErr::NonDeterministic);
    }

    #[test]
    fn test_shax() {
        let s = &mut init_stack();

        assert_jet_ubig(
            s,
            jet_shax,
            D(7303014), // 'foo'
            ubig!(_0xaee76662885e8af9a0bf8364702d42133441301d3c459bf98fc6ff686bb4262c),
        );

        let a = A(s, &ubig!(_0xaee76662885e8af9a0bf8364702d42133441301d3c459bf98fc6ff686bb4262c));
        assert_jet_ubig(
            s,
            jet_shax,
            a,
            ubig!(_0x9ee26e46c2028aa4a9c463aa722b82ed8bf6e185c3e5a5a69814a2c78fe8adc7)
        );

        assert_jet_ubig(
            s,
            jet_shax,
            D(123456789),
            ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72)
        );

        let a = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72));
        assert_jet_ubig(
            s,
            jet_shax,
            a,
            ubig!(_0xf90f3184d7347a20cfdd2d5f7ac5c82eb9ab7af54c9419fbc18832c5a33360c9)
        )
    }

    #[test]
    fn test_shas() {
        let s = &mut init_stack();

        let sam = T(s, &[D(1), D(1)]);
        assert_jet_ubig(
            s,
            jet_shas,
            sam,
            ubig!(_0x547da92584bc986e5784edb746c29504bfd6b34572c83b7b96440ca77d35cdfc)
        );

        let sam = T(s, &[D(2), D(2)]);
        assert_jet_ubig(
            s,
            jet_shas,
            sam,
            ubig!(_0x4cf01fe7cc56ef70d17735322488de0d31857afcfe451e199abe6295f78f5328)
        );

        let a = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72));
        let b = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de73));
        let sam = T(s, &[a, b]);
        assert_jet_ubig(
            s,
            jet_shas,
            sam,
            ubig!(_0xf7569a89650553ef13f9a8f0bb751fd42b70a4821be6bc1cbe197af33ce4843c)
        );
    }

    #[test]
    fn test_shal() {
        let s = &mut init_stack();

        let sam = T(s, &[D(1), D(1)]);
        assert_jet_ubig(
            s,
            jet_shal,
            sam,
            ubig!(_0x39e3d936c6e31eaac08fcfcfe7bb443460c61c0bd5b74408c8bcc35a6b8d6f5700bdcddeaa4b466ae65f8fb67f67ca62dc34149e1d44d213ddfbc13668b6547b)
        );

        let sam = T(s, &[D(1), D(2)]);
        assert_jet_ubig(
            s,
            jet_shal,
            sam,
            ubig!(_0xcadc698fca01cf2935f760278554b4e61f35453975a5bb45389003159bc8485b7018dd8152d9cc23b6e9dd91b107380b9d14ddbf9cc037ee53a857b6c948b8fa)
        );

        let wid = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72));
        let dat = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de73));
        let sam = T(s, &[wid, dat]);
        assert_jet_err(
            s,
            jet_shal,
            sam,
            JetErr::NonDeterministic
        );

        let wid = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72));
        let sam = T(s, &[wid, D(1)]);
        assert_jet_err(
            s,
            jet_shal,
            sam,
            JetErr::NonDeterministic
        );
    }

    #[test]
    fn test_sha1() {
        let s = &mut init_stack();

        let sam = T(s, &[D(1), D(1)]);
        assert_jet_ubig(
            s,
            jet_sha1,
            sam,
            ubig!(_0xbf8b4530d8d246dd74ac53a13471bba17941dff7)
        );

        let sam = T(s, &[D(1), D(2)]);
        assert_jet_ubig(
            s,
            jet_sha1,
            sam,
            ubig!(_0xc4ea21bb365bbeeaf5f2c654883e56d11e43c44e)
        );

        let wid = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72));
        let dat = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de73));
        let sam = T(s, &[wid, dat]);
        assert_jet_err(
            s,
            jet_sha1,
            sam,
            JetErr::NonDeterministic
        );

        let wid = A(s, &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72));
        let sam = T(s, &[wid, D(1)]);
        assert_jet_err(
            s,
            jet_sha1,
            sam,
            JetErr::NonDeterministic
        );
    }
}
