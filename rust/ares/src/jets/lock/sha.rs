use crate::interpreter::{Context, Error};
use crate::jets::bits::util::met;
use crate::jets::util::slot;
use crate::jets::{JetErr, Result};
use crate::noun::{IndirectAtom, Noun, D};
use urcrypt_sys::*;
use ares_crypto::sha::{ac_sha1, ac_shal, ac_shas, ac_shay};

crate::gdb!();

pub fn jet_shas(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    let sal = slot(sam, 2)?.as_atom()?;
    let mut ruz = slot(sam, 3)?.as_atom()?;

    let sal_bytes = &(sal.as_bytes())[0..met(3, sal)]; // drop trailing zeros
    let (mut _salt_ida, mut salt) = unsafe { IndirectAtom::new_raw_mut_bytes(stack, sal_bytes.len()) };
    salt.copy_from_slice(sal_bytes);

    let msg_len = met(3, ruz);
    let mut message = &mut (ruz.as_mut_bytes())[0..msg_len]; // drop trailing zeros

    unsafe {
        let (mut out_ida, mut out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        ac_shas(
            &mut message,
            &mut salt,
            &mut out,
        );
        Ok(out_ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_shax(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    let mut msg = sam.as_atom()?;
    let len = met(3, msg);

    unsafe {
        let (mut ida, mut out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        ac_shay(&mut (msg.as_mut_bytes())[0..len], &mut out);
        Ok(ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_shay(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    let wid = slot(sam, 2)?.as_atom()?;
    let mut dat = slot(sam, 3)?.as_atom()?;

    let width = match wid.as_direct() {
        Ok(direct) => direct.data() as usize,
        Err(_) => return Err(JetErr::Fail(Error::NonDeterministic(D(0)))),
    };

    let message = &mut (dat.as_mut_bytes())[0..width];

    unsafe {
        let (mut out_ida, mut out) = IndirectAtom::new_raw_mut_bytes(stack, 32);
        ac_shay(message, &mut out);
        Ok(out_ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_shal(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    let wid = slot(sam, 2)?.as_atom()?;
    let mut dat = slot(sam, 3)?.as_atom()?;

    let width = match wid.as_direct() {
        Ok(direct) => direct.data() as usize,
        Err(_) => return Err(JetErr::Fail(Error::NonDeterministic(D(0)))),
    };

    let message = &mut (dat.as_mut_bytes())[0..width]; // drop trailing zeros

    unsafe {
        let (mut ida, mut out) = IndirectAtom::new_raw_mut_bytes(stack, 64);
        ac_shal(message, &mut out);
        Ok(ida.normalize_as_atom().as_noun())
    }
}

pub fn jet_sha1(context: &mut Context, subject: Noun) -> Result {
    let stack = &mut context.stack;
    let sam = slot(subject, 6)?;
    let wid = slot(sam, 2)?.as_atom()?;
    let dat = slot(sam, 3)?.as_atom()?;

    let width = match wid.as_direct() {
        Ok(direct) => direct.data() as usize,
        Err(_) => return Err(JetErr::Fail(Error::NonDeterministic(D(0)))),
    };

    unsafe {
        let msg_bytes = &(dat.as_bytes())[0..width];
        let (mut _msg_ida, mut msg) = IndirectAtom::new_raw_mut_bytes(stack, msg_bytes.len());
        msg.copy_from_slice(msg_bytes);

        let (mut out_ida, mut out) = IndirectAtom::new_raw_mut_bytes(stack, 20);
        ac_sha1(&mut msg, &mut out);
        Ok(out_ida.normalize_as_atom().as_noun())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, assert_jet_err, assert_jet_ubig, init_context, A};
    use crate::jets::JetErr;
    use crate::noun::{D, DIRECT_MAX, T};
    use ibig::ubig;

    #[test]
    fn test_shas() {
        let c = &mut init_context();

        let sam = T(&mut c.stack, &[D(1), D(1)]);
        assert_jet_ubig(
            c,
            jet_shas,
            sam,
            ubig!(_0x547da92584bc986e5784edb746c29504bfd6b34572c83b7b96440ca77d35cdfc),
        );

        let sam = T(&mut c.stack, &[D(2), D(2)]);
        assert_jet_ubig(
            c,
            jet_shas,
            sam,
            ubig!(_0x4cf01fe7cc56ef70d17735322488de0d31857afcfe451e199abe6295f78f5328),
        );

        let a = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72),
        );
        let b = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de73),
        );
        let sam = T(&mut c.stack, &[a, b]);
        assert_jet_ubig(
            c,
            jet_shas,
            sam,
            ubig!(_0xf7569a89650553ef13f9a8f0bb751fd42b70a4821be6bc1cbe197af33ce4843c),
        );
    }

    #[test]
    fn test_shax() {
        let c = &mut init_context();

        assert_jet_ubig(
            c,
            jet_shax,
            D(7303014), // 'foo'
            ubig!(_0xaee76662885e8af9a0bf8364702d42133441301d3c459bf98fc6ff686bb4262c),
        );

        let a = A(
            &mut c.stack,
            &ubig!(_0xaee76662885e8af9a0bf8364702d42133441301d3c459bf98fc6ff686bb4262c),
        );
        assert_jet_ubig(
            c,
            jet_shax,
            a,
            ubig!(_0x9ee26e46c2028aa4a9c463aa722b82ed8bf6e185c3e5a5a69814a2c78fe8adc7),
        );

        assert_jet_ubig(
            c,
            jet_shax,
            D(123456789),
            ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72),
        );

        let a = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72),
        );
        assert_jet_ubig(
            c,
            jet_shax,
            a,
            ubig!(_0xf90f3184d7347a20cfdd2d5f7ac5c82eb9ab7af54c9419fbc18832c5a33360c9),
        )
    }

    #[test]
    fn test_shay() {
        let c = &mut init_context();

        let sam = T(&mut c.stack, &[D(1), D(0)]);
        let ret = A(
            &mut c.stack,
            &ubig!(_0x1da0af1706a31185763837b33f1d90782c0a78bbe644a59c987ab3ff9c0b346e),
        );
        assert_jet(c, jet_shay, sam, ret);

        let sam = T(&mut c.stack, &[D(0), D(1)]);
        let ret = A(
            &mut c.stack,
            &ubig!(_0x55b852781b9995a44c939b64e441ae2724b96f99c8f4fb9a141cfc9842c4b0e3),
        );
        assert_jet(c, jet_shay, sam, ret);

        let sam = T(&mut c.stack, &[D(1), D(478560413032)]); // [1 'hello']
        let ret = A(
            &mut c.stack,
            &ubig!(_0x23b14de6713b28aadf8f95026636eb6ab63e99c952bceb401fa4f1642640a9aa),
        );
        assert_jet(c, jet_shay, sam, ret);

        let sam = T(&mut c.stack, &[D(2), D(478560413032)]); // [2 'hello']
        let ret = A(
            &mut c.stack,
            &ubig!(_0xde1e7ee30cecf453f1d77c08a125fdc6a4bbac72c01dd7a1e21cd0d22f7e2f37),
        );
        assert_jet(c, jet_shay, sam, ret);

        let big = DIRECT_MAX + 1;
        let ida = unsafe {
            IndirectAtom::new_raw_bytes(&mut c.stack, 8, &big as *const u64 as *const u8)
        };
        let sam = T(&mut c.stack, &[ida.as_noun(), D(478560413032)]);
        assert_jet_err(
            c,
            jet_shay,
            sam,
            JetErr::Fail(Error::NonDeterministic(D(0))),
        );

        let big: u128 = (DIRECT_MAX as u128) << 64;
        let ida = unsafe {
            IndirectAtom::new_raw_bytes(&mut c.stack, 8, &big as *const u128 as *const u8)
        };
        let sam = T(&mut c.stack, &[ida.as_noun(), D(478560413032)]);
        assert_jet_err(
            c,
            jet_shay,
            sam,
            JetErr::Fail(Error::NonDeterministic(D(0))),
        );
    }

    #[test]
    fn test_shal() {
        let c = &mut init_context();

        let sam = T(&mut c.stack, &[D(1), D(1)]);
        assert_jet_ubig(
            c,
            jet_shal,
            sam,
            ubig!(_0x39e3d936c6e31eaac08fcfcfe7bb443460c61c0bd5b74408c8bcc35a6b8d6f5700bdcddeaa4b466ae65f8fb67f67ca62dc34149e1d44d213ddfbc13668b6547b)
        );

        let sam = T(&mut c.stack, &[D(1), D(2)]);
        assert_jet_ubig(
            c,
            jet_shal,
            sam,
            ubig!(_0xcadc698fca01cf2935f760278554b4e61f35453975a5bb45389003159bc8485b7018dd8152d9cc23b6e9dd91b107380b9d14ddbf9cc037ee53a857b6c948b8fa)
        );

        let wid = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72),
        );
        let dat = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de73),
        );
        let sam = T(&mut c.stack, &[wid, dat]);
        assert_jet_err(
            c,
            jet_shal,
            sam,
            JetErr::Fail(Error::NonDeterministic(D(0))),
        );

        let wid = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72),
        );
        let sam = T(&mut c.stack, &[wid, D(1)]);
        assert_jet_err(
            c,
            jet_shal,
            sam,
            JetErr::Fail(Error::NonDeterministic(D(0))),
        );
    }

    #[test]
    fn test_sha1() {
        let c = &mut init_context();

        let sam = T(&mut c.stack, &[D(1), D(1)]);
        assert_jet_ubig(
            c,
            jet_sha1,
            sam,
            ubig!(_0xbf8b4530d8d246dd74ac53a13471bba17941dff7),
        );

        let sam = T(&mut c.stack, &[D(1), D(2)]);
        assert_jet_ubig(
            c,
            jet_sha1,
            sam,
            ubig!(_0xc4ea21bb365bbeeaf5f2c654883e56d11e43c44e),
        );

        let wid = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72),
        );
        let dat = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de73),
        );
        let sam = T(&mut c.stack, &[wid, dat]);
        assert_jet_err(
            c,
            jet_sha1,
            sam,
            JetErr::Fail(Error::NonDeterministic(D(0))),
        );

        let wid = A(
            &mut c.stack,
            &ubig!(_0xa1d6eb6ef33f233ae6980ca7c4fc65f90fe1bdee11c730d41607b4747c83de72),
        );
        let sam = T(&mut c.stack, &[wid, D(1)]);
        assert_jet_err(
            c,
            jet_sha1,
            sam,
            JetErr::Fail(Error::NonDeterministic(D(0))),
        );
    }
}
