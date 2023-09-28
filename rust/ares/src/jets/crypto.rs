use either::{Left, Right};
use crate::jets::util::{met, slot};
use crate::jets::{JetErr, Result};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{IndirectAtom, Noun};
use urcrypt_sys::*;

crate::gdb!();

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
    }
}
