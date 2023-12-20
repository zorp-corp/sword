use sha::sha1::Sha1;
use sha::sha2::{Sha256, Sha512};
use sha::utils::{Digest, DigestExt};


pub fn ac_sha1(message: &mut [u8], out: &mut [u8]) {
    message.reverse();
    let result = Sha1::default().digest(message);
    println!("result: {:?}", result);
}

#[cfg(test)]
#[cfg(feature = "test_vs_urcrypt")]
mod urcrypt_tests {
    use super::{ac_sha1};
    use ibig::ubig;
    use urcrypt_sys::{urcrypt_sha1};

    #[test]
    fn test_sha1() {
        let mut message: [u8; 32] = [42; 32];

        let mut uc_out: [u8; 32] = [0; 32];
        unsafe { urcrypt_sha1(message.as_ptr(), uc_out.as_mut_ptr()) };

        let mut ac_out: [u8; 32] = [0; 32];
        ac_sha1(&mut message, &mut ac_out);

        assert_eq!(ac_out, uc_out);
    }
}

