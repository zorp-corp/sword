use rand::{rngs::StdRng, SeedableRng};
use ed25519_dalek::{SigningKey, VerifyingKey};
use x25519_dalek::{EphemeralSecret, PublicKey, SharedSecret};

/// Generate a public key from the given seed and write it to the given output slice.
pub fn ac_ed_puck(seed: &mut [u8; 32], out: &mut [u8; 32]) {
    let signing_key = SigningKey::from_bytes(seed);
    let verifying_key = signing_key.verifying_key();
    *out = verifying_key.to_bytes();
}

/// Perform a key exchange between the given public key and the private key of
/// the keypair generate from the given seed, writing the resulting shared key
/// to the given output slice.
pub fn ac_ed_shar(public: &[u8; 32], seed: &[u8; 32], out: &mut [u8; 32]) {
    // let self_key = SigningKey::from_bytes(seed);

    // let other_key = PublicKey::from(*public);
    // let shared_secret = self_key.diffie_hellman(&other_key);
    // *out = shared_secret.to_bytes();
}

#[cfg(test)]
mod tests {
    use super::ac_ed_puck;

    #[test]
    fn test_ed_puck() {
        let mut seed: [u8; 32] = [0; 32];
        let mut public_key: [u8; 32] = [0; 32];
        ac_ed_puck(&mut seed, &mut public_key);
        assert_eq!(
            public_key,
            [
                59, 106, 39, 188, 206, 182, 164, 45, 98, 163, 168, 208, 42, 111, 13, 115, 101, 50,
                21, 119, 29, 226, 67, 166, 58, 192, 72, 161, 139, 89, 218, 41
            ]
        );
    }
}

#[cfg(test)]
#[cfg(feature = "test_vs_urcrypt")]
mod ucrypt_tests {
    use super::{ac_ed_puck, ac_ed_shar};
    use ibig::ubig;
    use urcrypt_sys::{urcrypt_ed_puck, urcrypt_ed_shar};

    #[test]
    fn test_ed_puck() {
        let mut seed: [u8; 32] = [0; 32];

        let mut uc_out: [u8; 32] = [0; 32];
        unsafe { urcrypt_ed_puck(seed.as_ptr(), uc_out.as_mut_ptr()) };

        let mut ac_out: [u8; 32] = [0; 32];
        ac_ed_puck(&mut seed, &mut ac_out);

        assert_eq!(ac_out, uc_out);
    }

    #[test]
    fn test_ed_shar() {
        let seed_src =
            &ubig!(_0x4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb)
                .to_le_bytes();
        let mut seed: [u8; 32] = [0; 32];
        seed.copy_from_slice(seed_src);

        let public_key_src =
            &ubig!(_0xd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a)
                .to_le_bytes();
        let mut public_key: [u8; 32] = [0; 32];
        public_key.copy_from_slice(public_key_src);

        let mut uc_out: [u8; 32] = [0; 32];
        unsafe { urcrypt_ed_shar(public_key.as_ptr(), seed.as_ptr(), uc_out.as_mut_ptr()) };

        let mut ac_out: [u8; 32] = [0; 32];
        ac_ed_shar(&public_key, &seed, &mut ac_out);

        assert_eq!(ac_out, uc_out);
    }
}
