use std::ptr::copy_nonoverlapping;

use ed25519_dalek::{SignatureError, SigningKey};
use rand::{rngs::StdRng, SeedableRng};

/// Generate a public key from the given seed and write it to the given 32-byte output buffer,
pub fn ac_ed_puck(seed: &mut [u8; 32], out: *mut u8) -> Result<(), SignatureError> {
    let mut csprng = StdRng::from_seed(*seed);
    let signing_key = SigningKey::generate(&mut csprng);
    let verifying_key = signing_key.verifying_key();
    unsafe {
        copy_nonoverlapping(verifying_key.as_bytes().as_ptr(), out, 32);
    }
    Ok(())
}

/// Perform a key exchange between the given public key and the keypair generated from the given seed,
/// writing the resulting shared key to the given 32-byte output buffer.
pub fn ac_ed_shar(public: &[u8; 32], seed: &[u8; 32], out: *mut u8) -> Result<(), SignatureError> {
    // Generate a keypair from the given seed.
    let mut csprng = StdRng::from_seed(*seed);
    let self_key = SigningKey::generate(&mut csprng);

    let mut keypair_bytes = [0u8; 64];
    keypair_bytes[..32].copy_from_slice(&self_key.to_bytes());
    keypair_bytes[32..].copy_from_slice(public);
    let shared_key = SigningKey::from_keypair_bytes(&keypair_bytes)?;
    unsafe {
        copy_nonoverlapping(shared_key.to_bytes().as_ptr(), out, 32);
    }
    Ok(())
}
