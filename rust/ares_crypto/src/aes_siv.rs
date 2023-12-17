use aes_siv::{
    aead::{heapless::Vec, AeadInPlace, Error, KeyInit},
    Aes256SivAead,
    Nonce, // Or `Aes128SivAead`
};

pub fn _ac_aes_siv_en() -> Result<(), Error> {
    todo!();
    // let key = Aes256SivAead::generate_key(&mut OsRng);
    // let cipher = Aes256SivAead::new(&key);
    // let nonce = Nonce::from_slice(b"any unique nonce"); // 128-bits; unique per message

    // let mut buffer: Vec<u8, 128> = Vec::new(); // Note: buffer needs 16-bytes overhead for auth tag tag
    // buffer.extend_from_slice(b"plaintext message");

    // // Encrypt `buffer` in-place, replacing the plaintext contents with ciphertext
    // cipher.encrypt_in_place(nonce, b"", &mut buffer)?;

    // // `buffer` now contains the message ciphertext
    // assert_ne!(&buffer, b"plaintext message");

    // // Decrypt `buffer` in-place, replacing its ciphertext context with the original plaintext
    // cipher.decrypt_in_place(nonce, b"", &mut buffer)?;
    // assert_eq!(&buffer, b"plaintext message");
    // Ok(())
}

pub fn _ac_aes_siv_de() -> Result<(), Error> {
    todo!();
}

pub fn ac_aes_siva_en() -> Result<(), Error> {
    todo!();
}

pub fn ac_aes_siva_de() -> Result<(), Error> {
    todo!();
}

pub fn ac_aes_sivb_en() -> Result<(), Error> {
    todo!();
}

pub fn ac_aes_sivb_de() -> Result<(), Error> {
    todo!();
}

pub fn ac_aes_sivc_en() -> Result<(), Error> {
    todo!();
}

pub fn ac_aes_sivc_de() -> Result<(), Error> {
    todo!();
}
