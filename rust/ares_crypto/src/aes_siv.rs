use aes_siv::{
    aead::KeyInit,
    siv::Aes128Siv,
    Error
};

pub fn _ac_aes_siv_en(
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    iv: &mut [u8; 16],
    out: &mut [u8],
) -> Result<(), Error> {
    let mut cipher = Aes128Siv::new_from_slice(&key).unwrap();
    let iv_tag = cipher.encrypt_in_place_detached(data, message)?;
    let mut iv_slice = iv_tag.as_slice().to_owned();
    iv_slice.reverse();
    iv.copy_from_slice(&iv_slice);
    message.reverse();
    out.copy_from_slice(message);
    Ok(())
}

// pub fn _ac_aes_siv_de(
//     key: &mut [u8],
//     message: &mut [u8],
//     data: &mut [&mut [u8]],
//     iv: &[u8; 16],
//     out: &mut [u8],
// ) {
//     todo!();
// }

pub fn ac_aes_siva_en(
    key: &mut [u8; 32],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    iv: &mut [u8; 16],
    out: &mut [u8],
) {
    _ac_aes_siv_en(key, message, data, iv, out).unwrap();
}

// pub fn ac_aes_siva_de(
//     key: &mut [u8; 32],
//     message: &mut [u8],
//     data: &mut [&mut [u8]],
//     iv: &[u8; 16],
//     out: &mut [u8],
// ) {
//     todo!();
// }

// pub fn ac_aes_sivb_en(
//     key: &mut [u8; 48],
//     message: &mut [u8],
//     data: &mut [&mut [u8]],
//     iv: &[u8; 16],
//     out: &mut [u8],
// ) {
//     _ac_aes_siv_en(key, message, data, iv, out);
// }

// pub fn ac_aes_sivb_de(
//     key: &mut [u8; 48],
//     message: &mut [u8],
//     data: &mut [&mut [u8]],
//     iv: &[u8; 16],
//     out: &mut [u8],
// ) {
//     todo!();
// }

// pub fn ac_aes_sivc_en(
//     key: &mut [u8; 64],
//     message: &mut [u8],
//     data: &mut [&mut [u8]],
//     iv: &[u8; 16],
//     out: &mut [u8],
// ) {
//     _ac_aes_siv_en(key, message, data, iv, out);
// }

// pub fn ac_aes_sivc_de(
//     key: &mut [u8; 64],
//     message: &mut [u8],
//     data: &mut [&mut [u8]],
//     iv: &[u8; 16],
//     out: &mut [u8],
// ) {
//     todo!();
// }

#[cfg(test)]
#[cfg(feature = "test_vs_urcrypt")]
mod urcrypt_tests {
    use super::{
        ac_aes_siva_en,
        // ac_aes_siva_de, ac_aes_siva_en, ac_aes_sivb_de, ac_aes_sivb_en, ac_aes_sivc_de,
        // ac_aes_sivc_en,
    };
    use urcrypt_sys::{
        urcrypt_aes_siv_data,
        // urcrypt_aes_siva_de,
        urcrypt_aes_siva_en,
        // urcrypt_aes_sivb_de,
        // urcrypt_aes_sivb_en, urcrypt_aes_sivc_de, urcrypt_aes_sivc_en,
    };

    #[test]
    fn test_aes_siva_en() {
        let mut key: [u8; 32] = [42; 32];
        let mut message: [u8; 32] = [42; 32];
        let mut iv: [u8; 16] = [42; 16];

        let mut uc_bytes = [42; 32];
        let mut uc_bytes_two = [43; 32];
        let mut uc_out: [u8; 32] = [0; 32];
        let mut uc_data: [urcrypt_aes_siv_data; 2] = [
            urcrypt_aes_siv_data {
                bytes: uc_bytes.as_mut_ptr(),
                length: uc_bytes.len(),
            },
            urcrypt_aes_siv_data {
                bytes: uc_bytes_two.as_mut_ptr(),
                length: uc_bytes_two.len(),
            },
        ];

        unsafe {
            urcrypt_aes_siva_en(
                message.as_mut_ptr(),
                message.len(),
                uc_data.as_mut_ptr(),
                uc_data.len(),
                key.as_mut_ptr(),
                iv.as_mut_ptr(),
                uc_out.as_mut_ptr(),
            )
        };

        let mut ac_data: [&mut [u8]; 2] = [&mut [42; 32], &mut [43; 32]];
        let mut ac_out: [u8; 32] = [0; 32];
        ac_aes_siva_en(&mut key, &mut message, &mut ac_data, &mut iv, &mut ac_out);

        assert_eq!(ac_out, uc_out);
    }
}
