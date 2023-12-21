use aes_siv::aead::{generic_array::GenericArray, KeyInit};
use aes_siv::siv::{Aes128Siv, Aes256Siv, CmacSiv};

use aes::Aes192;
type Aes192Siv = CmacSiv<Aes192>;

#[derive(Debug)]
pub enum Error {
    InvalidKeyLength,
    InvalidOutputLength,
    InvalidHeadersLength,
    CipherNotAuthentic,
}

pub fn _ac_aes_siv_en(
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    key.reverse();
    message.reverse();
    for i in 0..data.len() {
        data[i].reverse();
    }

    let iv_tag;
    if key.len() == 32 {
        if let Ok(mut cipher) = Aes128Siv::new_from_slice(&key) {
            match cipher.encrypt_in_place_detached(data, message) {
                Ok(tag) => iv_tag = tag,
                Err(_) => return Err(Error::InvalidOutputLength),
            }
        } else {
            return Err(Error::InvalidKeyLength);
        }
    } else if key.len() == 48 {
        if let Ok(mut cipher) = Aes192Siv::new_from_slice(&key) {
            match cipher.encrypt_in_place_detached(data, message) {
                Ok(tag) => iv_tag = tag,
                Err(_) => return Err(Error::InvalidOutputLength),
            }
        } else {
            return Err(Error::InvalidKeyLength);
        }
    } else if key.len() == 64 {
        if let Ok(mut cipher) = Aes256Siv::new_from_slice(&key) {
            match cipher.encrypt_in_place_detached(data, message) {
                Ok(tag) => iv_tag = tag,
                Err(_) => return Err(Error::InvalidOutputLength),
            }
        } else {
            return Err(Error::InvalidKeyLength);
        }
    } else {
        return Err(Error::InvalidKeyLength);
    }
    let mut iv_slice = iv_tag.as_slice().to_owned();
    iv_slice.reverse();
    iv.copy_from_slice(&iv_slice);
    message.reverse();
    out[0..message.len()].copy_from_slice(message);
    Ok(())
}

pub fn _ac_aes_siv_de(
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    key.reverse();
    message.reverse();
    iv.reverse();
    for i in 0..data.len() {
        data[i].reverse();
    }

    let iv_array = GenericArray::from_slice(iv);
    if key.len() == 32 {
        if let Ok(mut cipher) = Aes128Siv::new_from_slice(&key) {
            match cipher.decrypt_in_place_detached(data, message, iv_array) {
                Ok(_) => (),
                Err(_) => return Err(Error::CipherNotAuthentic),
            }
        } else {
            return Err(Error::InvalidKeyLength);
        }
    } else if key.len() == 48 {
        if let Ok(mut cipher) = Aes192Siv::new_from_slice(&key) {
            match cipher.decrypt_in_place_detached(data, message, iv_array) {
                Ok(_) => (),
                Err(_) => return Err(Error::CipherNotAuthentic),
            }
        } else {
            return Err(Error::InvalidKeyLength);
        }
    } else if key.len() == 64 {
        if let Ok(mut cipher) = Aes256Siv::new_from_slice(&key) {
            match cipher.decrypt_in_place_detached(data, message, iv_array) {
                Ok(_) => (),
                Err(_) => return Err(Error::CipherNotAuthentic),
            }
        } else {
            return Err(Error::InvalidKeyLength);
        }
    } else {
        return Err(Error::InvalidKeyLength);
    }
    message.reverse();
    out.copy_from_slice(message);
    Ok(())
}

pub fn ac_aes_siva_en(
    // key: &mut [u8; 32],
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    _ac_aes_siv_en(key, message, data, iv, out)
}

pub fn ac_aes_siva_de(
    // key: &mut [u8; 32],
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    _ac_aes_siv_de(key, message, data, iv, out)
}

pub fn ac_aes_sivb_en(
    // key: &mut [u8; 48],
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    _ac_aes_siv_en(key, message, data, iv, out)
}

pub fn ac_aes_sivb_de(
    // key: &mut [u8; 48],
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    _ac_aes_siv_de(key, message, data, iv, out)
}

pub fn ac_aes_sivc_en(
    // key: &mut [u8; 64],
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    _ac_aes_siv_en(key, message, data, iv, out)
}

pub fn ac_aes_sivc_de(
    // key: &mut [u8; 64],
    key: &mut [u8],
    message: &mut [u8],
    data: &mut [&mut [u8]],
    // iv: &mut [u8; 16],
    iv: &mut [u8],
    out: &mut [u8],
) -> Result<(), Error> {
    _ac_aes_siv_de(key, message, data, iv, out)
}

#[cfg(test)]
#[cfg(feature = "test_vs_urcrypt")]
mod urcrypt_tests {
    use super::{
        ac_aes_siva_de, ac_aes_siva_en, ac_aes_sivb_de, ac_aes_sivb_en, ac_aes_sivc_de,
        ac_aes_sivc_en,
    };
    use urcrypt_sys::{
        urcrypt_aes_siv_data, urcrypt_aes_siva_de, urcrypt_aes_siva_en, urcrypt_aes_sivb_de,
        urcrypt_aes_sivb_en, urcrypt_aes_sivc_de, urcrypt_aes_sivc_en,
    };

    #[test]
    fn test_aes_siva_de() {
        todo!();
    }

    #[test]
    fn test_aes_siva_en() {
        // https://datatracker.ietf.org/doc/html/rfc5297#section-4
        let mut uc_key: [u8; 32] = [255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244, 243, 242, 241, 240, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255];
        let mut uc_message: [u8; 14] = [238, 221, 204, 187, 170, 153, 136, 119, 102, 85, 68, 51, 34, 17];
        let mut uc_iv = [0u8; 16];

        let mut uc_bytes = [39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16];
        let mut uc_out: [u8; 32] = [0; 32];
        let mut uc_data: [urcrypt_aes_siv_data; 1] = [
            urcrypt_aes_siv_data {
                bytes: uc_bytes.as_mut_ptr(),
                length: uc_bytes.len(),
            },
        ];

        unsafe {
            urcrypt_aes_siva_en(
                uc_message.as_mut_ptr(),
                uc_message.len(),
                uc_data.as_mut_ptr(),
                uc_data.len(),
                uc_key.as_mut_ptr(),
                uc_iv.as_mut_ptr(),
                uc_out.as_mut_ptr(),
            )
        };

        let mut ac_key: [u8; 32] = [255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244, 243, 242, 241, 240, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255];
        let mut ac_message: [u8; 14] = [238, 221, 204, 187, 170, 153, 136, 119, 102, 85, 68, 51, 34, 17];
        let mut ac_iv = [0u8; 16];

        let ac_data: &mut[&mut [u8]] = &mut[&mut [39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16]];
        let mut ac_out: [u8; 32] = [0; 32];
        ac_aes_siva_en(&mut ac_key, &mut ac_message, ac_data, &mut ac_iv, &mut ac_out).unwrap();

        assert_eq!(ac_out, uc_out);
    }

    #[test]
    fn test_aes_sivb_en() {
        let mut key: [u8; 48] = [42; 48];
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
            urcrypt_aes_sivb_en(
                message.as_mut_ptr(),
                message.len(),
                uc_data.as_mut_ptr(),
                uc_data.len(),
                key.as_mut_ptr(),
                iv.as_mut_ptr(),
                uc_out.as_mut_ptr(),
            )
        };

        let mut ac_data: [&mut [u8]; 2] = [&mut uc_bytes, &mut uc_bytes_two];
        let mut ac_out: [u8; 32] = [0; 32];
        ac_aes_sivb_en(&mut key, &mut message, &mut ac_data, &mut iv, &mut ac_out).unwrap();

        assert_eq!(ac_out, uc_out);
    }

    #[test]
    fn test_aes_sivb_de() {
        todo!();
    }

    #[test]
    fn test_aes_sivc_en() {
        let mut key: [u8; 64] = [42; 64];
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
            urcrypt_aes_sivc_en(
                message.as_mut_ptr(),
                message.len(),
                uc_data.as_mut_ptr(),
                uc_data.len(),
                key.as_mut_ptr(),
                iv.as_mut_ptr(),
                uc_out.as_mut_ptr(),
            )
        };

        let mut ac_data: [&mut [u8]; 2] = [&mut uc_bytes, &mut uc_bytes_two];
        let mut ac_out: [u8; 32] = [0; 32];
        ac_aes_sivc_en(&mut key, &mut message, &mut ac_data, &mut iv, &mut ac_out).unwrap();

        assert_eq!(ac_out, uc_out);
    }

    #[test]
    fn test_aes_sivc_de() {
        todo!();
    }
}
