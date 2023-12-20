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
    out.copy_from_slice(message);
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
        let mut key: [u8; 32] = [42; 32];
        let mut message: [u8; 32] = [
            61, 88, 88, 36, 83, 232, 120, 45, 27, 159, 15, 145, 140, 231, 114, 229, 61, 243, 54,
            183, 156, 53, 217, 103, 88, 36, 53, 37, 165, 240, 92, 133,
        ];
        let mut iv: [u8; 16] = [
            16, 90, 129, 170, 175, 145, 229, 78, 107, 253, 192, 138, 136, 52, 159, 219,
        ];

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
            urcrypt_aes_siva_de(
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
        ac_aes_siva_de(&mut key, &mut message, &mut ac_data, &mut iv, &mut ac_out).unwrap();

        assert_eq!(ac_out, uc_out);
    }

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

        let mut ac_data: [&mut [u8]; 2] = [&mut uc_bytes, &mut uc_bytes_two];
        let mut ac_out: [u8; 32] = [0; 32];
        ac_aes_siva_en(&mut key, &mut message, &mut ac_data, &mut iv, &mut ac_out).unwrap();

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
        let mut key: [u8; 48] = [42; 48];
        let mut message: [u8; 32] = [
            20, 249, 192, 238, 22, 92, 186, 62, 26, 194, 51, 61, 88, 148, 89, 208, 114, 24, 67, 99,
            35, 241, 247, 133, 64, 18, 144, 54, 126, 121, 100, 145,
        ];
        let mut iv: [u8; 16] = [
            9, 146, 75, 192, 45, 169, 211, 188, 36, 212, 236, 80, 49, 197, 78, 141,
        ];

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
            urcrypt_aes_sivb_de(
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
        ac_aes_sivb_de(&mut key, &mut message, &mut ac_data, &mut iv, &mut ac_out).unwrap();

        assert_eq!(ac_out, uc_out);
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
        let mut key: [u8; 64] = [42; 64];
        let mut message: [u8; 32] = [
            213, 96, 61, 200, 217, 8, 33, 147, 58, 213, 99, 8, 221, 23, 89, 206, 164, 237, 59, 231,
            235, 50, 93, 122, 50, 202, 78, 248, 218, 41, 170, 175,
        ];
        let mut iv: [u8; 16] = [
            105, 123, 123, 122, 45, 244, 179, 136, 167, 164, 134, 30, 97, 14, 241, 223,
        ];

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
            urcrypt_aes_sivc_de(
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
        ac_aes_sivc_de(&mut key, &mut message, &mut ac_data, &mut iv, &mut ac_out).unwrap();

        assert_eq!(ac_out, uc_out);
    }
}
