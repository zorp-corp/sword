use libc::{c_char, c_int, c_void, size_t};
use std::ffi::CString;
use std::path::Path;

mod raw {
    use super::*;

    #[link(name = "pma_malloc", kind = "static")]
    extern "C" {
        pub(super) fn pma_init(path: *const c_char) -> c_int;
        pub(super) fn pma_load(path: *const c_char) -> c_int;
        pub(super) fn pma_close(epoch: u64, event: u64) -> c_int;
        pub(super) fn pma_malloc(size: size_t) -> *mut c_void;
        pub(super) fn pma_free(ptr: *mut c_void) -> c_int;
        pub(super) fn pma_sync(epoch: u64, event: u64) -> c_int;
    }
}

pub unsafe fn pma_init<P: AsRef<Path>>(path: P) -> i32 {
    let path = CString::new(path.as_ref().to_str().unwrap()).unwrap();
    raw::pma_init(path.as_ptr()) as i32
}

pub unsafe fn pma_load<P: AsRef<Path>>(path: P) -> i32 {
    let path = CString::new(path.as_ref().to_str().unwrap()).unwrap();
    raw::pma_load(path.as_ptr()) as i32
}

pub unsafe fn pma_close(epoch: u64, event: u64) -> i32 {
    raw::pma_close(epoch, event) as i32
}

pub unsafe fn pma_malloc(size: usize) -> *mut c_void {
    raw::pma_malloc(size as size_t)
}

pub unsafe fn pma_free(ptr: *mut c_void) -> i32 {
    raw::pma_free(ptr) as i32
}

pub unsafe fn pma_sync(epoch: u64, event: u64) -> i32 {
    raw::pma_sync(epoch, event) as i32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pma() {
        let path = "/tmp/ares_pma_test";
        if let Err(err) = std::fs::remove_dir_all(path) {
            if err.kind() != std::io::ErrorKind::NotFound {
                panic!("failed to remove dir: {}", err);
            }
        }

        unsafe {
            pma_init(path);
            let eight = pma_malloc(8) as *mut u64;
            *eight = 0xdeadbeef;
            assert!(0 == pma_close(10, 12));
            pma_load(path);
            assert_eq!(0, pma_sync(13, 15));
            let _ = pma_malloc(8) as *mut u64;
            assert_eq!(0, pma_sync(14, 16));
            let _ = pma_malloc(8) as *mut u64;
            assert_eq!(0, pma_sync(15, 16));
            let twenty = pma_malloc(8) as *mut u64;
            pma_free(twenty as *mut c_void);
            assert_eq!(0, pma_sync(16, 15));
            let _ = pma_malloc(8) as *mut u64;
            assert_eq!(0, pma_sync(17, 15));
            let _ = pma_malloc(8) as *mut u64;
            assert_eq!(0, pma_sync(18, 15));
            let _ = pma_malloc(8) as *mut u64;
            let twenty = pma_malloc(8) as *mut u64;
            *twenty = 0xcafebabe;
            pma_free(twenty as *mut c_void);
            pma_close(123, 124);
        }

        if let Err(err) = std::fs::remove_dir_all(path) {
            if err.kind() != std::io::ErrorKind::NotFound {
                panic!("failed to remove dir: {}", err);
            }
        }
    }
}
