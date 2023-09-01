use super::Snapshot;
use crate::mem::NockStack;
use crate::mug::mug_u32;
use crate::noun::{Noun, D};
use libc::{c_char, c_int, c_void, size_t};
use std::ffi::CString;
use std::path::{Path, PathBuf};

crate::gdb!();

mod raw {
    use super::*;

    #[repr(C)]
    pub struct RootState {
        pub epoch: u64,
        pub event: u64,
        pub root: u64,
    }

    #[link(name = "pma_malloc", kind = "static")]
    extern "C" {
        pub(super) fn pma_init(path: *const c_char) -> c_int;
        pub(super) fn pma_load(path: *const c_char) -> RootState;
        pub(super) fn pma_close(epoch: u64, event: u64, root: u64) -> c_int;
        pub(super) fn pma_malloc(size: size_t) -> *mut c_void;
        pub(super) fn pma_free(ptr: *mut c_void) -> c_int;
        pub(super) fn pma_sync(epoch: u64, event: u64, root: u64) -> c_int;
        pub(super) fn pma_in_arena(ptr: *const c_void) -> bool;
    }

    #[link(name = "test_pma_malloc_unit", kind = "static")]
    extern "C" {
        pub(super) fn test_pma(path: *const c_char) -> c_void;
    }
}

unsafe fn pma_init<P: AsRef<Path>>(path: P) -> i32 {
    let path = CString::new(path.as_ref().to_str().unwrap()).unwrap();
    raw::pma_init(path.as_ptr())
}

unsafe fn pma_load<P: AsRef<Path>>(path: P) -> (u64, u64, Noun) {
    let path = CString::new(path.as_ref().to_str().unwrap()).unwrap();
    let rs = raw::pma_load(path.as_ptr());
    (rs.epoch, rs.event, Noun::from_raw(rs.root))
}

#[allow(dead_code)]
unsafe fn pma_close(epoch: u64, event: u64, root: Noun) -> i32 {
    raw::pma_close(epoch, event, root.as_raw())
}

pub fn pma_malloc<T>(size: usize) -> *mut T {
    unsafe { raw::pma_malloc(size as size_t) as *mut T }
}

/** Allocate a block of memory from the persistent memory arena.
 *
 * Size is in *words*, unlike the underlying pma_malloc.
 */
pub fn pma_malloc_w<T>(size: usize) -> *mut T {
    unsafe { raw::pma_malloc(size << 3 as size_t) as *mut T }
}

#[allow(dead_code)]
unsafe fn pma_free<T>(ptr: *mut T) -> i32 {
    raw::pma_free(ptr as *mut c_void)
}

unsafe fn pma_sync(epoch: u64, event: u64, root: Noun) -> i32 {
    raw::pma_sync(epoch, event, root.as_raw())
}

pub fn pma_in_arena<T>(ptr: *const T) -> bool {
    unsafe { raw::pma_in_arena(ptr as *const c_void) }
}

#[allow(dead_code)]
unsafe fn test_pma<P: AsRef<Path>>(path: P) {
    let path = CString::new(path.as_ref().to_str().unwrap()).unwrap();
    raw::test_pma(path.as_ptr());
}

pub struct Pma {
    path: PathBuf,
    noun: Noun,
}

impl Pma {
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        let path = path.as_ref().to_path_buf();
        Self { path, noun: D(0) }
    }
}

impl Snapshot for Pma {
    fn save(&mut self, stack: &mut NockStack, noun: &mut Noun) {
        // Required so everything in the PMA has a cached mug, otherwise we would try to write
        let _mug = mug_u32(stack, *noun);

        unsafe { stack.copy_pma(noun) };
        self.noun = *noun;
    }

    fn sync(&mut self, _stack: &mut NockStack, epoch: u64, event: u64) {
        unsafe {
            pma_sync(epoch, event, self.noun);
        }
    }

    fn load(&mut self, _stack: &mut NockStack) -> std::io::Result<(u64, u64, Noun)> {
        let path = self.path.join(".bin/page.bin");
        if path.is_file() {
            eprintln!("\rload: found snapshot at {:?}", path);
            unsafe { Ok(pma_load(&self.path)) }
        } else {
            eprintln!("\rload: creating snapshot at {:?}", path);
            unsafe { pma_init(&self.path) };
            Ok((0, 0, D(0)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::noun::IndirectAtom;

    fn clean_test_dir(path: &str) {
        if let Err(err) = std::fs::remove_dir_all(path) {
            if err.kind() != std::io::ErrorKind::NotFound {
                panic!("failed to remove dir: {}", err);
            }
        }
    }

    #[test]
    fn test_pma_unit() {
        let path = "/tmp/ares_pma_test_unit";
        clean_test_dir(path);

        unsafe {
            test_pma(path);
        }

        clean_test_dir(path);
    }

    #[test]
    fn test_pma_sanity() {
        let path = "/tmp/ares_pma_test_sanity";
        clean_test_dir(path);

        unsafe {
            let stack = &mut NockStack::new(8 << 10 << 10, 0);
            let root = IndirectAtom::new_raw(stack, 1, &0xffff_ffff_ffff_ffff).as_noun();
            let mut base: *mut u8;
            let mut new_alloc: *mut u8;

            assert!(0 == pma_init(path));

            // 2 allocations of every size
            // assert that slots / pages are correct number of bytes apart
            base = pma_malloc::<u8>(16);
            let alloc_16 = pma_malloc::<u8>(16);
            assert!(alloc_16 == (base.add(16)));
            assert!(0 != (alloc_16 as u64 % 4096));

            base = pma_malloc::<u8>(32);
            let alloc_32 = pma_malloc::<u8>(32);
            assert!(alloc_32 == (base.add(32)));
            assert!(0 != (alloc_32 as u64 % 4096));

            base = pma_malloc::<u8>(64);
            let alloc_64 = pma_malloc::<u8>(64);
            assert!(alloc_64 == (base.add(64)));
            assert!(0 != (alloc_64 as u64 % 4096));

            base = pma_malloc::<u8>(128);
            let alloc_128 = pma_malloc::<u8>(128);
            assert!(alloc_128 == (base.add(128)));
            assert!(0 != (alloc_128 as u64 % 4096));

            base = pma_malloc::<u8>(256);
            let alloc_256 = pma_malloc::<u8>(256);
            assert!(alloc_256 == (base.add(256)));
            assert!(0 != (alloc_256 as u64 % 4096));

            base = pma_malloc::<u8>(512);
            let alloc_512 = pma_malloc::<u8>(512);
            assert!(alloc_512 == (base.add(512)));
            assert!(0 != (alloc_512 as u64 % 4096));

            base = pma_malloc::<u8>(1024);
            let alloc_1024 = pma_malloc::<u8>(1024);
            assert!(alloc_1024 == (base.add(1024)));
            assert!(0 != (alloc_1024 as u64 % 4096));

            base = pma_malloc::<u8>(2048);
            let alloc_2048 = pma_malloc::<u8>(2048);
            assert!(alloc_2048 == (base.add(4096)));
            assert!(0 == (alloc_2048 as u64 % 4096));

            base = pma_malloc::<u8>(4096);
            let alloc_4096 = pma_malloc::<u8>(4096);
            assert!(alloc_4096 == (base.add(4096)));
            assert!(0 == (alloc_4096 as u64 % 4096));

            base = pma_malloc::<u8>(8192);
            let alloc_8192 = pma_malloc::<u8>(8192);
            assert!(alloc_8192 == (base.add(8192)));
            assert!(0 == (alloc_8192 as u64 % 4096));

            // sync
            // check that everything is still where it should be
            *alloc_16 = 0x01;
            *alloc_32 = 0x02;
            *alloc_64 = 0x03;
            *alloc_128 = 0x04;
            *alloc_256 = 0x05;
            *alloc_512 = 0x06;
            *alloc_1024 = 0x07;
            *alloc_2048 = 0x08;
            *alloc_4096 = 0x09;
            *alloc_8192 = 0x0a;

            assert!(0 == pma_sync(1, 1, root));

            assert!(0x01 == *alloc_16);
            assert!(0x02 == *alloc_32);
            assert!(0x03 == *alloc_64);
            assert!(0x04 == *alloc_128);
            assert!(0x05 == *alloc_256);
            assert!(0x06 == *alloc_512);
            assert!(0x07 == *alloc_1024);
            assert!(0x08 == *alloc_2048);
            assert!(0x09 == *alloc_4096);
            assert!(0x0a == *alloc_8192);

            // close PMA
            // load
            // check that everything is still where it should be
            assert!(0 == pma_close(2, 2, root));

            let root_state = pma_load(path);
            assert!(2 == root_state.0);
            assert!(2 == root_state.1);
            assert!(root_state.2.raw_equals(root));

            assert!(0x01 == *alloc_16);
            assert!(0x02 == *alloc_32);
            assert!(0x03 == *alloc_64);
            assert!(0x04 == *alloc_128);
            assert!(0x05 == *alloc_256);
            assert!(0x06 == *alloc_512);
            assert!(0x07 == *alloc_1024);
            assert!(0x08 == *alloc_2048);
            assert!(0x09 == *alloc_4096);
            assert!(0x0a == *alloc_8192);

            // free 1-page allocation
            // sync
            // make new 1-page allocation
            // sync
            // check that page is being re-used
            assert!(0 == pma_free(alloc_4096));
            assert!(0 == pma_sync(3, 3, root));
            new_alloc = pma_malloc(4096);
            assert!(new_alloc == alloc_4096);

            // free 2-page allocation
            // sync
            // make new 2-page allocation
            // sync
            // check that page run is being re-used
            assert!(0 == pma_free(alloc_8192));
            assert!(0 == pma_sync(4, 4, root));
            new_alloc = pma_malloc(8192);
            assert!(new_alloc == alloc_8192);

            // free 2-page allocation
            // make new 2-page allocation
            // sync
            // check that page run is NOT being re-used
            assert!(0 == pma_sync(5, 5, root));
            assert!(0 == pma_free(alloc_8192));
            new_alloc = pma_malloc(8192);
            assert!(new_alloc != alloc_8192);

            // multiple syncs
            // close
            // load
            // everything is where it should be
            assert!(0 == pma_sync(6, 6, root));
            assert!(0 == pma_sync(7, 7, root));
            assert!(0 == pma_close(8, 8, root));

            let root_state = pma_load(path);
            assert!(8 == root_state.0);
            assert!(8 == root_state.1);
            assert!(root_state.2.raw_equals(root));

            assert!(0x01 == *alloc_16);
            assert!(0x02 == *alloc_32);
            assert!(0x03 == *alloc_64);
            assert!(0x04 == *alloc_128);
            assert!(0x05 == *alloc_256);
            assert!(0x06 == *alloc_512);
            assert!(0x07 == *alloc_1024);
            assert!(0x08 == *alloc_2048);

            // free many allocations
            // close
            assert!(0 == pma_free(alloc_16));
            assert!(0 == pma_free(alloc_32));
            assert!(0 == pma_free(alloc_64));
            assert!(0 == pma_free(alloc_128));
            assert!(0 == pma_free(alloc_256));
            assert!(0 == pma_free(alloc_512));
            assert!(0 == pma_free(alloc_1024));
            assert!(0 == pma_free(alloc_2048));
            assert!(0 == pma_free(base));
            assert!(0 == pma_free(new_alloc));
            assert!(0 == pma_close(9, 9, root));
        }

        clean_test_dir(path);
    }
}
