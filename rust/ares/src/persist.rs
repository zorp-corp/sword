use crate::jets::cold::Cold;
use crate::mem::NockStack;
use crate::noun::{Allocated, Atom, Cell, CellMemory, IndirectAtom, Noun};
use ares_pma::*;
use either::Either::{Left, Right};
use std::convert::TryInto;
use std::ffi::{c_void, CString};
use std::mem::size_of;
use std::path::PathBuf;
use std::ptr::copy_nonoverlapping;

const PMA_MODE: mode_t = 0o600; // RW for user only
const PMA_FLAGS: ULONG = 0; // ignored for now

const NOUN_MARKED: u64 = 1 << 63;

/// Handle to a PMA
pub struct PMA(*mut BT_state);

impl PMA {
    #[cfg(unix)]
    pub fn open(path: PathBuf) -> Result<Self, std::io::Error> {
        let mut state: *mut BT_state = std::ptr::null_mut();

        // correct for Unix thus cfg gated
        let path_cstring = CString::new(path.into_os_string().as_encoded_bytes())?;
        unsafe {
            bt_state_new(&mut state);
            let err = bt_state_open(state, path_cstring.as_ptr(), PMA_FLAGS, PMA_MODE);
            if err == 0 {
                Ok(PMA(state))
            } else {
                // XX need to free the state
                Err(std::io::Error::from_raw_os_error(err))
            }
        }
    }

    #[cfg(windows)]
    pub fn open(path: PathBuf) -> Result<Self, std::io::Error> {
        unimplemented!()
    }

    #[inline]
    pub fn meta_get(&self, field: usize) -> u64 {
        unsafe { bt_meta_get(self.0, field) }
    }

    #[inline]
    pub fn meta_set(&self, field: usize, val: u64) {
        unsafe { bt_meta_set(self.0, field, val) };
    }

    pub unsafe fn contains<T>(&self, ptr: *const T, count: usize) -> bool {
        bt_inbounds(self.0, ptr as *mut c_void) != 0
            && bt_inbounds(self.0, ptr.add(count) as *mut c_void) != 0
    }

    pub fn sync(&self) {
        unsafe {
            if bt_sync(self.0) != 0 {
                panic!("PMA sync failed but did not abort: this should never happen.");
            }
        }
    }

    pub fn close(self) -> Result<(), std::io::Error> {
        // XX need a way to free the state after
        let err = unsafe { bt_state_close(self.0) };
        if err == 0 {
            Ok(())
        } else {
            Err(std::io::Error::from_raw_os_error(err))
        }
    }
}

/**
 * This trait defines operations for copying a structure into the PMA.
 *
 * This is done in two phases. The [space_needed] phase counts how much space the structure needs in
 * the PMA, not counting referenced structures already in the PMA. Then a buffer is allocated in
 * the PMA of at least the computed size, and the [copy_to_buffer] phase copies the structure into
 * this buffer.
 *
 * The phases are separated so that instances of the trait may compose, while still allocating a
 * single buffer. Thus, in the instance for a HAMT, the [space_needed] method for the HAMT will
 * call the [space_needed] method on each noun key, and on each value, as well as computing the
 * size of the HAMT's own structures. Similarly, the [copy_to_buffer] method for the HAMT will call
 * the [copy_to_buffer] method for the keys and values as it copies its own structures in.
 */
pub trait Persist {
    /// Count how much space is needed, in bytes. May set marks so long as marks are cleaned up by
    /// [copy_into_buffer]
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize;

    /// Copy into the provided buffer, which may be assumed to be at least as large as the size
    /// returned by [space_needed] on the same structure.
    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, pma: &PMA, buffer: &mut *mut u8);

    /// Persist an object into the PMA using [space_needed] and [copy_to_buffer], returning
    /// a [u64] (probably a pointer or tagged pointer) that can be saved into metadata.
    unsafe fn save_to_pma(&mut self, stack: &mut NockStack, pma: &PMA) -> u64 {
        unsafe {
            let space = self.space_needed(stack, pma);

            if space == 0 {
                return self.handle_to_u64();
            }

            let space_as_pages = (space + (BT_PAGESIZE as usize - 1)) >> BT_PAGEBITS;

            let mut buffer = bt_malloc(pma.0, space_as_pages) as *mut u8;
            let orig_buffer = buffer;
            self.copy_to_buffer(stack, pma, &mut buffer);
            let space_isize: isize = space.try_into().unwrap();
            assert!(buffer.offset_from(orig_buffer) == space_isize);
            self.handle_to_u64()
        }
    }

    unsafe fn handle_to_u64(&self) -> u64;
    unsafe fn handle_from_u64(meta_handle: u64) -> Self;
}

/// Ensure an allocated noun is marked and return if it was already marked
unsafe fn mark(a: Allocated) -> bool {
    let metadata = a.get_metadata();
    a.set_metadata(metadata | NOUN_MARKED);
    metadata & NOUN_MARKED != 0
}

/// Unmark an allocated noun
unsafe fn unmark(a: Allocated) {
    let metadata = a.get_metadata();
    a.set_metadata(metadata & !NOUN_MARKED);
}

impl Persist for Atom {
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize {
        if let Ok(indirect) = self.as_indirect() {
            let count = indirect.raw_size();
            if !pma.contains(indirect.to_raw_pointer(), count) {
                if !mark(indirect.as_allocated()) {
                    return count * size_of::<u64>();
                }
            }
        }
        0
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, pma: &PMA, buffer: &mut *mut u8) {
        if let Ok(mut indirect) = self.as_indirect() {
            let count = indirect.raw_size();
            if !pma.contains(indirect.to_raw_pointer(), count) {
                if let Some(forward) = indirect.forwarding_pointer() {
                    *self = forward.as_atom();
                } else {
                    let indirect_buffer_ptr = *buffer as *mut u64;
                    copy_nonoverlapping(indirect.to_raw_pointer(), indirect_buffer_ptr, count);
                    *buffer = indirect_buffer_ptr.add(count) as *mut u8;

                    indirect.set_forwarding_pointer(indirect_buffer_ptr);

                    *self = IndirectAtom::from_raw_pointer(indirect_buffer_ptr).as_atom();
                }
            }
        }
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.as_noun().as_raw()
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Atom::from_raw(meta_handle)
    }
}

impl Persist for Noun {
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize {
        let mut space = 0usize;
        stack.frame_push(0);
        *(stack.push::<Noun>()) = *self;
        loop {
            if stack.stack_is_empty() {
                break;
            }
            let noun = *(stack.top::<Noun>());
            stack.pop::<Noun>();

            match noun.as_either_atom_cell() {
                Left(mut atom) => {
                    space += atom.space_needed(stack, pma);
                }
                Right(cell) => {
                    if !pma.contains(cell.to_raw_pointer(), 1) {
                        if !mark(cell.as_allocated()) {
                            space += size_of::<CellMemory>();
                            (*stack.push::<Noun>()) = cell.tail();
                            (*stack.push::<Noun>()) = cell.head();
                        }
                    }
                }
            }
        }
        stack.frame_pop();
        space
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, pma: &PMA, buffer: &mut *mut u8) {
        let mut buffer_u64 = (*buffer) as *mut u64;
        stack.frame_push(0);
        *(stack.push::<*mut Noun>()) = (self as *mut Noun);

        loop {
            if stack.stack_is_empty() {
                break;
            }

            let dest = *(stack.top::<*mut Noun>());
            stack.pop::<*mut Noun>();

            match (*dest).as_either_direct_allocated() {
                Left(direct) => {}
                Right(allocated) => {
                    if let Some(a) = allocated.forwarding_pointer() {
                        *dest = a.as_noun();
                        continue;
                    }

                    match allocated.as_either() {
                        Left(mut indirect) => {
                            let count = indirect.raw_size();
                            if pma.contains(indirect.to_raw_pointer(), count) {
                                continue;
                            }

                            unmark(allocated);
                            copy_nonoverlapping(indirect.to_raw_pointer(), buffer_u64, count);
                            indirect.set_forwarding_pointer(buffer_u64);
                            *dest = IndirectAtom::from_raw_pointer(buffer_u64).as_noun();
                            buffer_u64 = buffer_u64.add(count);
                        }
                        Right(mut cell) => {
                            if pma.contains(cell.to_raw_pointer(), 1) {
                                continue;
                            }

                            unmark(allocated);

                            let new_cell_mem = buffer_u64 as *mut CellMemory;
                            copy_nonoverlapping(cell.to_raw_pointer(), new_cell_mem, 1);
                            cell.set_forwarding_pointer(new_cell_mem);

                            *dest = Cell::from_raw_pointer(new_cell_mem).as_noun();

                            *(stack.push::<*mut Noun>()) = &mut (*new_cell_mem).tail;
                            *(stack.push::<*mut Noun>()) = &mut (*new_cell_mem).head;

                            buffer_u64 = new_cell_mem.add(1) as *mut u64;
                        }
                    }
                }
            }
        }
        *buffer = buffer_u64 as *mut u8;
        stack.frame_pop();
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.as_raw()
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Noun::from_raw(meta_handle)
    }
}
