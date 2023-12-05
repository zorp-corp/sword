use crate::jets::cold::Cold;
use crate::mem::NockStack;
use crate::noun::{Allocated, CellMemory, Noun};
use ares_pma::*;
use either::Either::{Left, Right};
use std::ffi::{c_void, CString};
use std::mem::size_of;
use std::path::PathBuf;

const PMA_MODE: mode_t = 0o600; // RW for user only
const PMA_FLAGS: ULONG = 0; // ignored for now

const PMA_CURRENT_SNAPSHOT_VERSION: u64 = 1;

/// Handle to a PMA
pub struct PMA(*mut BT_state);

pub struct Snapshot(pub *mut SnapshotMem);

#[repr(C)]
#[repr(packed)]
pub struct SnapshotMem {
    pub epoch: u64,
    pub event_num: u64,
    pub arvo: Noun,
    pub cold: Cold,
}

#[repr(usize)]
enum BTMetaField {
    SnapshotVersion = 0,
    Snapshot = 1,
}

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
        todo!()
    }

    #[inline]
    fn meta_get(&self, field: BTMetaField) -> u64 {
        unsafe { bt_meta_get(self.0, field as usize) }
    }

    #[inline]
    fn meta_set(&self, field: BTMetaField, val: u64) {
        unsafe { bt_meta_set(self.0, field as usize, val) };
    }

    pub unsafe fn contains<T>(&self, ptr: *const T, count: usize) -> bool {
        bt_inbounds(self.0, ptr as *mut c_void) != 0
            && bt_inbounds(self.0, ptr.add(count) as *mut c_void) != 0
    }

    pub fn load(&self) -> Snapshot {
        let snapshot_version = self.meta_get(BTMetaField::SnapshotVersion);

        match snapshot_version {
            1 => Snapshot(self.meta_get(BTMetaField::Snapshot) as *mut SnapshotMem),
            _ => panic!("Unsupported snapshot version"),
        }
    }

    pub fn save(&self, stack: &mut NockStack, snapshot: &mut Snapshot) {
        self.meta_set(BTMetaField::SnapshotVersion, PMA_CURRENT_SNAPSHOT_VERSION);
        self.meta_set(BTMetaField::Snapshot, snapshot.save_to_pma(stack, self));
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

pub trait Persist {
    /// Count how much space is needed, in bytes. May set marks so long as marks are cleaned up by
    /// [copy_into_buffer]
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize;

    /// Copy into the provided buffer, which may be assumed to be at least as large as the size
    /// returned by [space_needed] on the same structure. Return a u64 handle that could be saved
    /// in metadata
    unsafe fn copy_to_buffer(
        &mut self,
        stack: &mut NockStack,
        pma: &PMA,
        buffer: *mut u8,
    ) -> (u64, *mut u8);

    /// Persist an object into the PMA using [space_needed] and [copy_to_buffer], returning
    /// a [u64] (probably a pointer or tagged pointer) that can be saved into
    fn save_to_pma(&mut self, stack: &mut NockStack, pma: &PMA) -> u64 {
        unsafe {
            let space_as_pages =
                self.space_needed(stack, pma) + (BT_PAGESIZE as usize - 1) >> BT_PAGEBITS;
            let buffer = bt_malloc(pma.0, space_as_pages) as *mut u8;
            self.copy_to_buffer(stack, pma, buffer).0
        }
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self;
}

impl Persist for Snapshot {
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize {
        let mut arvo = (*(self.0)).arvo;
        let mut cold = (*(self.0)).cold;
        let arvo_space_needed = arvo.space_needed(stack, pma);
        let cold_space_needed = cold.space_needed(stack, pma);
        (((size_of::<SnapshotMem>() + 7) >> 3) << 3) + arvo_space_needed + cold_space_needed
    }

    unsafe fn copy_to_buffer(
        &mut self,
        stack: &mut NockStack,
        pma: &PMA,
        buffer: *mut u8,
    ) -> (u64, *mut u8) {
        let snapshot_buffer = buffer as *mut SnapshotMem;
        let arvo_buffer = buffer.add(((size_of::<SnapshotMem>() + 7) >> 3) << 3);
        std::ptr::copy_nonoverlapping(self.0, snapshot_buffer, 1);
        *self = Snapshot(snapshot_buffer);

        let mut arvo = (*snapshot_buffer).arvo;
        let (_, cold_buffer) = arvo.copy_to_buffer(stack, pma, arvo_buffer);
        (*snapshot_buffer).arvo = arvo;

        let mut cold = (*snapshot_buffer).cold;
        let (_, rest_buffer) = cold.copy_to_buffer(stack, pma, cold_buffer);
        (*snapshot_buffer).cold = cold;

        (snapshot_buffer as u64, rest_buffer)
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Snapshot(meta_handle as *mut SnapshotMem)
    }
}

/// Ensure an allocated noun is marked and return if it was already marked
fn mark(a: Allocated) -> bool {
    todo!()
}

/// Unmark an allocated noun
fn unmark_noun(a: Allocated) -> bool {
    todo!()
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

            if let Ok(allocated) = noun.as_allocated() {
                // not counting direct atoms, they go in
                match allocated.as_either() {
                    Left(indirect) => {
                        let count = indirect.raw_size();
                        if !pma.contains(indirect.to_raw_pointer(), count) {
                            if !mark(allocated) {
                                space += count * size_of::<u64>();
                            }
                        }
                    }
                    Right(cell) => {
                        if !pma.contains(cell.to_raw_pointer(), 1) {
                            if !mark(allocated) {
                                space += size_of::<CellMemory>();
                            }
                        }
                    }
                }
            }
        }
        stack.frame_pop();
        space
    }

    unsafe fn copy_to_buffer(
        &mut self,
        stack: &mut NockStack,
        pma: &PMA,
        buffer: *mut u8,
    ) -> (u64, *mut u8) {
        todo!()
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Noun::from_raw(meta_handle)
    }
}
