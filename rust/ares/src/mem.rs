use crate::assert_acyclic;
use crate::noun::{Atom, Cell, CellMemory, IndirectAtom, Noun, NounAllocator};
use crate::snapshot::pma::{pma_in_arena, pma_malloc};
use either::Either::{self, Left, Right};
use ibig::Stack;
use libc::{c_void, memcmp};
use memmap::MmapMut;
use std::alloc::Layout;
use std::mem;
use std::ptr;
use std::ptr::copy_nonoverlapping;

crate::gdb!();

/** Number of reserved slots for alloc_pointer and frame_pointer in each frame */
pub const RESERVED: usize = 2;

/** Word offsets for alloc and frame pointers  */
pub const FRAME: usize = 0;
pub const ALLOC: usize = 1;

/**  Utility function to get size in words */
pub const fn word_size_of<T>() -> usize {
    (mem::size_of::<T>() + 7) >> 3
}

/** Utility function to compute the raw memory usage of an IndirectAtom */
fn indirect_raw_size(atom: IndirectAtom) -> usize {
    debug_assert!(atom.size() > 0);
    atom.size() + 2
}

/** A stack for Nock computation, which supports stack allocation and delimited copying collection
 * for returned nouns
 */
#[allow(dead_code)] // We need the memory field to keep our memory from being unmapped
#[derive(Debug)]
pub struct NockStack {
    /** The base pointer */
    start: *const u64,
    /** The size of the memory region */
    size: usize,
    /** Base pointer for the current stack frame. Accesses to slots are computed from this base. */
    frame_pointer: *mut u64,
    /** Alloc pointer for the current stack frame. */
    alloc_pointer: *mut u64,
    /** MMap which must be kept alive as long as this NockStack is */
    memory: MmapMut,
}

impl NockStack {
    /**  Initialization
     * The initial frame is a west frame. When the stack is initialized, a number of slots is given.
     * We add two extra slots to store the “previous” frame pointer and allocation pointer. For the
     * initial frame, the previous allocation pointer is set to the beginning (low boundary) of the
     * arena, and the previous frame pointer is set to NULL. */

    /** Size is in 64 bit words.
     * top_slots is how many slots to allocate to the top stack frame.
     */
    pub fn new(size: usize, top_slots: usize) -> NockStack {
        let mut memory = MmapMut::map_anon(size << 3).expect("Mapping memory for nockstack failed");
        let start = memory.as_ptr() as *const u64;
        // Here, frame_pointer < alloc_pointer, so the initial frame is West
        let frame_pointer = memory.as_mut_ptr() as *mut u64;
        let alloc_pointer = unsafe { frame_pointer.add(size) };
        unsafe {
            *frame_pointer = ptr::null::<u64>() as u64;  // "frame pointer" from "previous" frame
            *frame_pointer.add(ALLOC) = start as u64;    // "alloc pointer" from "previous" frame
        };
        NockStack {
            start,
            size,
            frame_pointer,
            alloc_pointer,
            memory,
        }
    }

    /** Checks if the current stack frame has West polarity */
    #[inline]
    fn is_west(&self) -> bool {
        if self.frame_pointer < self.alloc_pointer {
            true
        } else if self.frame_pointer > self.alloc_pointer {
            false
        } else {
            panic!("frame_pointer == alloc_pointer");
        }
    }

    /** Checks if the current stack frame has East polarity */
    #[inline]
    fn is_east(&self) -> bool {
        if self.frame_pointer > self.alloc_pointer {
            true
        } else if self.frame_pointer < self.alloc_pointer {
            false
        } else {
            panic!("frame_pointer == alloc_pointer");
        }
    }

    /** Size **in 64-bit words** of this NockStack */
    pub fn size(&self) -> usize {
        self.size
    }

    //TODO what makes something "in frame" with a split frame? here we're
    //taking in a pointer. i think something is "in frame" if the pointer
    // points to the allocation arena for this frame. so i need to check
    // that the pointer is between the current allocation pointer and
    // the previous stack pointer.
    //
    // on the other hand, not every address between the alloc_pointer and
    // previous frame_pointer is a valid address for an allocated object.
    // i believe the FP-relative slots store the addresses of the allocated
    // objects for that frame. but they could also point to more senior
    // memory outside of that frame. so maybe i first check that the pointer
    // is in range, then check to see if that is a legitimate pointer or
    // if its pointing to the middle of an allocated object
    #[inline]
    pub unsafe fn in_frame<T>(&self, ptr: *const T) -> bool {
        let ptr_u64 = ptr as *const u64;
        match self.is_west() {
            true  => ptr_u64 >= self.alloc_pointer
                && ptr_u64 <= *self.prev_frame_pointer_pointer_west()
                && todo!(), //TODO check that this isnt pointing to the middle of an allocated object
            false => ptr_u64 <= self.alloc_pointer
                && ptr_u64 >= *self.prev_frame_pointer_pointer_east()
                && todo!(), //TODO check that this isnt pointing to the middle of an allocated object
        }
    }

    /** Mutable pointer to a slot in a stack frame: east stack */
    unsafe fn slot_pointer_east(&mut self, slot: usize) -> *mut u64 {
        self.frame_pointer.sub(slot + 1)
    }

    /** Mutable pointer to a slot in a stack frame: west stack */
    unsafe fn slot_pointer_west(&mut self, slot: usize) -> *mut u64 {
        self.frame_pointer.add(slot)
    }

    /** Mutable pointer to a slot in a stack frame */
    unsafe fn slot_pointer(&mut self, slot: usize) -> *mut u64 {
        match self.is_west() {
            true  => self.slot_pointer_west(slot),
            false => self.slot_pointer_east(slot),
        }
    }

    /** Pointer to a local slot typed as Noun */
    pub unsafe fn local_noun_pointer(&mut self, local: usize) -> *mut Noun {
        self.slot_pointer(local + RESERVED) as *mut Noun
    }

    //TODO the following functions (save_prev_alloc_pointer_to_local_east through
    // prev_alloc_pointer_equals_local) have the same semantics as the old
    // stack_pointer version, but I'm holding off on seeing what they need to look like
    // until I get to a function that uses them.

    /** Save the alloc pointer for the previous frame in a slot of an east frame */
    unsafe fn save_prev_alloc_pointer_to_local_east(&mut self, local: usize) {
        *(self.slot_pointer_east(local + RESERVED) as *mut *mut u64) =
            *(self.prev_alloc_pointer_pointer_east())
    }

    /** Save the alloc pointer for the previous frame in a slot of a west frame */
    unsafe fn save_prev_alloc_pointer_to_local_west(&mut self, local: usize) {
        *(self.slot_pointer_west(local + RESERVED) as *mut *mut u64) =
            *(self.prev_alloc_pointer_pointer_west())
    }

    /** Save the alloc pointer for the previous frame in a slot */
    pub unsafe fn save_prev_alloc_pointer_to_local(&mut self, local: usize) {
        match self.is_west() {
            true  => self.save_prev_alloc_pointer_to_local_west(local),
            false => self.save_prev_alloc_pointer_to_local_east(local),
        }
    }

    unsafe fn restore_prev_alloc_pointer_from_local_east(&mut self, local: usize) {
        *(self.prev_alloc_pointer_pointer_east()) =
            *(self.slot_pointer_east(local + RESERVED) as *mut *mut u64);
    }

    unsafe fn restore_prev_alloc_pointer_from_local_west(&mut self, local: usize) {
        *(self.prev_alloc_pointer_pointer_west()) =
            *(self.slot_pointer_west(local + RESERVED) as *mut *mut u64);
    }

    unsafe fn prev_alloc_pointer_equals_local_east(&mut self, local: usize) -> bool {
        *(self.slot_pointer_east(local + RESERVED) as *const *mut u64)
            == *(self.prev_alloc_pointer_pointer_east())
    }

    unsafe fn prev_alloc_pointer_equals_local_west(&mut self, local: usize) -> bool {
        *(self.slot_pointer_west(local + RESERVED) as *const *mut u64)
            == *(self.prev_alloc_pointer_pointer_west())
    }

    /** Test the alloc pointer for the previous frame against a slot */
    pub unsafe fn prev_alloc_pointer_equals_local(&mut self, local: usize) -> bool {
        match self.is_west() {
            true  => self.prev_alloc_pointer_equals_local_west(local),
            false => self.prev_alloc_pointer_equals_local_east(local),
        }
    }

    /**  Allocation
     * In a west frame, the allocation pointer is higher than the frame pointer, and so the allocation
     * size is subtracted from the allocation pointer, and then the allocation pointer is returned as
     * the pointer to the newly allocated memory.
     *
     * In an east frame, the allocation pointer is lower than the frame pointer, and so the allocation
     * pointer is saved in a temporary, then the allocation size added to it, and finally the original
     * allocation pointer is returned as the pointer to the newly allocated memory. */

    unsafe fn struct_alloc_in_prev_frame_east<T>(&mut self, count: usize) -> *mut T {
        todo!()
    }

    unsafe fn struct_alloc_in_prev_frame_west<T>(&mut self, count: usize) -> *mut T {
        todo!()
    }

    pub unsafe fn struct_alloc_in_prev_frame<T>(&mut self, count: usize) -> *mut T {
        match self.is_west() {
            true  => self.struct_alloc_in_prev_frame_west(count),
            false => self.struct_alloc_in_prev_frame_east(count),
        }
    }

    /** Pointer to where the previous (west) alloc pointer is saved in an east frame */
    unsafe fn prev_alloc_pointer_pointer_east(&mut self) -> *mut *mut u64 {
        self.slot_pointer_east(ALLOC) as *mut *mut u64
    }

    /** Pointer to where the previous (east) alloc pointer is saved in an west frame */
    unsafe fn prev_alloc_pointer_pointer_west(&mut self) -> *mut *mut u64 {
        self.slot_pointer_west(ALLOC) as *mut *mut u64
    }

    /** Pointer to where the previous (west) frame pointer is saved in an east frame */
    unsafe fn prev_frame_pointer_pointer_east(&mut self) -> *mut *mut u64 {
        self.slot_pointer_east(FRAME) as *mut *mut u64
    }

    /** Pointer to where the previous (east) frame pointer is saved in an west frame */
    unsafe fn prev_frame_pointer_pointer_west(&mut self) -> *mut *mut u64 {
        self.slot_pointer_west(FRAME) as *mut *mut u64
    }

    unsafe fn copy_east(&mut self, noun: &mut Noun) {
        todo!()
    }

    unsafe fn copy_west(&mut self, noun: &mut Noun) {
        todo!()
    }

    unsafe fn pop_east(&mut self) {
        todo!()
    }

    unsafe fn pop_west(&mut self) {
        todo!()
    }

    pub unsafe fn pop(&mut self) {
        match self.is_west() {
            true  => self.pop_west(),
            false => self.pop_east(),
        }
    }

    pub unsafe fn unifying_equality(stack: &mut NockStack, a: *mut Noun, b: *mut Noun) -> bool {
        todo!()
    }
}

/// Immutable, acyclic objects which may be copied up the stack
pub trait Preserve {
    /// Ensure an object will not be invalidated by popping the NockStack
    unsafe fn preserve(&mut self, stack: &mut NockStack);
}

impl Preserve for IndirectAtom {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        todo!()
    }
}

impl Preserve for Atom {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        todo!()
    }
}
