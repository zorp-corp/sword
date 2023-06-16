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
        if self.is_west() {
            ptr_u64 >= self.alloc_pointer
                && ptr_u64 <= *self.prev_frame_pointer_pointer_west()
                && todo!() //TODO check that this isnt pointing to the middle of an allocated object
        } else {
            ptr_u64 <= self.alloc_pointer
                && ptr_u64 >= *self.prev_frame_pointer_pointer_east()
                && todo!() //TODO check that this isnt pointing to the middle of an allocated object
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
        if self.is_west() {
            self.slot_pointer_west(slot)
        } else {
            self.slot_pointer_east(slot)
        }
    }

    /** Pointer to a local slot typed as Noun */
    pub unsafe fn local_noun_pointer(&mut self, local: usize) -> *mut Noun {
        self.slot_pointer(local + RESERVED) as *mut Noun
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
        if self.is_west() {
            self.struct_alloc_in_prev_frame_west(count)
        } else {
            self.struct_alloc_in_prev_frame_east(count)
        }
    }

    /** Bump the alloc pointer for a west frame to make space for an allocation */
    unsafe fn raw_alloc_west(&mut self, words: usize) -> *mut u64 {
        self.alloc_pointer = self.alloc_pointer.sub(words);
        self.alloc_pointer
    }

    /** Bump the alloc pointer for an east frame to make space for an allocation */
    unsafe fn raw_alloc_east(&mut self, words: usize) -> *mut u64 {
        let alloc = self.alloc_pointer;
        self.alloc_pointer = self.alloc_pointer.add(words);
        alloc
    }

    /** Allocate space for an indirect pointer in a west frame */
    unsafe fn indirect_alloc_west(&mut self, words: usize) -> *mut u64 {
        //TODO I believe the `+ 2` here is for the pointer and the size? not the
        // same `+ 2` used by RESERVED
        self.raw_alloc_west(words + 2)
    }

    /** Allocate space for an indirect pointer in an east frame */
    unsafe fn indirect_alloc_east(&mut self, words: usize) -> *mut u64 {
        //TODO I believe the `+ 2` here is for the pointer and the size? not the
        // same `+ 2` used by RESERVED
        self.raw_alloc_east(words + 2)
    }

    /** Allocate space for an indirect pointer in a stack frame */
    unsafe fn indirect_alloc(&mut self, words: usize) -> *mut u64 {
        if self.is_west() {
            self.indirect_alloc_west(words)
        } else {
            self.indirect_alloc_east(words)
        }
    }

    /** Allocate space for a struct in a west frame */
    unsafe fn struct_alloc_west<T>(&mut self, count: usize) -> *mut T {
        self.raw_alloc_west(word_size_of::<T>() * count) as *mut T
    }

    /** Allocate space for a struct in an east frame */
    unsafe fn struct_alloc_east<T>(&mut self, count: usize) -> *mut T {
        self.raw_alloc_east(word_size_of::<T>() * count) as *mut T
    }

    /** Allocate space for a struct in a stack frame */
    pub unsafe fn struct_alloc<T>(&mut self, count: usize) -> *mut T {
        if self.is_west() {
            self.struct_alloc_west::<T>(count)
        } else {
            self.struct_alloc_east::<T>(count)
        }
    }

    /** Allocate space for an alloc::Layout in a stack frame */
    unsafe fn layout_alloc(&mut self, layout: Layout) -> *mut u64 {
        todo!()
    }

    /**  Copying and Popping
     * Prior to any copying step, the saved frame and allocation pointers must
     * be moved out of the frame. A two-word allocation is made to hold the saved
     * frame and allocation pointers. After this they will be accessed by reference
     * to the allocation pointer, so no more allocations must be made between now
     * and restoration.
     *
     * Copying can then proceed by updating the saved allocation pointer for each
     * copied object. This will almost immediately clobber the frame, so return by
     * writing to a slot in the previous frame or in a register is necessary.
     *
     * Finally, the frame and allocation pointers are restored from the saved
     * location.
     */

    unsafe fn copy_east(&mut self, noun: &mut Noun) {
        todo!()
    }

    unsafe fn copy_west(&mut self, noun: &mut Noun) {
        todo!()
    }

    pub unsafe fn copy_pma(&mut self, noun: &mut Noun) {
        todo!()
    }

    unsafe fn pop_east(&mut self) {
        todo!()
    }

    unsafe fn pop_west(&mut self) {
        todo!()
    }

    pub unsafe fn pop(&mut self) {
        if self.is_west() {
            self.pop_west()
        } else {
            self.pop_east()
        }
    }

    pub unsafe fn preserve<T: Preserve>(&mut self, x: &mut T) {
        x.preserve(self)
    }

    /**   Pushing
     * When pushing, we swap the allocation and frame pointers, then add (when pushing a west frame
     * onto an east frame) or subtract (when pushing an east frame onto a west frame) the appropriate
     * number of slots (plus two for previous pointers) to the frame pointer. We then store the
     * original pointers into the appropriate slots in the frame
     */

    /** Push a frame onto the east stack with 0 or more local variable slots.
     *
     * (The method is `push_west` because the naming convention refers to the beginning state of the
     * stack, not the final state.)
     */
    unsafe fn push_west(&mut self, num_locals: usize) {
        let current_frame_pointer = self.frame_pointer;
        let current_alloc_pointer = self.alloc_pointer;
        self.alloc_pointer = current_frame_pointer;
        self.frame_pointer = current_alloc_pointer.sub(num_locals + RESERVED);
        //TODO should I use slot_pointer() here? I think doing so might be confusing since this
        // is happening in the middle of pushing a new frame.
        *(self.frame_pointer.sub(1)) = current_frame_pointer as u64;
        *(self.frame_pointer.sub(2)) = current_alloc_pointer as u64;
    }

    /** Push a frame onto the west stack with 0 or more local variable slots.
     *
     * (The method is `push_east` because the naming convention refers to the beginning state of
     * the stack, not the final state.)
     */
    unsafe fn push_east(&mut self, num_locals: usize) {
        let current_frame_pointer = self.frame_pointer;
        let current_alloc_pointer = self.alloc_pointer;
        self.alloc_pointer = current_frame_pointer;
        self.frame_pointer = current_alloc_pointer.add(num_locals + RESERVED);
        //TODO should I use slot_pointer() here? I think doing so might be confusing since this
        // is happening in the middle of pushing a new frame
        *self.frame_pointer = current_frame_pointer as u64;
        *(self.frame_pointer.add(1)) = current_alloc_pointer as u64;
    }

    /** Push a frame onto the stack with 0 or more local variable slots. */
    pub unsafe fn push(&mut self, num_locals: usize) {
        if self.is_west() {
            self.push_west(num_locals)
        } else {
            self.push_east(num_locals)
        }
    }
}

pub unsafe fn unifying_equality(stack: &mut NockStack, a: *mut Noun, b: *mut Noun) -> bool {
    todo!()
}

unsafe fn senior_pointer_first<T>(
    stack: &NockStack,
    a: *const T,
    b: *const T
) -> (*const T, *const T) {
    todo!()
}

impl NounAllocator for NockStack {
    unsafe fn alloc_indirect(&mut self, words: usize) -> *mut u64 {
        self.indirect_alloc(words)
    }

    unsafe fn alloc_cell(&mut self) -> *mut CellMemory {
        self.struct_alloc::<CellMemory>(1)
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
        match self.as_either() {
            Left(_direct) => {}
            Right(mut indirect) => {
                indirect.preserve(stack);
                *self = indirect.as_atom();
            }
        }
    }
}

impl Preserve for Noun {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if stack.is_west() {
            stack.copy_west(self);
        } else {
            stack.copy_east(self);
        }
    }
}

impl Stack for NockStack {
    unsafe fn alloc_layout(&mut self, layout: Layout) -> *mut u64 {
        todo!()
    }
}
