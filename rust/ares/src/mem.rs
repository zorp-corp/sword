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
pub const RESERVED: usize = 3;

/** Word offsets for alloc and frame pointers  */
pub const FRAME: usize = 0;
pub const STACK: usize = 1;
pub const ALLOC: usize = 2;

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
pub struct NockStack {
    /** The base pointer */
    start: *const u64,
    /** The size of the memory region */
    size: usize,
    /** Base pointer for the current stack frame. Accesses to slots are computed from this base. */
    frame_pointer: *mut u64,
    /** Stack pointer for the current stack frame. */
    stack_pointer: *mut u64,
    /** Alloc pointer for the current stack frame. */
    alloc_pointer: *mut u64,
    /** MMap which must be kept alive as long as this NockStack is */
    memory: MmapMut,
    /** Whether or not pre_copy() has been called on the current stack frame. */
    pc: bool,
}

impl NockStack {
    /**  Initialization
     * The initial frame is a west frame. When the stack is initialized, a number of slots is given.
     * We add three extra slots to store the “previous” frame, stack, and allocation pointer. For the
     * initial frame, the previous allocation pointer is set to the beginning (low boundary) of the
     * arena, the previous frame pointer is set to NULL, and the previous stack pointer is set to XX */

    /** Size is in 64 bit words.
     * top_slots is how many slots to allocate to the top stack frame.
     */
    pub fn new(size: usize, top_slots: usize) -> NockStack {
        //TODO memory doesn't have to be mut as written, but it was before split stack. not sure what
        // it was accomplishing.
        let mut memory = MmapMut::map_anon(size << 3).expect("Mapping memory for nockstack failed");
        let start = memory.as_ptr() as *const u64;
        // Here, frame_pointer < alloc_pointer, so the initial frame is West
        let frame_pointer = unsafe { start.add(top_slots + RESERVED) } as *mut u64;
        let stack_pointer = frame_pointer;
        let alloc_pointer = unsafe { start.add(size) } as *mut u64;
        unsafe {
            *frame_pointer = ptr::null::<u64>() as u64;            // "frame pointer" from "previous" frame
            *frame_pointer.sub(STACK) = ptr::null::<u64>() as u64; // "stack pointer" from "previous" frame TODO is this right?
            *frame_pointer.sub(ALLOC) = start as u64;              // "alloc pointer" from "previous" frame
        };
        NockStack {
            start,
            size,
            frame_pointer,
            stack_pointer,
            alloc_pointer,
            memory,
            pc: false,
        }
    }

    /** Checks if the current stack frame has West polarity */
    #[inline]
    pub fn is_west(&self) -> bool {
        //TODO the interpreter won't know when this invariant is violated unless
        // stack_pointer == alloc_pointer, for that we should implement a guard page
        if self.stack_pointer < self.alloc_pointer {
            true
        } else if self.stack_pointer > self.alloc_pointer {
            false
        } else {
            panic!("stack_pointer == alloc_pointer");
        }
    }

    /** Size **in 64-bit words** of this NockStack */
    pub fn size(&self) -> usize {
        self.size
    }

    /** Check to see if an allocation is in frame */
    #[inline]
    pub unsafe fn in_frame<T>(&self, ptr: *const T) -> bool {
        let ptr_u64 = ptr as *const u64;
        let prev = *self.prev_stack_pointer_pointer();
        if self.is_west() {
            ptr_u64 >= self.alloc_pointer
                && ptr_u64 < prev
        } else {
            ptr_u64 >= prev
                && ptr_u64 < self.alloc_pointer
        }
    }

    /** Mutable pointer to a slot in a stack frame: east stack */
    unsafe fn slot_pointer_east(&self, slot: usize) -> *mut u64 {
        self.frame_pointer.add(slot)
    }

    /** Mutable pointer to a slot in a stack frame: west stack */
    unsafe fn slot_pointer_west(&self, slot: usize) -> *mut u64 {
        self.frame_pointer.sub(slot + 1)
    }

    /** Mutable pointer to a slot in a stack frame */
    unsafe fn slot_pointer(&self, slot: usize) -> *mut u64 {
        if self.is_west() {
            self.slot_pointer_west(slot)
        } else {
            self.slot_pointer_east(slot)
        }
    }

    /** Mutable pointer into a slot in free space east of allocation pointer */
    unsafe fn free_slot_east(&self, slot: usize) -> *mut u64 {
        self.alloc_pointer.add(slot)
    }

    /** Mutable pointer into a slot in free space west of allocation pointer */
    unsafe fn free_slot_west(&self, slot: usize) -> *mut u64 {
        self.alloc_pointer.sub(slot + 1)
    }

    unsafe fn free_slot(&self, slot: usize) -> *mut u64 {
        if self.is_west() {
            self.free_slot_west(slot)
        } else {
            self.free_slot_east(slot)
        }
    }

    unsafe fn free_alloc_east<T>(&mut self, _count: usize) -> *mut T {
        self.alloc_pointer.add(RESERVED) as *mut T
    }

    unsafe fn free_alloc_west<T>(&mut self, count: usize) -> *mut T {
        self.alloc_pointer.sub(RESERVED + 1)
                          .sub(word_size_of::<T>() * count) as *mut T
    }

    /** This allocates space past the reserved slots stored for pointers
     * past the allocation arena, for the purpose of allocating after
     * pre_copy() has been called.
     */
    pub unsafe fn free_alloc<T>(&mut self, count: usize) -> *mut T {
        if self.is_west() {
            self.free_alloc_west(count)
        } else {
            self.free_alloc_east(count)
        }
    }

    /** Pointer to a local slot typed as Noun */
    pub unsafe fn local_noun_pointer(&mut self, local: usize) -> *mut Noun {
        self.slot_pointer(local + RESERVED) as *mut Noun
    }

    /** Pointer to where the previous stack pointer is saved in a frame */
    unsafe fn prev_stack_pointer_pointer(&self) -> *mut *mut u64 {
        if self.pc == false {
            self.slot_pointer(STACK) as *mut *mut u64
        } else {
            self.free_slot(STACK) as *mut *mut u64
        }
    }

    /** Pointer to where the previous stack pointer is saved in a frame */
    unsafe fn prev_alloc_pointer_pointer(&self) -> *mut *mut u64 {
        if self.pc == false {
            self.slot_pointer(ALLOC) as *mut *mut u64
        } else {
            self.free_slot(ALLOC) as *mut *mut u64
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

    /** Bump the alloc pointer for a west frame to make space for an allocation */
    unsafe fn raw_alloc_west(&mut self, words: usize) -> *mut u64 {
        if self.pc == true {
            panic!("Attempted to allocate in copying mode");
        }
        self.alloc_pointer = self.alloc_pointer.sub(words);
        self.alloc_pointer
    }

    /** Bump the alloc pointer for an east frame to make space for an allocation */
    unsafe fn raw_alloc_east(&mut self, words: usize) -> *mut u64 {
        if self.pc == true {
            panic!("Attempted to allocate in copying mode");
        }
        let alloc = self.alloc_pointer;
        self.alloc_pointer = self.alloc_pointer.add(words);
        alloc
    }

    /** Allocate space for an indirect pointer in a west frame */
    unsafe fn indirect_alloc_west(&mut self, words: usize) -> *mut u64 {
        self.raw_alloc_west(words + 2)
    }

    /** Allocate space for an indirect pointer in an east frame */
    unsafe fn indirect_alloc_east(&mut self, words: usize) -> *mut u64 {
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

    unsafe fn struct_alloc_in_previous_frame_west<T>(&mut self, count: usize) -> *mut T {
        // This is only called when using preserve(), at which point pre_copy() has been
        // called, at which point we need to be looking for the saved pointers next
        // to from-space instead of next to frame_pointer
        if self.pc == false {
            panic!("Attempted to allocate in previous frame outside of copying mode");
        }
        // note that the allocation is on the east frame, and thus resembles raw_alloc_east
        let prev_alloc_pointer_pointer = self.prev_alloc_pointer_pointer();
        let alloc = *prev_alloc_pointer_pointer;
        *prev_alloc_pointer_pointer = (*prev_alloc_pointer_pointer).add(word_size_of::<T>() * count);
        alloc as *mut T
    }

    unsafe fn struct_alloc_in_previous_frame_east<T>(&mut self, count: usize) -> *mut T {
        // This is only called when using preserve(), at which point pre_copy() has been
        // called, at which point we need to be looking for the saved pointers next
        // to from-space instead of next to frame_pointer
        if self.pc == false {
            panic!("Attempted to allocate in previous frame outside of copying mode");
        }
        // note that the allocation is on the west frame, and thus resembles raw_alloc_west
        let prev_alloc_pointer_pointer = self.prev_alloc_pointer_pointer();
        *prev_alloc_pointer_pointer = *(prev_alloc_pointer_pointer).sub(word_size_of::<T>() * count);
        *prev_alloc_pointer_pointer as *mut T
    }

    pub unsafe fn struct_alloc_in_previous_frame<T>(&mut self, count: usize) -> *mut T {
        if self.is_west() {
            self.struct_alloc_in_previous_frame_west(count)
        } else {
            self.struct_alloc_in_previous_frame_east(count)
        }
    }

    /** Allocate space for an alloc::Layout in a stack frame */
    unsafe fn layout_alloc(&mut self, layout: Layout) -> *mut u64 {
        assert!(layout.align() <= 64, "layout alignment must be <= 64");
        if self.is_west() {
            self.raw_alloc_west((layout.size() + 7) >> 3)
        } else {
            self.raw_alloc_east((layout.size() + 7) >> 3)
        }
    }

    /**  Copying and Popping
     * Prior to any copying step, the saved frame, stack, and allocation pointers must
     * be moved out of the frame. A three-word allocation is made to hold the saved
     * frame, stack, and allocation pointers. After this they will be accessed by reference
     * to the allocation pointer, so no more allocations must be made between now
     * and restoration.
     *
     * Copying can then proceed by updating the saved allocation pointer for each
     * copied object. This will almost immediately clobber the frame, so return by
     * writing to a slot in the previous frame or in a register is necessary.
     *
     * Finally, the frame, stack, and allocation pointers are restored from the saved
     * location.
     */

    /** Copies reserved pointers to free space adjacent to the allocation arena. */
    pub unsafe fn pre_copy(&mut self) {
        if self.pc == false {
            *(self.free_slot(FRAME)) = *(self.slot_pointer(FRAME));
            *(self.free_slot(STACK)) = *(self.slot_pointer(STACK));
            *(self.free_slot(ALLOC)) = *(self.slot_pointer(ALLOC));

            // Change polarity of lightweight stack.
            self.pc = true;
            if self.is_west() {
                self.stack_pointer = self.alloc_pointer.sub(RESERVED + 1);
            } else {
                self.stack_pointer = self.alloc_pointer.add(RESERVED);
            }
        } else {
            eprintln!("pre_copy() called more than once on stack frame!");
        }
    }

    unsafe fn copy_east(&mut self, noun: &mut Noun) {
        let noun_ptr = noun as *mut Noun;
        // Location to which allocations are made
        let mut other_alloc_pointer = *(self.prev_alloc_pointer_pointer()) as *mut u64;
        // Add two slots to the lightweight stack
        // Set the first new slot to the noun to be copied
        *(self.stack_push::<Noun>()) = *noun;
        // Set the second new slot to a pointer to the noun being copied. this is the destination pointer, which will change
        *(self.stack_push::<*mut Noun>()) = noun_ptr;

        loop {
            if self.stack_is_empty() {
                break;
            }

            // Pop a noun to copy from the stack
            let next_dest = *(self.stack_top::<*mut Noun>());
            self.stack_pop::<*mut Noun>();
            let next_noun = *(self.stack_top::<Noun>());
            self.stack_pop::<Noun>();

            // If it's a direct atom, just write it to the destination.
            // Otherwise, we have allocations to make.
            match next_noun.as_either_direct_allocated() {
                Either::Left(_direct) => {
                    *next_dest = next_noun;
                }
                Either::Right(allocated) => {
                    // If it's an allocated noun with a forwarding pointer, just write the
                    // noun resulting from the forwarding pointer to the destination
                    //
                    // Otherwise, we have to allocate space for and copy the allocated noun
                    match allocated.forwarding_pointer() {
                        Option::Some(new_allocated) => {
                            *next_dest = new_allocated.as_noun();
                        }
                        Option::None => {
                            // Check to see if its allocated within this frame
                            if self.in_frame(allocated.to_raw_pointer())
                            {
                                match allocated.as_either() {
                                    Either::Left(mut indirect) => {
                                        // Make space for the atom
                                        other_alloc_pointer =
                                            other_alloc_pointer.sub(indirect_raw_size(indirect));
                                        let new_indirect_alloc = other_alloc_pointer;

                                        // Indirect atoms can be copied directly
                                        copy_nonoverlapping(
                                            indirect.to_raw_pointer(),
                                            new_indirect_alloc,
                                            indirect_raw_size(indirect),
                                        );

                                        // Set a forwarding pointer so we don't create duplicates from other
                                        // references
                                        indirect.set_forwarding_pointer(new_indirect_alloc);

                                        *next_dest =
                                            IndirectAtom::from_raw_pointer(new_indirect_alloc)
                                                .as_noun();
                                    }
                                    Either::Right(mut cell) => {
                                        // Make space for the cell
                                        other_alloc_pointer =
                                            other_alloc_pointer.sub(word_size_of::<CellMemory>());
                                        let new_cell_alloc = other_alloc_pointer as *mut CellMemory;

                                        // Copy the cell metadata
                                        (*new_cell_alloc).metadata =
                                            (*cell.to_raw_pointer()).metadata;

                                        // Push the tail and the head to the work stack
                                        *(self.stack_push::<Noun>()) = cell.tail();
                                        *(self.stack_push::<*mut Noun>()) = &mut (*new_cell_alloc).tail;
                                        *(self.stack_push::<Noun>()) = cell.head();
                                        *(self.stack_push::<*mut Noun>()) = &mut (*new_cell_alloc).head;

                                        // Set the forwarding pointer
                                        cell.set_forwarding_pointer(new_cell_alloc);

                                        *next_dest =
                                            Cell::from_raw_pointer(new_cell_alloc).as_noun();
                                    }
                                }
                            } else {
                                *next_dest = allocated.as_noun(); // Don't copy references outside the current frame
                            }
                        }
                    }
                }
            }
        }
        // Set saved previous allocation pointer its new value after this allocation
        *(self.prev_alloc_pointer_pointer()) = other_alloc_pointer;
        assert_acyclic!(*noun);
    }

    unsafe fn copy_west(&mut self, noun: &mut Noun) {
        let noun_ptr = noun as *mut Noun;
        // Location to which allocations are made
        let mut other_alloc_pointer = *(self.prev_alloc_pointer_pointer()) as *mut u64;
        // Add two slots to the lightweight stack
        // Set the first new slot to the noun to be copied
        *(self.stack_push::<Noun>()) = *noun;
        // Set the second new slot to a pointer to the noun being copied. this is the destination pointer, which will change
        *(self.stack_push::<*mut Noun>()) = noun_ptr;
        loop {
            if self.stack_is_empty() {
                break;
            }

            // Pop a noun to copy from the stack
            let next_dest = *(self.stack_top::<*mut Noun>());
            self.stack_pop::<*mut Noun>();
            let next_noun = *(self.stack_top::<Noun>());
            self.stack_pop::<Noun>();

            // If it's a direct atom, just write it to the destination.
            // Otherwise, we have allocations to make.
            match next_noun.as_either_direct_allocated() {
                Either::Left(_direct) => {
                    *next_dest = next_noun;
                }
                Either::Right(allocated) => {
                    // If it's an allocated noun with a forwarding pointer, just write the
                    // noun resulting from the forwarding pointer to the destination
                    //
                    // Otherwise, we have to allocate space for and copy the allocated noun
                    match allocated.forwarding_pointer() {
                        Option::Some(new_allocated) => {
                            *next_dest = new_allocated.as_noun();
                        }
                        Option::None => {
                            // Check to see if its allocated within this frame
                            if self.in_frame(allocated.to_raw_pointer())
                            {
                                match allocated.as_either() {
                                    Either::Left(mut indirect) => {
                                        // Make space for the atom
                                        let new_indirect_alloc = other_alloc_pointer;
                                        other_alloc_pointer =
                                            other_alloc_pointer.add(indirect_raw_size(indirect));

                                        // Indirect atoms can be copied directly
                                        copy_nonoverlapping(
                                            indirect.to_raw_pointer(),
                                            new_indirect_alloc,
                                            indirect_raw_size(indirect),
                                        );

                                        // Set a forwarding pointer so we don't create duplicates from other
                                        // references
                                        indirect.set_forwarding_pointer(new_indirect_alloc);

                                        *next_dest =
                                            IndirectAtom::from_raw_pointer(new_indirect_alloc)
                                                .as_noun();
                                    }
                                    Either::Right(mut cell) => {
                                        // Make space for the cell
                                        let new_cell_alloc = other_alloc_pointer as *mut CellMemory;
                                        other_alloc_pointer =
                                            other_alloc_pointer.add(word_size_of::<CellMemory>());

                                        // Copy the cell metadata
                                        (*new_cell_alloc).metadata =
                                            (*cell.to_raw_pointer()).metadata;

                                        // Push the tail and the head to the work stack
                                        *(self.stack_push::<Noun>()) = cell.tail();
                                        *(self.stack_push::<*mut Noun>()) = &mut (*new_cell_alloc).tail;
                                        *(self.stack_push::<Noun>()) = cell.head();
                                        *(self.stack_push::<*mut Noun>()) = &mut (*new_cell_alloc).head;

                                        // Set the forwarding pointer
                                        cell.set_forwarding_pointer(new_cell_alloc);

                                        *next_dest =
                                            Cell::from_raw_pointer(new_cell_alloc).as_noun();
                                    }
                                }
                            } else {
                                *next_dest = allocated.as_noun(); // Don't copy references outside the current frame
                            }
                        }
                    }
                }
            }
        }
        // Set saved previous allocation pointer its new value after this allocation
        *(self.prev_alloc_pointer_pointer()) = other_alloc_pointer;
        assert_acyclic!(*noun);
    }

    pub unsafe fn copy_pma(&mut self, noun: &mut Noun) {
        assert!(self.is_west());
        let noun_ptr = noun as *mut Noun;
        self.stack_pointer = self.alloc_pointer.sub(RESERVED + 1);
        *(self.stack_push::<Noun>()) = *noun;
        *(self.stack_push::<*mut Noun>()) = noun_ptr;
        loop {
            if self.stack_is_empty() {
                break;
            }

            let next_dest = *(self.stack_top::<*mut Noun>());
            self.stack_pop::<*mut Noun>();
            let next_noun = *(self.stack_top::<Noun>());
            self.stack_pop::<Noun>();

            match next_noun.as_either_direct_allocated() {
                Either::Left(_direct) => {
                    *next_dest = next_noun;
                },
                Either::Right(allocated) => {
                    match allocated.forwarding_pointer() {
                        Option::Some(new_allocated) => {
                            *next_dest = new_allocated.as_noun();
                        }
                        Option::None => {
                            if pma_in_arena(allocated.to_raw_pointer()) {
                                *next_dest = allocated.as_noun();
                            } else {
                                match allocated.as_either() {
                                    Either::Left(mut indirect) => {
                                        let new_indirect_alloc =
                                            pma_malloc(indirect_raw_size(indirect));

                                        copy_nonoverlapping(
                                            indirect.to_raw_pointer(),
                                            new_indirect_alloc,
                                            indirect_raw_size(indirect),
                                        );

                                        indirect.set_forwarding_pointer(new_indirect_alloc);

                                        *next_dest =
                                            IndirectAtom::from_raw_pointer(new_indirect_alloc)
                                                .as_noun();
                                    }
                                    Either::Right(mut cell) => {
                                        let new_cell_alloc: *mut CellMemory =
                                            pma_malloc(word_size_of::<CellMemory>());

                                        (*new_cell_alloc).metadata =
                                            (*cell.to_raw_pointer()).metadata;

                                        *(self.stack_push::<Noun>()) = cell.tail();
                                        *(self.stack_push::<*mut Noun>()) = &mut (*new_cell_alloc).tail;
                                        *(self.stack_push::<Noun>()) = cell.head();
                                        *(self.stack_push::<*mut Noun>()) = &mut (*new_cell_alloc).head;

                                        cell.set_forwarding_pointer(new_cell_alloc);

                                        *next_dest =
                                            Cell::from_raw_pointer(new_cell_alloc).as_noun();
                                    }
                                }
                            }
                        }
                    }
                },
            }
        }
        assert_acyclic!(*noun);
    }

    /** We restore from the free_slots instead of the FP-relative slots since they are destroyed by
     * copy_west/copy_east. We call free_slot instead of using the prev_xyz_pointer_pointer interface
     * since we're changing the pointers that that interface uses to determine which location to point
     * to.
     */
    unsafe fn pop_east(&mut self) {
        self.frame_pointer = *(self.free_slot_east(FRAME) as *const *mut u64);
        self.stack_pointer = *(self.free_slot_east(STACK) as *const *mut u64);
        self.alloc_pointer = *(self.free_slot_east(ALLOC) as *const *mut u64);
        self.pc = false;
    }

    unsafe fn pop_west(&mut self) {
        self.frame_pointer = *(self.free_slot_west(FRAME) as *const *mut u64);
        self.stack_pointer = *(self.free_slot_west(STACK) as *const *mut u64);
        self.alloc_pointer = *(self.free_slot_west(ALLOC) as *const *mut u64);
        self.pc = false;
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

    /**  Pushing
     *  When pushing, we swap the stack and alloc pointers, set the frame pointer to be the stack
     *  pointer, move both frame and stack pointer by number of locals (eastward for west frames,
     *  westward for east frame), and then save the old stack/frame/alloc pointers in slots
     *  adjacent to the frame pointer.
     */

    /** Push a frame onto the east stack with 0 or more local variable slots.
     *
     * (The method is `push_west` because the naming convention refers to the beginning state of the
     * stack, not the final state.)
     */

    unsafe fn push_west(&mut self, num_locals: usize) {
        let current_frame_pointer = self.frame_pointer;
        let current_stack_pointer = self.stack_pointer;
        let current_alloc_pointer = self.alloc_pointer;

        self.alloc_pointer = current_stack_pointer;
        self.frame_pointer = current_alloc_pointer.sub(num_locals + RESERVED);
        self.stack_pointer = self.frame_pointer;

        // At this point, stack_pointer > alloc_pointer so slot_pointer() will
        // call slot_pointer_east()
        *(self.slot_pointer(FRAME)) = current_frame_pointer as u64;
        *(self.slot_pointer(STACK)) = current_stack_pointer as u64;
        *(self.slot_pointer(ALLOC)) = current_alloc_pointer as u64;

        self.pc = false;
    }

    /** Push a frame onto the west stack with 0 or more local variable slots.
     *
     * (The method is `push_east` because the naming convention refers to the beginning state of
     * the stack, not the final state.)
     */
    unsafe fn push_east(&mut self, num_locals: usize) {
        let current_frame_pointer = self.frame_pointer;
        let current_stack_pointer = self.stack_pointer;
        let current_alloc_pointer = self.alloc_pointer;

        self.alloc_pointer = current_stack_pointer;
        self.frame_pointer = current_alloc_pointer.add(num_locals + RESERVED);
        self.stack_pointer = self.frame_pointer;

        // At this point, stack_pointer < alloc_pointer so slot_pointer() will
        // call slot_pointer_west()
        *(self.slot_pointer(FRAME)) = current_frame_pointer as u64;
        *(self.slot_pointer(STACK)) = current_stack_pointer as u64;
        *(self.slot_pointer(ALLOC)) = current_alloc_pointer as u64;

        self.pc = false;
    }

    /** Push a frame onto the stack with 0 or more local variable slots. */
    pub fn push(&mut self, num_locals: usize) {
        unsafe {
            if self.is_west() {
                self.push_west(num_locals)
            } else {
                self.push_east(num_locals)
            }
        }
    }

    /** Push onto a west-oriented lightweight stack, moving the stack_pointer.
     * */
    unsafe fn stack_push_west<T>(&mut self) -> *mut T {
        let alloc = self.stack_pointer;
        self.stack_pointer = self.stack_pointer.add(word_size_of::<T>());
        alloc as *mut T
    }

    /** Push onto an east-oriented ligthweight stack, moving the stack_pointer */
    unsafe fn stack_push_east<T>(&mut self) -> *mut T {
        self.stack_pointer = self.stack_pointer.sub(word_size_of::<T>());
        self.stack_pointer as *mut T
    }

    /** Push onto the lightweight stack, moving the stack_pointer. Note that
     * this violates the _east/_west naming convention somewhat, since e.g.
     * a west frame when pc == false has a west-oriented lightweight stack,
     * but when pc == true it becomes east-oriented.*/
    pub unsafe fn stack_push<T>(&mut self) -> *mut T {
        if self.is_west() && self.pc == false ||
           !self.is_west() && self.pc == true {
            self.stack_push_west::<T>()
        } else {
            self.stack_push_east::<T>()
        }
    }

    /** Pop a west-oriented lightweight stack, moving the stack pointer. */
    unsafe fn stack_pop_west<T>(&mut self) {
        self.stack_pointer = self.stack_pointer.sub(word_size_of::<T>());
    }

    /** Pop an east-oriented lightweight stack, moving the stack pointer. */
    unsafe fn stack_pop_east<T>(&mut self) {
        self.stack_pointer = self.stack_pointer.add(word_size_of::<T>());
    }

    /** Pop the lightweight stack, moving the stack_pointer. Note that
     * this violates the _east/_west naming convention somewhat, since e.g.
     * a west frame when pc == false has a west-oriented lightweight stack,
     * but when pc == true it becomes east-oriented.*/
    pub unsafe fn stack_pop<T>(&mut self) {
        if self.is_west() && self.pc == false ||
           !self.is_west() && self.pc == true {
            self.stack_pop_west::<T>();
        } else {
            self.stack_pop_east::<T>();
        };
    }

    /** Peek the top of the lightweight stack. Note that
     * this violates the _east/_west naming convention somewhat, since e.g.
     * a west frame when pc == false has a west-oriented lightweight stack,
     * but when pc == true it becomes east-oriented.*/
    pub unsafe fn stack_top<T>(&mut self) -> *mut T {
        if self.is_west() && self.pc == false ||
           !self.is_west() && self.pc == true {
            self.stack_top_west()
        } else {
            self.stack_top_east()
        }
    }

    /** Peek the top of a west-oriented lightweight stack. */
    unsafe fn stack_top_west<T>(&mut self) -> *mut T {
        self.stack_pointer.sub(word_size_of::<T>()) as *mut T
    }

    /** Peek the top of an east-oriented lightweight stack. */
    unsafe fn stack_top_east<T>(&mut self) -> *mut T {
        self.stack_pointer as *mut T
    }

    /** Checks to see if the lightweight stack is empty. Note that this doesn't work
     * when the stack pointer has been moved to be close to the allocation arena, such
     * as in copy_west(). */
    pub fn stack_is_empty(&self) -> bool {
        if self.pc == false {
            self.stack_pointer == self.frame_pointer
        } else {
            if self.is_west() {
                unsafe { self.stack_pointer == self.alloc_pointer.sub(RESERVED + 1) }
            } else {
                unsafe { self.stack_pointer == self.alloc_pointer.add(RESERVED) }
            }
        }
    }
}

pub unsafe fn unifying_equality(stack: &mut NockStack, a: *mut Noun, b: *mut Noun) -> bool {
    /* This version of unifying equality is not like that of vere.
     * Vere does a tree comparison (accelerated by pointer equality and short-circuited by mug
     * equality) and then unifies the nouns at the top level if they are equal.
     *
     * Here we recursively attempt to unify nouns. Pointer-equal nouns are already unified.
     * Disequal mugs again short-circuit the unification and equality check.
     *
     * Since we expect atoms to be normalized, direct and indirect atoms do not unify with each
     * other. For direct atoms, no unification is possible as there is no pointer involved in their
     * representation. Equality is simply direct equality on the word representation. Indirect
     * atoms require equality first of the size and then of the memory buffers' contents.
     *
     * Cell equality is tested (after mug and pointer equality) by attempting to unify the heads and tails,
     * respectively, of cells, and then re-testing. If unification succeeds then the heads and
     * tails will be pointer-wise equal and the cell itself can be unified. A failed unification of
     * the head or the tail will already short-circuit the unification/equality test, so we will
     * not return to re-test the pointer equality.
     *
     * When actually mutating references for unification, we must be careful to respect seniority.
     * A reference to a more junior noun should always be replaced with a reference to a more
     * senior noun, *never vice versa*, to avoid introducing references from more senior frames
     * into more junior frames, which would result in incorrect operation of the copier.
     */
    // If the nouns are already word-equal we have nothing to do
    if (*a).raw_equals(*b) {
        return true;
    };
    // If the nouns have cached mugs which are disequal we have nothing to do
    if let (Ok(a_alloc), Ok(b_alloc)) = ((*a).as_allocated(), (*b).as_allocated()) {
        if let (Some(a_mug), Some(b_mug)) = (a_alloc.get_cached_mug(), b_alloc.get_cached_mug()) {
            if a_mug != b_mug {
                return false;
            };
        };
    };
    stack.push(0);
    *(stack.stack_push::<(*mut Noun, *mut Noun)>()) = (a, b);
    loop {
        if stack.stack_is_empty() {
            break;
        };
        let (x, y): (*mut Noun, *mut Noun) = *(stack.stack_top());
        if (*x).raw_equals(*y) {
            stack.stack_pop::<(*mut Noun, *mut Noun)>();
            continue;
        };
        if let (Ok(x_alloc), Ok(y_alloc)) = (
            // equal direct atoms return true for raw_equals()
            (*x).as_allocated(),
            (*y).as_allocated(),
        ) {
            if let (Some(x_mug), Some(y_mug)) = (x_alloc.get_cached_mug(), y_alloc.get_cached_mug())
            {
                if x_mug != y_mug {
                    break; // short-circuit, the mugs differ therefore the nouns must differ
                }
            };
            match (x_alloc.as_either(), y_alloc.as_either()) {
                (Left(x_indirect), Left(y_indirect)) => {
                    let x_as_ptr = x_indirect.to_raw_pointer();
                    let y_as_ptr = y_indirect.to_raw_pointer();
                    if x_indirect.size() == y_indirect.size()
                        && memcmp(
                            x_indirect.data_pointer() as *const c_void,
                            y_indirect.data_pointer() as *const c_void,
                            x_indirect.size() << 3,
                        ) == 0
                    {
                        let (_senior, junior) = senior_pointer_first(stack, x_as_ptr, y_as_ptr);
                        // unify
                        if x_as_ptr == junior {
                            *x = *y;
                        } else {
                            *y = *x;
                        }
                        stack.stack_pop::<(*mut Noun, *mut Noun)>();
                        continue;
                    } else {
                        break;
                    }
                }
                (Right(x_cell), Right(y_cell)) => {
                    let x_as_ptr = x_cell.to_raw_pointer();
                    let y_as_ptr = y_cell.to_raw_pointer();
                    if x_cell.head().raw_equals(y_cell.head())
                        && x_cell.tail().raw_equals(y_cell.tail())
                    {
                        let (_senior, junior) = senior_pointer_first(stack, x_as_ptr, y_as_ptr);
                        if x_as_ptr == junior {
                            *x = *y;
                        } else {
                            *y = *x;
                        }
                        stack.stack_pop::<(*mut Noun, *mut Noun)>();
                        continue;
                    } else {
                        /* THIS ISN'T AN INFINITE LOOP
                         * If we discover a disequality in either side, we will
                         * short-circuit the entire loop and reset the work stack.
                         *
                         * If both sides are equal, then we will discover pointer
                         * equality when we return and unify the cell.
                         */
                        *(stack.stack_push::<(*mut Noun, *mut Noun)>()) =
                            (x_cell.tail_as_mut(), y_cell.tail_as_mut());
                        *(stack.stack_push::<(*mut Noun, *mut Noun)>()) =
                            (x_cell.head_as_mut(), y_cell.head_as_mut());
                        continue;
                    }
                }
                (_, _) => {
                    break; // cells don't unify with atoms
                }
            }
        } else {
            break; // direct atom not raw equal, so short circuit
        }
    }
    stack.pre_copy();
    stack.pop();
    assert_acyclic!(*a);
    assert_acyclic!(*b);
    (*a).raw_equals(*b)
}

unsafe fn senior_pointer_first<T>(
    stack: &NockStack,
    a: *const T,
    b: *const T
) -> (*const T, *const T) {
    let mut frame_pointer = stack.frame_pointer as *const u64;
    let mut stack_pointer = stack.stack_pointer as *const u64;
    let mut alloc_pointer = stack.alloc_pointer as *const u64;
    let prev_stack_pointer = *(stack.prev_stack_pointer_pointer()) as *const u64;

    let (mut high_pointer, mut low_pointer) = if stack.is_west() { (
        prev_stack_pointer as *const T,
        (stack.alloc_pointer as *const T),
        )
    } else { (
        stack.alloc_pointer as *const T,
        prev_stack_pointer as *const T,
        )
    };

    loop {
        match (
            a < high_pointer && a >= low_pointer,
            b < high_pointer && b >= low_pointer,
        ) {
            (true, true)   => break (a, b), // both pointers are in the same frame, pick arbitrarily
            (true, false)  => break (b, a), // a is in the frame, b is not, so b is senior
            (false, true)  => break (a, b), // b is in the frame, a is not, so a is senior
            (false, false) => {
                // chase up the stack
                if (alloc_pointer as *const u64) == stack.start.add(stack.size) {
                    // we found the top of the stack
                    break (a, b); // both are out of the stack, pick arbitrarily
                } else {
                    // test to see if the frame under consideration is a west frame
                   if stack_pointer < alloc_pointer {
                       high_pointer = *(frame_pointer.sub(ALLOC + 1)) as *const T; // previous allocation pointer
                       // "previous previous" stack pointer. this is the other boundary of the previous allocation arena
                       low_pointer = *((*(frame_pointer.sub(FRAME + 1)) as *const u64).add(STACK)) as *const T;
                       stack_pointer = *(frame_pointer.sub(STACK + 1)) as *const u64;
                       alloc_pointer = *(frame_pointer.sub(ALLOC + 1)) as *const u64;
                       frame_pointer = *(frame_pointer.sub(FRAME + 1)) as *const u64;
                       continue;
                   } else if stack_pointer > alloc_pointer {
                       // "previous previous" stack pointer. this is the other boundary of the previous allocation arena
                       high_pointer = *((*(frame_pointer.add(FRAME)) as *const u64).sub(STACK + 1)) as *const T;
                       low_pointer = *(frame_pointer.add(ALLOC)) as *const T; // previous allocation pointer
                       stack_pointer = *(frame_pointer.add(STACK)) as *const u64;
                       alloc_pointer = *(frame_pointer.add(ALLOC)) as *const u64;
                       frame_pointer = *(frame_pointer.add(FRAME)) as *const u64;
                   } else {
                       panic!("senior_pointer_first: stack_pointer == alloc_pointer");
                   }
                }
            }
        }
    }
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
        let size = indirect_raw_size(*self);
        let buf = stack.struct_alloc_in_previous_frame::<u64>(size);
        copy_nonoverlapping(self.to_raw_pointer(), buf, size);
        *self = IndirectAtom::from_raw_pointer(buf);
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
        self.layout_alloc(layout)
    }
}
