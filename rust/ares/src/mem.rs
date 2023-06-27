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
#[derive(Debug)]
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
        let stack_pointer = unsafe { frame_pointer.add(top_slots + RESERVED) };
        let alloc_pointer = unsafe { frame_pointer.add(size) };
        unsafe {
            *frame_pointer = ptr::null::<u64>() as u64;            // "frame pointer" from "previous" frame
            *frame_pointer.add(STACK) = ptr::null::<u64>() as u64; // "stack pointer" from "previous" frame TODO is this right?
            *frame_pointer.add(ALLOC) = start as u64;              // "alloc pointer" from "previous" frame
        };
        NockStack {
            start,
            size,
            frame_pointer,
            stack_pointer,
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

    /** Pointer to where the previous (west) stack pointer is saved in an east frame */
    unsafe fn prev_stack_pointer_pointer_east(&mut self) -> *mut *mut u64 {
        self.slot_pointer_east(STACK) as *mut *mut u64
    }

    /** Pointer to where the previous (east) stack pointer is saved in an west frame */
    unsafe fn prev_stack_pointer_pointer_west(&mut self) -> *mut *mut u64 {
        self.slot_pointer_west(STACK) as *mut *mut u64
    }

    /**  Allocation
     * In a west frame, the allocation pointer is higher than the frame pointer, and so the allocation
     * size is subtracted from the allocation pointer, and then the allocation pointer is returned as
     * the pointer to the newly allocated memory.
     *
     * In an east frame, the allocation pointer is lower than the frame pointer, and so the allocation
     * pointer is saved in a temporary, then the allocation size added to it, and finally the original
     * allocation pointer is returned as the pointer to the newly allocated memory. */

    unsafe fn alloc_in_prev_frame_west<T>(&mut self) -> *mut T {
        todo!()
    }

    unsafe fn alloc_in_prev_frame_east<T>(&mut self) -> *mut T {
        todo!()
    }

    pub unsafe fn alloc_in_prev_frame<T>(&mut self) -> *mut T {
        if self.is_west() {
            self.alloc_in_prev_frame_west()
        } else {
            self.alloc_in_prev_frame_east()
        }
    }

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

    /** Before copying allocations from child to parent frame, we need to save the reserved pointers. */
    unsafe fn pre_copy_east(&mut self) {
        *(self.alloc_pointer) = *(self.prev_frame_pointer_pointer_east()) as u64;
        *(self.alloc_pointer.add(1)) = *(self.prev_stack_pointer_pointer_east()) as u64;
        *(self.alloc_pointer.add(2)) = *(self.prev_alloc_pointer_pointer_east()) as u64;
    }

    /** Before copying allocations from child to parent frame, we need to save the reserved pointers. */
    unsafe fn pre_copy_west(&mut self) {
        *(self.alloc_pointer.sub(1)) = *(self.prev_frame_pointer_pointer_west()) as u64;
        *(self.alloc_pointer.sub(2)) = *(self.prev_stack_pointer_pointer_west()) as u64;
        *(self.alloc_pointer.sub(3)) = *(self.prev_alloc_pointer_pointer_west()) as u64;
    }

    pub unsafe fn pre_copy(&mut self) {
        if self.is_west() {
            self.pre_copy_west()
        } else {
            self.pre_copy_east()
        }
    }

    pub unsafe fn restore_frame(&mut self) {
        //TODO set frame and alloc pointer to what was saved in pre_copy
        //TODO this should probably just be part of pop(), or at least called during pop()
        todo!()
    }

    unsafe fn copy_east(&mut self, noun: &mut Noun) {
        let noun_ptr = noun as *mut Noun;

        todo!()
    }

    /**  Lightweight stacks
      A lightweight stack for traversing nouns can be implemented by initializing a slot in the frame
      to the frame pointer. This slot can then be incremented by word (to push on west or pop on east)
      or decremented by word (to pop on west or push on east), since the frame is adjacent to free memory.
      Stack nullity can be tested by comparing the slot to the frame pointer.


      if the lightweight stack is occupying slots adjacent to the frame, couldn't it be overwritten when
      copying a noun from a child frame to a parent frame? here's a quick graphic illustrating what
      i mean, hope it makes sense:


      key: --- senior memory
            |  boundaries of stack frame (prev frame and prev alloc pointers)
            x  frame-pointer relative slots
            F  frame pointer
            o  free memory
            A  allocation pointer
            y  allocations
            L  lightweight noun traversal stack


      ----|xxFoooooooooAyyyyyyy|----  initial state
      ----|xxFLooooooooAyyyyyyy|----  noun traversal stack initialized
      ----|yyFLooooooooAyyyyyyy|----  noun copying to previous stack frame in progress
      ----|yyyyyyooooooAyyyyyyy|----  noun is so big it overwrites the lightweight stack

    This is an attempt to implement this with the lightweight stack next to the frame_pointer anyways,
    but maybe it needs to be done next to the allocations of the current frame instead.
    */

    unsafe fn copy_west(&mut self, noun: &mut Noun) {
        let noun_ptr = noun as *mut Noun; // coerce &mut into *mut

        // lightweight stack starts at the edge of the frame
        let work_start = self.frame_pointer;
        // location to which allocations are made
        let mut other_alloc_pointer = *(self.alloc_pointer.sub(2)) as *mut u64; // from pre_copy_west
        // used for determining whether an allocation is in the current frame
        let other_frame_pointer = *(self.alloc_pointer.sub(1)) as *const u64; // from pre_copy_west
        // add two slots to the frame for the lightweight stack
        self.frame_pointer = self.frame_pointer.add(2);
        // set the first new slot to the noun to be copied
        *(self.frame_pointer.sub(2) as *mut Noun) = *noun;
        // set the second new slot a pointer to the noun being copied. this is the destination pointer, which will change when we copy
        *(self.frame_pointer.sub(1) as *mut *mut Noun) = noun_ptr;

        loop {
            if self.frame_pointer == work_start {
                break;
            }

            // Pop a noun to copy from the stack
            let next_noun = *(self.frame_pointer.sub(2) as *const Noun);
            let next_dest = *(self.frame_pointer.sub(1) as *const *mut Noun);
            self.frame_pointer = self.frame_pointer.sub(2);

            // If it's a direct atom, just write it to the destination.
            // Otherwise, we have allocations to make
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
                            // The next destination is set to the new location of the atom
                            *next_dest = new_allocated.as_noun();
                        }
                        Option::None => {
                            if (allocated.to_raw_pointer() as *const u64) < other_frame_pointer
                                && (allocated.to_raw_pointer() as *const u64) >= self.alloc_pointer
                            {
                                match allocated.as_either() {
                                    Either::Left(mut indirect) => {
                                        // Make space for an indirect atom
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
                                        *(self.frame_pointer as *mut Noun) = cell.tail();
                                        *(self.frame_pointer.add(1) as *mut *mut Noun) =
                                            &mut (*new_cell_alloc).tail;
                                        *(self.frame_pointer.add(2) as *mut Noun) = cell.head();
                                        *(self.frame_pointer.add(3) as *mut *mut Noun) =
                                            &mut (*new_cell_alloc).head;

                                        self.frame_pointer = self.frame_pointer.add(4);

                                        // Set the forwarding pointer
                                        cell.set_forwarding_pointer(new_cell_alloc);

                                        *next_dest =
                                            Cell::from_raw_pointer(new_cell_alloc).as_noun();
                                    }
                                }
                            } else {
                                *next_dest = allocated.as_noun();
                            }
                        }
                    }
                }
            }
        }
        *(self.alloc_pointer.sub(2)) = other_alloc_pointer as u64;
        assert_acyclic!(*noun);
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
        let current_stack_pointer = self.stack_pointer;
        let current_alloc_pointer = self.alloc_pointer;

        self.alloc_pointer = current_stack_pointer;
        self.stack_pointer = current_alloc_pointer;
        self.frame_pointer = self.stack_pointer;

        *(self.frame_pointer.sub(1)) = current_frame_pointer as u64;
        *(self.frame_pointer.sub(2)) = current_stack_pointer as u64;
        *(self.frame_pointer.sub(3)) = current_alloc_pointer as u64;

        self.frame_pointer = self.frame_pointer.sub(num_locals + RESERVED);
        self.stack_pointer = self.stack_pointer.sub(num_locals + RESERVED);
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
        self.stack_pointer = current_alloc_pointer;
        self.frame_pointer = self.stack_pointer;

        *(self.frame_pointer) = current_frame_pointer as u64;
        *(self.frame_pointer.add(1)) = current_stack_pointer as u64;
        *(self.frame_pointer.add(2)) = current_alloc_pointer as u64;

        self.frame_pointer = self.frame_pointer.sub(num_locals + RESERVED);
        self.stack_pointer = self.stack_pointer.sub(num_locals + RESERVED);
    }

    /** Push a frame onto the stack with 0 or more local variable slots. */
    pub unsafe fn push(&mut self, num_locals: usize) {
        if self.is_west() {
            self.push_west(num_locals)
        } else {
            self.push_east(num_locals)
        }
    }

    /** Push onto the lightweight stack in a west frame, moving the stack_pointer */
    unsafe fn lightweight_push_west(&mut self) {
        todo!()
    }

    /** Push onto the lightweight stack in an east frame, moving the stack_pointer */
    unsafe fn lightweight_push_east(&mut self) {
        todo!()
    }

    /** Push onto the lightweight stack, moving the stack_pointer */
    unsafe fn lightweight_push(&mut self) {
        if self.is_west() {
            self.lightweight_push_west()
        } else {
            self.lightweight_push_east()
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
    //TODO a fn called alloc_indirect that just calls one called indirect_alloc
    // seems a little confusing but i don't have a better idea yet
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
