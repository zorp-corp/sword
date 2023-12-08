use crate::assert_acyclic;
use crate::assert_no_forwarding_pointers;
use crate::assert_no_junior_pointers;
use crate::noun::{Atom, Cell, CellMemory, IndirectAtom, Noun, NounAllocator};
use crate::snapshot::pma::{pma_in_arena, pma_malloc_w};
use assert_no_alloc::permit_alloc;
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
     * arena, the previous frame pointer is set to NULL, and the previous stack pointer is set to NULL */

    /** size is in 64-bit (i.e. 8-byte) words.
     * top_slots is how many slots to allocate to the top stack frame.
     */
    pub fn new(size: usize, top_slots: usize) -> NockStack {
        let memory = MmapMut::map_anon(size << 3).expect("Mapping memory for nockstack failed");
        let start = memory.as_ptr() as *const u64;
        // Here, frame_pointer < alloc_pointer, so the initial frame is West
        let frame_pointer = unsafe { start.add(RESERVED + top_slots) } as *mut u64;
        let stack_pointer = frame_pointer;
        let alloc_pointer = unsafe { start.add(size) } as *mut u64;
        unsafe {
            *frame_pointer.sub(FRAME + 1) = ptr::null::<u64>() as u64; // "frame pointer" from "previous" frame
            *frame_pointer.sub(STACK + 1) = ptr::null::<u64>() as u64; // "stack pointer" from "previous" frame
            *frame_pointer.sub(ALLOC + 1) = ptr::null::<u64>() as u64; // "alloc pointer" from "previous" frame
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

    /** Resets the NockStack. */
    pub fn reset(&mut self, top_slots: usize) {
        self.frame_pointer = unsafe { self.start.add(RESERVED + top_slots) } as *mut u64;
        self.stack_pointer = self.frame_pointer;
        self.alloc_pointer = unsafe { self.start.add(self.size) } as *mut u64;
        self.pc = false;
        unsafe {
            *self.frame_pointer.sub(FRAME + 1) = ptr::null::<u64>() as u64; // "frame pointer" from "previous" frame
            *self.frame_pointer.sub(STACK + 1) = ptr::null::<u64>() as u64; // "stack pointer" from "previous" frame
            *self.frame_pointer.sub(ALLOC + 1) = ptr::null::<u64>() as u64; // "alloc pointer" from "previous" frame
        };
    }

    /** Current frame pointer of this NockStack */
    pub fn get_frame_pointer(&self) -> *const u64 {
        self.frame_pointer
    }

    /** Checks if the current stack frame has West polarity */
    #[inline]
    pub fn is_west(&self) -> bool {
        self.stack_pointer < self.alloc_pointer
    }

    /** Size **in 64-bit words** of this NockStack */
    pub fn size(&self) -> usize {
        self.size
    }

    /** Check to see if an allocation is in frame */
    #[inline]
    pub unsafe fn is_in_frame<T>(&self, ptr: *const T) -> bool {
        let ptr_u64 = ptr as *const u64;
        let prev = *self.prev_stack_pointer_pointer();
        if self.is_west() {
            ptr_u64 >= self.alloc_pointer && ptr_u64 < prev
        } else {
            ptr_u64 >= prev && ptr_u64 < self.alloc_pointer
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

    /** Pointer to a local slot typed as Noun */
    pub unsafe fn local_noun_pointer(&mut self, local: usize) -> *mut Noun {
        self.slot_pointer(local + RESERVED) as *mut Noun
    }

    /** Pointer to where the previous frame pointer is saved in a frame */
    unsafe fn prev_frame_pointer_pointer(&self) -> *mut *mut u64 {
        if !self.pc {
            self.slot_pointer(FRAME) as *mut *mut u64
        } else {
            self.free_slot(FRAME) as *mut *mut u64
        }
    }

    /** Pointer to where the previous stack pointer is saved in a frame */
    unsafe fn prev_stack_pointer_pointer(&self) -> *mut *mut u64 {
        if !self.pc {
            self.slot_pointer(STACK) as *mut *mut u64
        } else {
            self.free_slot(STACK) as *mut *mut u64
        }
    }

    /** Pointer to where the previous stack pointer is saved in a frame */
    unsafe fn prev_alloc_pointer_pointer(&self) -> *mut *mut u64 {
        if !self.pc {
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
        if self.pc {
            panic!("Allocation during cleanup phase is prohibited.");
        }
        self.alloc_pointer = self.alloc_pointer.sub(words);
        self.alloc_pointer
    }

    /** Bump the alloc pointer for an east frame to make space for an allocation */
    unsafe fn raw_alloc_east(&mut self, words: usize) -> *mut u64 {
        if self.pc {
            panic!("Allocation during cleanup phase is prohibited.");
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
        // note that the allocation is on the east frame, and thus resembles raw_alloc_east
        let alloc = *self.prev_alloc_pointer_pointer();
        *(self.prev_alloc_pointer_pointer()) =
            (*(self.prev_alloc_pointer_pointer())).add(word_size_of::<T>() * count);
        alloc as *mut T
    }

    unsafe fn struct_alloc_in_previous_frame_east<T>(&mut self, count: usize) -> *mut T {
        // note that the allocation is on the east frame, and thus resembles raw_alloc_west
        *(self.prev_alloc_pointer_pointer()) =
            (*(self.prev_alloc_pointer_pointer())).sub(word_size_of::<T>() * count);
        *(self.prev_alloc_pointer_pointer()) as *mut T
    }

    /** Allocates space in the previous frame for some number of T's. This calls pre_copy()
     * first to ensure that the stack frame is in cleanup phase, which is the only time we
     * should be allocating in a previous frame.*/
    pub unsafe fn struct_alloc_in_previous_frame<T>(&mut self, count: usize) -> *mut T {
        self.pre_copy();
        if self.is_west() {
            self.struct_alloc_in_previous_frame_west(count)
        } else {
            self.struct_alloc_in_previous_frame_east(count)
        }
    }

    unsafe fn indirect_alloc_in_previous_frame_west(&mut self, words: usize) -> *mut u64 {
        let alloc = *self.prev_alloc_pointer_pointer();
        *(self.prev_alloc_pointer_pointer()) =
            (*(self.prev_alloc_pointer_pointer())).add(words + 2);
        alloc
    }

    unsafe fn indirect_alloc_in_previous_frame_east(&mut self, words: usize) -> *mut u64 {
        *(self.prev_alloc_pointer_pointer()) =
            (*(self.prev_alloc_pointer_pointer())).sub(words + 2);
        *self.prev_alloc_pointer_pointer()
    }

    /** Allocate space for an indirect atom in the previous stack frame. This call pre_copy()
     * first to ensure that the stack frame is in cleanup phase, which is the only time we
     * should be allocating in a previous frame. */
    unsafe fn indirect_alloc_in_previous_frame(&mut self, words: usize) -> *mut u64 {
        self.pre_copy();
        if self.is_west() {
            self.indirect_alloc_in_previous_frame_west(words)
        } else {
            self.indirect_alloc_in_previous_frame_east(words)
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

    /** Copies reserved pointers to free space adjacent to the allocation arena, and
     * moves the lightweight stack to the free space adjacent to that.
     *
     * Once this function is called a on stack frame, we say that it is now in the "cleanup
     * phase". At this point, no more allocations can be made, and all that is left to
     * do is figure out what data in this frame needs to be preserved and thus copied to
     * the parent frame.
     *
     * This might be the most confusing part of the split stack system. But we've tried
     * to make it so that the programmer doesn't need to think about it at all. The
     * interface for using the reserved pointers (prev_xyz_pointer_pointer()) and
     * lightweight stack (push(), pop(), top()) are the same regardless of whether
     * or not pre_copy() has been called.*/
    unsafe fn pre_copy(&mut self) {
        if !self.pc {
            *(self.free_slot(FRAME)) = *(self.slot_pointer(FRAME));
            *(self.free_slot(STACK)) = *(self.slot_pointer(STACK));
            *(self.free_slot(ALLOC)) = *(self.slot_pointer(ALLOC));

            self.pc = true;
            // Change polarity of lightweight stack.
            if self.is_west() {
                self.stack_pointer = self.alloc_pointer.sub(RESERVED + 1);
            } else {
                self.stack_pointer = self.alloc_pointer.add(RESERVED);
            }
        }
    }

    unsafe fn copy(&mut self, noun: &mut Noun) {
        assert_acyclic!(*noun);
        assert_no_forwarding_pointers!(*noun);
        assert_no_junior_pointers!(self, *noun);

        self.pre_copy();
        assert!(self.stack_is_empty());
        let noun_ptr = noun as *mut Noun;
        // Add two slots to the lightweight stack
        // Set the first new slot to the noun to be copied
        *(self.push::<Noun>()) = *noun;
        // Set the second new slot to a pointer to the noun being copied. this is the destination pointer, which will change
        *(self.push::<*mut Noun>()) = noun_ptr;
        loop {
            if self.stack_is_empty() {
                break;
            }

            // Pop a noun to copy from the stack
            let next_dest = *(self.top::<*mut Noun>());
            self.pop::<*mut Noun>();
            let next_noun = *(self.top::<Noun>());
            self.pop::<Noun>();

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
                            if self.is_in_frame(allocated.to_raw_pointer()) {
                                match allocated.as_either() {
                                    Either::Left(mut indirect) => {
                                        // Make space for the atom
                                        let alloc =
                                            self.indirect_alloc_in_previous_frame(indirect.size());

                                        // Indirect atoms can be copied directly
                                        copy_nonoverlapping(
                                            indirect.to_raw_pointer(),
                                            alloc,
                                            indirect_raw_size(indirect),
                                        );

                                        // Set a forwarding pointer so we don't create duplicates from other
                                        // references
                                        indirect.set_forwarding_pointer(alloc);

                                        *next_dest =
                                            IndirectAtom::from_raw_pointer(alloc).as_noun();
                                    }
                                    Either::Right(mut cell) => {
                                        // Make space for the cell
                                        let alloc =
                                            self.struct_alloc_in_previous_frame::<CellMemory>(1);

                                        // Copy the cell metadata
                                        (*alloc).metadata = (*cell.to_raw_pointer()).metadata;

                                        // Push the tail and the head to the work stack
                                        *(self.push::<Noun>()) = cell.tail();
                                        *(self.push::<*mut Noun>()) = &mut (*alloc).tail;
                                        *(self.push::<Noun>()) = cell.head();
                                        *(self.push::<*mut Noun>()) = &mut (*alloc).head;

                                        // Set the forwarding pointer
                                        cell.set_forwarding_pointer(alloc);

                                        *next_dest = Cell::from_raw_pointer(alloc).as_noun();
                                    }
                                }
                            } else {
                                // Don't copy references outside the current frame
                                *next_dest = allocated.as_noun();
                            }
                        }
                    }
                }
            }
        }
        // Set saved previous allocation pointer its new value after this allocation

        assert_acyclic!(*noun);
        assert_no_forwarding_pointers!(*noun);
        assert_no_junior_pointers!(self, *noun);
    }

    pub unsafe fn assert_struct_is_in<T>(&self, ptr: *const T, count: usize) {
        let ap = (if self.pc {
            *(self.prev_alloc_pointer_pointer())
        } else {
            self.alloc_pointer
        }) as usize;
        let sp = (if self.pc {
            *(self.prev_stack_pointer_pointer())
        } else {
            self.stack_pointer
        }) as usize;
        let (low, hi) = if ap > sp { (sp, ap) } else { (ap, sp) };
        if ((ptr as usize) < low && (ptr.add(count) as usize) <= low)
            || ((ptr as usize) >= hi && (ptr.add(count) as usize) > hi)
        {
            return;
        }
        panic!(
            "Use after free: allocation from {:#x} to {:#x}, free space from {:#x} to {:#x}",
            ptr as usize,
            ptr.add(count) as usize,
            low,
            hi
        );
    }

    unsafe fn assert_noun_in(&self, noun: Noun) {
        let mut dbg_stack = Vec::new();
        dbg_stack.push(noun);
        let ap = (if self.pc {
            *(self.prev_alloc_pointer_pointer())
        } else {
            self.alloc_pointer
        }) as usize;
        let sp = (if self.pc {
            *(self.prev_stack_pointer_pointer())
        } else {
            self.stack_pointer
        }) as usize;
        let (low, hi) = if ap > sp { (sp, ap) } else { (ap, sp) };
        loop {
            if let Some(subnoun) = dbg_stack.pop() {
                if let Ok(a) = subnoun.as_allocated() {
                    let np = a.to_raw_pointer() as usize;
                    if np >= low && np < hi {
                        panic!("noun not in {:?}: {:?}", (low, hi), subnoun);
                    }
                    if let Right(c) = a.as_either() {
                        dbg_stack.push(c.tail());
                        dbg_stack.push(c.head());
                    }
                }
            } else {
                return;
            }
        }
    }

    pub unsafe fn copy_pma(&mut self, noun: &mut Noun) {
        // copy_pma() should only be called when there is a single stack
        // frame; these asserts assure that.
        assert!(
            self.is_west()
                && (*(self.prev_stack_pointer_pointer())).is_null()
                && (*(self.prev_frame_pointer_pointer())).is_null()
        );
        assert!(self.stack_is_empty());
        let noun_ptr = noun as *mut Noun;
        *(self.push::<Noun>()) = *noun;
        *(self.push::<*mut Noun>()) = noun_ptr;
        loop {
            if self.stack_is_empty() {
                break;
            }

            let next_dest = *(self.top::<*mut Noun>());
            self.pop::<*mut Noun>();
            let next_noun = *(self.top::<Noun>());
            self.pop::<Noun>();

            match next_noun.as_either_direct_allocated() {
                Either::Left(_direct) => {
                    *next_dest = next_noun;
                }
                Either::Right(allocated) => match allocated.forwarding_pointer() {
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
                                        pma_malloc_w(indirect_raw_size(indirect));

                                    copy_nonoverlapping(
                                        indirect.to_raw_pointer(),
                                        new_indirect_alloc,
                                        indirect_raw_size(indirect),
                                    );

                                    indirect.set_forwarding_pointer(new_indirect_alloc);

                                    *next_dest = IndirectAtom::from_raw_pointer(new_indirect_alloc)
                                        .as_noun();
                                }
                                Either::Right(mut cell) => {
                                    let new_cell_alloc: *mut CellMemory =
                                        pma_malloc_w(word_size_of::<CellMemory>());

                                    (*new_cell_alloc).metadata = (*cell.to_raw_pointer()).metadata;

                                    *(self.push::<Noun>()) = cell.tail();
                                    *(self.push::<*mut Noun>()) = &mut (*new_cell_alloc).tail;
                                    *(self.push::<Noun>()) = cell.head();
                                    *(self.push::<*mut Noun>()) = &mut (*new_cell_alloc).head;

                                    cell.set_forwarding_pointer(new_cell_alloc);

                                    *next_dest = Cell::from_raw_pointer(new_cell_alloc).as_noun();
                                }
                            }
                        }
                    }
                },
            }
        }
    }

    pub unsafe fn frame_pop(&mut self) {
        let prev_frame_ptr = *self.prev_frame_pointer_pointer();
        let prev_stack_ptr = *self.prev_stack_pointer_pointer();
        let prev_alloc_ptr = *self.prev_alloc_pointer_pointer();

        self.frame_pointer = prev_frame_ptr;
        self.stack_pointer = prev_stack_ptr;
        self.alloc_pointer = prev_alloc_ptr;

        if self.frame_pointer.is_null()
            || self.stack_pointer.is_null()
            || self.alloc_pointer.is_null()
        {
            permit_alloc(|| {
                panic!(
                    "serf: frame_pop: null NockStack pointer f={:p} s={:p} a={:p}",
                    self.frame_pointer, self.stack_pointer, self.alloc_pointer
                );
            });
        }

        self.pc = false;
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

    /** Push a frame onto the stack with 0 or more local variable slots. */
    pub fn frame_push(&mut self, num_locals: usize) {
        let current_frame_pointer = self.frame_pointer;
        let current_stack_pointer = self.stack_pointer;
        let current_alloc_pointer = self.alloc_pointer;
        unsafe {
            self.frame_pointer = if self.is_west() {
                current_alloc_pointer.sub(num_locals + RESERVED)
            } else {
                current_alloc_pointer.add(num_locals + RESERVED)
            };
            self.alloc_pointer = current_stack_pointer;
            self.stack_pointer = self.frame_pointer;

            *(self.slot_pointer(FRAME)) = current_frame_pointer as u64;
            *(self.slot_pointer(STACK)) = current_stack_pointer as u64;
            *(self.slot_pointer(ALLOC)) = current_alloc_pointer as u64;
        }

        self.pc = false;
    }

    /** Run a closure inside a frame, popping regardless of the value returned by the closure.
     * This is useful for writing fallible computations with the `?` operator.
     *
     * Note that results allocated on the stack *must* be `preserve()`d by the closure.
     */
    pub unsafe fn with_frame<F, O>(&mut self, num_locals: usize, f: F) -> O
    where
        F: FnOnce(&mut NockStack) -> O,
        O: Preserve
    {
        self.frame_push(num_locals);
        let mut ret = f(self);
        ret.preserve(self);
        self.frame_pop();
        ret
    }

    /** Lightweight stack.
     * The lightweight stack is a stack data structure present in each stack
     * frame, often used for noun traversal. During normal operation (self.pc
     * == false),a west frame has a "west-oriented" lightweight stack, which
     * means that it sits immediately eastward of the frame pointer, pushing
     * moves the stack pointer eastward, and popping moves the frame pointer
     * westward. The stack is empty when stack_pointer == frame_pointer. The
     * east frame situation is the same, swapping west for east.
     *
     * Once a stack frame is preparing to be popped, pre_copy() is called (pc == true)
     * and this reverses the orientation of the lightweight stack. For a west frame,
     * that means it starts at the eastward most free byte west of the allocation
     * arena (which is now more words west than the allocation pointer, to account
     * for slots containing the previous frame's pointers), pushing moves the
     * stack pointer westward, and popping moves it eastward. Again, the east
     * frame situation is the same, swapping west for east.
     *
     * When pc == true, the lightweight stack is used for copying from the current
     * frame's allocation arena to the previous frames. */

    /** Push onto the lightweight stack, moving the stack_pointer. Note that
     * this violates the _east/_west naming convention somewhat, since e.g.
     * a west frame when pc == false has a west-oriented lightweight stack,
     * but when pc == true it becomes east-oriented.*/
    pub unsafe fn push<T>(&mut self) -> *mut T {
        if self.is_west() && !self.pc || !self.is_west() && self.pc {
            self.push_west::<T>()
        } else {
            self.push_east::<T>()
        }
    }

    /** Push onto a west-oriented lightweight stack, moving the stack_pointer.
     * */
    unsafe fn push_west<T>(&mut self) -> *mut T {
        let alloc = self.stack_pointer;
        self.stack_pointer = self.stack_pointer.add(word_size_of::<T>());
        alloc as *mut T
    }

    /** Push onto an east-oriented ligthweight stack, moving the stack_pointer */
    unsafe fn push_east<T>(&mut self) -> *mut T {
        self.stack_pointer = self.stack_pointer.sub(word_size_of::<T>());
        self.stack_pointer as *mut T
    }

    /** Pop a west-oriented lightweight stack, moving the stack pointer. */
    unsafe fn pop_west<T>(&mut self) {
        self.stack_pointer = self.stack_pointer.sub(word_size_of::<T>());
    }

    /** Pop an east-oriented lightweight stack, moving the stack pointer. */
    unsafe fn pop_east<T>(&mut self) {
        self.stack_pointer = self.stack_pointer.add(word_size_of::<T>());
    }

    /** Pop the lightweight stack, moving the stack_pointer. Note that
     * this violates the _east/_west naming convention somewhat, since e.g.
     * a west frame when pc == false has a west-oriented lightweight stack,
     * but when pc == true it becomes east-oriented.*/
    pub unsafe fn pop<T>(&mut self) {
        if self.is_west() && !self.pc || !self.is_west() && self.pc {
            self.pop_west::<T>();
        } else {
            self.pop_east::<T>();
        };
    }

    /** Peek the top of the lightweight stack. Note that
     * this violates the _east/_west naming convention somewhat, since e.g.
     * a west frame when pc == false has a west-oriented lightweight stack,
     * but when pc == true it becomes east-oriented.*/
    pub unsafe fn top<T>(&mut self) -> *mut T {
        if self.is_west() && !self.pc || !self.is_west() && self.pc {
            self.top_west()
        } else {
            self.top_east()
        }
    }

    /** Peek the top of a west-oriented lightweight stack. */
    unsafe fn top_west<T>(&mut self) -> *mut T {
        self.stack_pointer.sub(word_size_of::<T>()) as *mut T
    }

    /** Peek the top of an east-oriented lightweight stack. */
    unsafe fn top_east<T>(&mut self) -> *mut T {
        self.stack_pointer as *mut T
    }

    /** Checks to see if the lightweight stack is empty. Note that this doesn't work
     * when the stack pointer has been moved to be close to the allocation arena, such
     * as in copy_west(). */
    pub fn stack_is_empty(&self) -> bool {
        if !self.pc {
            self.stack_pointer == self.frame_pointer
        } else if self.is_west() {
            unsafe { self.stack_pointer == self.alloc_pointer.sub(RESERVED + 1) }
        } else {
            unsafe { self.stack_pointer == self.alloc_pointer.add(RESERVED) }
        }
    }

    pub fn no_junior_pointers(&self, noun: Noun) -> bool {
        unsafe {
            if let Ok(c) = noun.as_cell() {
                let mut fp: *mut u64;
                let mut sp = self.stack_pointer;
                let mut ap = self.alloc_pointer;
                let mut pfp = *(self.prev_frame_pointer_pointer());
                let mut psp = *(self.prev_stack_pointer_pointer());
                let mut pap = *(self.prev_alloc_pointer_pointer());

                let mut dbg_stack = Vec::new();

                // Detemine range
                let (rlo, rhi) = loop {
                    if psp.is_null() {
                        psp = ((self.start as u64) + ((self.size << 3) as u64)) as *mut u64;
                    }
                    let (lo, hi) = if sp < ap { (ap, psp) } else { (psp, ap) };
                    let ptr = c.to_raw_pointer() as *mut u64;
                    if ptr >= lo && ptr < hi {
                        break if sp < ap { (sp, ap) } else { (ap, sp) };
                    } else {
                        fp = pfp;
                        sp = psp;
                        ap = pap;
                        if sp < ap {
                            pfp = *(fp.sub(FRAME + 1)) as *mut u64;
                            psp = *(fp.sub(STACK + 1)) as *mut u64;
                            pap = *(fp.sub(ALLOC + 1)) as *mut u64;
                        } else {
                            pfp = *(fp.add(FRAME)) as *mut u64;
                            psp = *(fp.add(STACK)) as *mut u64;
                            pap = *(fp.add(ALLOC)) as *mut u64;
                        }
                    }
                };

                dbg_stack.push(c.head());
                dbg_stack.push(c.tail());
                while let Some(n) = dbg_stack.pop() {
                    if let Ok(a) = n.as_allocated() {
                        let ptr = a.to_raw_pointer();
                        if ptr >= rlo && ptr < rhi {
                            eprintln!(
                                "\rserf: Noun {:x} has Noun {:x} in junior of range {:p}-{:p}",
                                (noun.raw << 3),
                                (n.raw << 3),
                                rlo,
                                rhi
                            );
                            return false;
                        }
                        if let Some(c) = a.cell() {
                            dbg_stack.push(c.tail());
                            dbg_stack.push(c.head());
                        }
                    }
                }

                true
            } else {
                true
            }
        }
    }
}

#[cfg(feature = "check_junior")]
#[macro_export]
macro_rules! assert_no_junior_pointers {
    ( $x:expr, $y:expr ) => {
        assert_no_alloc::permit_alloc(|| {
            assert!($x.no_junior_pointers($y));
        })
    };
}

#[cfg(not(feature = "check_junior"))]
#[macro_export]
macro_rules! assert_no_junior_pointers {
    ( $x:expr, $y:expr ) => {};
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
    assert_acyclic!(*a);
    assert_acyclic!(*b);
    assert_no_forwarding_pointers!(*a);
    assert_no_forwarding_pointers!(*b);
    assert_no_junior_pointers!(stack, *a);
    assert_no_junior_pointers!(stack, *b);

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
    stack.frame_push(0);
    *(stack.push::<(*mut Noun, *mut Noun)>()) = (a, b);
    loop {
        if stack.stack_is_empty() {
            break;
        };
        let (x, y): (*mut Noun, *mut Noun) = *(stack.top());
        if (*x).raw_equals(*y) {
            stack.pop::<(*mut Noun, *mut Noun)>();
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
                        if x_as_ptr == junior {
                            *x = *y;
                        } else {
                            *y = *x;
                        }
                        stack.pop::<(*mut Noun, *mut Noun)>();
                        continue;
                    } else {
                        break;
                    }
                }
                (Right(x_cell), Right(y_cell)) => {
                    let x_as_ptr = x_cell.to_raw_pointer() as *const u64;
                    let y_as_ptr = y_cell.to_raw_pointer() as *const u64;
                    if x_cell.head().raw_equals(y_cell.head())
                        && x_cell.tail().raw_equals(y_cell.tail())
                    {
                        let (_senior, junior) = senior_pointer_first(stack, x_as_ptr, y_as_ptr);
                        if x_as_ptr == junior {
                            *x = *y;
                        } else {
                            *y = *x;
                        }
                        stack.pop::<(*mut Noun, *mut Noun)>();
                        continue;
                    } else {
                        /* THIS ISN'T AN INFINITE LOOP
                         * If we discover a disequality in either side, we will
                         * short-circuit the entire loop and reset the work stack.
                         *
                         * If both sides are equal, then we will discover pointer
                         * equality when we return and unify the cell.
                         */
                        *(stack.push::<(*mut Noun, *mut Noun)>()) =
                            (x_cell.tail_as_mut(), y_cell.tail_as_mut());
                        *(stack.push::<(*mut Noun, *mut Noun)>()) =
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
    stack.frame_pop();

    assert_acyclic!(*a);
    assert_acyclic!(*b);
    assert_no_forwarding_pointers!(*a);
    assert_no_forwarding_pointers!(*b);
    assert_no_junior_pointers!(stack, *a);
    assert_no_junior_pointers!(stack, *b);

    (*a).raw_equals(*b)
}

unsafe fn senior_pointer_first(
    stack: &NockStack,
    a: *const u64,
    b: *const u64,
) -> (*const u64, *const u64) {
    let mut frame_pointer: *const u64 = stack.frame_pointer;
    let mut stack_pointer: *const u64 = stack.stack_pointer;
    let mut alloc_pointer: *const u64 = stack.alloc_pointer;
    let prev_stack_pointer = *(stack.prev_stack_pointer_pointer());

    let (mut high_pointer, mut low_pointer): (*const u64, *const u64) = if stack.is_west() {
        (prev_stack_pointer, alloc_pointer)
    } else {
        (alloc_pointer, prev_stack_pointer)
    };

    loop {
        if low_pointer.is_null() || high_pointer.is_null() {
            // we found the bottom of the stack; check entirety of the stack
            low_pointer = stack.start;
            high_pointer = stack.start.add(stack.size);
        }

        match (
            a < high_pointer && a >= low_pointer,
            b < high_pointer && b >= low_pointer,
        ) {
            (true, true) => {
                // both pointers are in the same frame, pick arbitrarily (lower in mem)
                break lower_pointer_first(a, b);
            }
            (true, false) => break (b, a), // a is in the frame, b is not, so b is senior
            (false, true) => break (a, b), // b is in the frame, a is not, so a is senior
            (false, false) => {
                // chase up the stack
                #[allow(clippy::comparison_chain)]
                // test to see if the frame under consideration is a west frame
                if stack_pointer < alloc_pointer {
                    stack_pointer = *(frame_pointer.sub(STACK + 1)) as *const u64;
                    alloc_pointer = *(frame_pointer.sub(ALLOC + 1)) as *const u64;
                    frame_pointer = *(frame_pointer.sub(FRAME + 1)) as *const u64;

                    // both pointers are in the PMA, pick arbitrarily (lower in mem)
                    if frame_pointer.is_null() {
                        break lower_pointer_first(a, b);
                    };

                    // previous allocation pointer
                    high_pointer = alloc_pointer;
                    // "previous previous" stack pointer. this is the other boundary of the previous allocation arena
                    low_pointer = *(frame_pointer.add(STACK)) as *const u64;
                } else if stack_pointer > alloc_pointer {
                    stack_pointer = *(frame_pointer.add(STACK)) as *const u64;
                    alloc_pointer = *(frame_pointer.add(ALLOC)) as *const u64;
                    frame_pointer = *(frame_pointer.add(FRAME)) as *const u64;

                    // both pointers are in the PMA, pick arbitrarily (lower in mem)
                    if frame_pointer.is_null() {
                        break lower_pointer_first(a, b);
                    };

                    // previous allocation pointer
                    low_pointer = alloc_pointer;
                    // "previous previous" stack pointer. this is the other boundary of the previous allocation arena
                    high_pointer = *(frame_pointer.sub(STACK + 1)) as *const u64;
                } else {
                    panic!("senior_pointer_first: stack_pointer == alloc_pointer");
                }
            }
        }
    }
}

fn lower_pointer_first(a: *const u64, b: *const u64) -> (*const u64, *const u64) {
    if a < b {
        (a, b)
    } else {
        (b, a)
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
    unsafe fn assert_in_stack(&self, stack: &NockStack);
}

impl Preserve for IndirectAtom {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        let size = indirect_raw_size(*self);
        let buf = stack.struct_alloc_in_previous_frame::<u64>(size);
        copy_nonoverlapping(self.to_raw_pointer(), buf, size);
        *self = IndirectAtom::from_raw_pointer(buf);
    }
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_noun_in(self.as_atom().as_noun());
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
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_noun_in(self.as_noun());
    }
}

impl Preserve for Noun {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        stack.copy(self);
    }
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_noun_in(*self);
    }
}

impl Stack for NockStack {
    unsafe fn alloc_layout(&mut self, layout: Layout) -> *mut u64 {
        self.layout_alloc(layout)
    }
}

impl<T: Preserve, E: Preserve> Preserve for Result<T, E> {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        match self.as_mut() {
            Ok(t_ref) => {
                t_ref.preserve(stack)
            },
            Err(e_ref) => {
                e_ref.preserve(stack)
            }
        }
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        match self.as_ref() {
            Ok(t_ref) => {
                t_ref.assert_in_stack(stack)
            },
            Err(e_ref) => {
                e_ref.assert_in_stack(stack)
            }
        }
    }
}

