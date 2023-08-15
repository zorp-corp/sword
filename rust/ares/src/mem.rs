use crate::assert_acyclic;
use crate::noun::{Atom, Cell, CellMemory, IndirectAtom, Noun, NounAllocator};
use crate::snapshot::pma::{pma_in_arena, pma_malloc_w};
use either::Either::{self, Left, Right};
use ibig::Stack;
use libc::{c_void, memcmp};
use memmap::MmapMut;
use std::alloc::Layout;
use std::mem;
use std::ptr;
use std::ptr::copy_nonoverlapping;

crate::gdb!();

/** Utility function to get size in words */
pub const fn word_size_of<T>() -> usize {
    (mem::size_of::<T>() + 7) >> 3
}

/** Utility function to compute the raw memory usage of an IndirectAtom */
fn indirect_raw_size(atom: IndirectAtom) -> usize {
    debug_assert!(atom.size() > 0);
    atom.size() + 2
}

/** Which side of the two opposing stacks are we working on? */
#[derive(Copy, Clone, PartialEq)]
pub enum Polarity {
    /** Stack growing down from high memory */
    East,
    /** Stack growing up from low memory */
    West,
}

/* XX
 * ~master-morzod suggests splitting stack frames:
 * when FP-relative slots are on the west stack, allocate on the east stack, and vice versa.
 * This would mean that traversal stacks must be allocated adjacent to the current frame's locals,
 * rather than being allocated adjacent to the previous frame, which may in fact make more sense.
 *
 * This would enable completely reliable tail calls. Currently we cannot do tail-call optimization
 * reliably for indirect calls, since the called arm might need arbitrarily many stack slots. With
 * the proposed "split" layoud we can simply extend the number of slots if a tail-called arm
 * requires more.
 *
 * Unless I'm mistaken this requires a three-pointer stack: the frame pointer is the basis for
 * relative-offset locals as per usual, the stack pointer denotes the extent of the current frame,
 * and the allocation denotes the current frontier of allocations on the opposite stack.
 *
 * Allocation means bumping the allocation pointer as per usual. Traversal stacks are implemented
 * by saving the stack pointer to a local and then manipulating the stack pointer.
 *
 * Pushing is implemented by setting the frame pointer to the allocation pointer, the stack pointer
 * to the necessary offset from the frame pointer to accomodate the locals, and the allocation
 * pointer to the stack pointer. The previous values of all three are saved in the new frame's
 * first three locals.
 *
 * Popping must first save the parent-frame stored values to temporaries. Then a copy is run which
 * will, in general, run over the stored locals, and update the temporary for the allocation
 * pointer. Finally, the temporaries are restored as the current frame/stack/allocation pointer.
 *
 * An alternative to this model is to copy anyway when making a tail call, using the tail call's
 * parameters as roots. (Notably this requires the copier to take a list of mutable references to
 * roots.) This ensures that no allocations are in the way if we need to extend the list of locals,
 * at the expense of removing an obvious point of programmer control over the timing of memory
 * management.
 */

/** A stack for Nock computation, which supports stack allocation and delimited copying collection
 * for returned nouns
 */
#[allow(dead_code)] // We need the memory field to keep our memory from being unmapped
pub struct NockStack {
    /** The base pointer */
    start: *const u64,
    /** The size of the memory region */
    size: usize,
    /** Which side of the stack is the active stack frame on? */
    polarity: Polarity,
    /** Furthest extent of the current stack frame */
    stack_pointer: *mut u64,
    /** Base pointer for the current stack frame. Accesses to slots are computed from this base. */
    frame_pointer: *mut u64,
    /** MMap which must be kept alive as long as this NockStack is */
    memory: MmapMut,
}

impl NockStack {
    /** Size is in 64 bit words.
     * top_slots is how many slots to allocate to the top stack frame.
     */
    pub fn new(size: usize, top_slots: usize) -> NockStack {
        let mut memory = MmapMut::map_anon(size << 3).expect("Mapping memory for nockstack failed");
        let start = memory.as_ptr() as *const u64;
        let frame_pointer = memory.as_mut_ptr() as *mut u64;
        let stack_pointer = unsafe { frame_pointer.add(top_slots + 2) };
        unsafe {
            *frame_pointer = frame_pointer.add(size) as u64;
            *frame_pointer.add(1) = ptr::null::<u64>() as u64;
        };
        NockStack {
            start,
            size,
            polarity: Polarity::West,
            stack_pointer,
            frame_pointer,
            memory,
        }
    }

    /** Size **in 64-bit words** of this NockStack */
    pub fn size(&self) -> usize {
        self.size
    }

    #[inline]
    pub fn in_frame<T>(&self, ptr: *const T) -> bool {
        let ptr_u64 = ptr as *const u64;
        match &self.polarity {
            Polarity::East => ptr_u64 >= self.stack_pointer && ptr_u64 < self.frame_pointer,
            Polarity::West => ptr_u64 >= self.frame_pointer && ptr_u64 < self.stack_pointer,
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
        match &self.polarity {
            Polarity::East => self.slot_pointer_east(slot),
            Polarity::West => self.slot_pointer_west(slot),
        }
    }

    /** Pointer to a local slot typed as Noun */
    pub unsafe fn local_noun_pointer(&mut self, local: usize) -> *mut Noun {
        self.slot_pointer(local + 2) as *mut Noun
    }

    /** Save the stack pointer for the previous frame in a slot of an east frame */
    unsafe fn save_prev_stack_pointer_to_local_east(&mut self, local: usize) {
        *(self.slot_pointer_east(local + 2) as *mut *mut u64) =
            *(self.previous_stack_pointer_pointer_east())
    }

    /** Save the stack pointer for the previous frame in a slot of a west frame */
    unsafe fn save_prev_stack_pointer_to_local_west(&mut self, local: usize) {
        *(self.slot_pointer_west(local + 2) as *mut *mut u64) =
            *(self.previous_stack_pointer_pointer_west())
    }

    /** Save the stack pointer for the previous frame in a slot */
    pub unsafe fn save_prev_stack_pointer_to_local(&mut self, local: usize) {
        match &self.polarity {
            Polarity::East => self.save_prev_stack_pointer_to_local_east(local),
            Polarity::West => self.save_prev_stack_pointer_to_local_west(local),
        }
    }

    unsafe fn restore_prev_stack_pointer_from_local_east(&mut self, local: usize) {
        *(self.previous_stack_pointer_pointer_east()) =
            *(self.slot_pointer_east(local + 2) as *mut *mut u64);
    }

    unsafe fn restore_prev_stack_pointer_from_local_west(&mut self, local: usize) {
        *(self.previous_stack_pointer_pointer_west()) =
            *(self.slot_pointer_west(local + 2) as *mut *mut u64);
    }

    unsafe fn restore_prev_stack_pointer_from_local(&mut self, local: usize) {
        match &self.polarity {
            Polarity::East => self.restore_prev_stack_pointer_from_local_east(local),
            Polarity::West => self.restore_prev_stack_pointer_from_local_west(local),
        }
    }

    unsafe fn prev_stack_pointer_equals_local_east(&mut self, local: usize) -> bool {
        *(self.slot_pointer_east(local + 2) as *const *mut u64)
            == *(self.previous_stack_pointer_pointer_east())
    }

    unsafe fn prev_stack_pointer_equals_local_west(&mut self, local: usize) -> bool {
        *(self.slot_pointer_west(local + 2) as *const *mut u64)
            == *(self.previous_stack_pointer_pointer_west())
    }

    /** Test the stack pointer for the previous frame against a slot */
    pub unsafe fn prev_stack_pointer_equals_local(&mut self, local: usize) -> bool {
        match &self.polarity {
            Polarity::East => self.prev_stack_pointer_equals_local_east(local),
            Polarity::West => self.prev_stack_pointer_equals_local_west(local),
        }
    }

    unsafe fn alloc_in_previous_frame_west<T>(&mut self) -> *mut T {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_west();
        // note that the allocation is on the east frame, and thus resembles raw_alloc_east
        *prev_stack_pointer_pointer = (*prev_stack_pointer_pointer).sub(word_size_of::<T>());
        *prev_stack_pointer_pointer as *mut T
    }

    unsafe fn alloc_in_previous_frame_east<T>(&mut self) -> *mut T {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_east();
        // note that the allocation is on the west frame, and thus resembles raw_alloc_west
        let alloc = *(prev_stack_pointer_pointer);
        *prev_stack_pointer_pointer = (*prev_stack_pointer_pointer).add(word_size_of::<T>());
        alloc as *mut T
    }

    pub unsafe fn alloc_in_previous_frame<T>(&mut self) -> *mut T {
        match &self.polarity {
            Polarity::East => self.alloc_in_previous_frame_east(),
            Polarity::West => self.alloc_in_previous_frame_west(),
        }
    }

    unsafe fn reclaim_in_previous_frame_east<T>(&mut self) {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_east();
        *prev_stack_pointer_pointer = (*prev_stack_pointer_pointer).sub(word_size_of::<T>());
    }

    unsafe fn reclaim_in_previous_frame_west<T>(&mut self) {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_west();
        *prev_stack_pointer_pointer = (*prev_stack_pointer_pointer).add(word_size_of::<T>());
    }

    /** Reclaim allocated space at the end of the previous stack frame.
     * This is unsafe because if we're not checking against a saved pointer, we could reclaim
     * space used for noun allocations and cause them to be overwritten
     */
    pub unsafe fn reclaim_in_previous_frame<T>(&mut self) {
        match &self.polarity {
            Polarity::East => self.reclaim_in_previous_frame_east::<T>(),
            Polarity::West => self.reclaim_in_previous_frame_west::<T>(),
        }
    }

    unsafe fn struct_alloc_in_previous_frame_east<T>(&mut self, count: usize) -> *mut T {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_east();
        // note that the allocation is on the west frame, and thus resembles raw_alloc_west
        let alloc = *(prev_stack_pointer_pointer);
        *prev_stack_pointer_pointer =
            (*prev_stack_pointer_pointer).add(word_size_of::<T>() * count);
        alloc as *mut T
    }

    unsafe fn struct_alloc_in_previous_frame_west<T>(&mut self, count: usize) -> *mut T {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_west();
        // note that the allocation is on the east frame, and thus resembles raw_alloc_east
        *prev_stack_pointer_pointer =
            (*prev_stack_pointer_pointer).sub(word_size_of::<T>() * count);
        *prev_stack_pointer_pointer as *mut T
    }

    pub unsafe fn struct_alloc_in_previous_frame<T>(&mut self, count: usize) -> *mut T {
        match &self.polarity {
            Polarity::East => self.struct_alloc_in_previous_frame_east(count),
            Polarity::West => self.struct_alloc_in_previous_frame_west(count),
        }
    }

    unsafe fn top_in_previous_frame_east<T>(&mut self) -> *mut T {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_east();
        (*prev_stack_pointer_pointer).sub(word_size_of::<T>()) as *mut T
    }

    unsafe fn top_in_previous_frame_west<T>(&mut self) -> *mut T {
        let prev_stack_pointer_pointer = self.previous_stack_pointer_pointer_west();
        *prev_stack_pointer_pointer as *mut T
    }

    /** Get a pointer to the top entry in the previous stack frame.
     *
     * Note that if the there are no entries the behavior is undefined.
     */
    pub unsafe fn top_in_previous_frame<T>(&mut self) -> *mut T {
        match &self.polarity {
            Polarity::East => self.top_in_previous_frame_east::<T>(),
            Polarity::West => self.top_in_previous_frame_west::<T>(),
        }
    }

    /** Pointer to where the previous (west) stack pointer is saved in an east frame */
    unsafe fn previous_stack_pointer_pointer_east(&mut self) -> *mut *mut u64 {
        self.slot_pointer_east(0) as *mut *mut u64
    }

    /** Pointer to where the previous (east) stack pointer is saved in a west frame */
    unsafe fn previous_stack_pointer_pointer_west(&mut self) -> *mut *mut u64 {
        self.slot_pointer_west(0) as *mut *mut u64
    }

    /** Pointer to where the previous (west) frame pointer is saved in an east frame */
    unsafe fn previous_frame_pointer_pointer_east(&mut self) -> *mut *mut u64 {
        self.slot_pointer_east(1) as *mut *mut u64
    }

    /** Pointer to where the previous (east) frame pointer is saved in a west frame */
    unsafe fn previous_frame_pointer_pointer_west(&mut self) -> *mut *mut u64 {
        self.slot_pointer_west(1) as *mut *mut u64
    }

    /** Bump the stack pointer for an east frame to make space for an allocation */
    unsafe fn raw_alloc_east(&mut self, words: usize) -> *mut u64 {
        self.stack_pointer = self.stack_pointer.sub(words);
        self.stack_pointer
    }

    /** Bump the stack pointer for a west frame to make space for an allocation */
    unsafe fn raw_alloc_west(&mut self, words: usize) -> *mut u64 {
        let alloc = self.stack_pointer;
        self.stack_pointer = self.stack_pointer.add(words);
        alloc
    }

    /** Allocate space for an indirect pointer in an east frame */
    unsafe fn indirect_alloc_east(&mut self, words: usize) -> *mut u64 {
        self.raw_alloc_east(words + 2)
    }

    /** Allocate space for an indirect pointer in a west frame */
    unsafe fn indirect_alloc_west(&mut self, words: usize) -> *mut u64 {
        self.raw_alloc_west(words + 2)
    }

    /** Allocate space for an indirect pointer in a stack frame */
    unsafe fn indirect_alloc(&mut self, words: usize) -> *mut u64 {
        match &self.polarity {
            Polarity::East => self.indirect_alloc_east(words),
            Polarity::West => self.indirect_alloc_west(words),
        }
    }

    unsafe fn struct_alloc_east<T>(&mut self, count: usize) -> *mut T {
        self.raw_alloc_east(word_size_of::<T>() * count) as *mut T
    }

    unsafe fn struct_alloc_west<T>(&mut self, count: usize) -> *mut T {
        self.raw_alloc_west(word_size_of::<T>() * count) as *mut T
    }

    pub unsafe fn struct_alloc<T>(&mut self, count: usize) -> *mut T {
        match &self.polarity {
            Polarity::East => self.struct_alloc_east::<T>(count),
            Polarity::West => self.struct_alloc_west::<T>(count),
        }
    }

    /** Allocate space for an alloc::Layout in a stack frame */
    unsafe fn layout_alloc(&mut self, layout: Layout) -> *mut u64 {
        assert!(layout.align() <= 64, "layout alignment must be <= 64");
        match &self.polarity {
            Polarity::East => self.raw_alloc_east((layout.size() + 7) >> 3),
            Polarity::West => self.raw_alloc_west((layout.size() + 7) >> 3),
        }
    }

    /** Copy a result noun and its subnouns from an east frame to its parent west frame
     *
     * This is a fairly standard copying collector algorithm where the from arena is the current
     * (east) frame, and the to arena is the parent (west) frame.
     *
     * There can be references outside the current frame, but since only the current frame will be
     * discarded these can be left in place and not copied. Since there are no recursive or mutable
     * references, there cannot be references from outside the current frame into the current
     * frame. Thus, once we have copied out nouns which are reachable from the given result noun
     * and are in the current frame, we are done.
     *
     * Since our to-space is the previous frame, we maintain a work stack at the end of the current
     * frame, past the allocations. This is inverse from when we do a noun traversal generally,
     * where we may want to allocate on the current frame, so we maintain a work stack adjacent to
     * the previous frame.
     */
    unsafe fn copy_east(&mut self, noun: &mut Noun) {
        let noun_ptr = noun as *mut Noun;
        let work_start = self.stack_pointer;
        let mut other_stack_pointer = *(self.previous_stack_pointer_pointer_east());
        self.stack_pointer = self.stack_pointer.sub(2);
        *(self.stack_pointer as *mut Noun) = *noun;
        *(self.stack_pointer.add(1) as *mut *mut Noun) = noun_ptr;
        loop {
            if self.stack_pointer == work_start {
                break;
            }

            // Pop a noun to copy from the stack
            let next_noun = *(self.stack_pointer as *const Noun);
            let next_dest = *(self.stack_pointer.add(1) as *const *mut Noun);
            self.stack_pointer = self.stack_pointer.add(2);

            // If it's a direct atom, just write it to the destination
            // Otherwise we have allocations to make
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
                            if (allocated.to_raw_pointer() as *const u64) >= work_start
                                && (allocated.to_raw_pointer() as *const u64) < self.frame_pointer
                            {
                                match allocated.as_either() {
                                    Either::Left(mut indirect) => {
                                        // Make space for the atom
                                        let new_indirect_alloc = other_stack_pointer;
                                        other_stack_pointer =
                                            other_stack_pointer.add(indirect_raw_size(indirect));

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
                                        let new_cell_alloc = other_stack_pointer as *mut CellMemory;
                                        other_stack_pointer =
                                            other_stack_pointer.add(word_size_of::<CellMemory>());

                                        // Copy the cell metadata
                                        (*new_cell_alloc).metadata =
                                            (*cell.to_raw_pointer()).metadata;

                                        // Push the tail and the head to the work stack
                                        self.stack_pointer = self.stack_pointer.sub(4);
                                        *(self.stack_pointer as *mut Noun) = cell.tail();
                                        *(self.stack_pointer.add(1) as *mut *mut Noun) =
                                            &mut (*new_cell_alloc).tail;
                                        *(self.stack_pointer.add(2) as *mut Noun) = cell.head();
                                        *(self.stack_pointer.add(3) as *mut *mut Noun) =
                                            &mut (*new_cell_alloc).head;

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
        *self.previous_stack_pointer_pointer_east() = other_stack_pointer;
        // assert_acyclic!(*noun);
    }

    /** Copy a result noun and its subnouns from a west frame to its parent east frame
     *
     * This is a fairly standard copying collector algorithm where the from arena is the current
     * (west) frame, and the to arena is the parent (east) frame.
     *
     * There can be references outside the current frame, but since only the current frame will be
     * discarded these can be left in place and not copied. Since there are no recursive or mutable
     * references, there cannot be references from outside the current frame into the current
     * frame. Thus, once we have copied out nouns which are reachable from the given result noun
     * and are in the current frame, we are done.
     *
     * Since our to-space is the previous frame, we maintain a work stack at the end of the current
     * frame, past the allocations. This is inverse from when we do a noun traversal generally,
     * where we may want to allocate on the current frame, so we maintain a work stack adjacent to
     * the previous frame.
     */
    unsafe fn copy_west(&mut self, noun: &mut Noun) {
        let noun_ptr = noun as *mut Noun;
        let work_start = self.stack_pointer;
        let mut other_stack_pointer = *(self.previous_stack_pointer_pointer_west());
        self.stack_pointer = self.stack_pointer.add(2);
        *(self.stack_pointer.sub(2) as *mut Noun) = *noun;
        *(self.stack_pointer.sub(1) as *mut *mut Noun) = noun_ptr;
        loop {
            if self.stack_pointer == work_start {
                break;
            }

            // Pop a noun to copy from the stack
            let next_noun = *(self.stack_pointer.sub(2) as *const Noun);
            let next_dest = *(self.stack_pointer.sub(1) as *const *mut Noun);
            self.stack_pointer = self.stack_pointer.sub(2);

            // If it's a direct atom, just write it to the destination.
            // Otherwise we have allocations to make.
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
                            if (allocated.to_raw_pointer() as *const u64) < work_start
                                && (allocated.to_raw_pointer() as *const u64) >= self.frame_pointer
                            {
                                match allocated.as_either() {
                                    Either::Left(mut indirect) => {
                                        // Make space for the atom
                                        other_stack_pointer =
                                            other_stack_pointer.sub(indirect_raw_size(indirect));
                                        let new_indirect_alloc = other_stack_pointer;

                                        // Indirect atoms can be copied directly
                                        copy_nonoverlapping(
                                            indirect.to_raw_pointer(),
                                            new_indirect_alloc,
                                            indirect_raw_size(indirect),
                                        );

                                        // Set a forwarding pointer so we don't create duplicates
                                        // from other references
                                        indirect.set_forwarding_pointer(new_indirect_alloc);

                                        *next_dest =
                                            IndirectAtom::from_raw_pointer(new_indirect_alloc)
                                                .as_noun();
                                    }
                                    Either::Right(mut cell) => {
                                        // Make space for the cell
                                        other_stack_pointer =
                                            other_stack_pointer.sub(word_size_of::<CellMemory>());
                                        let new_cell_alloc = other_stack_pointer as *mut CellMemory;

                                        // Copy the cell metadata
                                        (*new_cell_alloc).metadata =
                                            (*cell.to_raw_pointer()).metadata;

                                        *(self.stack_pointer as *mut Noun) = cell.tail();
                                        *(self.stack_pointer.add(1) as *mut *mut Noun) =
                                            &mut (*new_cell_alloc).tail;
                                        *(self.stack_pointer.add(2) as *mut Noun) = cell.head();
                                        *(self.stack_pointer.add(3) as *mut *mut Noun) =
                                            &mut (*new_cell_alloc).head;
                                        self.stack_pointer = self.stack_pointer.add(4);

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
        *self.previous_stack_pointer_pointer_west() = other_stack_pointer;
        assert_acyclic!(*noun);
    }

    /** Copy out to the PMA
     *
     * See copy_east/west for inline comments
     */
    pub unsafe fn copy_pma(&mut self, noun: &mut Noun) {
        assert!(self.polarity == Polarity::West);
        let work_start = self.stack_pointer;
        self.stack_pointer = self.stack_pointer.add(2);
        *(self.stack_pointer.sub(2) as *mut Noun) = *noun;
        *(self.stack_pointer.sub(1) as *mut *mut Noun) = noun as *mut Noun;
        loop {
            if self.stack_pointer == work_start {
                break;
            }

            let next_noun = *(self.stack_pointer.sub(2) as *const Noun);
            let next_dest = *(self.stack_pointer.sub(1) as *const *mut Noun);
            self.stack_pointer = self.stack_pointer.sub(2);

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

                                    *(self.stack_pointer as *mut Noun) = cell.tail();
                                    *(self.stack_pointer.add(1) as *mut *mut Noun) =
                                        &mut (*new_cell_alloc).tail;
                                    *(self.stack_pointer.add(2) as *mut Noun) = cell.head();
                                    *(self.stack_pointer.add(3) as *mut *mut Noun) =
                                        &mut (*new_cell_alloc).head;
                                    self.stack_pointer = self.stack_pointer.add(4);

                                    cell.set_forwarding_pointer(new_cell_alloc);

                                    *next_dest = Cell::from_raw_pointer(new_cell_alloc).as_noun();
                                }
                            }
                        }
                    }
                },
            }
        }
        assert_acyclic!(*noun);
    }

    pub fn frame_size(&self) -> usize {
        match self.polarity {
            Polarity::East => self.frame_pointer as usize - self.stack_pointer as usize,
            Polarity::West => self.stack_pointer as usize - self.frame_pointer as usize,
        }
    }

    unsafe fn pop_east(&mut self) {
        self.stack_pointer = *self.previous_stack_pointer_pointer_east();
        self.frame_pointer = *self.previous_frame_pointer_pointer_east();
        self.polarity = Polarity::West;
    }

    unsafe fn pop_west(&mut self) {
        self.stack_pointer = *self.previous_stack_pointer_pointer_west();
        self.frame_pointer = *self.previous_frame_pointer_pointer_west();
        self.polarity = Polarity::East;
    }

    /** Pop a stack frame. If references to stack allocated functions are maintained past a pop,
     * then call `preserve()` to ensure those objects are copied out.
     */
    pub unsafe fn pop(&mut self) {
        match &self.polarity {
            Polarity::East => self.pop_east(),
            Polarity::West => self.pop_west(),
        }
    }

    pub unsafe fn preserve<T: Preserve>(&mut self, x: &mut T) {
        x.preserve(self);
    }

    /** Push a frame onto the west stack with 0 or more local variable slots.
     *
     * (The method is `push_east` because the naming convention refers to the beginning state of
     * the stack, not the final state.)
     */
    unsafe fn push_east(&mut self, num_locals: usize) {
        let previous_stack_pointer: *mut u64 = *self.previous_stack_pointer_pointer_east();
        *previous_stack_pointer = self.stack_pointer as u64;
        *(previous_stack_pointer.add(1)) = self.frame_pointer as u64;
        self.stack_pointer = previous_stack_pointer.add(num_locals + 2);
        self.frame_pointer = previous_stack_pointer;
        self.polarity = Polarity::West;
    }

    /** Push a frame onto the east stack with 0 or more local variable slots.
     *
     * (The method is `push_west` because the naming convention refers to the beginning state of the
     * stack, not the final state.)
     */
    unsafe fn push_west(&mut self, num_locals: usize) {
        let previous_stack_pointer: *mut u64 = *self.previous_stack_pointer_pointer_west();
        *(previous_stack_pointer.sub(1)) = self.stack_pointer as u64;
        *(previous_stack_pointer.sub(2)) = self.frame_pointer as u64;
        self.stack_pointer = previous_stack_pointer.sub(num_locals + 2);
        self.frame_pointer = previous_stack_pointer;
        self.polarity = Polarity::East;
    }

    /** Push a frame onto the stack with 0 or more local variable slots. */
    pub fn push(&mut self, num_locals: usize) {
        unsafe {
            match &self.polarity {
                Polarity::East => self.push_east(num_locals),
                Polarity::West => self.push_west(num_locals),
            }
        }
    }
}

/** Unifying equality compares nouns for equality structurally. It *unifies* noun representations
 * by rewriting a reference to the more junior (resident in a more recently allocated stack frame)
 * noun to a reference to the more senior noun, but does so partially and recursively, so as
 * subnouns are discovered to be equal, they too are unified. Thus even if the overall result is
 * disequality, some equal subnouns may have been unified.
 *
 * This function is unsafe because it demands that all atoms be normalized: direct and indirect atoms
 * will be considered non-equal without comparing their values, and indirects of different sizes
 * will be considered non-equal.
 */
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
    stack.push(1);
    stack.save_prev_stack_pointer_to_local(0);
    *(stack.alloc_in_previous_frame()) = (a, b);
    loop {
        if stack.prev_stack_pointer_equals_local(0) {
            break;
        };
        let (x, y): (*mut Noun, *mut Noun) = *(stack.top_in_previous_frame());
        if (*x).raw_equals(*y) {
            stack.reclaim_in_previous_frame::<(*mut Noun, *mut Noun)>();
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
                        stack.reclaim_in_previous_frame::<(*mut Noun, *mut Noun)>();
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
                        stack.reclaim_in_previous_frame::<(*mut Noun, *mut Noun)>();
                        continue;
                    } else {
                        /* THIS ISN'T AN INFINITE LOOP
                         * If we discover a disequality in either side, we will
                         * short-circuit the entire loop and reset the work stack.
                         *
                         * If both sides are equal, then we will discover pointer
                         * equality when we return and unify the cell.
                         */
                        *(stack.alloc_in_previous_frame()) =
                            (x_cell.tail_as_mut(), y_cell.tail_as_mut());
                        *(stack.alloc_in_previous_frame()) =
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
    stack.restore_prev_stack_pointer_from_local(0);
    stack.pop();
    assert_acyclic!(*a);
    assert_acyclic!(*b);
    (*a).raw_equals(*b)
}

unsafe fn senior_pointer_first<T>(
    stack: &NockStack,
    a: *const T,
    b: *const T,
) -> (*const T, *const T) {
    let mut polarity = stack.polarity;
    let mut frame_pointer = stack.frame_pointer as *const u64;
    let (mut high_pointer, mut low_pointer) = match polarity {
        Polarity::East => (
            stack.frame_pointer as *const T,
            stack.stack_pointer as *const T,
        ),
        Polarity::West => (
            stack.stack_pointer as *const T,
            stack.frame_pointer as *const T,
        ),
    };
    loop {
        match (
            a < high_pointer && a >= low_pointer,
            b < high_pointer && b >= low_pointer,
        ) {
            (true, true) => break (a, b), // both pointers are in the same frame, pick arbitrarily
            (true, false) => break (b, a), // a is in the frame, b is not, so b is senior
            (false, true) => break (a, b), // b is in the frame, a is not, so a is senior
            (false, false) => {
                // chase up the stack
                if (frame_pointer as *const u64) == stack.start {
                    // we found the top of the stack!
                    break (a, b); // both are out of the stack, pick arbitrarily
                } else {
                    match polarity {
                        Polarity::East => {
                            high_pointer = *(frame_pointer.sub(1)) as *const T;
                            low_pointer = *(frame_pointer.sub(2)) as *const T;
                            frame_pointer = *(frame_pointer.sub(2)) as *const u64;
                            polarity = Polarity::West;
                            continue;
                        }
                        Polarity::West => {
                            high_pointer = *frame_pointer.add(1) as *const T;
                            low_pointer = *frame_pointer as *const T;
                            frame_pointer = *(frame_pointer.add(1)) as *const u64;
                            polarity = Polarity::East;
                            continue;
                        }
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
        match stack.polarity {
            Polarity::East => {
                stack.copy_east(self);
            }
            Polarity::West => {
                stack.copy_west(self);
            }
        }
    }
}

impl Stack for NockStack {
    unsafe fn alloc_layout(&mut self, layout: Layout) -> *mut u64 {
        self.layout_alloc(layout)
    }
}
