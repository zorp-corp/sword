use crate::assert_acyclic;
use crate::assert_no_forwarding_pointers;
use crate::assert_no_junior_pointers;
use crate::noun::{Atom, Cell, CellMemory, IndirectAtom, Noun, NounAllocator};
use assert_no_alloc::permit_alloc;
use either::Either::{self, Left, Right};
use ibig::Stack;
use memmap::MmapMut;
use thiserror::Error;
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

/** Utility function to compute the raw memory usage of an [IndirectAtom] */
fn indirect_raw_size(atom: IndirectAtom) -> usize {
    debug_assert!(atom.size() > 0);
    atom.size() + 2
}

#[derive(Debug, Clone)]
pub struct MemoryState {
    pub intended_alloc_words: Option<usize>,
    pub frame_pointer: usize,
    pub stack_pointer: usize,
    pub alloc_pointer: usize,
    pub prev_stack_pointer: usize,
    // pub prev_frame_pointer: usize,
    pub prev_alloc_pointer: usize,
    pub pc: bool,
}

/// Error type for when a potential allocation would cause an OOM error
#[derive(Debug, Clone)]
pub struct OutOfMemoryError(pub MemoryState, pub Allocation,);

/// Error type for allocation errors in [NockStack]
#[derive(Debug, Clone, Error)]
pub enum AllocationError {
    #[error("Out of memory: {0:?}")]
    OutOfMemory(OutOfMemoryError),
    #[error("Cannot allocate in copy phase: {0:?}")]
    CannotAllocateInPreCopy(MemoryState),
    // No slots being available is always a programming error, just panic.
    // #[error("No slots available")]
    // NoSlotsAvailable,
}

impl From<AllocationError> for std::io::Error {
    fn from(_e: AllocationError) -> std::io::Error {
        std::io::ErrorKind::OutOfMemory.into()
    }
}

pub type AllocResult<T> = core::result::Result<T, AllocationError>;

#[derive(Debug, Clone, Copy)]
pub enum ArenaOrientation {
    /// stack_pointer < alloc_pointer
    /// stack_pointer increases on push
    /// frame_pointer increases on push
    /// alloc_pointer decreases on alloc
    West,
    /// stack_pointer > alloc_pointer
    /// stack_pointer decreases on push
    /// frame_pointer decreases on push
    /// alloc_pointer increases on alloc
    East,
}

#[derive(Debug, Clone, Copy)]
pub enum AllocationType {
    /// alloc pointer moves
    Alloc,
    /// stack pointer moves
    Push,
    /// On a frame push, the frame pointer becomes the current_alloc_pointer (+/- words),
    /// the stack pointer is set to the value of the new frame pointer, and the alloc pointer
    /// is set to the pre-frame-push stack pointer.
    FramePush,
    /// To check for a valid slot_pointer you need to check the space between frame pointer
    /// and previous alloc pointer and then subtract RESERVED
    SlotPointer,
    /// Allocate in the previous stack frame
    AllocPreviousFrame,
}

impl AllocationType {
    pub fn is_alloc_previous_frame(&self) -> bool {
        matches!(self, AllocationType::AllocPreviousFrame)
    }
}

// unsafe {
//     self.frame_pointer = if self.is_west() {
//         current_alloc_pointer.sub(words)
//     } else {
//         current_alloc_pointer.add(words)
//     };
//     self.alloc_pointer = current_stack_pointer;
//     self.stack_pointer = self.frame_pointer;
//     *(self.slot_pointer(FRAME)) = current_frame_pointer as u64;
//     *(self.slot_pointer(STACK)) = current_stack_pointer as u64;
//     *(self.slot_pointer(ALLOC)) = current_alloc_pointer as u64;

/// Non-size parameters for validating an allocation
#[derive(Debug, Clone)]
pub struct Allocation {
    pub orientation: ArenaOrientation,
    pub alloc_type: AllocationType,
    pub pc: bool,
}

#[derive(Debug, Clone)]
pub enum Direction {
    Increasing,
    Decreasing,
}

/// A stack for Nock computation, which supports stack allocation and delimited copying collection
/// for returned nouns
#[allow(dead_code)] // We need the memory field to keep our memory from being unmapped
pub struct NockStack {
    /// The base pointer
    start: *const u64,
    /// The size of the memory region
    size: usize,
    /// Base pointer for the current stack frame. Accesses to slots are computed from this base.
    frame_pointer: *mut u64,
    /// Stack pointer for the current stack frame.
    stack_pointer: *mut u64,
    /// Alloc pointer for the current stack frame.
    alloc_pointer: *mut u64,
    /// MMap which must be kept alive as long as this [NockStack] is
    memory: MmapMut,
    /// PMA from which we will copy into the [NockStack]
    /// Whether or not [`Self::pre_copy()`] has been called on the current stack frame.
    pc: bool,
}

impl NockStack {
    /**  Initialization:
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
            *frame_pointer.sub(ALLOC + 1) = start as u64; // "alloc pointer" from "previous" frame
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

    // pub fn middle_of_stack(&self) -> *const u64 {
    //     // is that right? off by one?
    //     unsafe { self.start.add(self.size >> 1) }
    // }

    fn memory_state(&self, words: Option<usize>) -> MemoryState {
        MemoryState {
            intended_alloc_words: words,
            frame_pointer: self.frame_pointer as usize,
            stack_pointer: self.stack_pointer as usize,
            alloc_pointer: self.alloc_pointer as usize,
            prev_stack_pointer: unsafe { *self.prev_stack_pointer_pointer() as usize },
            // prev_frame_pointer: unsafe { *self.prev_frame_pointer_pointer() as usize },
            prev_alloc_pointer: unsafe { *self.prev_alloc_pointer_pointer() as usize },
            pc: self.pc,
        }
    }

    fn cannot_alloc_in_pc(&self, size: Option<usize>) -> AllocationError {
        AllocationError::CannotAllocateInPreCopy(self.memory_state(size))
    }

    fn out_of_memory(&self, alloc: Allocation, words: Option<usize>) -> AllocationError {
        AllocationError::OutOfMemory(OutOfMemoryError(self.memory_state(words), alloc))
    }

    pub(crate) fn get_alloc_config(&self, alloc_type: AllocationType) -> Allocation {
        Allocation {
            orientation: if self.is_west() {
                ArenaOrientation::West
            } else {
                ArenaOrientation::East
            },
            alloc_type,
            pc: self.pc,
        }
    }

    // When frame_pointer < alloc_pointer, the frame is West
    // West frame layout:
    // - start
    // - *prev_alloc_ptr
    // - frame_pointer
    // - stack_pointer
    // - (middle)
    // - alloc_pointer
    // - *prev_stack_ptr
    // - *prev_frame_ptr
    // - end
    // East frame layout:
    // - start
    // - *prev_frame_ptr
    // - *prev_stack_ptr
    // - alloc_pointer
    // - (middle)
    // - stack_pointer
    // - frame_pointer
    // - *prev_alloc_ptr
    // - end
    // sometimes the stack pointer is moving, sometimes the alloc pointer is moving
    // if you're allocating you're just bumping the alloc pointer
    // pushing a frame is more complicated
    // it's fine to cross the middle of the stack, it's not fine for them to cross each other
    // push vs. frame_push
    // push_east/push_west use prev_alloc_pointer_pointer instead of alloc_pointer when self.pc is true
    // Species of allocation: alloc, push, frame_push
    // Size modifiers: raw, indirect, struct, layout
    // Directionality parameters: (East/West), (Stack/Alloc), (pc: true/false)
    // Types of size: word (words: usize)
    /// Check if an allocation or pointer retrieval indicates an invalid request or an invalid state
    pub fn alloc_would_oom_(&self, alloc: Allocation, words: usize) -> AllocResult<()> {
        let memory_state = self.memory_state(Some(words));
        println!("alloc_would_oom_: self.memory_state(): {:#?}, self:is_west(): {}", memory_state, self.is_west());
        println!("alloc_would_oom_: alloc: {:#?}", alloc);
        if self.pc && !alloc.alloc_type.is_alloc_previous_frame() {
            return Err(self.cannot_alloc_in_pc(Some(words)));
        }
        // When self.pc is true
        // west:
        // *prev_alloc_ptr + size <= noun_ptr
        let bytes = words * 8;
        // east:
        // noun_ptr <= *prev_alloc_ptr - size
        // West: the stack pointer must not overlap the alloc pointer
        let (target_point, limit_point, direction) = match (alloc.alloc_type, alloc.orientation) {
            // West + Alloc, alloc is decreasing
            (AllocationType::Alloc, ArenaOrientation::West) => {
                let start_point = self.alloc_pointer as usize;
                let limit_point = self.stack_pointer as usize;
                let target_point = start_point - bytes;
                (target_point, limit_point, Direction::Decreasing)
            },
            // East + Alloc, alloc is increasing
            (AllocationType::Alloc, ArenaOrientation::East) => {
                let start_point = self.alloc_pointer as usize;
                let limit_point = self.stack_pointer as usize;
                let target_point = start_point + bytes;
                (target_point, limit_point, Direction::Increasing)
            },
            // West + Push, stack is increasing
            (AllocationType::Push, ArenaOrientation::West) => {
                let start_point = self.stack_pointer as usize;
                let limit_point = self.alloc_pointer as usize;
                let target_point = start_point + bytes;
                (target_point, limit_point, Direction::Increasing)
            },
            // East + Push, stack is decreasing
            (AllocationType::Push, ArenaOrientation::East) => {
                let start_point = self.stack_pointer as usize;
                let limit_point = self.alloc_pointer as usize;
                let target_point = start_point - bytes;
                (target_point, limit_point, Direction::Decreasing)
            },
            // West + FramePush, alloc is decreasing, kinda (TODO: does fp matter?)
            (AllocationType::FramePush, ArenaOrientation::West) => {
                let start_point = self.alloc_pointer as usize;
                let limit_point = self.stack_pointer as usize;
                let target_point = start_point - bytes;
                (target_point, limit_point, Direction::Decreasing)
            },
            // East + FramePush, alloc is increasing, kinda (TODO: does fp matter?)
            (AllocationType::FramePush, ArenaOrientation::East) => {
                let start_point = self.alloc_pointer as usize;
                let limit_point = self.stack_pointer as usize;
                let target_point = start_point + bytes;
                (target_point, limit_point, Direction::Increasing)
            },
            // West + SlotPointer, polarity is reversed because we're getting the prev pointer
            (AllocationType::SlotPointer, ArenaOrientation::West) => {
                let _slots_available = unsafe { self.slots_available().expect("No slots available on slot_pointer alloc check") };
                let start_point = self.frame_pointer as usize;
                println!("start_point: {start_point}");
                let limit_point = unsafe { *self.prev_alloc_pointer_pointer() as usize };
                // When it's west, reserve is RESERVED + 1, otherwise it's just RESERVED.
                let reserve = RESERVED + 1;
                let target_point = start_point - bytes - (reserve * 8);
                (target_point, limit_point, Direction::Increasing)
            },
            // East + SlotPointer, polarity is reversed because we're getting the prev pointer
            (AllocationType::SlotPointer, ArenaOrientation::East) => {
                let _slots_available = unsafe { self.slots_available().expect("No slots available on slot_pointer alloc check") };
                let start_point = self.frame_pointer as usize;
                println!("start_point: {start_point}");
                let limit_point = unsafe { *self.prev_alloc_pointer_pointer() as usize };
                let reserve = RESERVED;
                let target_point = start_point + bytes + (reserve * 8);
                (target_point, limit_point, Direction::Decreasing)
            },
            // The alloc previous frame stuff is like doing a normal alloc but start point is prev alloc and limit pointer is prev stack pointer
            // TODO: Pretty sure this is wrong
            // polarity is reversed because we're getting the prev pointer
            (AllocationType::AllocPreviousFrame, ArenaOrientation::West) => {
                let start_point = unsafe { *self.prev_alloc_pointer_pointer() as usize };
                let limit_point = unsafe { *self.prev_stack_pointer_pointer() as usize };
                let target_point = start_point + bytes;
                (target_point, limit_point, Direction::Increasing)
            },
            // polarity is reversed because we're getting the prev pointer
            (AllocationType::AllocPreviousFrame, ArenaOrientation::East) => {
                let start_point = unsafe { *self.prev_alloc_pointer_pointer() as usize };
                let limit_point = unsafe { *self.prev_stack_pointer_pointer() as usize };
                let target_point = start_point - bytes;
                (target_point, limit_point, Direction::Decreasing)
            },
        };
        println!("bytes: {bytes} alloc_would_oom_: target_point: {target_point}, limit_point: {limit_point}, direction: {direction:#?}");
        match direction {
            Direction::Increasing => {
                if target_point <= limit_point {
                    Ok(())
                } else {
                    Err(self.out_of_memory(alloc, Some(words)))
                }
            },
            Direction::Decreasing => {
                if target_point >= limit_point {
                    Ok(())
                } else {
                    Err(self.out_of_memory(alloc, Some(words)))
                }
            },
        }
    }
    pub fn alloc_would_oom(&self, alloc_type: AllocationType, words: usize) -> AllocResult<()> {
        let alloc = self.get_alloc_config(alloc_type);
        self.alloc_would_oom_(alloc, words)
    }

    /** Resets the NockStack but flipping the top-frame polarity and unsetting PC. Sets the alloc
     * pointer to the "previous" alloc pointer stored in the top frame to keep things "preserved"
     * from the top frame. This allows us to do a copying GC on the top frame without erroneously
     * "popping" the top frame.
     */
    // Pop analogue, doesn't need OOM check.
    pub unsafe fn flip_top_frame(&mut self, top_slots: usize) -> () {
        // Assert that we are at the top
        assert!((*self.prev_frame_pointer_pointer()).is_null());
        assert!((*self.prev_stack_pointer_pointer()).is_null());
        let new_alloc_pointer = *(self.prev_alloc_pointer_pointer());

        if self.is_west() {
            // new top frame will be east
            let new_frame_pointer = self.start.add(self.size).sub(RESERVED + top_slots) as *mut u64;
            *new_frame_pointer.add(FRAME) = ptr::null::<u64>() as u64;
            *new_frame_pointer.add(STACK) = ptr::null::<u64>() as u64;
            *new_frame_pointer.add(ALLOC) = self.start.add(self.size) as u64;
            self.frame_pointer = new_frame_pointer;
            self.stack_pointer = new_frame_pointer;
            self.alloc_pointer = new_alloc_pointer;
            self.pc = false;
            assert!(!self.is_west());
        } else {
            // new top frame will be west
            let new_frame_pointer = self.start.add(RESERVED + top_slots) as *mut u64;
            *new_frame_pointer.sub(FRAME + 1) = ptr::null::<u64>() as u64;
            *new_frame_pointer.sub(STACK + 1) = ptr::null::<u64>() as u64;
            *new_frame_pointer.sub(ALLOC + 1) = self.start as u64;
            self.frame_pointer = new_frame_pointer;
            self.stack_pointer = new_frame_pointer;
            self.alloc_pointer = new_alloc_pointer;
            self.pc = false;
            assert!(self.is_west());
        };
    }

    /// Resets the NockStack. The top frame is west as in the initial creation of the NockStack.
    // Doesn't need an OOM check, pop analogue
    pub fn reset(&mut self, top_slots: usize) {
        self.frame_pointer = unsafe { self.start.add(RESERVED + top_slots) } as *mut u64;
        self.stack_pointer = self.frame_pointer;
        self.alloc_pointer = unsafe { self.start.add(self.size) } as *mut u64;
        self.pc = false;
        unsafe {
            *self.frame_pointer.sub(FRAME + 1) = ptr::null::<u64>() as u64; // "frame pointer" from "previous" frame
            *self.frame_pointer.sub(STACK + 1) = ptr::null::<u64>() as u64; // "stack pointer" from "previous" frame
            *self.frame_pointer.sub(ALLOC + 1) = self.start as u64; // "alloc pointer" from "previous" frame
            assert!(self.is_west());
        };
    }

    pub fn copying(&self) -> bool {
        self.pc
    }

    /** Current frame pointer of this NockStack */
    pub fn get_frame_pointer(&self) -> *const u64 {
        self.frame_pointer
    }

    /** Current stack pointer of this NockStack */
    pub fn get_stack_pointer(&self) -> *const u64 {
        self.stack_pointer
    }

    /** Current alloc pointer of this NockStack */
    pub fn get_alloc_pointer(&self) -> *const u64 {
        self.alloc_pointer
    }

    /** Current stack pointer of this NockStack */
    pub fn get_stack_pointer_pointer(&self) -> *const *mut u64 {
        &self.stack_pointer
    }

    /** Current alloc pointer of this NockStack */
    pub fn get_alloc_pointer_pointer(&self) -> *const *mut u64 {
        &self.alloc_pointer
    }

    /** Start of the memory range for this NockStack */
    pub fn get_start(&self) -> *const u64 {
        self.start
    }

    /** End of the memory range for this NockStack */
    pub fn get_size(&self) -> usize {
        self.size
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
        let res = if self.is_west() {
            // If we are in a top/west frame, the stack pointer will be null, so our allocation
            // arena was the alloc pointer to the top of the NockStack arena
            if prev.is_null() {
                ptr_u64 >= self.alloc_pointer && ptr_u64 < self.start.add(self.size)
            } else {
                ptr_u64 >= self.alloc_pointer && ptr_u64 < prev
            }
        // If we are in a top/east frame, the stack pointer will be null, so our allocation arena
        // was the alloc pointer to the bottom of the NockStack arena
        } else if prev.is_null() {
            ptr_u64 >= self.start && ptr_u64 < self.alloc_pointer
        } else {
            ptr_u64 >= prev && ptr_u64 < self.alloc_pointer
        };
        res
    }

    pub fn div_rem_nonzero(a: usize, b: std::num::NonZeroUsize) -> (usize, usize) {
        (a / b, a % b)
    }

    fn divide_evenly(divisor: usize, quotient: usize) -> usize {
        let non_zero_quotient = std::num::NonZeroUsize::new(quotient).expect("Quotient cannot be zero, cannot divide by zero");
        let (div, rem) = Self::div_rem_nonzero(divisor, non_zero_quotient);
        assert!(rem == 0);
        div
    }

    unsafe fn slots_available(&self) -> Option<usize> {
        let prev = *self.prev_alloc_pointer_pointer() as usize;
        // For slot pointer we have to add 1 to reserved, but frame_push is just reserved.
        let reserved_bytes = RESERVED * 8;
        let frame_pointer = self.frame_pointer as usize;
        let (left, right) = if self.is_west() {
            (frame_pointer, prev)
        } else {
            (prev, frame_pointer)
        };
        let bytes_difference =
            left.checked_sub(right)
            .and_then(|v| v.checked_sub(reserved_bytes))
            .map(|v| Self::divide_evenly(v, 8));
        bytes_difference
    }

    /** Mutable pointer to a slot in a stack frame: east stack */
    // TODO: slot_pointer_east_: Needs a simple bounds check
    #[cfg(test)]
    unsafe fn slot_pointer_east_(&self, slot: usize) -> AllocResult<*mut u64> {
        let () = self.alloc_would_oom_(Allocation {
            orientation: ArenaOrientation::East,
            alloc_type: AllocationType::SlotPointer,
            pc: self.pc,
        }, slot)?;
        Ok(self.frame_pointer.add(slot))
    }

    /** Mutable pointer to a slot in a stack frame: west stack */
    // TODO: slot_pointer_west_: Needs a simple bounds check
    #[cfg(test)]

    unsafe fn slot_pointer_west_(&self, slot: usize) -> AllocResult<*mut u64> {
        let () = self.alloc_would_oom_(Allocation {
            orientation: ArenaOrientation::West,
            alloc_type: AllocationType::SlotPointer,
            pc: self.pc,
        }, slot)?;
        Ok(self.frame_pointer.sub(slot + 1))
    }

    /** Mutable pointer to a slot in a stack frame: east stack */
    // TODO: slot_pointer_east: Needs a simple bounds check
    unsafe fn slot_pointer_east(&self, slot: usize) -> *mut u64 {
        self.frame_pointer.add(slot)
    }

    /** Mutable pointer to a slot in a stack frame: west stack */
    // TODO: slot_pointer_west: Needs a simple bounds check

    unsafe fn slot_pointer_west(&self, slot: usize) -> *mut u64 {
        self.frame_pointer.sub(slot + 1)
    }

    /// Mutable pointer to a slot in a stack frame
    /// Panics on out-of-bounds conditions
    #[cfg(test)]
    unsafe fn slot_pointer_(&self, slot: usize) -> AllocResult<*mut u64> {
        if self.is_west() {
            self.slot_pointer_west_(slot)
        } else {
            self.slot_pointer_east_(slot)
        }
    }

    /// Mutable pointer to a slot in a stack frame
    /// Panics on out-of-bounds conditions
    // TODO: slot_pointer: Needs a simple bounds check
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
        let res = self.slot_pointer(local + RESERVED);
        res as *mut Noun
    }

    /** Pointer to where the previous frame pointer is saved in a frame */
    unsafe fn prev_frame_pointer_pointer(&self) -> *mut *mut u64 {
        let res = if !self.pc {
            self.slot_pointer(FRAME)
        } else {
            self.free_slot(FRAME)
        };
        res as *mut *mut u64
    }

    /** Pointer to where the previous stack pointer is saved in a frame */
    pub unsafe fn prev_stack_pointer_pointer(&self) -> *mut *mut u64 {
        let res = if !self.pc {
            self.slot_pointer(STACK)
        } else {
            self.free_slot(STACK)
        };
        res as *mut *mut u64
    }

    /** Pointer to where the previous alloc pointer is saved in a frame */
    unsafe fn prev_alloc_pointer_pointer(&self) -> *mut *mut u64 {
        let res = if !self.pc {
            self.slot_pointer(ALLOC)
        } else {
            self.free_slot(ALLOC)
        };
        res as *mut *mut u64
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
    unsafe fn raw_alloc_west(&mut self, words: usize) -> AllocResult<*mut u64> {
        let () = self.alloc_would_oom(AllocationType::Alloc, words)?;
        if self.pc {
            panic!("Allocation during cleanup phase is prohibited.");
        }
        self.alloc_pointer = self.alloc_pointer.sub(words);
        Ok(self.alloc_pointer)
    }

    /** Bump the alloc pointer for an east frame to make space for an allocation */
    unsafe fn raw_alloc_east(&mut self, words: usize) -> AllocResult<*mut u64> {
        let () = self.alloc_would_oom(AllocationType::Alloc, words)?;
        if self.pc {
            panic!("Allocation during cleanup phase is prohibited.");
        }
        let alloc = self.alloc_pointer;
        self.alloc_pointer = self.alloc_pointer.add(words);
        Ok(alloc)
    }

    /** Allocate space for an indirect pointer in a west frame */
    unsafe fn indirect_alloc_west(&mut self, words: usize) -> AllocResult<*mut u64> {
        self.raw_alloc_west(words + 2)
    }

    /** Allocate space for an indirect pointer in an east frame */
    unsafe fn indirect_alloc_east(&mut self, words: usize) -> AllocResult<*mut u64> {
        self.raw_alloc_east(words + 2)
    }

    /** Allocate space for an indirect pointer in a stack frame */
    unsafe fn indirect_alloc(&mut self, words: usize) -> AllocResult<*mut u64> {
        if self.is_west() {
            self.indirect_alloc_west(words)
        } else {
            self.indirect_alloc_east(words)
        }
    }

    /** Allocate space for a struct in a west frame */
    unsafe fn struct_alloc_west<T>(&mut self, count: usize) -> AllocResult<*mut T> {
        let eigen_pointer = self.raw_alloc_west(word_size_of::<T>() * count)?;
        Ok(eigen_pointer as *mut T)
    }

    /** Allocate space for a struct in an east frame */
    unsafe fn struct_alloc_east<T>(&mut self, count: usize) -> AllocResult<*mut T> {
        let eigen_pointer = self.raw_alloc_east(word_size_of::<T>() * count)?;
        Ok(eigen_pointer as *mut T)
    }

    /** Allocate space for a struct in a stack frame */
    pub unsafe fn struct_alloc<T>(&mut self, count: usize) -> AllocResult<*mut T> {
        if self.is_west() {
            self.struct_alloc_west::<T>(count)
        } else {
            self.struct_alloc_east::<T>(count)
        }
    }

    // TODO: raw_alloc_in_previous_frame_west: Add OOM checks here
    unsafe fn raw_alloc_in_previous_frame_west(&mut self, words: usize) -> AllocResult<*mut u64> {
        let () = self.alloc_would_oom_(Allocation { orientation: ArenaOrientation::West, alloc_type: AllocationType::AllocPreviousFrame, pc: self.pc }, words)?;
        // note that the allocation is on the east frame, and thus resembles raw_alloc_east
        let alloc = *self.prev_alloc_pointer_pointer();
        *(self.prev_alloc_pointer_pointer()) = (*(self.prev_alloc_pointer_pointer())).add(words);
        Ok(alloc)
    }

    // TODO: raw_alloc_in_previous_frame_east: Add OOM checks here
    unsafe fn raw_alloc_in_previous_frame_east(&mut self, words: usize) -> AllocResult<*mut u64> {
        let () = self.alloc_would_oom_(Allocation { orientation: ArenaOrientation::East, alloc_type: AllocationType::AllocPreviousFrame, pc: self.pc }, words)?;
        // note that the allocation is on the west frame, and thus resembles raw_alloc_west
        *(self.prev_alloc_pointer_pointer()) = (*(self.prev_alloc_pointer_pointer())).sub(words);
        Ok(*(self.prev_alloc_pointer_pointer()))
    }

    /** Allocate space in the previous stack frame. This calls pre_copy() first to ensure that the
     * stack frame is in cleanup phase, which is the only time we should be allocating in a previous
     * frame. */
    unsafe fn raw_alloc_in_previous_frame(&mut self, words: usize) -> AllocResult<*mut u64> {
        self.pre_copy()?;
        if self.is_west() {
            self.raw_alloc_in_previous_frame_west(words)
        } else {
            self.raw_alloc_in_previous_frame_east(words)
        }
    }

    /** Allocates space in the previous frame for some number of T's. */
    pub unsafe fn struct_alloc_in_previous_frame<T>(&mut self, count: usize) -> AllocResult<*mut T> {
        let res = self.raw_alloc_in_previous_frame(word_size_of::<T>() * count)?;
        Ok(res as *mut T)
    }

    /** Allocate space for an indirect atom in the previous stack frame. */
    unsafe fn indirect_alloc_in_previous_frame(&mut self, words: usize) -> AllocResult<*mut u64> {
        self.raw_alloc_in_previous_frame(words + 2)
    }

    /** Allocate space for an alloc::Layout in a stack frame */
    unsafe fn layout_alloc(&mut self, layout: Layout) -> AllocResult<*mut u64> {
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
     * or not pre_copy() has been called.
     * */
    unsafe fn pre_copy(&mut self) -> AllocResult<()> {
        // pre_copy is intended to be idempotent, so we don't need to do anything if it's already been called
        if !self.pc {
            let is_west = self.is_west();
            let words = if is_west { RESERVED + 1 } else { RESERVED };
            // TODO: pre_copy: Treating pre_copy like a FramePush for OOM checking purposes
            // Is this correct?
            let () = self.alloc_would_oom_(self.get_alloc_config(AllocationType::FramePush), words)?;
            *(self.free_slot(FRAME)) = *(self.slot_pointer(FRAME));
            *(self.free_slot(STACK)) = *(self.slot_pointer(STACK));
            *(self.free_slot(ALLOC)) = *(self.slot_pointer(ALLOC));
            self.pc = true;
            // Change polarity of lightweight stack.
            if is_west {
                self.stack_pointer = self.alloc_pointer.sub(words);
            } else {
                self.stack_pointer = self.alloc_pointer.add(words);
            }
        }
        Ok(())
    }

    unsafe fn copy(&mut self, noun: &mut Noun) -> AllocResult<()> {
        assert_acyclic!(*noun);
        assert_no_forwarding_pointers!(*noun);
        assert_no_junior_pointers!(self, *noun);

        self.pre_copy()?;
        assert!(self.stack_is_empty());
        let noun_ptr = noun as *mut Noun;
        // Add two slots to the lightweight stack
        // Set the first new slot to the noun to be copied
        *(self.push::<Noun>()?) = *noun;
        // Set the second new slot to a pointer to the noun being copied. this is the destination pointer, which will change
        *(self.push::<*mut Noun>()?) = noun_ptr;
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
                                            self.indirect_alloc_in_previous_frame(indirect.size())?;

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
                                            self.struct_alloc_in_previous_frame::<CellMemory>(1)?;

                                        // Copy the cell metadata
                                        (*alloc).metadata = (*cell.to_raw_pointer()).metadata;

                                        // Push the tail and the head to the work stack
                                        *(self.push::<Noun>()?) = cell.tail();
                                        *(self.push::<*mut Noun>()?) = &mut (*alloc).tail;
                                        *(self.push::<Noun>()?) = cell.head();
                                        *(self.push::<*mut Noun>()?) = &mut (*alloc).head;

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
        Ok(())
    }

    // Doesn't need an OOM check, just an assertion. We expect it to panic.
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

    // Doesn't need an OOM check, just an assertion. We expect it to panic.
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

    // Note re: #684: We don't need OOM checks on de-alloc
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

    pub unsafe fn preserve<T: Preserve>(&mut self, x: &mut T) -> AllocResult<()> {
        x.preserve(self)
    }

    /**  Pushing
     *  When pushing, we swap the stack and alloc pointers, set the frame pointer to be the stack
     *  pointer, move both frame and stack pointer by number of locals (eastward for west frames,
     *  westward for east frame), and then save the old stack/frame/alloc pointers in slots
     *  adjacent to the frame pointer.
     */

    /** Push a frame onto the stack with 0 or more local variable slots. */
    /// This computation for num_locals is done in the east/west variants, but roughly speaking it's the input n words + 3 for prev frame alloc/stack/frame pointers
    pub fn frame_push(&mut self, num_locals: usize) -> AllocResult<()> {
        if self.pc {
            panic!("frame_push during cleanup phase is prohibited.");
        }
        let words = num_locals + RESERVED;
        let () = self.alloc_would_oom(AllocationType::FramePush, words)?;

        let current_frame_pointer = self.frame_pointer;
        let current_stack_pointer = self.stack_pointer;
        let current_alloc_pointer = self.alloc_pointer;
        unsafe {
            self.frame_pointer = if self.is_west() {
                current_alloc_pointer.sub(words)
            } else {
                current_alloc_pointer.add(words)
            };
            self.alloc_pointer = current_stack_pointer;
            self.stack_pointer = self.frame_pointer;
            *(self.slot_pointer(FRAME)) = current_frame_pointer as u64;
            *(self.slot_pointer(STACK)) = current_stack_pointer as u64;
            *(self.slot_pointer(ALLOC)) = current_alloc_pointer as u64;
        }
        Ok(())
    }

    /** Run a closure inside a frame, popping regardless of the value returned by the closure.
     * This is useful for writing fallible computations with the `?` operator.
     *
     * Note that results allocated on the stack *must* be `preserve()`d by the closure.
     */
    pub unsafe fn with_frame<F, O>(&mut self, num_locals: usize, f: F) -> AllocResult<O>
    where
        F: FnOnce(&mut NockStack) -> O,
        O: Preserve,
    {
        self.frame_push(num_locals)?;
        let mut ret = f(self);
        ret.preserve(self)?;
        self.frame_pop();
        Ok(ret)
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
    pub unsafe fn push<T>(&mut self) -> AllocResult<*mut T> {
        if self.is_west() && !self.pc || !self.is_west() && self.pc {
            self.push_west::<T>()
        } else {
            self.push_east::<T>()
        }
    }

    /// Push onto a west-oriented lightweight stack, moving the stack_pointer.
    unsafe fn push_west<T>(&mut self) -> AllocResult<*mut T> {
        let words = word_size_of::<T>();
        let () = self.alloc_would_oom_(Allocation { orientation: ArenaOrientation::West, alloc_type: AllocationType::Push, pc: self.pc }, words)?;
        let ap = if self.pc {
            *(self.prev_alloc_pointer_pointer())
        } else {
            self.alloc_pointer
        };
        let alloc = self.stack_pointer;
        let new_sp = self.stack_pointer.add(words);
        if new_sp > ap {
            // Previously we would return a null pointer in this error case.
            // Now that we have the alloc_would_oom methods, this shouldn't ever happen.
            // ptr::null_mut()
            // If it does, somehow, happen, we should panic.
            panic!("Out of memory, alloc_would_oom didn't catch it. memory_state: {:#?}", self.memory_state(Some(words)));
        } else {
            self.stack_pointer = new_sp;
            Ok(alloc as *mut T)
        }
    }

    /// Push onto an east-oriented ligthweight stack, moving the stack_pointer
    unsafe fn push_east<T>(&mut self) -> AllocResult<*mut T> {
        let words = word_size_of::<T>();
        let () = self.alloc_would_oom_(Allocation { orientation: ArenaOrientation::East, alloc_type: AllocationType::Push, pc: self.pc }, words)?;
        let ap = if self.pc {
            *(self.prev_alloc_pointer_pointer())
        } else {
            self.alloc_pointer
        };
        let alloc = self.stack_pointer.sub(words);
        if alloc < ap {
            // Previously we would return a null pointer in this error case.
            // Now that we have the alloc_would_oom methods, this shouldn't ever happen.
            // ptr::null_mut()
            // If it does, somehow, happen, we should panic.
            panic!("Out of memory, alloc_would_oom didn't catch it. memory_state: {:#?}", self.memory_state(Some(words)));
        } else {
            self.stack_pointer = alloc;
            Ok(alloc as *mut T)
        }
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
     // Re: #684: We don't need OOM checks on pop
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

    /**
     * Debugging
     *
     * The below functions are useful for debugging NockStack issues. */

    /**
     * Walk down the NockStack, printing frames. Absolutely no safety checks are peformed, as the
     * purpose is to discover garbage data; just print pointers until the bottom of the NockStack
     * (i.e. a null frame pointer) is encountered. Possible to crash, if a frame pointer gets
     * written over.
     */
    pub fn print_frames(&mut self) {
        let mut fp = self.frame_pointer;
        let mut sp = self.stack_pointer;
        let mut ap = self.alloc_pointer;
        let mut c = 0u64;

        eprintln!("\r start = {:p}", self.start);

        loop {
            c += 1;

            eprintln!("\r {}:", c);
            eprintln!("\r frame_pointer = {:p}", fp);
            eprintln!("\r stack_pointer = {:p}", sp);
            eprintln!("\r alloc_pointer = {:p}", ap);

            if fp.is_null() {
                break;
            }

            unsafe {
                if fp < ap {
                    sp = *(fp.sub(STACK + 1) as *mut *mut u64);
                    ap = *(fp.sub(ALLOC + 1) as *mut *mut u64);
                    fp = *(fp.sub(FRAME + 1) as *mut *mut u64);
                } else {
                    sp = *(fp.add(STACK) as *mut *mut u64);
                    ap = *(fp.add(ALLOC) as *mut *mut u64);
                    fp = *(fp.add(FRAME) as *mut *mut u64);
                }
            }
        }
    }

    /**
     * Sanity check every frame of the NockStack. Most useful paired with a gdb session set to
     * catch rust_panic.
     */
    // #684: Don't need OOM checks here
    pub fn assert_sane(&mut self) {
        let start = self.start;
        let limit = unsafe { self.start.add(self.size) };
        let mut fp = self.frame_pointer;
        let mut sp = self.stack_pointer;
        let mut ap = self.alloc_pointer;
        let mut ought_west: bool = fp < ap;

        loop {
            // fp is null iff sp is null
            assert!(!(fp.is_null() ^ sp.is_null()));

            // ap should never be null
            assert!(!ap.is_null());

            if fp.is_null() {
                break;
            }

            // all pointers must be between start and size
            assert!(fp as *const u64 >= start);
            assert!(fp as *const u64 <= limit);
            assert!(sp as *const u64 >= start);
            assert!(sp as *const u64 <= limit);
            assert!(ap as *const u64 >= start);
            assert!(ap as *const u64 <= limit);

            // frames should flip between east-west correctly
            assert!((fp < ap) == ought_west);

            // sp should be between fp and ap
            if ought_west {
                assert!(sp >= fp);
                assert!(sp < ap);
            } else {
                assert!(sp <= fp);
                assert!(sp > ap);
            }

            unsafe {
                if ought_west {
                    sp = *(fp.sub(STACK + 1) as *mut *mut u64);
                    ap = *(fp.sub(ALLOC + 1) as *mut *mut u64);
                    fp = *(fp.sub(FRAME + 1) as *mut *mut u64);
                } else {
                    sp = *(fp.add(STACK) as *mut *mut u64);
                    ap = *(fp.add(ALLOC) as *mut *mut u64);
                    fp = *(fp.add(FRAME) as *mut *mut u64);
                }
            }
            ought_west = !ought_west;
        }
    }
}

impl NounAllocator for NockStack {
    unsafe fn alloc_indirect(&mut self, words: usize) -> AllocResult<*mut u64> {
        self.indirect_alloc(words)
    }

    unsafe fn alloc_cell(&mut self) -> AllocResult<*mut CellMemory> {
        self.struct_alloc::<CellMemory>(1)
    }

    unsafe fn alloc_struct<T>(&mut self, count: usize) -> AllocResult<*mut T> {
        self.struct_alloc::<T>(count)
    }
}

/// Immutable, acyclic objects which may be copied up the stack
pub trait Preserve {
    /// Ensure an object will not be invalidated by popping the NockStack
    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()>;
    unsafe fn assert_in_stack(&self, stack: &NockStack);
}

impl Preserve for IndirectAtom {
    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        let size = indirect_raw_size(*self);
        let buf = stack.struct_alloc_in_previous_frame::<u64>(size)?;
        copy_nonoverlapping(self.to_raw_pointer(), buf, size);
        *self = IndirectAtom::from_raw_pointer(buf);
        Ok(())
    }
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_noun_in(self.as_atom().as_noun());
    }
}

impl Preserve for Atom {
    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        match self.as_either() {
            Left(_direct) => {}
            Right(mut indirect) => {
                indirect.preserve(stack)?;
                *self = indirect.as_atom();
            }
        }
        Ok(())
    }
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_noun_in(self.as_noun());
    }
}

impl Preserve for Noun {
    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        stack.copy(self)
    }
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_noun_in(*self);
    }
}

impl Stack for NockStack {
    type AllocError = AllocationError;
    unsafe fn alloc_layout(&mut self, layout: Layout) -> AllocResult<*mut u64> {
        self.layout_alloc(layout)
    }
}

impl<T: Preserve, E: Preserve> Preserve for Result<T, E> {
    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        match self.as_mut() {
            Ok(t_ref) => t_ref.preserve(stack),
            Err(e_ref) => e_ref.preserve(stack),
        }
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        match self.as_ref() {
            Ok(t_ref) => t_ref.assert_in_stack(stack),
            Err(e_ref) => e_ref.assert_in_stack(stack),
        }
    }
}

#[cfg(test)]
mod test {
    use std::iter::FromIterator;

    use super::*;
    use crate::{
        jets::cold::{test::{make_noun_list, make_test_stack}, NounList, Nounable}, mem::NockStack, noun::D, unifying_equality::test::unifying_equality,
    };

    fn test_noun_list_alloc_fn(stack_size: usize, item_count: u64) -> crate::jets::cold::NounableResult<()> {
        unsafe {
            // fails at 512, works at 1024
            // const STACK_SIZE: usize = 1;
            // println!("TEST_SIZE: {}", STACK_SIZE);
            let mut stack = make_test_stack(stack_size);
            // Stack size 1 works until 15 elements, 14 passes, 15 fails.
            // const ITEM_COUNT: u64 = 15;
            let vec = Vec::from_iter(0..item_count);
            let items = vec.iter().map(|&x| D(x)).collect::<Vec<Noun>>();
            let slice = vec.as_slice();
            let noun_list = make_noun_list(&mut stack, slice)?;
            assert!(!noun_list.0.is_null());
            let noun = noun_list.into_noun(&mut stack)?;
            let new_noun_list: NounList =
                <NounList as Nounable>::from_noun::<NockStack>(&mut stack, &noun)?;
            let mut tracking_item_count = 0;
            println!("items: {:?}", items);
            for (a, b) in new_noun_list.zip(items.iter()) {
                let a_ptr = a;
                let b_ptr = &mut b.clone() as *mut Noun;
                let a_val = *a_ptr;
                println!("a: {:?}, b: {:?}", a_val, b);
                assert!(
                    unifying_equality(&mut stack, a_ptr, b_ptr),
                    "Items don't match: {:?} {:?}",
                    a_val,
                    b
                );
                tracking_item_count += 1;
            }
            assert_eq!(tracking_item_count, item_count as usize);
        }
        Ok(())
    }

    // cargo test -p sword test_noun_list_alloc -- --nocapture
    #[test]
    fn test_noun_list_alloc() {
        let should_fail_to_alloc = test_noun_list_alloc_fn(1, 15);
        assert!(should_fail_to_alloc.map_err(|err| err.is_alloc_error()).unwrap_err());
        let should_succeed = test_noun_list_alloc_fn(1, 14);
        assert!(should_succeed.is_ok());
    }

    // cargo test -p sword test_frame_push -- --nocapture
    #[test]
    fn test_frame_push() {
        // fails at 100, passes at 99, top_slots default to 100?
        const PASSES: usize = 99;
        const FAILS: usize = 100;
        let stack_size = 1;
        let mut stack = make_test_stack(stack_size);
        let frame_push_res = stack.frame_push(FAILS);
        assert!(frame_push_res.is_err());
        let mut stack = make_test_stack(stack_size);
        let frame_push_res = stack.frame_push(PASSES);
        assert!(frame_push_res.is_ok());
    }

    // cargo test -p sword test_stack_push -- --nocapture
    #[test]
    fn test_stack_push() {
        let stack_size = 1;
        let mut stack = make_test_stack(stack_size);
        let mut counter = 0;
        // Fails at 102, probably because top_slots is 100?
        while counter < 102 {
            let push_res = unsafe { stack.push::<u64>() };
            assert!(push_res.is_ok(), "Failed to push, counter: {}", counter);
            counter += 1;
        }
        let push_res = unsafe { stack.push::<u64>() };
        assert!(push_res.is_err());
    }

    // cargo test -p sword test_frame_and_stack_push -- --nocapture
    #[test]
    fn test_frame_and_stack_push() {
        let stack_size = 1;
        let mut stack = make_test_stack(stack_size);
        let mut counter = 0;
        while counter < 20 {
            let frame_push_res = stack.frame_push(1);
            assert!(frame_push_res.is_ok(), "Failed to frame_push, counter: {}", counter);
            let push_res: Result<*mut u64, AllocationError> = unsafe { stack.push::<u64>() };
            assert!(push_res.is_ok(), "Failed to push, counter: {}", counter);
            counter += 1;
        }
        let frame_push_res = stack.frame_push(1);
        assert!(frame_push_res.is_err());
        // a single stack u64 push won't cause an error but a frame push will
        let push_res = unsafe { stack.push::<u64>() };
        assert!(push_res.is_ok());
        // pushing an array of 1 u64 will NOT cause an error
        let push_res = unsafe { stack.push::<[u64; 1]>() };
        assert!(push_res.is_ok());
        // pushing an array of 2 u64s WILL cause an error
        let push_res = unsafe { stack.push::<[u64; 2]>() };
        assert!(push_res.is_err());
    }

    // cargo test -p sword test_slot_pointer -- --nocapture
    // Test the slot_pointer checking by pushing frames and slots until we run out of space
    #[test]
    fn test_slot_pointer() {
        let stack_size = 1;
        let mut stack = make_test_stack(stack_size);
        // let push_res: Result<*mut u64, AllocationError> = unsafe { stack.push::<u64>() };
        let frame_push_res = stack.frame_push(1);
        assert!(frame_push_res.is_ok());
        let mut counter = 0;
        while counter < 102 {
            println!("counter: {counter}");
            let slot_pointer_res = unsafe { stack.slot_pointer_(counter) };
            assert!(slot_pointer_res.is_ok(), "Failed to slot_pointer, counter: {}", counter);
            counter += 1;
        }
    }

    // cargo test -p sword test_prev_alloc -- --nocapture
    // Test the alloc in previous frame checking by pushing a frame and then allocating in the previous frame until we run out of space
    #[test]
    fn test_prev_alloc() {
        let stack_size = 1;
        let mut stack = make_test_stack(stack_size);
        println!("\n############## frame push \n");
        let frame_push_res = stack.frame_push(1);
        assert!(frame_push_res.is_ok());
        let mut counter = 0;
        // This should be the same boundary as the repeated stack pushes
        while counter < 102 {
            println!("counter: {counter}");
            let prev_alloc_res = unsafe { stack.raw_alloc_in_previous_frame(1) };
            assert!(prev_alloc_res.is_ok(), "Failed to prev_alloc, counter: {}", counter);
            counter += 1;
        }
        println!("### This next raw_alloc_in_previous_frame should fail ###\n");
        let prev_alloc_res = unsafe { stack.raw_alloc_in_previous_frame(1) };
        assert!(prev_alloc_res.is_err(), "Didn't get expected alloc error, res: {:#?}", prev_alloc_res);
    }
}
