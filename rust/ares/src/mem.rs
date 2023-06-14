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

/** Which side of the two opposing stacks are we working on? */
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Polarity {
    /** Stack growing down from high memory */
    East,
    /** Stack growing up from low memory */
    West,
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
    /** Which side of the stack is the active stack frame on? */
    polarity: Polarity,
    /** Base pointer for the current stack frame. Accesses to slots are computed from this base. */
    frame_pointer: *mut u64,
    /** Alloc pointer for the current stack frame. */
    alloc_pointer: *mut u64,
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
        let alloc_pointer = unsafe { frame_pointer.add(size) };
        unsafe {
            *frame_pointer = ptr::null::<u64>() as u64;  // "frame pointer" from "previous" frame
            *frame_pointer.add(ALLOC) = start as u64;    // "alloc pointer" from "previous" frame
        };
        NockStack {
            start,
            size,
            polarity: Polarity::West,
            frame_pointer,
            alloc_pointer,
            memory,
        }
    }

    /** Size **in 64-bit words** of this NockStack */
    pub fn size(&self) -> usize {
        self.size
    }
}
