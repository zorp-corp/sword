use bitvec::prelude::{BitSlice, Lsb0};
use either::Either;
use std::ptr;
use std::slice::{from_raw_parts, from_raw_parts_mut};

/** Tag for a direct atom. */
const DIRECT_TAG: u64 = 0x0;

/** Tag mask for a direct atom. */
const DIRECT_MASK: u64 = !(u64::MAX >> 1);

/** Maximum value of a direct atom. Values higher than this must be represented by indirect atoms. */
pub const DIRECT_MAX: u64 = u64::MAX >> 1;

/** Tag for an indirect atom. */
const INDIRECT_TAG: u64 = u64::MAX & DIRECT_MASK;

/** Tag mask for an indirect atom. */
const INDIRECT_MASK: u64 = !(u64::MAX >> 2);

/** Tag for a cell. */
const CELL_TAG: u64 = u64::MAX & INDIRECT_MASK;

/** Tag mask for a cell. */
const CELL_MASK: u64 = !(u64::MAX >> 3);

/** Tag for a forwarding pointer */
const FORWARDING_TAG: u64 = u64::MAX & CELL_MASK;

/** Tag mask for a forwarding pointer */
const FORWARDING_MASK: u64 = CELL_MASK;

/** Test if a noun is a direct atom. */
fn is_direct_atom(noun: u64) -> bool {
    noun & DIRECT_MASK == DIRECT_TAG
}

/** Test if a noun is an indirect atom. */
fn is_indirect_atom(noun: u64) -> bool {
    noun & INDIRECT_MASK == INDIRECT_TAG
}

/** Test if a noun is a cell. */
fn is_cell(noun: u64) -> bool {
    noun & CELL_MASK == CELL_TAG
}

/** A direct atom.
 *
 * Direct atoms represent an atom up to and including DIRECT_MAX as a machine word.
 */
#[derive(Copy, Clone)]
#[repr(packed(8))]
pub struct DirectAtom(u64);

impl DirectAtom {
    /** Create a new direct atom, or panic if the value is greater than DIRECT_MAX */
    pub const fn new_panic(value: u64) -> Self {
        if value > DIRECT_MAX {
            panic!("Number is greater than DIRECT_MAX")
        } else {
            DirectAtom(value)
        }
    }

    /** Create a new direct atom, or return Err if the value is greater than DIRECT_MAX */
    pub const fn new(value: u64) -> Result<Self, ()> {
        if value > DIRECT_MAX {
            Err(())
        } else {
            Ok(DirectAtom(value))
        }
    }

    /** Create a new direct atom. This is unsafe because the value is not checked.
     *
     * Attempting to create a direct atom with a value greater than DIRECT_MAX will
     * result in this value being interpreted by the runtime as a cell or indirect atom,
     * with corresponding memory accesses. Thus, this function is marked as unsafe.
     */
    pub const unsafe fn new_unchecked(value: u64) -> Self {
        DirectAtom(value)
    }

    pub fn as_atom(self) -> Atom {
        Atom { direct: self }
    }

    pub fn data(self) -> u64 {
        self.0
    }

    pub fn as_bitslice<'a>(&'a self) -> &'a BitSlice<u64, Lsb0> {
        &(BitSlice::from_element(&self.0))
    }
}

/** An indirect atom.
 *
 * Indirect atoms represent atoms above DIRECT_MAX as a tagged pointer to a memory buffer
 * structured as:
 * - first word: metadata
 * - second word: size in 64-bit words
 * - remaining words: data
 * Indirect atoms are always stored in little-endian byte order
 */
#[derive(Copy, Clone)]
#[repr(packed(8))]
pub struct IndirectAtom(u64);

impl IndirectAtom {
    /** Tag the pointer and type it as an indirect atom. */
    pub unsafe fn from_raw_pointer(ptr: *const u64) -> Self {
        IndirectAtom((ptr as u64) >> 3 | INDIRECT_TAG)
    }

    /** Strip the tag from an indirect atom and return it as a mutable pointer to its memory buffer. */
    unsafe fn to_raw_pointer_mut(&mut self) -> *mut u64 {
        (self.0 << 3) as *mut u64
    }

    /** Strip the tag from an indirect atom and return it as a pointer to its memory buffer. */
    pub unsafe fn to_raw_pointer(&self) -> *const u64 {
        (self.0 << 3) as *const u64
    }

    pub unsafe fn set_forwarding_pointer(&mut self, new_me: *const u64) {
        // This is OK because the size is stored as 64 bit words, not bytes.
        // Thus, a true size value will never be larger than U64::MAX >> 3, and so
        // any of the high bits set as an MSB
        *self.to_raw_pointer_mut().add(1) = (new_me as u64) >> 3 | FORWARDING_TAG;
    }

    pub unsafe fn forwarding_pointer(&self) -> Option<IndirectAtom> {
        let size_raw = *self.to_raw_pointer().add(1);
        if size_raw & FORWARDING_MASK == FORWARDING_TAG {
            // we can replace this by masking out the forwarding pointer and putting in the
            // indirect tag
            Some(Self::from_raw_pointer((size_raw << 3) as *const u64))
        } else {
            None
        }
    }

    /** Make an indirect atom by copying from other memory.
     *
     *  The size is specified in 64 bit words, not in bytes.
     */
    pub unsafe fn new_raw(
        allocator: &mut dyn NounAllocator,
        size: usize,
        data: *const u64,
    ) -> Self {
        let (mut indirect, buffer) = Self::new_raw_mut(allocator, size);
        ptr::copy_nonoverlapping(data, buffer.add(2), size);
        *(indirect.normalize())
    }

    /** Make an indirect atom that can be written into. Return the atom (which should not be used
     * until it is written and normalized) and a mutable pointer which is the data buffer for the
     * indirect atom, to be written into.
     */
    pub unsafe fn new_raw_mut(allocator: &mut dyn NounAllocator, size: usize) -> (Self, *mut u64) {
        let buffer = allocator.alloc_indirect(size);
        *buffer = 0;
        *buffer.add(1) = size as u64;
        (Self::from_raw_pointer(buffer), buffer.add(2))
    }

    /** Make an indirect atom that can be written into, and zero the whole data buffer.
     * Return the atom (which should not be used until it is written and normalized) and a mutable
     * pointer which is the data buffer for the indirect atom, to be written into.
     */
    pub unsafe fn new_raw_mut_zeroed(
        allocator: &mut dyn NounAllocator,
        size: usize,
    ) -> (Self, *mut u64) {
        let allocation = Self::new_raw_mut(allocator, size);
        ptr::write_bytes(allocation.1, 0, size << 3);
        allocation
    }

    /** Make an indirect atom that can be written into as a bitslice. The constraints of
     * [new_raw_mut_zeroed] also apply here
     */
    pub unsafe fn new_raw_mut_bitslice<'a>(
        allocator: &mut dyn NounAllocator,
        size: usize,
    ) -> (Self, &'a mut BitSlice<u64, Lsb0>) {
        let (noun, ptr) = Self::new_raw_mut_zeroed(allocator, size);
        (
            noun,
            BitSlice::from_slice_mut(from_raw_parts_mut(ptr, size)),
        )
    }

    /** Size of an indirect atom in 64-bit words */
    pub fn size(&self) -> usize {
        unsafe { *(self.to_raw_pointer().add(1)) as usize }
    }

    /** Pointer to data for indirect atom */
    pub fn data_pointer(&self) -> *const u64 {
        unsafe { self.to_raw_pointer().add(2) as *const u64 }
    }

    pub fn as_slice<'a>(&'a self) -> &'a [u64] {
        unsafe { from_raw_parts(self.data_pointer(), self.size()) }
    }

    pub fn as_byte_size<'a>(&'a self) -> &'a [u8] {
        unsafe { from_raw_parts(self.data_pointer() as *const u8, self.size() << 3) }
    }

    /** BitSlice view on an indirect atom, with lifetime tied to reference to indirect atom. */
    pub fn as_bitslice<'a>(&'a self) -> &'a BitSlice<u64, Lsb0> {
        BitSlice::from_slice(self.as_slice())
    }

    /** Ensure that the size does not contain any trailing 0 words */
    pub unsafe fn normalize(&mut self) -> &Self {
        let mut index = self.size() - 1;
        let data = self.data_pointer();
        loop {
            if index == 0 || *(data.add(index)) != 0 {
                break;
            }
            index = index - 1;
        }
        *(self.to_raw_pointer_mut().add(1)) = (index + 1) as u64;
        self
    }

    /** Normalize, but convert to direct atom if it will fit */
    pub unsafe fn normalize_as_atom(&mut self) -> Atom {
        self.normalize();
        if self.size() == 1 && *(self.data_pointer()) <= DIRECT_MAX {
            Atom {
                direct: DirectAtom(*(self.data_pointer())),
            }
        } else {
            Atom { indirect: *self }
        }
    }

    pub fn as_atom(self) -> Atom {
        Atom { indirect: self }
    }

    pub fn as_allocated(self) -> Allocated {
        Allocated { indirect: self }
    }

    pub fn as_noun(self) -> Noun {
        Noun { indirect: self }
    }
}

/**
 * A cell.
 *
 * A cell is represented by a tagged pointer to a memory buffer with metadata, a word describing
 * the noun which is the cell's head, and a word describing a noun which is the cell's tail, each
 * at a fixed offset.
 */
#[derive(Copy, Clone)]
#[repr(packed(8))]
pub struct Cell(u64);

impl Cell {
    pub unsafe fn from_raw_pointer(ptr: *const CellMemory) -> Self {
        Cell((ptr as u64) >> 3 | CELL_TAG)
    }

    pub unsafe fn to_raw_pointer(&self) -> *const CellMemory {
        (self.0 << 3) as *const CellMemory
    }

    unsafe fn to_raw_pointer_mut(&mut self) -> *mut CellMemory {
        (self.0 << 3) as *mut CellMemory
    }

    pub unsafe fn head_as_mut(mut self) -> *mut Noun {
        &mut (*self.to_raw_pointer_mut()).head as *mut Noun
    }

    pub unsafe fn tail_as_mut<'a>(mut self) -> *mut Noun {
        &mut (*self.to_raw_pointer_mut()).tail as *mut Noun
    }

    pub unsafe fn set_forwarding_pointer(&mut self, new_me: *const CellMemory) {
        (*self.to_raw_pointer_mut()).head = Noun {
            raw: (new_me as u64) >> 3 | FORWARDING_TAG,
        }
    }

    pub unsafe fn forwarding_pointer(&self) -> Option<Cell> {
        let head_raw = (*self.to_raw_pointer()).head.raw;
        if head_raw & FORWARDING_MASK == FORWARDING_TAG {
            // we can replace this by masking out the forwarding pointer and putting in the cell
            // tag
            Some(Self::from_raw_pointer((head_raw << 3) as *const CellMemory))
        } else {
            None
        }
    }

    pub fn new(allocator: &mut dyn NounAllocator, head: Noun, tail: Noun) -> Cell {
        unsafe {
            let (cell, memory) = Self::new_raw_mut(allocator);
            (*memory).head = head;
            (*memory).tail = tail;
            cell
        }
    }

    pub unsafe fn new_raw_mut(allocator: &mut dyn NounAllocator) -> (Cell, *mut CellMemory) {
        let memory = allocator.alloc_cell();
        (*memory).metadata = 0;
        (Self::from_raw_pointer(memory), memory)
    }

    pub fn head(&self) -> Noun {
        unsafe { (*(self.to_raw_pointer())).head }
    }

    pub fn tail(&self) -> Noun {
        unsafe { (*(self.to_raw_pointer())).tail }
    }

    pub fn as_allocated(&self) -> Allocated {
        Allocated { cell: *self }
    }

    pub fn as_noun(&self) -> Noun {
        Noun { cell: *self }
    }
}

/**
 * Memory representation of the contents of a cell
 */
#[derive(Copy, Clone)]
#[repr(packed(8))]
pub struct CellMemory {
    pub metadata: u64,
    pub head: Noun,
    pub tail: Noun,
}

#[derive(Copy, Clone)]
#[repr(packed(8))]
pub union Atom {
    raw: u64,
    direct: DirectAtom,
    indirect: IndirectAtom,
}

impl Atom {
    pub fn new(allocator: &mut dyn NounAllocator, value: u64) -> Atom {
        if value <= DIRECT_MAX {
            unsafe { DirectAtom::new_unchecked(value).as_atom() }
        } else {
            unsafe { IndirectAtom::new_raw(allocator, 1, &value).as_atom() }
        }
    }
    pub fn is_direct(&self) -> bool {
        unsafe { is_direct_atom(self.raw) }
    }

    pub fn is_indirect(&self) -> bool {
        unsafe { is_indirect_atom(self.raw) }
    }

    pub fn as_direct(&self) -> Result<DirectAtom, ()> {
        if self.is_direct() {
            unsafe { Ok(self.direct) }
        } else {
            Err(())
        }
    }

    pub fn as_indirect(&self) -> Result<IndirectAtom, ()> {
        if self.is_indirect() {
            unsafe { Ok(self.indirect) }
        } else {
            Err(())
        }
    }

    pub fn as_either(&self) -> Either<DirectAtom, IndirectAtom> {
        if self.is_indirect() {
            unsafe { Either::Right(self.indirect) }
        } else {
            unsafe { Either::Left(self.direct) }
        }
    }

    pub fn as_bitslice<'a>(&'a self) -> &'a BitSlice<u64, Lsb0> {
        if self.is_indirect() {
            unsafe { self.indirect.as_bitslice() }
        } else {
            unsafe { &(self.direct.as_bitslice()) }
        }
    }

    pub fn size(&self) -> usize {
        match self.as_either() {
            Either::Left(_direct) => 1,
            Either::Right(indirect) => indirect.size(),
        }
    }

    pub fn data_pointer(&self) -> *const u64 {
        match self.as_either() {
            Either::Left(direct) => (self as *const Atom) as *const u64,
            Either::Right(indirect) => indirect.data_pointer(),
        }
    }

    pub unsafe fn normalize(&mut self) -> Atom {
        if self.is_indirect() {
            self.indirect.normalize_as_atom()
        } else {
            *self
        }
    }

    pub fn as_noun(self) -> Noun {
        Noun { atom: self }
    }
}

#[derive(Copy, Clone)]
#[repr(packed(8))]
pub union Allocated {
    raw: u64,
    indirect: IndirectAtom,
    cell: Cell,
}

impl Allocated {
    pub fn is_indirect(&self) -> bool {
        unsafe { is_indirect_atom(self.raw) }
    }

    pub fn is_cell(&self) -> bool {
        unsafe { is_cell(self.raw) }
    }

    pub unsafe fn to_raw_pointer(&self) -> *const u64 {
        (self.raw << 3) as *const u64
    }

    pub unsafe fn to_raw_pointer_mut(&mut self) -> *mut u64 {
        (self.raw << 3) as *mut u64
    }

    unsafe fn const_to_raw_pointer_mut(self) -> *mut u64 {
        (self.raw << 3) as *mut u64
    }

    pub unsafe fn forwarding_pointer(&self) -> Option<Allocated> {
        match self.as_either() {
            Either::Left(indirect) => indirect.forwarding_pointer().map(|i| i.as_allocated()),
            Either::Right(cell) => cell.forwarding_pointer().map(|c| c.as_allocated()),
        }
    }

    pub unsafe fn get_metadata(&self) -> u64 {
        *(self.to_raw_pointer() as *const u64)
    }

    pub unsafe fn set_metadata(self, metadata: u64) {
        *(self.const_to_raw_pointer_mut() as *mut u64) = metadata;
    }

    pub fn as_either(&self) -> Either<IndirectAtom, Cell> {
        if self.is_indirect() {
            unsafe { Either::Left(self.indirect) }
        } else {
            unsafe { Either::Right(self.cell) }
        }
    }

    pub fn as_noun(&self) -> Noun {
        Noun { allocated: *self }
    }

    pub fn get_cached_mug(self: Allocated) -> Option<u32> {
        unsafe {
            let bottom_metadata = self.get_metadata() as u32 & 0x7FFFFFFF; // magic number: LS 31 bits
            if bottom_metadata > 0 {
                Some(bottom_metadata)
            } else {
                None
            }
        }
    }
}

#[derive(Copy, Clone)]
#[repr(packed(8))]
pub union Noun {
    raw: u64,
    direct: DirectAtom,
    indirect: IndirectAtom,
    atom: Atom,
    cell: Cell,
    allocated: Allocated,
}

impl Noun {
    pub fn is_direct(&self) -> bool {
        unsafe { is_direct_atom(self.raw) }
    }

    pub fn is_indirect(&self) -> bool {
        unsafe { is_indirect_atom(self.raw) }
    }

    pub fn is_atom(&self) -> bool {
        self.is_direct() || self.is_indirect()
    }

    pub fn is_allocated(&self) -> bool {
        self.is_indirect() || self.is_cell()
    }

    pub fn is_cell(&self) -> bool {
        unsafe { is_cell(self.raw) }
    }

    pub fn as_direct(&self) -> Result<DirectAtom, ()> {
        if self.is_direct() {
            unsafe { Ok(self.direct) }
        } else {
            Err(())
        }
    }

    pub fn as_indirect(&self) -> Result<IndirectAtom, ()> {
        if self.is_indirect() {
            unsafe { Ok(self.indirect) }
        } else {
            Err(())
        }
    }

    pub fn as_cell(&self) -> Result<Cell, ()> {
        if self.is_cell() {
            unsafe { Ok(self.cell) }
        } else {
            Err(())
        }
    }

    pub fn as_atom(&self) -> Result<Atom, ()> {
        if self.is_atom() {
            unsafe { Ok(self.atom) }
        } else {
            Err(())
        }
    }

    pub fn as_allocated(&self) -> Result<Allocated, ()> {
        if self.is_allocated() {
            unsafe { Ok(self.allocated) }
        } else {
            Err(())
        }
    }

    pub fn as_either_atom_cell(&self) -> Either<Atom, Cell> {
        if self.is_cell() {
            unsafe { Either::Right(self.cell) }
        } else {
            unsafe { Either::Left(self.atom) }
        }
    }

    pub fn as_either_direct_allocated(&self) -> Either<DirectAtom, Allocated> {
        if self.is_direct() {
            unsafe { Either::Left(self.direct) }
        } else {
            unsafe { Either::Right(self.allocated) }
        }
    }

    /** Are these the same noun */
    pub unsafe fn raw_equals(self, other: Noun) -> bool {
        self.raw == other.raw
    }
}

/**
 * An allocation object (probably a mem::NockStack) which can allocate a memory buffer sized to
 * a certain number of nouns
 */
pub trait NounAllocator {
    /** Allocate memory for some multiple of the size of a noun
     *
     * This should allocate *two more* `u64`s than `words` to make space for the size and metadata
     */
    unsafe fn alloc_indirect(&mut self, words: usize) -> *mut u64;

    /** Allocate memory for a cell */
    unsafe fn alloc_cell(&mut self) -> *mut CellMemory;
}
