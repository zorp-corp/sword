use crate::mem::{word_size_of, NockStack};
use crate::persist::{pma_contains,pma_dirty};
use sword_macros::tas;
use bitvec::prelude::{BitSlice, Lsb0};
use either::{Either, Left, Right};
use ibig::{Stack, UBig};
use intmap::IntMap;
use std::error;
use std::fmt;
use std::ptr;
use std::slice::{from_raw_parts, from_raw_parts_mut};

crate::gdb!();

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

/*  A note on forwarding pointers:
 *
 *  Forwarding pointers are only used temporarily during copies between NockStack frames and between
 *  the NockStack and the PMA. Since unifying equality checks can create structural sharing between
 *  Noun objects, forwarding pointers act as a signal that a Noun has already been copied to the
 *  "to" space. The old Noun object in the "from" space is given a forwarding pointer so that any
 *  future refernces to the same structure know that it has already been copied and that they should
 *  retain the structural sharing relationship by referencing the new copy in the "to" copy space.
 *
 *  The Nouns in the "from" space marked with forwarding pointers are dangling pointers after a copy
 *  operation. No code outside of the copying code checks for forwarding pointers. This invariant
 *  must be enforced in two ways:
 *      1. The current frame must be immediately popped after preserving data, when
 *          copying from a junior NockStack frame to a senior NockStack frame.
 *      2. All persistent derived state (e.g. Hot state, Warm state) must be preserved
 *          and the root NockStack frame flipped after saving data to the PMA.
 */

/** Tag for a forwarding pointer */
const FORWARDING_TAG: u64 = u64::MAX & CELL_MASK;

/** Tag mask for a forwarding pointer */
const FORWARDING_MASK: u64 = CELL_MASK;

/** Loobeans */
pub const YES: Noun = D(0);
pub const NO: Noun = D(1);
pub const NONE: Noun = unsafe { DirectAtom::new_unchecked(tas!(b"MORMAGIC")).as_noun() };

#[cfg(feature = "check_acyclic")]
#[macro_export]
macro_rules! assert_acyclic {
    ( $x:expr ) => {
        assert_no_alloc::permit_alloc(|| {
            assert!(crate::noun::acyclic_noun($x));
        })
    };
}

#[cfg(not(feature = "check_acyclic"))]
#[macro_export]
macro_rules! assert_acyclic {
    ( $x:expr ) => {};
}

pub fn acyclic_noun(noun: Noun) -> bool {
    let mut seen = IntMap::new();
    acyclic_noun_go(noun, &mut seen)
}

fn acyclic_noun_go(noun: Noun, seen: &mut IntMap<()>) -> bool {
    match noun.as_either_atom_cell() {
        Left(_atom) => true,
        Right(cell) => {
            if seen.get(cell.0).is_some() {
                false
            } else {
                seen.insert(cell.0, ());
                if acyclic_noun_go(cell.head(), seen) {
                    if acyclic_noun_go(cell.tail(), seen) {
                        seen.remove(cell.0);
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
        }
    }
}

#[cfg(feature = "check_forwarding")]
#[macro_export]
macro_rules! assert_no_forwarding_pointers {
    ( $x:expr ) => {
        assert_no_alloc::permit_alloc(|| {
            assert!(crate::noun::no_forwarding_pointers($x));
        })
    };
}

#[cfg(not(feature = "check_forwarding"))]
#[macro_export]
macro_rules! assert_no_forwarding_pointers {
    ( $x:expr ) => {};
}

pub fn no_forwarding_pointers(noun: Noun) -> bool {
    let mut dbg_stack = Vec::new();
    dbg_stack.push(noun);

    while !dbg_stack.is_empty() {
        if let Some(noun) = dbg_stack.pop() {
            if unsafe { noun.raw & FORWARDING_MASK == FORWARDING_TAG } {
                return false;
            } else if let Ok(cell) = noun.as_cell() {
                dbg_stack.push(cell.tail());
                dbg_stack.push(cell.head());
            }
        } else {
            break;
        }
    }

    true
}

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

/** A noun-related error. */
#[derive(Debug, PartialEq)]
pub enum Error {
    /** Expected type [`Allocated`]. */
    NotAllocated,
    /** Expected type [`Atom`]. */
    NotAtom,
    /** Expected type [`Cell`]. */
    NotCell,
    /** Expected type [`DirectAtom`]. */
    NotDirectAtom,
    /** Expected type [`IndirectAtom`]. */
    NotIndirectAtom,
    /** The value can't be represented by the given type. */
    NotRepresentable,
}

impl error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NotAllocated => f.write_str("not an allocated noun"),
            Error::NotAtom => f.write_str("not an atom"),
            Error::NotCell => f.write_str("not a cell"),
            Error::NotDirectAtom => f.write_str("not a direct atom"),
            Error::NotIndirectAtom => f.write_str("not an indirect atom"),
            Error::NotRepresentable => f.write_str("unrepresentable value"),
        }
    }
}

impl From<Error> for () {
    fn from(_: Error) -> Self {}
}

/** A [`Result`] that returns an [`Error`] on error. */
pub type Result<T> = std::result::Result<T, Error>;

/** A direct atom.
 *
 * Direct atoms represent an atom up to and including DIRECT_MAX as a machine word.
 */
#[derive(Copy, Clone)]
#[repr(C)]
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
    pub const fn new(value: u64) -> Result<Self> {
        if value > DIRECT_MAX {
            Err(Error::NotRepresentable)
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

    pub fn bit_size(self) -> usize {
        (64 - self.0.leading_zeros()) as usize
    }

    pub fn as_atom(self) -> Atom {
        Atom { direct: self }
    }

    pub fn as_ubig<S: Stack>(self, _stack: &mut S) -> UBig {
        UBig::from(self.0)
    }

    pub const fn as_noun(self) -> Noun {
        Noun { direct: self }
    }

    pub fn data(self) -> u64 {
        self.0
    }

    pub fn as_bitslice(&self) -> &BitSlice<u64, Lsb0> {
        BitSlice::from_element(&self.0)
    }

    pub fn as_bitslice_mut(&mut self) -> &mut BitSlice<u64, Lsb0> {
        BitSlice::from_element_mut(&mut self.0)
    }

    pub fn as_bytes(&self) -> &[u8] {
        let bytes: &[u8; 8] = unsafe { std::mem::transmute(&self.0) };
        &bytes[..]
    }
}

impl fmt::Display for DirectAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0 == 0 {
            return write!(f, "0");
        }

        let mut null = false;
        let mut n = 0;
        let bytes = self.0.to_le_bytes();
        for byte in bytes.iter() {
            if *byte == 0 {
                null = true;
                continue;
            }
            if (null && *byte != 0) || *byte < 33 || *byte > 126 {
                return write!(f, "{}", self.0);
            }
            n += 1;
        }
        if n > 1 {
            write!(f, "%{}", unsafe {
                std::str::from_utf8_unchecked(&bytes[..n])
            })
        } else {
            write!(f, "{}", self.0)
        }
    }
}

#[allow(non_snake_case)]
pub const fn D(n: u64) -> Noun {
    DirectAtom::new_panic(n).as_noun()
}

#[allow(non_snake_case)]
pub fn T<A: NounAllocator>(allocator: &mut A, tup: &[Noun]) -> Noun {
    Cell::new_tuple(allocator, tup).as_noun()
}

/// Create $tape Noun from ASCII string
pub fn tape<A: NounAllocator>(allocator: &mut A, text: &str) -> Noun {
    //  XX: Needs unit tests
    let mut res = D(0);
    for c in text.bytes().rev() {
        res = T(allocator, &[D(c as u64), res])
    }
    res
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
#[repr(C)]
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
     *  Note: size is in 64-bit words, not bytes.
     */
    pub unsafe fn new_raw<A: NounAllocator>(
        allocator: &mut A,
        size: usize,
        data: *const u64,
    ) -> Self {
        let (mut indirect, buffer) = Self::new_raw_mut(allocator, size);
        ptr::copy_nonoverlapping(data, buffer, size);
        *(indirect.normalize())
    }

    /** Make an indirect atom by copying from other memory.
     *
     *  Note: size is bytes, not words
     */
    pub unsafe fn new_raw_bytes<A: NounAllocator>(
        allocator: &mut A,
        size: usize,
        data: *const u8,
    ) -> Self {
        let (mut indirect, buffer) = Self::new_raw_mut_bytes(allocator, size);
        ptr::copy_nonoverlapping(data, buffer.as_mut_ptr(), size);
        *(indirect.normalize())
    }

    pub unsafe fn new_raw_bytes_ref<A: NounAllocator>(allocator: &mut A, data: &[u8]) -> Self {
        IndirectAtom::new_raw_bytes(allocator, data.len(), data.as_ptr())
    }

    /** Make an indirect atom that can be written into. Return the atom (which should not be used
     * until it is written and normalized) and a mutable pointer which is the data buffer for the
     * indirect atom, to be written into.
     */
    pub unsafe fn new_raw_mut<A: NounAllocator>(
        allocator: &mut A,
        size: usize,
    ) -> (Self, *mut u64) {
        debug_assert!(size > 0);
        let buffer = allocator.alloc_indirect(size);
        *buffer = 0;
        *buffer.add(1) = size as u64;
        (Self::from_raw_pointer(buffer), buffer.add(2))
    }

    /** Make an indirect atom that can be written into, and zero the whole data buffer.
     * Return the atom (which should not be used until it is written and normalized) and a mutable
     * pointer which is the data buffer for the indirect atom, to be written into.
     */
    pub unsafe fn new_raw_mut_zeroed<A: NounAllocator>(
        allocator: &mut A,
        size: usize,
    ) -> (Self, *mut u64) {
        let allocation = Self::new_raw_mut(allocator, size);
        ptr::write_bytes(allocation.1, 0, size);
        allocation
    }

    /** Make an indirect atom that can be written into as a bitslice. The constraints of
     * [new_raw_mut_zeroed] also apply here
     */
    pub unsafe fn new_raw_mut_bitslice<'a, A: NounAllocator>(
        allocator: &mut A,
        size: usize,
    ) -> (Self, &'a mut BitSlice<u64, Lsb0>) {
        let (noun, ptr) = Self::new_raw_mut_zeroed(allocator, size);
        (
            noun,
            BitSlice::from_slice_mut(from_raw_parts_mut(ptr, size)),
        )
    }

    /** Make an indirect atom that can be written into as a slice of bytes. The constraints of
     * [new_raw_mut_zeroed] also apply here
     *
     * Note: size is bytes, not words
     */
    pub unsafe fn new_raw_mut_bytes<'a, A: NounAllocator>(
        allocator: &mut A,
        size: usize,
    ) -> (Self, &'a mut [u8]) {
        let word_size = (size + 7) >> 3;
        let (noun, ptr) = Self::new_raw_mut_zeroed(allocator, word_size);
        (noun, from_raw_parts_mut(ptr as *mut u8, size))
    }

    /// Create an indirect atom backed by a fixed-size array
    pub unsafe fn new_raw_mut_bytearray<'a, const N: usize, A: NounAllocator>(
        allocator: &mut A,
    ) -> (Self, &'a mut [u8; N]) {
        let word_size = (std::mem::size_of::<[u8; N]>() + 7) >> 3;
        let (noun, ptr) = Self::new_raw_mut_zeroed(allocator, word_size);
        (noun, &mut *(ptr as *mut [u8; N]))
    }

    /** Size of an indirect atom in 64-bit words */
    pub fn size(&self) -> usize {
        unsafe { *(self.to_raw_pointer().add(1)) as usize }
    }

    /** Memory size of an indirect atom (including size + metadata fields) in 64-bit words */
    pub fn raw_size(&self) -> usize {
        self.size() + 2
    }

    pub fn bit_size(&self) -> usize {
        unsafe {
            ((self.size() - 1) << 6) + 64
                - (*(self.to_raw_pointer().add(2 + self.size() - 1))).leading_zeros() as usize
        }
    }

    /** Pointer to data for indirect atom */
    pub fn data_pointer(&self) -> *const u64 {
        unsafe { self.to_raw_pointer().add(2) }
    }

    pub fn data_pointer_mut(&mut self) -> *mut u64 {
        unsafe { self.to_raw_pointer_mut().add(2) }
    }

    pub fn as_slice(&self) -> &[u64] {
        unsafe { from_raw_parts(self.data_pointer(), self.size()) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [u64] {
        unsafe { from_raw_parts_mut(self.data_pointer_mut(), self.size()) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { from_raw_parts(self.data_pointer() as *const u8, self.size() << 3) }
    }

    /** BitSlice view on an indirect atom, with lifetime tied to reference to indirect atom. */
    pub fn as_bitslice(&self) -> &BitSlice<u64, Lsb0> {
        BitSlice::from_slice(self.as_slice())
    }

    pub fn as_bitslice_mut(&mut self) -> &mut BitSlice<u64, Lsb0> {
        BitSlice::from_slice_mut(self.as_mut_slice())
    }

    pub fn as_ubig<S: Stack>(&self, stack: &mut S) -> UBig {
        UBig::from_le_bytes_stack(stack, self.as_bytes())
    }

    pub unsafe fn as_u64(self) -> Result<u64> {
        if self.size() == 1 {
            Ok(*(self.data_pointer()))
        } else {
            Err(Error::NotRepresentable)
        }
    }

    /** Produce a SoftFloat-compatible ordered pair of 64-bit words */
    pub fn as_u64_pair(self) -> Result<[u64; 2]> {
        if self.size() <= 2 {
            let u128_array = &mut [0u64; 2];
            u128_array.copy_from_slice(&(self.as_slice()[0..2]));
            Ok(*u128_array)
        } else {
            Err(Error::NotRepresentable)
        }
    }

    /** Ensure that the size does not contain any trailing 0 words */
    pub unsafe fn normalize(&mut self) -> &Self {
        let mut index = self.size() - 1;
        let data = self.data_pointer();
        loop {
            if index == 0 || *(data.add(index)) != 0 {
                break;
            }
            index -= 1;
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

// XX: Need a version that either:
//      a) allocates on the NockStack directly for creating a tape (or even a string?)
//      b) disables no-allocation, creates a string, utilitzes it (eprintf or generate tape), and then deallocates
impl fmt::Display for IndirectAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x")?;
        let mut i = self.size() - 1;
        loop {
            write!(f, "_{:016x}", unsafe { *(self.data_pointer().add(i)) })?;
            if i == 0 {
                break;
            }
            i -= 1;
        }
        Ok(())
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
#[repr(C)]
#[repr(packed(8))]
pub struct Cell(u64);

impl Cell {
    pub unsafe fn from_raw_pointer(ptr: *const CellMemory) -> Self {
        Cell((ptr as u64) >> 3 | CELL_TAG)
    }

    pub unsafe fn to_raw_pointer(&self) -> *const CellMemory {
        (self.0 << 3) as *const CellMemory
    }

    pub unsafe fn to_raw_pointer_mut(&mut self) -> *mut CellMemory {
        (self.0 << 3) as *mut CellMemory
    }

    pub unsafe fn head_as_mut(mut self) -> *mut Noun {
        &mut (*self.to_raw_pointer_mut()).head as *mut Noun
    }

    pub unsafe fn tail_as_mut(mut self) -> *mut Noun {
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

    pub fn new<T: NounAllocator>(allocator: &mut T, head: Noun, tail: Noun) -> Cell {
        unsafe {
            let (cell, memory) = Self::new_raw_mut(allocator);
            (*memory).head = head;
            (*memory).tail = tail;
            cell
        }
    }

    pub fn new_tuple<A: NounAllocator>(allocator: &mut A, tup: &[Noun]) -> Cell {
        if tup.len() < 2 {
            panic!("Cannot create tuple with fewer than 2 elements");
        }

        let len = tup.len();
        let mut cell = Cell::new(allocator, tup[len - 2], tup[len - 1]);
        for i in (0..len - 2).rev() {
            cell = Cell::new(allocator, tup[i], cell.as_noun());
        }
        cell
    }

    pub unsafe fn new_raw_mut<A: NounAllocator>(allocator: &mut A) -> (Cell, *mut CellMemory) {
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

impl fmt::Display for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let mut cell = *self;
        loop {
            write!(f, "{}", cell.head())?;
            match cell.tail().as_cell() {
                Ok(next_cell) => {
                    write!(f, " ")?;
                    cell = next_cell;
                }
                Err(_) => {
                    write!(f, " {}]", cell.tail())?;
                    break;
                }
            }
        }
        Ok(())
    }
}

impl Slots for Cell {}
impl private::RawSlots for Cell {
    fn raw_slot(&self, axis: &BitSlice<u64, Lsb0>) -> Result<Noun> {
        let mut noun: Noun = self.as_noun();
        // Axis cannot be 0
        let mut cursor = axis.last_one().ok_or(Error::NotRepresentable)?;

        while cursor != 0 {
            cursor -= 1;

            // Returns Err if axis tried to descend through atom
            if axis[cursor] {
                noun = noun.as_cell()?.tail();
            } else {
                noun = noun.as_cell()?.head();
            }
        }

        Ok(noun)
    }
}

/**
 * Memory representation of the contents of a cell
 */
#[derive(Copy, Clone)]
#[repr(C)]
#[repr(packed(8))]
pub struct CellMemory {
    pub metadata: u64,
    pub head: Noun,
    pub tail: Noun,
}

#[derive(Copy, Clone)]
#[repr(C)]
#[repr(packed(8))]
pub union Atom {
    raw: u64,
    direct: DirectAtom,
    indirect: IndirectAtom,
}

impl Atom {
    pub fn new<A: NounAllocator>(allocator: &mut A, value: u64) -> Atom {
        if value <= DIRECT_MAX {
            unsafe { DirectAtom::new_unchecked(value).as_atom() }
        } else {
            unsafe { IndirectAtom::new_raw(allocator, 1, &value).as_atom() }
        }
    }

    // to_le_bytes and new_raw are copies.  We should be able to do this completely without copies
    // if we integrate with ibig properly.
    pub fn from_ubig<A: NounAllocator>(allocator: &mut A, big: &UBig) -> Atom {
        let bit_size = big.bit_len();
        let buffer = big.to_le_bytes_stack();
        if bit_size < 64 {
            let mut value = 0u64;
            for i in (0..bit_size).step_by(8) {
                value |= (buffer[i / 8] as u64) << i;
            }
            unsafe { DirectAtom::new_unchecked(value).as_atom() }
        } else {
            let byte_size = (big.bit_len() + 7) >> 3;
            unsafe { IndirectAtom::new_raw_bytes(allocator, byte_size, buffer.as_ptr()).as_atom() }
        }
    }

    pub fn is_direct(&self) -> bool {
        unsafe { is_direct_atom(self.raw) }
    }

    pub fn is_indirect(&self) -> bool {
        unsafe { is_indirect_atom(self.raw) }
    }

    pub fn is_normalized(&self) -> bool {
        unsafe {
            if let Some(indirect) = self.indirect() {
                if (indirect.size() == 1 && *indirect.data_pointer() <= DIRECT_MAX)
                    || *indirect.data_pointer().add(indirect.size() - 1) == 0
                {
                    return false;
                }
            } // nothing to do for direct atom
        };

        true
    }

    pub fn as_direct(&self) -> Result<DirectAtom> {
        if self.is_direct() {
            unsafe { Ok(self.direct) }
        } else {
            Err(Error::NotDirectAtom)
        }
    }

    pub fn as_indirect(&self) -> Result<IndirectAtom> {
        if self.is_indirect() {
            unsafe { Ok(self.indirect) }
        } else {
            Err(Error::NotIndirectAtom)
        }
    }

    pub fn as_either(&self) -> Either<DirectAtom, IndirectAtom> {
        if self.is_indirect() {
            unsafe { Right(self.indirect) }
        } else {
            unsafe { Left(self.direct) }
        }
    }

    pub fn as_noun(self) -> Noun {
        Noun { atom: self }
    }

    pub fn as_bytes(&self) -> &[u8] {
        if self.is_direct() {
            unsafe { self.direct.as_bytes() }
        } else {
            unsafe { self.indirect.as_bytes() }
        }
    }

    pub fn as_u64(self) -> Result<u64> {
        if self.is_direct() {
            Ok(unsafe { self.direct.data() })
        } else {
            unsafe { self.indirect.as_u64() }
        }
    }

    /** Produce a SoftFloat-compatible ordered pair of 64-bit words */
    pub unsafe fn as_u64_pair(self) -> Result<[u64; 2]> {
        if self.is_direct() {
            let u128_array = &mut [0u64; 2];
            u128_array[0] = self.as_direct()?.data();
            u128_array[1] = 0x0_u64;
            Ok(*u128_array)
        } else {
            unsafe { self.indirect.as_u64_pair() }
        }
    }

    pub fn as_bitslice(&self) -> &BitSlice<u64, Lsb0> {
        if self.is_indirect() {
            unsafe { self.indirect.as_bitslice() }
        } else {
            unsafe { self.direct.as_bitslice() }
        }
    }

    pub fn as_bitslice_mut(&mut self) -> &mut BitSlice<u64, Lsb0> {
        if self.is_indirect() {
            unsafe { self.indirect.as_bitslice_mut() }
        } else {
            unsafe { self.direct.as_bitslice_mut() }
        }
    }

    pub fn as_ubig<S: Stack>(self, stack: &mut S) -> UBig {
        if self.is_indirect() {
            unsafe { self.indirect.as_ubig(stack) }
        } else {
            unsafe { self.direct.as_ubig(stack) }
        }
    }

    pub fn direct(&self) -> Option<DirectAtom> {
        if self.is_direct() {
            unsafe { Some(self.direct) }
        } else {
            None
        }
    }

    pub fn indirect(&self) -> Option<IndirectAtom> {
        if self.is_indirect() {
            unsafe { Some(self.indirect) }
        } else {
            None
        }
    }

    pub fn size(&self) -> usize {
        match self.as_either() {
            Left(_direct) => 1,
            Right(indirect) => indirect.size(),
        }
    }

    pub fn bit_size(&self) -> usize {
        match self.as_either() {
            Left(direct) => direct.bit_size(),
            Right(indirect) => indirect.bit_size(),
        }
    }

    pub fn data_pointer(&self) -> *const u64 {
        match self.as_either() {
            Left(_direct) => (self as *const Atom) as *const u64,
            Right(indirect) => indirect.data_pointer(),
        }
    }

    pub unsafe fn normalize(&mut self) -> Atom {
        if self.is_indirect() {
            self.indirect.normalize_as_atom()
        } else {
            *self
        }
    }

    /** Make an atom from a raw u64
     *
     * # Safety
     *
     * Note that the [u64] parameter is *not*, in general, the value of the atom!
     *
     * In particular, anything with the high bit set will be treated as a tagged pointer.
     * This method is only to be used to restore an atom from the raw [u64] representation
     * returned by [Noun::as_raw], and should only be used if we are sure the restored noun is in
     * fact an atom.
     */
    pub unsafe fn from_raw(raw: u64) -> Atom {
        Atom { raw }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_noun().fmt(f)
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
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
            Left(indirect) => indirect.forwarding_pointer().map(|i| i.as_allocated()),
            Right(cell) => cell.forwarding_pointer().map(|c| c.as_allocated()),
        }
    }

    pub unsafe fn get_metadata(&self) -> u64 {
        *(self.to_raw_pointer())
    }

    pub unsafe fn set_metadata(self, metadata: u64) {
        let ptr = self.const_to_raw_pointer_mut();
        if pma_contains(ptr, 1) {
            pma_dirty(ptr, 1);
        }
        *(self.const_to_raw_pointer_mut()) = metadata;
    }

    pub fn as_either(&self) -> Either<IndirectAtom, Cell> {
        if self.is_indirect() {
            unsafe { Left(self.indirect) }
        } else {
            unsafe { Right(self.cell) }
        }
    }

    pub fn cell(&self) -> Option<Cell> {
        if self.is_cell() {
            unsafe { Some(self.cell) }
        } else {
            None
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

impl fmt::Display for Allocated {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_noun().fmt(f)
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
#[repr(packed(8))]
pub union Noun {
    pub(crate) raw: u64,
    direct: DirectAtom,
    indirect: IndirectAtom,
    atom: Atom,
    cell: Cell,
    allocated: Allocated,
}

impl Noun {
    pub fn is_none(self) -> bool {
        unsafe { self.raw == u64::MAX }
    }

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

    pub fn as_direct(&self) -> Result<DirectAtom> {
        if self.is_direct() {
            unsafe { Ok(self.direct) }
        } else {
            Err(Error::NotDirectAtom)
        }
    }

    pub fn as_indirect(&self) -> Result<IndirectAtom> {
        if self.is_indirect() {
            unsafe { Ok(self.indirect) }
        } else {
            Err(Error::NotIndirectAtom)
        }
    }

    pub fn as_cell(&self) -> Result<Cell> {
        if self.is_cell() {
            unsafe { Ok(self.cell) }
        } else {
            Err(Error::NotCell)
        }
    }

    pub fn as_atom(&self) -> Result<Atom> {
        if self.is_atom() {
            unsafe { Ok(self.atom) }
        } else {
            Err(Error::NotAtom)
        }
    }

    pub fn as_allocated(&self) -> Result<Allocated> {
        if self.is_allocated() {
            unsafe { Ok(self.allocated) }
        } else {
            Err(Error::NotAllocated)
        }
    }

    pub fn as_either_atom_cell(&self) -> Either<Atom, Cell> {
        if self.is_cell() {
            unsafe { Right(self.cell) }
        } else {
            unsafe { Left(self.atom) }
        }
    }

    pub fn as_either_direct_allocated(&self) -> Either<DirectAtom, Allocated> {
        if self.is_direct() {
            unsafe { Left(self.direct) }
        } else {
            unsafe { Right(self.allocated) }
        }
    }

    pub fn atom(&self) -> Option<Atom> {
        if self.is_atom() {
            unsafe { Some(self.atom) }
        } else {
            None
        }
    }

    pub fn cell(&self) -> Option<Cell> {
        if self.is_cell() {
            unsafe { Some(self.cell) }
        } else {
            None
        }
    }

    pub fn direct(&self) -> Option<DirectAtom> {
        if self.is_direct() {
            unsafe { Some(self.direct) }
        } else {
            None
        }
    }

    pub fn indirect(&self) -> Option<IndirectAtom> {
        if self.is_indirect() {
            unsafe { Some(self.indirect) }
        } else {
            None
        }
    }

    pub fn allocated(&self) -> Option<Allocated> {
        if self.is_allocated() {
            unsafe { Some(self.allocated) }
        } else {
            None
        }
    }

    /** Are these the same noun */
    pub unsafe fn raw_equals(self, other: Noun) -> bool {
        self.raw == other.raw
    }

    pub unsafe fn as_raw(&self) -> u64 {
        self.raw
    }

    pub unsafe fn from_raw(raw: u64) -> Noun {
        Noun { raw }
    }

    /** Produce the total size of a noun, in words
     *
     * This counts the total size, see mass_frame() to count the size in the current frame.
     */
    pub fn mass(self) -> usize {
        unsafe {
            let res = self.mass_wind(&|_| true);
            self.mass_unwind(&|_| true);
            res
        }
    }

    /** Produce the size of a noun in the current frame, in words */
    pub fn mass_frame(self, stack: &NockStack) -> usize {
        unsafe {
            let res = self.mass_wind(&|p| stack.is_in_frame(p));
            self.mass_unwind(&|p| stack.is_in_frame(p));
            res
        }
    }

    /** Produce the total size of a noun, in words
     *
     * `inside` determines whether a pointer should be counted.  If it returns false, we also do
     * not recurse into that noun if it is a cell.  See mass_frame() for an example.
     *
     * This "winds up" the mass calculation, which includes setting the 32nd bit of the metadata to
     * mark nouns that have already been counted.
     *
     * This is unsafe because you *must* call mass_unwind() with the same `inside` function to
     * unmark the noun.  This is exposed so that you can count several the "mass difference" of a
     * series of nouns.  If you call this twice consecutively, the first result will be the mass of
     * the first noun, and the second will be the mass of the second noun minus the overlap with
     * the first noun.
     */
    pub unsafe fn mass_wind(self, inside: &impl Fn(*const u64) -> bool) -> usize {
        if let Ok(allocated) = self.as_allocated() {
            if inside(allocated.to_raw_pointer()) {
                if allocated.get_metadata() & (1 << 32) == 0 {
                    allocated.set_metadata(allocated.get_metadata() | (1 << 32));
                    match allocated.as_either() {
                        Left(indirect) => indirect.size() + 2,
                        Right(cell) => {
                            word_size_of::<CellMemory>()
                                + cell.head().mass_wind(inside)
                                + cell.tail().mass_wind(inside)
                        }
                    }
                } else {
                    0
                }
            } else {
                0
            }
        } else {
            0
        }
    }

    /** See mass_wind() */
    pub unsafe fn mass_unwind(self, inside: &impl Fn(*const u64) -> bool) {
        if let Ok(allocated) = self.as_allocated() {
            if inside(allocated.to_raw_pointer()) {
                allocated.set_metadata(allocated.get_metadata() & !(1 << 32));
                if let Right(cell) = allocated.as_either() {
                    cell.head().mass_unwind(inside);
                    cell.tail().mass_unwind(inside);
                }
            }
        }
    }
}

impl fmt::Debug for Noun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Noun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            if self.is_direct() {
                write!(f, "{}", self.direct)
            } else if self.is_indirect() {
                write!(f, "{}", self.indirect)
            } else if self.is_cell() {
                write!(f, "{}", self.cell)
            } else if self.allocated.forwarding_pointer().is_some() {
                write!(
                    f,
                    "Noun::Forwarding({})",
                    self.allocated.forwarding_pointer().unwrap()
                )
            } else {
                write!(f, "Noun::Unknown({:x})", self.raw)
            }
        }
    }
}

impl Slots for Noun {}
impl private::RawSlots for Noun {
    fn raw_slot(&self, axis: &BitSlice<u64, Lsb0>) -> Result<Noun> {
        match self.as_either_atom_cell() {
            Right(cell) => cell.raw_slot(axis),
            Left(_atom) => {
                if axis.last_one() == Some(0) {
                    Ok(*self)
                } else {
                    // Axis tried to descend through atom
                    Err(Error::NotCell)
                }
            }
        }
    }
}

/**
 * An allocation object (probably a mem::NockStack) which can allocate a memory buffer sized to
 * a certain number of nouns
 */
pub trait NounAllocator: Sized {
    /** Allocate memory for some multiple of the size of a noun
     *
     * This should allocate *two more* `u64`s than `words` to make space for the size and metadata
     */
    unsafe fn alloc_indirect(&mut self, words: usize) -> *mut u64;

    /** Allocate memory for a cell */
    unsafe fn alloc_cell(&mut self) -> *mut CellMemory;
}

/**
 * Implementing types allow component Nouns to be retreived by numeric axis
 */
pub trait Slots: private::RawSlots {
    /**
     * Retrieve component Noun at given axis, or fail with descriptive error
     */
    fn slot(&self, axis: u64) -> Result<Noun> {
        self.raw_slot(BitSlice::from_element(&axis))
    }

    /**
     * Retrieve component Noun at axis given as Atom, or fail with descriptive error
     */
    fn slot_atom(&self, atom: Atom) -> Result<Noun> {
        match atom.as_either() {
            Left(direct) => self.slot(direct.data()),
            Right(indirect) => self.raw_slot(indirect.as_bitslice()),
        }
    }
}

/**
 * Implementation methods that should not be made available to derived crates
 */
mod private {
    use crate::noun::{BitSlice, Lsb0, Noun, Result};

    /**
     * Implementation of the Slots trait
     */
    pub trait RawSlots {
        /**
         * Actual logic of retreiving Noun object at some axis
         */
        fn raw_slot(&self, axis: &BitSlice<u64, Lsb0>) -> Result<Noun>;
    }
}
