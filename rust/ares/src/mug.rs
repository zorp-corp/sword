use crate::assert_acyclic;
use crate::mem::*;
use crate::noun::{Allocated, Atom, DirectAtom, Noun};
use either::Either::*;
use murmur3::murmur3_32_nocopy;
use std::cmp::min;
use std::io::{Read, Result};
use std::ptr::{copy_nonoverlapping, write_bytes};
use crate::noun::acyclic_noun;

/** A reader for an atom which pads the atom out to a given length */
struct PaddedReadAtom {
    atom_bytes: usize,    // actual size of the stored atom
    atom_base: *const u8, // pointer to the atom data
    atom_cursor: usize,   // How many bytes we have read
    atom_len: usize,      // The total padded length
}

impl PaddedReadAtom {
    fn new(atom: Atom, len: usize) -> Self {
        match atom.as_either() {
            Left(direct) => PaddedReadAtom {
                atom_bytes: 8,
                atom_base: (&direct as *const DirectAtom) as *const u8,
                atom_cursor: 0,
                atom_len: len,
            },
            Right(indirect) => PaddedReadAtom {
                atom_bytes: indirect.size() << 3, // size is in 64 bit words, multiply by 8 to get bytes
                atom_base: indirect.data_pointer() as *const u8, // data pointer, but for bytes
                atom_cursor: 0,
                atom_len: len,
            },
        }
    }
}

impl Read for PaddedReadAtom {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        if self.atom_cursor >= self.atom_len {
            Ok(0) // we are done
        } else {
            let req = buf.len(); // How many bytes does the reading caller want?
            if self.atom_cursor < self.atom_bytes {
                // are we still reading bytes from the atom?
                let len = min(
                    self.atom_len - self.atom_cursor,
                    min(self.atom_bytes - self.atom_cursor, req),
                );
                // copy out bytes into the buffer, not running over the atom length itself, the
                // padded length, or the buffer length
                unsafe {
                    copy_nonoverlapping(
                        self.atom_base.add(self.atom_cursor),
                        buf.as_mut_ptr(),
                        len,
                    );
                }
                self.atom_cursor += len;
                Ok(len)
            } else {
                // We are past the atom and into padding
                let len = min(self.atom_len - self.atom_cursor, req);
                // write 0s until we hit the buffer length or the padded length
                unsafe {
                    write_bytes(buf.as_mut_ptr(), 0, len);
                }
                self.atom_cursor += len;
                Ok(len)
            }
        }
    }
}

// Murmur3 hash an atom with a given padded length
fn muk_u32(syd: u32, len: usize, key: Atom) -> u32 {
    match key.as_either() {
        Left(direct) => {
            murmur3_32_nocopy(&direct.data().to_le_bytes()[0..len], syd)
        },
        Right(indirect) => {
            murmur3_32_nocopy(&indirect.as_bytes()[..len], syd)
        },
    }
}

/** Byte size of an atom.
 *
 * Assumes atom is normalized
 */
fn met3_usize(atom: Atom) -> usize {
    match atom.as_either() {
        Left(direct) => (64 - (direct.data().leading_zeros() as usize) + 7) >> 3,
        Right(indirect) => {
            let last_word = unsafe { *(indirect.data_pointer().add(indirect.size() - 1)) };
            let last_word_bytes = (64 - (last_word.leading_zeros() as usize) + 7) >> 3;
            ((indirect.size() - 1) << 3) + last_word_bytes
        }
    }
}

fn mum_u32(syd: u32, fal: u32, key: Atom) -> u32 {
    let wyd = met3_usize(key);
    let mut i = 0;
    loop {
        if i == 8 {
            break fal;
        } else {
            let haz = muk_u32(syd, wyd, key);
            let ham = (haz >> 31) ^ (haz & !(1 << 31));
            if ham == 0 {
                i += 1;
                continue;
            } else {
                break ham;
            }
        }
    }
}

fn calc_atom_mug_u32(atom: Atom) -> u32 {
    mum_u32(0xCAFEBABE, 0x7FFF, atom)
}

/** Unsafe because this passes a direct atom to mum_u32 made by concatenating the two mugs,
 * so we must ensure that the tail_mug conforms to the mug invariant and is only 31 bits
 */
unsafe fn calc_cell_mug_u32(head_mug: u32, tail_mug: u32) -> u32 {
    let cat_mugs = (head_mug as u64) | ((tail_mug as u64) << 32);
    mum_u32(
        0xDEADBEEF,
        0xFFFE,
        DirectAtom::new_unchecked(cat_mugs).as_atom(),
    ) // this is safe on mugs since mugs are 31 bits
}

pub fn get_mug(noun: Noun) -> Option<u32> {
    match noun.as_either_direct_allocated() {
        Left(direct) => Some(calc_atom_mug_u32(direct.as_atom())),
        Right(allocated) => allocated.get_cached_mug(),
    }
}

const MASK_OUT_MUG: u64 = !(u32::MAX as u64);

unsafe fn set_mug(allocated: Allocated, mug: u32) {
    let metadata = allocated.get_metadata();
    allocated.set_metadata((metadata & MASK_OUT_MUG) | (mug as u64));
}

/** Calculate and cache the mug for a noun, but do *not* recursively calculate cache mugs for
 * children of cells.
 *
 * If called on a cell with no mug cached for the head or tail, this function will return `None`.
 */
pub fn allocated_mug_u32_one(allocated: Allocated) -> Option<u32> {
    match allocated.get_cached_mug() {
        Some(mug) => Some(mug),
        None => match allocated.as_either() {
            Left(indirect) => {
                let mug = calc_atom_mug_u32(indirect.as_atom());
                unsafe {
                    set_mug(allocated, mug);
                }
                Some(mug)
            }
            Right(cell) => match (get_mug(cell.head()), get_mug(cell.tail())) {
                (Some(head_mug), Some(tail_mug)) => {
                    let mug = unsafe { calc_cell_mug_u32(head_mug, tail_mug) };
                    unsafe {
                        set_mug(allocated, mug);
                    }
                    Some(mug)
                }
                _ => None,
            },
        },
    }
}

pub fn mug_u32_one(noun: Noun) -> Option<u32> {
    match noun.as_either_direct_allocated() {
        Left(direct) => Some(calc_atom_mug_u32(direct.as_atom())),
        Right(allocated) => allocated_mug_u32_one(allocated),
    }
}

pub fn mug_u32(stack: &mut NockStack, noun: Noun) -> u32 {
    assert_acyclic!(noun);
    stack.push(1);
    unsafe {
        stack.save_prev_stack_pointer_to_local(0);
        *(stack.alloc_in_previous_frame()) = noun;
    }
    loop {
        if unsafe { stack.prev_stack_pointer_equals_local(0) } {
            break;
        } else {
            let noun: Noun = unsafe { *(stack.top_in_previous_frame()) };
            match noun.as_either_direct_allocated() {
                Left(_direct) => {
                    unsafe {
                        stack.reclaim_in_previous_frame::<Noun>();
                    }
                    continue;
                } // no point in calculating a direct mug here as we wont cache it
                Right(allocated) => match allocated.get_cached_mug() {
                    Some(_mug) => {
                        unsafe {
                            stack.reclaim_in_previous_frame::<Noun>();
                        }
                        continue;
                    }
                    None => match allocated.as_either() {
                        Left(indirect) => unsafe {
                            set_mug(allocated, calc_atom_mug_u32(indirect.as_atom()));
                            stack.reclaim_in_previous_frame::<Noun>();
                            continue;
                        },
                        Right(cell) => unsafe {
                            match (get_mug(cell.head()), get_mug(cell.tail())) {
                                (Some(head_mug), Some(tail_mug)) => {
                                    set_mug(allocated, calc_cell_mug_u32(head_mug, tail_mug));
                                    stack.reclaim_in_previous_frame::<Noun>();
                                    continue;
                                }
                                _ => {
                                    *(stack.alloc_in_previous_frame()) = cell.tail();
                                    *(stack.alloc_in_previous_frame()) = cell.head();
                                    continue;
                                }
                            }
                        },
                    },
                },
            }
        }
    }
    unsafe {
        stack.pop_no_copy();
        get_mug(noun).expect("Noun should have a mug once it is mugged.")
    }
}

pub fn mug(stack: &mut NockStack, noun: Noun) -> DirectAtom {
    unsafe { DirectAtom::new_unchecked(mug_u32(stack, noun) as u64) }
}
