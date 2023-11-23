use crate::assert_acyclic;
use crate::hamt::MutHamt;
use crate::mem::NockStack;
use crate::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun};
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::{Left, Right};

crate::gdb!();

pub fn met0_usize(atom: Atom) -> usize {
    let atom_bitslice = atom.as_bitslice();
    match atom_bitslice.last_one() {
        Some(last_one) => last_one + 1,
        None => 0,
    }
}

pub fn met0_u64_to_usize(x: u64) -> usize {
    let usize_bitslice = BitSlice::<u64, Lsb0>::from_element(&x);
    match usize_bitslice.last_one() {
        Some(last_one) => last_one + 1,
        None => 0,
    }
}

pub fn cue_bytes(stack: &mut NockStack, buffer: &[u8]) -> Noun {
    let buffer_atom = unsafe {
        IndirectAtom::new_raw_bytes(stack, buffer.len(), buffer.as_ptr()).normalize_as_atom()
    };
    cue(stack, buffer_atom)
}

pub fn cue(stack: &mut NockStack, buffer: Atom) -> Noun {
    let buffer_bitslice = buffer.as_bitslice();
    let mut cursor: usize = 0;
    let backref_map = MutHamt::<Noun>::new(stack);
    stack.frame_push(1);
    unsafe {
        *(stack.push::<*mut Noun>()) = stack.local_noun_pointer(0);
    };
    loop {
        if stack.stack_is_empty() {
            let mut result = unsafe { *stack.local_noun_pointer(0) };
            assert_acyclic!(result);
            unsafe {
                stack.preserve(&mut result);
                stack.frame_pop();
            }
            break result;
        } else {
            let dest_ptr: *mut Noun = unsafe { *(stack.top()) };
            if buffer_bitslice[cursor] {
                // 1 bit
                if buffer_bitslice[cursor + 1] {
                    // 11 bits - cue backreference
                    cursor += 2;
                    unsafe {
                        let mut backref_noun =
                            Atom::new(stack, rub_backref(&mut cursor, buffer_bitslice)).as_noun();
                        let reffed_noun = backref_map
                            .lookup(stack, &mut backref_noun)
                            .expect("Invalid backref in cue");
                        assert_acyclic!(reffed_noun);
                        if let Ok(indirect) = reffed_noun.as_indirect() {
                            debug_assert!(indirect.size() > 0);
                        }
                        *dest_ptr = reffed_noun;
                        assert_acyclic!(reffed_noun);
                        stack.pop::<*mut Noun>();
                    }
                    continue;
                } else {
                    // 10 bits - cue cell
                    let backref = cursor;
                    cursor += 2;
                    unsafe {
                        let (cell, cell_mem_ptr) = Cell::new_raw_mut(stack);
                        *dest_ptr = cell.as_noun();
                        let mut backref_atom = Atom::new(stack, backref as u64).as_noun();
                        backref_map.insert(stack, &mut backref_atom, *dest_ptr);
                        stack.pop::<*mut Noun>();
                        (*cell_mem_ptr).tail =
                            DirectAtom::new_unchecked(0xEDBEEF).as_atom().as_noun();
                        (*cell_mem_ptr).head =
                            DirectAtom::new_unchecked(0xDEBEEF).as_atom().as_noun();
                        *(stack.push()) = &mut (*cell_mem_ptr).tail;
                        *(stack.push()) = &mut (*cell_mem_ptr).head;
                    }
                    continue;
                }
            } else {
                // 0 bit - cue atom
                let backref = cursor;
                cursor += 1;
                unsafe {
                    *dest_ptr = rub_atom(stack, &mut cursor, buffer_bitslice).as_noun();
                    let mut backref_atom = Atom::new(stack, backref as u64).as_noun();
                    backref_map.insert(stack, &mut backref_atom, *dest_ptr);
                    stack.pop::<*mut Noun>();
                };
                continue;
            }
        };
    }
}

// TODO: use first_zero() on a slice of the buffer
fn get_size(cursor: &mut usize, buffer: &BitSlice<u64, Lsb0>) -> usize {
    let buff_at_cursor = &buffer[*cursor..];
    let bitsize = buff_at_cursor
        .first_one()
        .expect("Size encoding must terminate with a 1 bit");
    if bitsize == 0 {
        *cursor += 1;
        0
    } else {
        let mut size: u64 = 0;
        BitSlice::from_element_mut(&mut size)[0..bitsize - 1]
            .copy_from_bitslice(&buffer[*cursor + bitsize + 1..*cursor + bitsize + bitsize]);
        *cursor += bitsize + bitsize;
        (size as usize) + (1 << (bitsize - 1))
    }
}

fn rub_atom(stack: &mut NockStack, cursor: &mut usize, buffer: &BitSlice<u64, Lsb0>) -> Atom {
    let size = get_size(cursor, buffer);
    if size == 0 {
        unsafe { DirectAtom::new_unchecked(0).as_atom() }
    } else if size < 64 {
        // fits in a direct atom
        let mut direct_raw = 0;
        BitSlice::from_element_mut(&mut direct_raw)[0..size]
            .copy_from_bitslice(&buffer[*cursor..*cursor + size]);
        *cursor += size;
        unsafe { DirectAtom::new_unchecked(direct_raw).as_atom() }
    } else {
        // need an indirect atom
        let wordsize = (size + 63) >> 6;
        let (atom, slice) = unsafe { IndirectAtom::new_raw_mut_bitslice(stack, wordsize) }; // fast round to wordsize
        slice[0..size].copy_from_bitslice(&buffer[*cursor..*cursor + size]);
        debug_assert!(atom.size() > 0);
        *cursor += size;
        atom.as_atom()
    }
}

fn rub_backref(cursor: &mut usize, buffer: &BitSlice<u64, Lsb0>) -> u64 {
    let size = get_size(cursor, buffer);
    if size == 0 {
        0
    } else if size <= 64 {
        let mut backref: u64 = 0;
        BitSlice::from_element_mut(&mut backref)[0..size]
            .copy_from_bitslice(&buffer[*cursor..*cursor + size]);
        *cursor += size;
        backref
    } else {
        panic!("Backreference size too big for vere")
    }
}

struct JamState<'a> {
    cursor: usize,
    size: usize,
    atom: IndirectAtom,
    slice: &'a mut BitSlice<u64, Lsb0>,
}

pub fn jam(stack: &mut NockStack, noun: Noun) -> Atom {
    let backref_map = MutHamt::new(stack);
    let size = 8;
    let (atom, slice) = unsafe { IndirectAtom::new_raw_mut_bitslice(stack, size) };
    let mut state = JamState {
        cursor: 0,
        size,
        atom,
        slice,
    };
    stack.frame_push(0);
    unsafe {
        *(stack.push::<Noun>()) = noun;
    };
    'jam: loop {
        if stack.stack_is_empty() {
            break;
        } else {
            let mut noun = unsafe { *(stack.top::<Noun>()) };
            if let Some(backref) = backref_map.lookup(stack, &mut noun) {
                match noun.as_either_atom_cell() {
                    Left(atom) => {
                        let atom_size = met0_usize(atom);
                        let backref_size = met0_u64_to_usize(backref);
                        if atom_size <= backref_size {
                            jam_atom(stack, &mut state, atom);
                        } else {
                            jam_backref(stack, &mut state, backref);
                        }
                    }
                    Right(_cell) => {
                        jam_backref(stack, &mut state, backref);
                    }
                }
                unsafe {
                    stack.pop::<Noun>();
                };
                continue 'jam;
            };
            backref_map.insert(stack, &mut noun, state.cursor as u64);
            match noun.as_either_atom_cell() {
                Left(atom) => {
                    jam_atom(stack, &mut state, atom);
                    unsafe {
                        stack.pop::<Noun>();
                    };
                    continue;
                }
                Right(cell) => {
                    jam_cell(stack, &mut state);
                    unsafe {
                        stack.pop::<Noun>();
                        *(stack.push::<Noun>()) = cell.tail();
                        *(stack.push::<Noun>()) = cell.head();
                    };
                    continue;
                }
            }
        }
    }
    unsafe {
        let mut result = state.atom.normalize_as_atom();
        stack.preserve(&mut result);
        stack.frame_pop();
        result
    }
}

fn jam_atom(traversal: &mut NockStack, state: &mut JamState, atom: Atom) {
    loop {
        if state.cursor + 1 > state.slice.len() {
            double_atom_size(traversal, state);
        } else {
            break;
        }
    }
    state.slice.set(state.cursor, false);
    state.cursor += 1;
    loop {
        if let Ok(()) = mat(traversal, state, atom) {
            break;
        } else {
            double_atom_size(traversal, state);
        }
    }
}

fn jam_cell(traversal: &mut NockStack, state: &mut JamState) {
    loop {
        if state.cursor + 2 > state.slice.len() {
            double_atom_size(traversal, state);
        } else {
            break;
        }
    }
    state.slice.set(state.cursor, true);
    state.slice.set(state.cursor + 1, false);
    state.cursor += 2;
}

fn jam_backref(traversal: &mut NockStack, state: &mut JamState, backref: u64) {
    loop {
        if state.cursor + 2 > state.slice.len() {
            double_atom_size(traversal, state);
        } else {
            break;
        }
    }
    state.slice.set(state.cursor, true);
    state.slice.set(state.cursor + 1, true);
    state.cursor += 2;
    let backref_atom = Atom::new(traversal, backref);
    loop {
        if let Ok(()) = mat(traversal, state, backref_atom) {
            break;
        } else {
            double_atom_size(traversal, state);
        }
    }
}

fn double_atom_size(traversal: &mut NockStack, state: &mut JamState) {
    let new_size = state.size + state.size;
    let (new_atom, new_slice) = unsafe { IndirectAtom::new_raw_mut_bitslice(traversal, new_size) };
    new_slice[0..state.cursor].copy_from_bitslice(&state.slice[0..state.cursor]);
    state.size = new_size;
    state.atom = new_atom;
    state.slice = new_slice;
}

// INVARIANT: mat must not modify state.cursor unless it will also return `Ok(())`
fn mat(traversal: &mut NockStack, state: &mut JamState, atom: Atom) -> Result<(), ()> {
    let b_atom_size = met0_usize(atom);
    let b_atom_size_atom = Atom::new(traversal, b_atom_size as u64);
    if b_atom_size == 0 {
        if state.cursor + 1 > state.slice.len() {
            Err(())
        } else {
            state.slice.set(state.cursor, true);
            state.cursor += 1;
            Ok(())
        }
    } else {
        let c_b_size = met0_usize(b_atom_size_atom);
        if state.cursor + c_b_size + c_b_size + b_atom_size > state.slice.len() {
            Err(())
        } else {
            state.slice[state.cursor..state.cursor + c_b_size].fill(false); // a 0 bit for each bit in the atom size
            state.slice.set(state.cursor + c_b_size, true); // a terminating 1 bit
            state.slice[state.cursor + c_b_size + 1..state.cursor + c_b_size + c_b_size]
                .copy_from_bitslice(&b_atom_size_atom.as_bitslice()[0..c_b_size - 1]); // the atom size excepting the most significant 1 (since we know where that is from the size-of-the-size)
            state.slice[state.cursor + c_b_size + c_b_size
                ..state.cursor + c_b_size + c_b_size + b_atom_size]
                .copy_from_bitslice(&atom.as_bitslice()[0..b_atom_size]); // the atom itself
            state.cursor += c_b_size + c_b_size + b_atom_size;
            Ok(())
        }
    }
}
