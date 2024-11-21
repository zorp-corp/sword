use crate::mem::{AllocResult, NockStack, Preserve};
use crate::mug::mug_u32;
use crate::noun::Noun;
use crate::unifying_equality::unifying_equality;
use either::Either::{self, *};
use std::ptr::{copy_nonoverlapping, null_mut};
use std::slice;

type MutStemEntry<T> = Either<*mut MutStem<T>, Leaf<T>>;

type StemEntry<T> = Either<Stem<T>, Leaf<T>>;

#[inline]
fn chunk_to_bit(chunk: u32) -> u32 {
    1u32 << chunk
}

#[inline]
fn chunk_to_mask(chunk: u32) -> u32 {
    // mask out the bit for the chunk and all more significant
    chunk_to_bit(chunk) - 1
}

#[repr(packed)]
struct MutStem<T: Copy> {
    bitmap: u32,
    typemap: u32,
    buffer: [MutEntry<T>; 32],
}

union MutEntry<T: Copy> {
    stem: *mut MutStem<T>,
    // XX we might want a mutable leaf field with some
    // preallocated capacity
    leaf: Leaf<T>,
}

impl<T: Copy> MutStem<T> {
    #[inline]
    fn has_index(&self, chunk: u32) -> bool {
        self.bitmap & chunk_to_bit(chunk) != 0
    }

    #[inline]
    fn entry(&self, chunk: u32) -> Option<MutStemEntry<T>> {
        if self.has_index(chunk) {
            if self.typemap & chunk_to_bit(chunk) != 0 {
                unsafe { Some(Left(self.buffer[chunk as usize].stem)) }
            } else {
                unsafe { Some(Right(self.buffer[chunk as usize].leaf)) }
            }
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
pub struct MutHamt<T: Copy>(*mut MutStem<T>);

impl<T: Copy> MutHamt<T> {
    pub fn new(stack: &mut NockStack) -> AllocResult<MutHamt<T>> {
        unsafe {
            let new_stem = stack.struct_alloc::<MutStem<T>>(1)?;
            (*new_stem).bitmap = 0;
            (*new_stem).typemap = 0;
            Ok(MutHamt(new_stem))
        }
    }

    pub fn lookup(self, stack: &mut NockStack, n: &mut Noun) -> AllocResult<Option<T>> {
        let mut stem = self.0;
        let mut mug = mug_u32(stack, *n)?;
        unsafe {
            'lookup: loop {
                let chunk = mug & 0x1f;
                mug >>= 5;
                match (*stem).entry(chunk) {
                    None => {
                        break Ok(None);
                    }
                    Some(Left(next_stem)) => {
                        stem = next_stem;
                    }
                    Some(Right(leaf)) => {
                        for pair in leaf.to_mut_slice().iter_mut() {
                            if unifying_equality(stack, n, &mut pair.0)? {
                                break 'lookup Ok(Some(pair.1));
                            }
                        }
                        break Ok(None);
                    }
                }
            }
        }
    }

    pub fn insert(self, stack: &mut NockStack, n: &mut Noun, t: T) -> AllocResult<()> {
        let mut stem = self.0;
        let mut mug = mug_u32(stack, *n)?;
        let mut depth = 0u8;
        unsafe {
            'insert: loop {
                let chunk = mug & 0x1f;
                mug >>= 5;
                match (*stem).entry(chunk) {
                    None => {
                        let new_leaf_buffer = stack.struct_alloc::<(Noun, T)>(1)?;
                        *new_leaf_buffer = (*n, t);
                        (*stem).bitmap |= chunk_to_bit(chunk);
                        (*stem).typemap &= !chunk_to_bit(chunk);
                        (*stem).buffer[chunk as usize] = MutEntry {
                            leaf: Leaf {
                                len: 1,
                                buffer: new_leaf_buffer,
                            },
                        };
                        break;
                    }
                    Some(Left(next_stem)) => {
                        depth += 1;
                        stem = next_stem;
                        continue;
                    }
                    Some(Right(leaf)) => {
                        for pair in leaf.to_mut_slice().iter_mut() {
                            if unifying_equality(stack, n, &mut pair.0)? {
                                pair.1 = t;
                                break 'insert;
                            }
                        }
                        if depth >= 5 {
                            let new_leaf_buffer = stack.struct_alloc::<(Noun, T)>(leaf.len + 1)?;
                            copy_nonoverlapping(leaf.buffer, new_leaf_buffer, leaf.len);
                            *new_leaf_buffer.add(leaf.len) = (*n, t);
                            (*stem).buffer[chunk as usize] = MutEntry {
                                leaf: Leaf {
                                    len: leaf.len + 1,
                                    buffer: new_leaf_buffer,
                                },
                            };
                            break;
                        } else {
                            assert!(leaf.len == 1);
                            let new_stem = stack.struct_alloc::<MutStem<T>>(1)?;
                            let leaf_mug = mug_u32(stack, (*leaf.buffer).0)?;
                            let leaf_chunk = (leaf_mug >> ((depth + 1) * 5)) & 0x1f;
                            (*new_stem).bitmap = chunk_to_bit(leaf_chunk);
                            (*new_stem).typemap = 0;
                            (*new_stem).buffer[leaf_chunk as usize] = MutEntry { leaf };
                            (*stem).buffer[chunk as usize] = MutEntry { stem: new_stem };
                            (*stem).typemap |= chunk_to_bit(chunk);
                            stem = new_stem;
                            depth += 1;
                            continue;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

/**
 * This is the core memory structure of an immutable HAMT.
 *
 * The root Stem lives in its own memory allocation, addressed by the pointer wrapped by [Hamt].
 * All other Stems and Leaves live in memory blocks pointed to by [buffer]. The memory pointed to
 * by this field may be zero to 32 entries, depending on the *number of bits set* in bitmap.
 *
 * Addressing a chunk of the key's hash is done by counting the number of set bits in the bitmap
 * before the chunk'th bit. The typemap is a parallel bitmap in which bits are set if the
 * corresponding entry is a stem, and cleared if it is a leaf.
 */
#[repr(packed)]
#[repr(C)]
struct Stem<T: Copy> {
    bitmap: u32,
    typemap: u32,
    buffer: *mut Entry<T>,
}

impl<T: Copy> Copy for Stem<T> {}

impl<T: Copy> Clone for Stem<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Copy> Stem<T> {
    #[inline]
    fn size(self) -> usize {
        self.bitmap.count_ones() as usize
    }

    #[inline]
    fn has_index(self, chunk: u32) -> bool {
        self.bitmap & chunk_to_bit(chunk) != 0
    }

    #[inline]
    fn hypothetical_index(self, chunk: u32) -> usize {
        (self.bitmap & chunk_to_mask(chunk)).count_ones() as usize
    }

    #[inline]
    fn index(self, chunk: u32) -> Option<usize> {
        if self.has_index(chunk) {
            Some(self.hypothetical_index(chunk))
        } else {
            None
        }
    }

    #[inline]
    fn entry(self, chunk: u32) -> Option<(StemEntry<T>, usize)> {
        self.index(chunk).map(|idx| {
            (
                unsafe {
                    if self.typemap & chunk_to_bit(chunk) != 0 {
                        Left((*self.buffer.add(idx)).stem)
                    } else {
                        Right((*self.buffer.add(idx)).leaf)
                    }
                },
                idx,
            )
        })
    }
}

#[repr(packed)]
#[repr(C)]
struct Leaf<T: Copy> {
    len: usize,
    buffer: *mut (Noun, T), // mutable for unifying equality
}

impl<T: Copy> Copy for Leaf<T> {}

impl<T: Copy> Clone for Leaf<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Copy> Leaf<T> {
    unsafe fn to_slice<'a>(self) -> &'a [(Noun, T)] {
        slice::from_raw_parts(self.buffer, self.len)
    }
    unsafe fn to_mut_slice<'a>(self) -> &'a mut [(Noun, T)] {
        slice::from_raw_parts_mut(self.buffer, self.len)
    }
}

#[derive(Copy, Clone)]
#[repr(packed)]
#[repr(C)]
union Entry<T: Copy> {
    stem: Stem<T>,
    leaf: Leaf<T>,
}

// Entries in our union are the same size and alignment
assert_eq_size!(Entry<()>, Leaf<()>);
assert_eq_align!(Entry<()>, Leaf<()>);
assert_eq_size!(Entry<()>, Stem<()>);
assert_eq_align!(Entry<()>, Stem<()>);

// Our custom leaf type is the same size as a fat pointer to key-value pairs
assert_eq_size!(&[(Noun, ())], Leaf<()>);

// Our custom stem type is the same size as a fat pointer to `Entry`s
assert_eq_size!(&[Entry<()>], Stem<()>);

#[derive(Copy, Clone)]
pub struct Hamt<T: Copy>(*mut Stem<T>);

impl<T: Copy + Preserve> Hamt<T> {
    pub fn is_null(&self) -> bool {
        unsafe { (*self.0).bitmap == 0 }
    }
    // Make a new, empty HAMT
    pub fn new(stack: &mut NockStack) -> AllocResult<Self> {
        unsafe {
            let stem_ptr = stack.struct_alloc::<Stem<T>>(1)?;
            *stem_ptr = Stem {
                bitmap: 0,
                typemap: 0,
                buffer: null_mut(),
            };
            Ok(Hamt(stem_ptr))
        }
    }

    /// Borrowing iterator for Hamt, the type name is a portmanteau of Hamt, iterator, and hamster.
    pub fn iter(&self) -> Hamsterator<T> {
        Hamsterator::new(self)
    }

    /**
     * Look up a pair keyed by a noun in the HAMT
     *
     * A mutable reference is required so that unifying equality can unify the key with a key entry
     * in the HAMT
     */
    pub fn lookup(&self, stack: &mut NockStack, n: &mut Noun) -> AllocResult<Option<T>> {
        let mut stem = unsafe { *self.0 };
        let mut mug = mug_u32(stack, *n)?;
        'lookup: loop {
            let chunk = mug & 0x1F; // 5 bits
            mug >>= 5;
            match stem.entry(chunk) {
                None => {
                    break Ok(None);
                }
                Some((Left(next_stem), _idx)) => {
                    stem = next_stem;
                    continue;
                }
                Some((Right(leaf), _idx)) => {
                    for pair in unsafe { leaf.to_mut_slice().iter_mut() } {
                        if unsafe { unifying_equality(stack, n, &mut pair.0)? } {
                            break 'lookup Ok(Some(pair.1));
                        }
                    }
                    break Ok(None);
                }
            }
        }
    }

    // XX a delete function requires a stack, do we need one?

    /// Make a new HAMT with the value inserted or replaced at the key.
    pub fn insert(&self, stack: &mut NockStack, n: &mut Noun, t: T) -> AllocResult<Hamt<T>> {
        let mut mug = mug_u32(stack, *n)?;
        let mut depth = 0u8;
        let mut stem = unsafe { *self.0 };
        let stem_ret = unsafe { stack.struct_alloc::<Stem<T>>(1) }?;
        let mut dest = stem_ret;
        unsafe {
            'insert: loop {
                let chunk = mug & 0x1F; // 5 bits
                mug >>= 5;
                match stem.entry(chunk) {
                    // No entry found at mug chunk index; add Leaf to current Stem
                    None => {
                        let new_leaf_buffer = stack.struct_alloc(1)?;
                        *new_leaf_buffer = (*n, t);
                        let split = stem.hypothetical_index(chunk);
                        let new_buffer = stack.struct_alloc(stem.size() + 1)?;
                        if split > 0 {
                            copy_nonoverlapping(stem.buffer, new_buffer, split);
                        }
                        *new_buffer.add(split) = Entry {
                            leaf: Leaf {
                                len: 1,
                                buffer: new_leaf_buffer,
                            },
                        };
                        if stem.size() - split > 0 {
                            copy_nonoverlapping(
                                stem.buffer.add(split),
                                new_buffer.add(split + 1),
                                stem.size() - split,
                            );
                        }
                        *dest = Stem {
                            bitmap: stem.bitmap | chunk_to_bit(chunk),
                            typemap: stem.typemap & !chunk_to_bit(chunk),
                            buffer: new_buffer,
                        };
                        break Ok(Hamt(stem_ret));
                    }
                    // Stem found at mug chunk index; insert into found Stem
                    Some((Left(next_stem), idx)) => {
                        let new_buffer = stack.struct_alloc(stem.size())?;
                        copy_nonoverlapping(stem.buffer, new_buffer, stem.size());
                        *dest = Stem {
                            bitmap: stem.bitmap,
                            typemap: stem.typemap,
                            buffer: new_buffer,
                        };
                        dest = &mut (*new_buffer.add(idx)).stem;
                        stem = next_stem;
                        depth += 1;
                        continue;
                    }
                    // Leaf found at mug chunk index
                    Some((Right(leaf), idx)) => {
                        // Override existing value for key, if one exists
                        for (ldx, pair) in leaf.to_mut_slice().iter_mut().enumerate() {
                            if unifying_equality(stack, n, &mut pair.0)? {
                                let new_leaf_buffer = stack.struct_alloc(leaf.len)?;
                                copy_nonoverlapping(leaf.buffer, new_leaf_buffer, leaf.len);
                                (*new_leaf_buffer.add(ldx)).1 = t;
                                let new_buffer = stack.struct_alloc(stem.size())?;
                                copy_nonoverlapping(stem.buffer, new_buffer, stem.size());
                                *new_buffer.add(idx) = Entry {
                                    leaf: Leaf {
                                        len: leaf.len,
                                        buffer: new_leaf_buffer,
                                    },
                                };
                                *dest = Stem {
                                    bitmap: stem.bitmap,
                                    typemap: stem.typemap,
                                    buffer: new_buffer,
                                };
                                break 'insert Ok(Hamt(stem_ret));
                            }
                        }
                        // No existing pair in this Leaf matches the key, and we've maxxed out the
                        // Hamt depth; add the the key-value pair to the list of pairs for this Leaf
                        if depth >= 5 {
                            let new_leaf_buffer = stack.struct_alloc(leaf.len + 1)?;
                            copy_nonoverlapping(leaf.buffer, new_leaf_buffer, leaf.len);
                            *new_leaf_buffer.add(leaf.len) = (*n, t);
                            let new_buffer = stack.struct_alloc(stem.size())?;
                            copy_nonoverlapping(stem.buffer, new_buffer, stem.size());
                            *new_buffer.add(idx) = Entry {
                                leaf: Leaf {
                                    len: leaf.len + 1,
                                    buffer: new_leaf_buffer,
                                },
                            };
                            *dest = Stem {
                                bitmap: stem.bitmap,
                                typemap: stem.typemap,
                                buffer: new_buffer,
                            };
                            break 'insert Ok(Hamt(stem_ret));
                        // No existing pair in this Leaf matches the key, but we haven't maxxed out
                        // the Hamt depth yet. If we haven't hit the depth limit yet, we shouldn't
                        // be making a linked list of pairs. Turn the Leaf into a Stem and insert
                        // the new pair into the new Stem (also insert the pair in the existing
                        // Leaf, too).
                        } else {
                            // Make a fake node pointing to the old leaf and "insert into it" the
                            // next time around
                            assert!(leaf.len == 1);
                            let fake_buffer = stack.struct_alloc(1)?;
                            *fake_buffer = Entry { leaf };
                            // Get the mug chunk for the Noun at the *next* level so that we can
                            // build a fake stem for it
                            let fake_mug = mug_u32(stack, (*leaf.buffer).0)?;
                            let fake_chunk = (fake_mug >> ((depth + 1) * 5)) & 0x1F;
                            let next_stem = Stem {
                                bitmap: chunk_to_bit(fake_chunk),
                                typemap: 0,
                                buffer: fake_buffer,
                            };
                            let new_buffer = stack.struct_alloc(stem.size())?;
                            copy_nonoverlapping(stem.buffer, new_buffer, stem.size());
                            *dest = Stem {
                                bitmap: stem.bitmap,
                                typemap: stem.typemap | chunk_to_bit(chunk), // node now
                                buffer: new_buffer,
                            };
                            dest = &mut (*new_buffer.add(idx)).stem;
                            stem = next_stem;
                            depth += 1;
                            continue;
                        }
                    }
                }
            }
        }
    }
}

impl<T: Copy + Preserve> Preserve for Hamt<T> {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_struct_is_in(self.0, 1);
        stack.assert_struct_is_in((*self.0).buffer, (*self.0).size());
        let mut traversal_stack: [Option<(Stem<T>, u32)>; 6] = [None; 6];
        traversal_stack[0] = Some(((*self.0), 0));
        let mut traversal_depth = 1;
        'check: loop {
            if traversal_depth == 0 {
                break;
            }
            let (stem, mut position) = traversal_stack[traversal_depth - 1]
                .expect("Attempted to access uninitialized array element");
            // can we loop over the size and count leading 0s remaining in the bitmap?
            'check_stem: loop {
                if position >= 32 {
                    traversal_depth -= 1;
                    continue 'check;
                }
                match stem.entry(position) {
                    None => {
                        position += 1;
                        continue 'check_stem;
                    }
                    Some((Left(next_stem), _idx)) => {
                        stack.assert_struct_is_in(next_stem.buffer, next_stem.size());
                        assert!(traversal_depth <= 5); // will increment
                        traversal_stack[traversal_depth - 1] = Some((stem, position + 1));
                        traversal_stack[traversal_depth] = Some((next_stem, 0));
                        traversal_depth += 1;
                        continue 'check;
                    }
                    Some((Right(leaf), _idx)) => {
                        stack.assert_struct_is_in(leaf.buffer, leaf.len);
                        for pair in leaf.to_mut_slice().iter() {
                            pair.0.assert_in_stack(stack);
                            pair.1.assert_in_stack(stack);
                        }
                        position += 1;
                        continue 'check_stem;
                    }
                }
            }
        }
    }

    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        let res = if stack.is_in_frame(self.0) {
            let dest_stem = stack.struct_alloc_in_previous_frame(1)?;
            copy_nonoverlapping(self.0, dest_stem, 1);
            self.0 = dest_stem;
            if stack.is_in_frame((*dest_stem).buffer) {
                let dest_buffer = stack.struct_alloc_in_previous_frame((*dest_stem).size())?;
                copy_nonoverlapping((*dest_stem).buffer, dest_buffer, (*dest_stem).size());
                (*dest_stem).buffer = dest_buffer;
                // Here we're using the Rust stack since the array is a fixed
                // size. Thus it will be cleaned up if the Rust thread running
                // this is killed, and is therefore not an issue vs. if it were allocated
                // on the heap.
                //
                // In the past, this traversal stack was allocated in NockStack, but
                // exactly the right way to do this is less clear with the split stack.
                let mut traversal_stack: [Option<(Stem<T>, u32)>; 6] = [None; 6];
                traversal_stack[0] = Some(((*dest_stem), 0));
                let mut traversal_depth = 1;
                'preserve: loop {
                    if traversal_depth == 0 {
                        break;
                    }
                    let (stem, mut position) = traversal_stack[traversal_depth - 1]
                        .expect("Attempted to access uninitialized array element");
                    // can we loop over the size and count leading 0s remaining in the bitmap?
                    'preserve_stem: loop {
                        if position >= 32 {
                            traversal_depth -= 1;
                            continue 'preserve;
                        }
                        match stem.entry(position) {
                            None => {
                                position += 1;
                                continue 'preserve_stem;
                            }
                            Some((Left(next_stem), idx)) => {
                                if stack.is_in_frame(next_stem.buffer) {
                                    let dest_buffer =
                                        stack.struct_alloc_in_previous_frame(next_stem.size())?;
                                    copy_nonoverlapping(
                                        next_stem.buffer,
                                        dest_buffer,
                                        next_stem.size(),
                                    );
                                    let new_stem = Stem {
                                        bitmap: next_stem.bitmap,
                                        typemap: next_stem.typemap,
                                        buffer: dest_buffer,
                                    };
                                    *stem.buffer.add(idx) = Entry { stem: new_stem };
                                    assert!(traversal_depth <= 5); // will increment
                                    traversal_stack[traversal_depth - 1] =
                                        Some((stem, position + 1));
                                    traversal_stack[traversal_depth] = Some((new_stem, 0));
                                    traversal_depth += 1;
                                    continue 'preserve;
                                } else {
                                    position += 1;
                                    continue 'preserve_stem;
                                }
                            }
                            Some((Right(leaf), idx)) => {
                                if stack.is_in_frame(leaf.buffer) {
                                    let dest_buffer =
                                        stack.struct_alloc_in_previous_frame(leaf.len)?;
                                    copy_nonoverlapping(leaf.buffer, dest_buffer, leaf.len);
                                    let new_leaf = Leaf {
                                        len: leaf.len,
                                        buffer: dest_buffer,
                                    };
                                    for pair in new_leaf.to_mut_slice().iter_mut() {
                                        pair.0.preserve(stack)?;
                                        pair.1.preserve(stack)?;
                                    }
                                    *stem.buffer.add(idx) = Entry { leaf: new_leaf };
                                }
                                position += 1;
                                continue 'preserve_stem;
                            }
                        }
                    }
                }
            }
        };
        Ok(res)
    }
}

/// 🐹
/// Humorously named iterator for Hamt, which is a portmanteau of Hamt and iterator.
/// Maximum depth of the HAMT is 6, so we can safely use a fixed size array for the traversal stack.
/// I dropped the IntoIterator implementation because T has to be Copy anyhow.
pub struct Hamsterator<'a, T: Copy> {
    depth: usize,
    traversal_stack: [(Stem<T>, u32); 6],
    // Gets accessed via the stem, it isn't actually unused.
    #[allow(dead_code)]
    hamt: &'a Hamt<T>,
}

impl<'a, T: Copy> Hamsterator<'a, T> {
    pub fn new(hamt: &'a Hamt<T>) -> Self {
        let stem = unsafe { *hamt.0 };
        let depth = 0;
        let mut traversal_stack: [(Stem<T>, u32); 6] = [(
            Stem {
                bitmap: 0,
                typemap: 0,
                buffer: std::ptr::null_mut(),
            },
            0,
        ); 6];
        traversal_stack[0] = (stem, 0);
        Hamsterator {
            depth,
            traversal_stack,
            hamt,
        }
    }
}

impl<'a, T: Copy> Iterator for Hamsterator<'a, T> {
    type Item = &'a [(Noun, T)];

    // Iterate over the values in the HAMT
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.depth == usize::MAX {
                return None; // We've finished iterating
            }
            let (current_stem, position) = self.traversal_stack[self.depth];
            if position >= 32 {
                // We've finished this stem, go back up
                self.depth = self.depth.wrapping_sub(1);
                continue;
            }
            match current_stem.entry(position) {
                None => {
                    // No entry at this position, move to next
                    self.traversal_stack[self.depth].1 += 1;
                }
                Some((Left(next_stem), _)) => {
                    // Found a stem, go deeper
                    self.traversal_stack[self.depth].1 += 1;
                    self.depth += 1;
                    self.traversal_stack[self.depth] = (next_stem, 0);
                }
                Some((Right(leaf), _)) => {
                    // Found a leaf, return its value and prepare for next
                    self.traversal_stack[self.depth].1 += 1;
                    let slice = unsafe { leaf.to_slice() };
                    return Some(slice);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use super::*;
    use crate::noun::{Noun, D};

    fn cdr_(h: &mut Hamsterator<Noun>) -> Option<(u64, u64)> {
        if let Some(tiny_vec) = h.next() {
            if let &[(noun, t)] = tiny_vec {
                unsafe { Some((noun.as_raw(), t.as_raw())) }
            } else {
                None
            }
        } else {
            None
        }
    }

    fn cdr(h: &mut Hamsterator<Noun>) -> (u64, u64) {
        if let &[(noun, t)] = h.next().unwrap() {
            unsafe { (noun.as_raw(), t.as_raw()) }
        } else {
            panic!("Expected a pair")
        }
    }

    #[test]
    fn test_hamt_into_iter() {
        let size = 1 << 27;
        let top_slots = 100;
        let mut stack = NockStack::new(size, top_slots);
        let mut hamt = Hamt::<Noun>::new(&mut stack).unwrap();
        hamt = hamt.insert(&mut stack, &mut D(0), D(1)).unwrap();
        hamt = hamt.insert(&mut stack, &mut D(2), D(3)).unwrap();
        hamt = hamt.insert(&mut stack, &mut D(4), D(5)).unwrap();
        let mut iter = hamt.iter();
        let three = cdr(&mut iter);
        let one = cdr(&mut iter);
        let five = cdr(&mut iter);
        assert_eq!(three.0, 2);
        assert_eq!(three.1, 3);
        assert_eq!(one.0, 0);
        assert_eq!(one.1, 1);
        assert_eq!(five.0, 4);
        assert_eq!(five.1, 5);
    }

    #[test]
    fn test_hamt_iter_big() {
        let size = 1 << 27;
        let top_slots = 100;
        let mut stack = NockStack::new(size, top_slots);
        let mut hamt = Hamt::<Noun>::new(&mut stack).unwrap();
        let mut hs = HashSet::new();
        for n in 0..100 {
            hamt = hamt.insert(&mut stack, &mut D(n), D(n)).unwrap();
            hs.insert((n, n));
        }
        let mut iter = hamt.iter();
        while let Some((n, t)) = cdr_(&mut iter) {
            assert!(hs.remove(&(n, t)));
        }
        assert!(hs.is_empty());
    }

    #[test]
    fn test_hamt() {
        let size = 1 << 27;
        let top_slots = 100;
        let mut stack = NockStack::new(size, top_slots);
        let mut hamt = Hamt::<Noun>::new(&mut stack).unwrap();
        let mut n = D(0);
        let t = D(1);
        hamt = hamt.insert(&mut stack, &mut n, t).unwrap();
        let lu = hamt
            .lookup(&mut stack, &mut n)
            .expect("lookup failed due to OOM");
        let lu_value = unsafe { lu.expect("lookup failed").as_raw() };
        assert_eq!(lu_value, 1);
        let mut n = D(2);
        let t = D(3);
        hamt = hamt.insert(&mut stack, &mut n, t).unwrap();
        let lu = hamt
            .lookup(&mut stack, &mut D(2))
            .expect("lookup failed due to OOM");
        let lu_value = unsafe { lu.expect("lookup failed").as_raw() };
        assert_eq!(lu_value, 3);
    }

    #[test]
    fn test_hamt_collision_check() {
        let size = 1 << 27;
        let top_slots = 100;
        let mut stack = NockStack::new(size, top_slots);
        let mut hamt = Hamt::<Noun>::new(&mut stack).unwrap();
        // 3-way collision
        // x: 0 y: 87699370 x_hash: 2046756072 y_hash: 2046756072
        // x: 0 z: 317365951 x_hash: 2046756072 z_hash: 2046756072

        let mut n = D(0);
        let t = D(0);
        hamt = hamt.insert(&mut stack, &mut n, t).unwrap();

        let mut n = D(87699370);
        let t = D(87699370);
        hamt = hamt.insert(&mut stack, &mut n, t).unwrap();

        let mut n = D(317365951);
        let t = D(317365951);
        hamt = hamt.insert(&mut stack, &mut n, t).unwrap();

        let lu = hamt
            .lookup(&mut stack, &mut D(0))
            .expect("lookup failed due to OOM");
        let lu_value = unsafe { lu.expect("0 lookup failed").as_raw() };
        assert_eq!(lu_value, 0);

        let lu = hamt
            .lookup(&mut stack, &mut D(87699370))
            .expect("lookup failed due to OOM");
        let lu_value = unsafe { lu.expect("87699370 lookup failed").as_raw() };
        assert_eq!(lu_value, 87699370);

        let lu = hamt
            .lookup(&mut stack, &mut D(317365951))
            .expect("lookup failed due to OOM");
        let lu_value = unsafe { lu.expect("317365951 lookup failed").as_raw() };
        assert_eq!(lu_value, 317365951);
    }

    #[test]
    fn test_hamt_collision_iter() {
        let size = 1 << 27;
        let top_slots = 100;
        let mut stack = NockStack::new(size, top_slots);
        let mut hamt = Hamt::<Noun>::new(&mut stack).unwrap();
        // 3-way collision
        // x: 0 y: 87699370 x_hash: 2046756072 y_hash: 2046756072
        // x: 0 z: 317365951 x_hash: 2046756072 z_hash: 2046756072
        let mut hs = HashSet::new();
        for x in &[0, 87699370, 317365951] {
            hamt = hamt.insert(&mut stack, &mut D(*x), D(*x)).unwrap();
            hs.insert((*x, *x));
        }
        for x in hamt.iter() {
            for (n, t) in x {
                let k_raw = unsafe { n.as_raw() };
                let v_raw = unsafe { t.as_raw() };
                assert!(hs.remove(&(k_raw, v_raw)));
            }
        }
        assert!(hs.is_empty(), "{:?}", hs);
    }

    // Hold onto this in case we need it later.
    // #[test]
    // fn test_supercollider() {
    //     let start = std::time::Instant::now();
    //     let size = 1 << 27;
    //     let top_slots = 100;
    //     let mut stack = NockStack::new(size, top_slots);
    //     for x in 0..u64::MAX {
    //         for y in 0..u64::MAX {
    //             if x == y {
    //                 continue;
    //             }
    //             let n = D(x);
    //             let t = D(y);
    //             let n_hash = mug_u32(&mut stack, n);
    //             let t_hash = mug_u32(&mut stack, t);
    //             if n_hash == t_hash {
    //                 println!("FOUND HASH COLLISION!!!!! {} {} {} {}", x, y, n_hash, t_hash);
    //                 let end = std::time::Instant::now();
    //                 println!("Time: {:?}", end - start);
    //             }
    //             if y % 100000000 == 0 {
    //                 println!("{} {}", x, y);
    //             }
    //         }
    //     }
    // }
}
