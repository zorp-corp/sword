use crate::mem::{NockStack, Preserve};
use crate::mug::mug_u32;
use crate::noun::Noun;
use crate::persist::{pma_contains, Persist};
use crate::unifying_equality::unifying_equality;
use either::Either::{self, *};
use std::mem::size_of;
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
    pub fn new(stack: &mut NockStack) -> MutHamt<T> {
        unsafe {
            let new_stem = stack.struct_alloc::<MutStem<T>>(1);
            (*new_stem).bitmap = 0;
            (*new_stem).typemap = 0;
            MutHamt(new_stem)
        }
    }

    pub fn lookup(self, stack: &mut NockStack, n: &mut Noun) -> Option<T> {
        let mut stem = self.0;
        let mut mug = mug_u32(stack, *n);
        unsafe {
            'lookup: loop {
                let chunk = mug & 0x1f;
                mug >>= 5;
                match (*stem).entry(chunk) {
                    None => {
                        break None;
                    }
                    Some(Left(next_stem)) => {
                        stem = next_stem;
                    }
                    Some(Right(leaf)) => {
                        for pair in leaf.to_mut_slice().iter_mut() {
                            if unifying_equality(stack, n, &mut pair.0) {
                                break 'lookup Some(pair.1);
                            }
                        }
                        break None;
                    }
                }
            }
        }
    }

    pub fn insert(self, stack: &mut NockStack, n: &mut Noun, t: T) {
        let mut stem = self.0;
        let mut mug = mug_u32(stack, *n);
        let mut depth = 0u8;
        unsafe {
            'insert: loop {
                let chunk = mug & 0x1f;
                mug >>= 5;
                match (*stem).entry(chunk) {
                    None => {
                        let new_leaf_buffer = stack.struct_alloc::<(Noun, T)>(1);
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
                            if unifying_equality(stack, n, &mut pair.0) {
                                pair.1 = t;
                                break 'insert;
                            }
                        }
                        if depth >= 5 {
                            let new_leaf_buffer = stack.struct_alloc::<(Noun, T)>(leaf.len + 1);
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
                            let new_stem = stack.struct_alloc::<MutStem<T>>(1);
                            let leaf_mug = mug_u32(stack, (*leaf.buffer).0);
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
    pub fn new(stack: &mut NockStack) -> Self {
        unsafe {
            let stem_ptr = stack.struct_alloc::<Stem<T>>(1);
            *stem_ptr = Stem {
                bitmap: 0,
                typemap: 0,
                buffer: null_mut(),
            };
            Hamt(stem_ptr)
        }
    }

    /**
     * Look up a pair keyed by a noun in the HAMT
     *
     * A mutable reference is required so that unifying equality can unify the key with a key entry
     * in the HAMT
     */
    pub fn lookup(&self, stack: &mut NockStack, n: &mut Noun) -> Option<T> {
        let mut stem = unsafe { *self.0 };
        let mut mug = mug_u32(stack, *n);
        'lookup: loop {
            let chunk = mug & 0x1F; // 5 bits
            mug >>= 5;
            match stem.entry(chunk) {
                None => {
                    break None;
                }
                Some((Left(next_stem), _idx)) => {
                    stem = next_stem;
                    continue;
                }
                Some((Right(leaf), _idx)) => {
                    for pair in unsafe { leaf.to_mut_slice().iter_mut() } {
                        if unsafe { unifying_equality(stack, n, &mut pair.0) } {
                            break 'lookup Some(pair.1);
                        }
                    }
                    break None;
                }
            }
        }
    }

    // XX a delete function requires a stack, do we need one?

    /// Make a new HAMT with the value inserted or replaced at the key.
    pub fn insert(&self, stack: &mut NockStack, n: &mut Noun, t: T) -> Hamt<T> {
        let mut mug = mug_u32(stack, *n);
        let mut depth = 0u8;
        let mut stem = unsafe { *self.0 };
        let stem_ret = unsafe { stack.struct_alloc::<Stem<T>>(1) };
        let mut dest = stem_ret;
        unsafe {
            'insert: loop {
                let chunk = mug & 0x1F; // 5 bits
                mug >>= 5;
                match stem.entry(chunk) {
                    // No entry found at mug chunk index; add Leaf to current Stem
                    None => {
                        let new_leaf_buffer = stack.struct_alloc(1);
                        *new_leaf_buffer = (*n, t);
                        let split = stem.hypothetical_index(chunk);
                        let new_buffer = stack.struct_alloc(stem.size() + 1);
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
                        break Hamt(stem_ret);
                    }
                    // Stem found at mug chunk index; insert into found Stem
                    Some((Left(next_stem), idx)) => {
                        let new_buffer = stack.struct_alloc(stem.size());
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
                            if unifying_equality(stack, n, &mut pair.0) {
                                let new_leaf_buffer = stack.struct_alloc(leaf.len);
                                copy_nonoverlapping(leaf.buffer, new_leaf_buffer, leaf.len);
                                (*new_leaf_buffer.add(ldx)).1 = t;
                                let new_buffer = stack.struct_alloc(stem.size());
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
                                break 'insert Hamt(stem_ret);
                            }
                        }
                        // No existing pair in this Leaf matches the key, and we've maxxed out the
                        // Hamt depth; add the the key-value pair to the list of pairs for this Leaf
                        if depth >= 5 {
                            let new_leaf_buffer = stack.struct_alloc(leaf.len + 1);
                            copy_nonoverlapping(leaf.buffer, new_leaf_buffer, leaf.len);
                            *new_leaf_buffer.add(leaf.len) = (*n, t);
                            let new_buffer = stack.struct_alloc(stem.size());
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
                            break 'insert Hamt(stem_ret);
                        // No existing pair in this Leaf matches the key, but we haven't maxxed out
                        // the Hamt depth yet. If we haven't hit the depth limit yet, we shouldn't
                        // be making a linked list of pairs. Turn the Leaf into a Stem and insert
                        // the new pair into the new Stem (also insert the pair in the existing
                        // Leaf, too).
                        } else {
                            // Make a fake node pointing to the old leaf and "insert into it" the
                            // next time around
                            assert!(leaf.len == 1);
                            let fake_buffer = stack.struct_alloc(1);
                            *fake_buffer = Entry { leaf };
                            // Get the mug chunk for the Noun at the *next* level so that we can
                            // build a fake stem for it
                            let fake_mug = mug_u32(stack, (*leaf.buffer).0);
                            let fake_chunk = (fake_mug >> ((depth + 1) * 5)) & 0x1F;
                            let next_stem = Stem {
                                bitmap: chunk_to_bit(fake_chunk),
                                typemap: 0,
                                buffer: fake_buffer,
                            };
                            let new_buffer = stack.struct_alloc(stem.size());
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

    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if stack.is_in_frame(self.0) {
            let dest_stem = stack.struct_alloc_in_previous_frame(1);
            copy_nonoverlapping(self.0, dest_stem, 1);
            self.0 = dest_stem;
            if stack.is_in_frame((*dest_stem).buffer) {
                let dest_buffer = stack.struct_alloc_in_previous_frame((*dest_stem).size());
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
                                        stack.struct_alloc_in_previous_frame(next_stem.size());
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
                                        stack.struct_alloc_in_previous_frame(leaf.len);
                                    copy_nonoverlapping(leaf.buffer, dest_buffer, leaf.len);
                                    let new_leaf = Leaf {
                                        len: leaf.len,
                                        buffer: dest_buffer,
                                    };
                                    for pair in new_leaf.to_mut_slice().iter_mut() {
                                        pair.0.preserve(stack);
                                        pair.1.preserve(stack);
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
        }
    }
}

impl<T: Copy + Persist> Persist for Hamt<T> {
    unsafe fn space_needed(&mut self, stack: &mut NockStack) -> usize {
        if pma_contains(self.0, 1) {
            return 0;
        }
        let mut bytes: usize = size_of::<Stem<T>>();
        if pma_contains((*self.0).buffer, (*self.0).size()) {
            return bytes;
        };

        bytes += (*self.0).size() * size_of::<Entry<T>>();

        let mut depth: usize = 0;
        let mut traversal = [Stem {
            bitmap: 0,
            typemap: 0,
            buffer: null_mut(),
        }; 6];
        traversal[0] = *self.0;

        loop {
            assert!(depth < 6);
            if traversal[depth].bitmap == 0 {
                if depth == 0 {
                    break bytes;
                }
                depth -= 1;
                continue;
            }

            let next_chunk = traversal[depth].bitmap.trailing_zeros();
            let next_type = traversal[depth].typemap & (1 << next_chunk) != 0;
            let next_entry = *traversal[depth].buffer;
            if next_chunk >= 31 {
                // if next_chunk == 31, then we will try to shift the bitmap by next_chunk+1 = 32 bits.
                // The datatype is a u32, so this is equivalent to setting it to 0. If we do try
                // to shift a u32 by 32 bits, then rust's overflow checking will catch it
                // and crash the process.
                traversal[depth].bitmap = 0;
                traversal[depth].typemap = 0;
            } else {
                traversal[depth].bitmap >>= next_chunk + 1;
                traversal[depth].typemap >>= next_chunk + 1;
            }
            traversal[depth].buffer = traversal[depth].buffer.add(1);

            if next_type {
                // true->stem false->leaf
                // found another stem
                traversal[depth + 1] = next_entry.stem;

                if pma_contains(traversal[depth + 1].buffer, traversal[depth + 1].size()) {
                    continue;
                }

                // count the buffer for the next stem
                bytes += traversal[depth + 1].size() * size_of::<Entry<T>>();
                depth += 1;
            } else {
                let mut leaf = next_entry.leaf;

                if leaf.len == 0 {
                    continue;
                }

                if pma_contains(leaf.buffer, leaf.len) {
                    continue;
                }

                bytes += size_of::<(Noun, T)>() * leaf.len;

                while leaf.len > 0 {
                    bytes += (*leaf.buffer).0.space_needed(stack);
                    bytes += (*leaf.buffer).1.space_needed(stack);
                    leaf.buffer = leaf.buffer.add(1);
                    leaf.len -= 1;
                }
            }
        }
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, buffer: &mut *mut u8) {
        if pma_contains(self.0, 1) {
            return;
        }
        let stem_ptr = *buffer as *mut Stem<T>;
        copy_nonoverlapping(self.0, stem_ptr, 1);
        *buffer = stem_ptr.add(1) as *mut u8;
        self.0 = stem_ptr;

        let stem_buffer_size = (*stem_ptr).size();
        if pma_contains((*stem_ptr).buffer, stem_buffer_size) {
            return;
        }
        let stem_buffer_ptr = *buffer as *mut Entry<T>;
        copy_nonoverlapping((*stem_ptr).buffer, stem_buffer_ptr, stem_buffer_size);
        *buffer = stem_buffer_ptr.add(stem_buffer_size) as *mut u8;
        (*stem_ptr).buffer = stem_buffer_ptr;

        let mut depth: usize = 0;
        let mut traversal = [Stem {
            bitmap: 0,
            typemap: 0,
            buffer: null_mut(),
        }; 6];

        traversal[0] = *stem_ptr;

        loop {
            if traversal[depth].bitmap == 0 {
                if depth == 0 {
                    break;
                }
                depth -= 1;
                continue;
            }

            let next_chunk = traversal[depth].bitmap.trailing_zeros();
            let next_type = traversal[depth].typemap & (1 << next_chunk) != 0;
            let next_entry_ptr = traversal[depth].buffer;

            if next_chunk >= 31 {
                // if next_chunk == 31, then we will try to shift the bitmap by next_chunk+1 = 32 bits.
                // The datatype is a u32, so this is equivalent to setting it to 0. If we do try
                // to shift a u32 by 32 bits, then rust's overflow checking will catch it
                // and crash the process.
                traversal[depth].bitmap = 0;
                traversal[depth].typemap = 0;
            } else {
                traversal[depth].bitmap >>= next_chunk + 1;
                traversal[depth].typemap >>= next_chunk + 1;
            }
            traversal[depth].buffer = traversal[depth].buffer.add(1);

            if next_type {
                // Stem case
                assert!(depth < 5);

                let stem_ptr: *mut Stem<T> = &mut (*next_entry_ptr).stem;
                let stem_size = (*stem_ptr).size();

                if pma_contains((*stem_ptr).buffer, stem_size) {
                    continue;
                }

                let stem_buffer_ptr = *buffer as *mut Entry<T>;

                copy_nonoverlapping((*stem_ptr).buffer, stem_buffer_ptr, stem_size);
                *buffer = stem_buffer_ptr.add(stem_size) as *mut u8;

                (*stem_ptr).buffer = stem_buffer_ptr;
                traversal[depth + 1] = *stem_ptr;
                depth += 1;
            } else {
                // Leaf case
                let leaf_ptr: *mut Leaf<T> = &mut (*next_entry_ptr).leaf;

                if (*leaf_ptr).len == 0 {
                    continue;
                }

                if pma_contains((*leaf_ptr).buffer, (*leaf_ptr).len) {
                    continue;
                }

                let leaf_buffer_ptr = *buffer as *mut (Noun, T);

                copy_nonoverlapping((*leaf_ptr).buffer, leaf_buffer_ptr, (*leaf_ptr).len);
                *buffer = leaf_buffer_ptr.add((*leaf_ptr).len) as *mut u8;

                (*leaf_ptr).buffer = leaf_buffer_ptr;

                let mut leaf_idx = 0;

                while leaf_idx < (*leaf_ptr).len {
                    (*(*leaf_ptr).buffer.add(leaf_idx))
                        .0
                        .copy_to_buffer(stack, buffer);
                    (*(*leaf_ptr).buffer.add(leaf_idx))
                        .1
                        .copy_to_buffer(stack, buffer);

                    leaf_idx += 1;
                }
            }
        }
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0 as u64
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Hamt(meta_handle as *mut Stem<T>)
    }
}
