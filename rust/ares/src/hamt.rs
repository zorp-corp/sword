use either::Either::{self, *};
use crate::noun::Noun;
use crate::mem::{NockStack, unifying_equality, Preserve};
use crate::mug::mug_u32;
use std::ptr::{copy_nonoverlapping, null};
use std::slice;

#[inline]
fn chunk_to_bit(chunk: u32) -> u32 {
    1u32 << chunk
}

#[inline]
fn chunk_to_mask(chunk: u32) -> u32 { // mask out the bit for the chunk and all more significant
    chunk_to_bit(chunk) - 1
}


#[repr(packed)]
#[derive(Copy,Clone)]
struct Stem<T: Copy> {
    bitmap: u32,
    typemap: u32,
    buffer: *const Entry<T>,
}

impl<T: Copy> Stem<T> {
    #[inline]
    fn size(self) -> usize {
        self.bitmap.count_ones() as usize
    }

    #[inline]
    fn has_index(self, chunk:u32) -> bool {
        self.bitmap & chunk_to_bit(chunk) == 1
    }

    #[inline]
    fn hypothetical_index(self, chunk:u32) -> usize {
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
    fn entry(self, chunk: u32) -> Option<(Either<Stem<T>,Leaf<T>>, usize)> {
        self.index(chunk).map(|idx| {
            (unsafe {
                if self.typemap & chunk_to_bit(chunk) == 1 {
                    Left((*self.buffer.add(idx)).stem)
                } else {
                    Right((*self.buffer.add(idx)).leaf)
                }
             },
             idx)
        })
    }
}

#[repr(packed)]
#[derive(Copy,Clone)]
struct Leaf<T: Copy> {
    len: usize,
    buffer: *mut (Noun, T) // mutable for unifying equality
}

impl<T: Copy> Leaf<T> {
    unsafe fn to_mut_slice<'a>(self) -> &'a mut [(Noun, T)] {
        slice::from_raw_parts_mut(self.buffer, self.len)
    }
}


#[derive(Copy,Clone)]
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


pub struct Hamt<T: Copy>(Stem<T>);

impl<T: Copy> Hamt<T> {
    // Make a new, empty HAMT
    pub fn new() -> Hamt<T> {
        Hamt(Stem {
            bitmap: 0,
            typemap: 0,
            buffer: null()
        })
    }

    /**
     * Look up a pair keyed by a noun in the HAMT
     *
     * A mutable reference is required so that unifying equality can unify the key with a key entry
     * in the HAMT
     */
    pub fn lookup(&self, stack: &mut NockStack, n: &mut Noun) -> Option<T> {
        let mut stem = self.0;
        let mut mug = mug_u32(stack, *n);
        'lookup: loop {
            let chunk = mug & 0x1F; // 5 bits
            mug = mug >> 5;
            match stem.entry(chunk) {
                None => {
                    break None;
                },
                Some((Left(next_stem), _idx)) => {
                    stem = next_stem;
                    continue;
                },
                Some((Right(leaf), _idx)) => {
                    for pair in unsafe { leaf.to_mut_slice().iter_mut() } {
                        if unsafe { unifying_equality(stack, n, &mut pair.0) } {
                            break 'lookup Some(pair.1);
                        }
                    }
                    break None;
                },

            }
        }

    }

    // XX a delete function requires a stack, do we need one?

    /// Make a new HAMT with the value inserted or replaced at the key.
    pub fn insert(&self, stack: &mut NockStack, n: &mut Noun, t: T) -> Hamt<T> {
        let mut mug = mug_u32(stack, *n);
        let mut depth = 0u8;
        let mut stem = self.0;
        let mut stem_ret = self.0;
        let mut dest = &mut stem_ret as *mut Stem<T>;
        unsafe {
            'insert: loop {
                let chunk = mug & 0x1F; // 5 bits
                mug = mug >> 5;
                match stem.entry(chunk) {
                    None => {
                        let new_leaf_buffer = stack.struct_alloc(1);
                        *new_leaf_buffer = (*n, t);
                        let split = stem.hypothetical_index(chunk);
                        let new_buffer = stack.struct_alloc(stem.size() + 1);
                        copy_nonoverlapping(stem.buffer, new_buffer, split);
                        *new_buffer.add(split) = Entry{
                            leaf: Leaf {
                                len: 1,
                                buffer: new_leaf_buffer,
                            }
                        };
                        copy_nonoverlapping(stem.buffer.add(split), new_buffer.add(split + 1), stem.size() - split);
                        *dest = Stem {
                            bitmap: stem.bitmap | chunk_to_bit(chunk),
                            typemap: stem.typemap & !chunk_to_bit(chunk),
                            buffer: new_buffer
                        };
                        break Hamt(stem_ret);
                    },
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
                    },
                    Some((Right(leaf), idx)) => {
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
                                    }
                                };
                                *dest = Stem {
                                    bitmap: stem.bitmap,
                                    typemap: stem.typemap,
                                    buffer: new_buffer,
                                };
                                break 'insert Hamt(stem_ret);
                            }
                        };
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
                                }
                            };
                            *dest = Stem {
                                bitmap: stem.bitmap,
                                typemap: stem.typemap,
                                buffer: new_buffer,
                            };
                            break 'insert Hamt(stem_ret);
                        } else {
                            // if we haven't hit depth limit yet we shouldn't be chaining
                            // we'll make a fake node pointing to the old leaf and "insert into" that
                            // next time around
                            assert!(leaf.len == 1);
                            let fake_buffer = stack.struct_alloc(1);
                            *fake_buffer = Entry {
                                leaf: leaf
                            };
                            let next_stem = Stem {
                                bitmap: chunk_to_bit(chunk),
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
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        // XX make in_frame not care about pointer type
        if stack.in_frame((*self).0.buffer as *const u64) {
            let dest_buffer = stack.struct_alloc_in_previous_frame((*self).0.size());
            copy_nonoverlapping((*self).0.buffer, dest_buffer, (*self).0.size());
            (*self).0.buffer = dest_buffer;
            let traversal_stack = stack.struct_alloc::<(Stem<T>, u32)>(6);
            let mut traversal_depth = 1;
            *traversal_stack = ((*self).0, 0);
            'preserve: loop {
                if traversal_depth == 0 { break; }
                let (stem, mut position) = *traversal_stack.add(traversal_depth - 1);
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
                        },
                        Some((Left(next_stem), idx)) => {
                            if stack.in_frame(next_stem.buffer as *const u64) {
                                let dest_buffer = stack.struct_alloc_in_previous_frame(next_stem.size());
                                copy_nonoverlapping(next_stem.buffer, dest_buffer, next_stem.size());
                                let new_stem = Stem {
                                    bitmap: next_stem.bitmap,
                                    typemap: next_stem.typemap,
                                    buffer: dest_buffer,
                                };
                                *(stem.buffer.add(idx) as *mut Entry<T>) = Entry { stem: new_stem };
                                assert!(traversal_depth <= 5); // will increment
                                (*traversal_stack.add(traversal_depth - 1)).1 = position + 1;
                                *traversal_stack.add(traversal_depth) = (new_stem, 0);
                                traversal_depth += 1;
                                continue 'preserve;
                            } else {
                                position += 1;
                                continue 'preserve_stem;
                            }
                        },
                        Some((Right(leaf), idx)) => {
                            if stack.in_frame(leaf.buffer as *const u64) {
                                let dest_buffer = stack.struct_alloc_in_previous_frame(leaf.len);
                                copy_nonoverlapping(leaf.buffer, dest_buffer, leaf.len);
                                let new_leaf = Leaf {
                                    len: leaf.len,
                                    buffer: dest_buffer,
                                };
                                for pair in new_leaf.to_mut_slice().iter_mut() {
                                    (*pair).0.preserve(stack);
                                    (*pair).1.preserve(stack);
                                };
                                *(stem.buffer.add(idx) as *mut Entry<T>) = Entry {
                                    leaf: new_leaf,
                                };
                            }
                            position += 1;
                            continue 'preserve_stem;
                        },
                    }
                }
            }
        }

    }
}

/*
impl <T: Copy + Preserve> Preserve for Hamt<T> {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        // we special case the outer copy because it's destination is just a pointer and not a
        // pointer + index
        if stack.in_frame((*self).0.ptr) {
            let dest_buf = stack.struct_alloc_in_previous_frame::<u64>((*self).0.size() + 2);
            copy_nonoverlapping((*self).0.ptr, dest_buf, (*self).0.size() + 2);
            (*self).0.ptr = dest_buf;
            let traversal_stack = stack.struct_alloc::<(*mut u64, u32)>(6);
            let mut traversal_depth = 1;
            *traversal_stack = (dest_buf, 0);
            'preserve: loop {
                if traversal_depth == 0 { break; }
                let (buffer, mut position) = *traversal_stack.add(traversal_depth - 1);
                let node: HamtNode<T> = ptr_as_node(buffer);
                'preserve_node: loop {
                    if position >= 64 {
                        traversal_depth -= 1;
                        continue 'preserve;
                    }
                    match node.entry(position) {
                        None => {
                            position += 1;
                            continue 'preserve_node;
                        },
                        Some((Left(next_node), idx)) => {
                            // Another node
                            let size = next_node.size();
                            if stack.in_frame(next_node.ptr) {
                                let dest_buf = stack.struct_alloc_in_previous_frame::<u64>(size + 2);
                                copy_nonoverlapping(next_node.ptr, dest_buf, size);
                                assert!(traversal_depth <= 5); // We're gonna be incrementing so it
                                                              // has to stay below 6
                                *node.ptr.add(idx+2) = dest_buf as u64;
                                (*traversal_stack.add(traversal_depth - 1)).1 = position + 1;
                                *traversal_stack.add(traversal_depth) = (dest_buf, 0);
                                traversal_depth = traversal_depth + 1;
                                continue 'preserve;
                            } else {
                                position += 1;
                                continue 'preserve_node;
                            }
                        },
                        Some((Right(leaf), idx)) => {
                            // Leaf structs and entry buffers get allocated together so no need to
                            // check the entry buffer separately
                            if stack.in_frame(leaf as *const u64) {
                                let new_entries = stack.struct_alloc_in_previous_frame::<(Noun, T)>((*leaf).claimants);
                                copy_nonoverlapping((*leaf).entries, new_entries, (*leaf).claimants);
                                for i in 0 .. (*leaf).claimants {
                                    (*new_entries.add(i)).0.preserve(stack);
                                    (*new_entries.add(i)).1.preserve(stack);
                                };
                                let new_leaf = stack.alloc_in_previous_frame::<HamtLeaf<T>>();
                                (*new_leaf) = HamtLeaf {
                                    claimants: (*leaf).claimants,
                                    entries: new_entries,
                                };
                                *node.ptr.add(idx + 2) = new_leaf as u64;
                            }
                            position +=1;
                            continue 'preserve_node;
                        }
                    }
                }
            }
        }
    }
}
*/
