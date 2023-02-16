use std::marker::PhantomData;
use either::Either::{self, *};
use crate::noun::Noun;
use crate::mem::{NockStack, unifying_equality, Preserve};
use crate::mug::mug_u32;
use std::ptr::{write_bytes, copy_nonoverlapping};

/// A HamtNode is a pointer to a buffer, structured as follows:
///
/// word 0: occupancy bitmap for mug chunk values
/// word 1: type bitmap for occupied mug chunk values (clear - node, set - leaf)
/// following words: an entry for each set bit in the occupancy bitmap, pointer to HamtNode or HamtLeaf as
/// given by type bitmap
#[derive(Copy,Clone)]
struct HamtNode<T> {
    ptr: *mut u64,
    phantom: PhantomData<T>
}

#[inline]
fn chunk_to_bit(chunk: u32) -> u64 {
    1u64 << chunk
}

#[inline]
fn chunk_to_mask(chunk: u32) -> u64 {
    chunk_to_bit(chunk) - 1
}

#[inline]
fn ptr_as_node<T>(ptr: *mut u64) -> HamtNode<T> {
    HamtNode {
        ptr: ptr,
        phantom: PhantomData::<T>,
    }
}

impl<T: Copy> HamtNode<T> {
    unsafe fn new_raw(stack: &mut NockStack, entries: usize) -> Self {
        let buf = stack.struct_alloc(entries + 2);
        write_bytes(buf, 0, entries + 2);
        ptr_as_node(buf)
    }
    fn size(self) -> usize {
        unsafe {
            (*self.ptr).count_ones() as usize
        }
    }

    fn bitmap(self) -> u64 {
        unsafe { *self.ptr }
    }

    fn typemap(self) -> u64 {
        unsafe { *self.ptr.add(1) }
    }

    #[inline]
    fn index(self, chunk: u32) -> Option<usize> {
        if self.bitmap() & chunk_to_bit(chunk) != 0 { 
            Some((self.bitmap() & chunk_to_mask(chunk)).count_ones() as usize)
        } else {
            None
        }
    }

    fn entry(self, chunk: u32) -> Option<(Either<HamtNode<T>, *const HamtLeaf<T>>, usize)>  {
        self.index(chunk).map(|idx| {
            (unsafe {
                if (*self.ptr.add(1)) & chunk_to_bit(chunk) == 0 {
                    Left(ptr_as_node(*(self.ptr.add(2 + idx)) as *mut u64))
                } else {
                    Right((*self.ptr.add(2 + idx)) as *const HamtLeaf<T>)
                }
            }, idx)
        })
    }
}

/// A HamtLeaf is a size and pointer to a memory buffer of map entries
struct HamtLeaf<T> {
    claimants: usize,
    entries: *mut (Noun, T),
}

pub struct Hamt<T>(HamtNode<T>);

impl<T: Copy> Hamt<T> {
    pub fn new(stack: &mut NockStack) -> Self {
        unsafe {
            Hamt(HamtNode::new_raw(stack, 0))
        }
    }

    /// Look up a noun in an immutable HAMT and return the associated value
    pub fn lookup(self, stack: &mut NockStack, n: &mut Noun) -> Option<T> {
        let mut node = self.0;
        let mut mug = mug_u32(stack, *n);
        'lookup: loop {
            unsafe {
                let mug_chunk = mug & 0x3f;
                mug = mug >> 6;
                match node.entry(mug_chunk) {
                    None => { break None; },
                    Some((Left(next_node), _idx)) => {
                        node = next_node;
                        continue;
                    },
                    Some((Right(leaf), _idx)) => {
                        for i in 0..(*leaf).claimants {
                            if unifying_equality(stack, &mut (*(*leaf).entries.add(i)).0, n) {
                                break 'lookup Some((*(*leaf).entries.add(i)).1);
                            }
                        };
                        break None;
                    },
                }
            }
        }
    }


    /// Insert a pair into an immutable HAMT, creating a new HAMT
    ///
    /// The noun key must be mutable to support unifying equality
    pub fn insert(self, stack: &mut NockStack, n: &mut Noun, t: T) -> Self {
        let mut node = self.0;
        let mut new_node = unsafe { HamtNode::<T>::new_raw(stack, node.size() + 1) };
        let ret = Hamt(node);
        let mut depth = 0u8;
        let mut mug = mug_u32(stack, *n);
        'insert: loop {
            unsafe {
                depth += 1;
                let mug_chunk = mug & 0x3f; // least-significant 6 bits
                mug = mug >> 6;
                match node.entry(mug_chunk) {
                    None => { // no entry in the bitmap, write a leaf
                        let new_bitmap = node.bitmap() | chunk_to_bit(mug_chunk);
                        let new_typemap = node.typemap() | chunk_to_bit(mug_chunk);
                        *new_node.ptr = new_bitmap;
                        *new_node.ptr.add(1) = new_typemap;
                        let new_leaf_buf = stack.struct_alloc(1);
                        *new_leaf_buf = (*n, t);
                        let new_leaf = stack.struct_alloc(1);
                        *new_leaf = HamtLeaf {
                            claimants: 1,
                            entries: new_leaf_buf
                        };
                        let split = (node.bitmap() & chunk_to_mask(mug_chunk)).count_ones() as usize;
                        copy_nonoverlapping(node.ptr.add(2), new_node.ptr.add(2), split);
                        *new_node.ptr.add(2+split) = new_leaf as u64;
                        copy_nonoverlapping(node.ptr.add(2+split), new_node.ptr.add(3+split), node.size() - split);
                        break;
                    },
                    // there's already a node at this entry, insert into it
                    Some((Left(next_node), idx)) => {
                        let next_new_node = HamtNode::new_raw(stack, next_node.size() + 1);
                        copy_nonoverlapping(node.ptr, new_node.ptr, node.size() + 2);
                        *new_node.ptr.add(2 + idx) = next_new_node.ptr as u64;
                        node = next_node;
                        new_node = next_new_node;
                        continue;
                    },
                    Some((Right(leaf), idx)) => {
                        // check whether we should overwrite a key
                        for i in 0..(*leaf).claimants {
                            if unifying_equality(stack, &mut (*(*leaf).entries.add(i)).0, n) {
                                let new_leaf_buf = stack.struct_alloc((*leaf).claimants);
                                copy_nonoverlapping((*leaf).entries, new_leaf_buf, (*leaf).claimants);
                                (*new_leaf_buf.add(i)).1 = t;
                                let new_leaf = stack.struct_alloc(1);
                                *new_leaf = HamtLeaf {
                                    claimants: (*leaf).claimants,
                                    entries: new_leaf_buf,
                                };
                                copy_nonoverlapping(node.ptr, new_node.ptr, node.size() + 2);
                                *new_node.ptr.add(2+idx) = new_leaf as u64;
                                break 'insert;
                            }
                        }
                        // We have gone as far as we can by distinguishing mugs, chain by nouns now
                        if depth >= 6 {
                            // append to this leaf
                            let new_leaf_buf = stack.struct_alloc((*leaf).claimants + 1);
                            copy_nonoverlapping((*leaf).entries, new_leaf_buf, (*leaf).claimants); 
                            *new_leaf_buf.add((*leaf).claimants) = (*n, t);
                            let new_leaf = stack.struct_alloc(1);
                            *new_leaf = HamtLeaf {
                                claimants: (*leaf).claimants + 1,
                                entries: new_leaf_buf,
                            };
                            copy_nonoverlapping(node.ptr, new_node.ptr, node.size() + 2);
                            *new_node.ptr.add(2+idx) = new_leaf as u64;
                            break;
                        // We encountered a leaf which we should push down as a node
                        } else {
                            // We make a node which won't go in our new tree, but contains the existing
                            // leaf in the proper spot in the bitmap for the next level. We use this as
                            // the value of `node` in the next iteration.
                            // We then allocate our next new node as usual, set up the references in the
                            // current new_node, update the iterators, and go around again
                            //
                            // assertion: we haven't gone deep enough to chain at leaves, so there is
                            // only one key-value pair at this leaf
                            assert!((*leaf).claimants == 1);
                            let rival = (*(*leaf).entries).0;
                            let rival_mug = mug_u32(stack, rival);
                            let rival_mug_chunk = rival_mug >> (depth * 6) & 0x3f;
                            let rival_mug_bit = chunk_to_bit(rival_mug_chunk);
                            let fake_next_leaf_buf = stack.struct_alloc(1);
                            copy_nonoverlapping((*leaf).entries, fake_next_leaf_buf, 1);
                            let fake_next_leaf = stack.struct_alloc(1);
                            *fake_next_leaf = HamtLeaf {
                                claimants: 1,
                                entries: fake_next_leaf_buf,
                            };
                            let fake_next_node = HamtNode::new_raw(stack, 1);
                            *fake_next_node.ptr = rival_mug_bit;
                            *fake_next_node.ptr.add(1) = rival_mug_bit;
                            *fake_next_node.ptr.add(2) = fake_next_leaf as u64;
                            copy_nonoverlapping(node.ptr, new_node.ptr, node.size() + 2);
                            let next_new_node = HamtNode::new_raw(stack, 2);
                            *new_node.ptr.add(2 + idx) = next_new_node.ptr as u64;
                            node = fake_next_node;
                            new_node = next_new_node;
                            continue;
                        }
                    },
                }
            }
        };
        return ret;
    }
}

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
