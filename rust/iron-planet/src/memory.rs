#![allow(dead_code)]

use memmap::MmapMut;
use std::{mem, ptr::copy_nonoverlapping};

/// Tag bits for a direct atom
const DIRECT: u64 = 0x0;

/// Tag mask for a direct atom (
const DIRECT_MASK: u64 = 0x8000000000000000;

/// Maximum direct atom
const DIRECT_MAX: u64 = 0x7FFFFFFFFFFFFFFF;

/// Tag bits for an indirect atom
const INDIRECT: u64 = 0x8000000000000000;

/// Tag mask for an indirect atom
const INDIRECT_MASK: u64 = 0xC000000000000000;

/// Tag bits for a cell
const CELL: u64 = 0xC000000000000000;

/// Tag mask for a cell
const CELL_MASK: u64 = 0xE000000000000000;

/// Tag bits for a forwarding pointer
const FORWARD: u64 = 0xE000000000000000;

/// Tag mask for a forwarding pointer
const FORWARD_MASK: u64 = 0xE000000000000000;

/// Mask to extract a pointer if not shifting
const PTR_MASK: u64 = 0x1FFFFFFFFFFFFFFF;

/// Various pointer-related methods.
trait Ptr {
    fn as_ptr(&self) -> *const u64;

    fn as_mut_ptr(&self) -> *mut u64 {
        self.as_ptr() as *mut u64
    }

    /// Extract a forwarding pointer.
    fn forward_ptr(&self) -> Option<u64>;
}

/// Annotated 64-bit direct atom pointer.
#[derive(Clone, Copy)]
struct DirectAtom(u64);

impl DirectAtom {
    // Peter: this fn replaces direct().
    fn new(val: u64) -> Result<Self, ()> {
        if val <= DIRECT_MAX {
            Ok(Self(val))
        } else {
            Err(())
        }
    }
}

/// Annotated 64-bit indirect atom pointer.
#[derive(Clone, Copy)]
struct IndirectAtom(u64);

impl IndirectAtom {
    // Peter: this fn replaces indirect_1().
    fn new(stack: &mut NockStack, atom: u64) -> Self {
        let indirect_dest = stack.alloc(2);
        unsafe {
            *indirect_dest = 8;
            *(indirect_dest.add(1)) = atom;
        }
        Self((indirect_dest as u64) >> 3 | INDIRECT)
    }

    /// Size in 64-bit words.
    // Peter: this fn replaces indirect_size_unchecked().
    fn size(&self) -> u64 {
        unsafe { *self.as_ptr() << 3 }
    }

    // Peter: this fn replaces indirect_data_unchecked().
    fn data(&self) -> *const u64 {
        unsafe { self.as_ptr().add(1) }
    }
}

impl Ptr for IndirectAtom {
    fn as_ptr(&self) -> *const u64 {
        (self.0 << 3) as *const u64
    }

    // Peter: this fn replaces is_forward() and indirect_forwarded_unchecked().
    fn forward_ptr(&self) -> Option<u64> {
        let raw_sz = unsafe { *self.as_ptr() };
        if raw_sz & FORWARD_MASK == FORWARD {
            Some(raw_sz & PTR_MASK | INDIRECT)
        } else {
            None
        }
    }
}

/// Annotated 64-bit cell pointer.
#[derive(Clone, Copy)]
struct Cell(u64);

impl Cell {
    // Peter: this fn replaces cell().
    fn new(stack: &mut NockStack, head: Noun, tail: Noun) -> Self {
        let cell_dest = stack.alloc(2);
        unsafe {
            *cell_dest = head.raw;
            *(cell_dest.add(1)) = tail.raw;
        }
        Self((cell_dest as u64) >> 3 | CELL)
    }

    // Peter: this fn replaces cell_head_unchecked().
    fn head(&self) -> Noun {
        let raw = unsafe { *((self.0 << 3) as *const u64) };
        Noun { raw }
    }

    // Peter: this fn replaces cell_tail_unchecked().
    fn tail(&self) -> Noun {
        let raw = unsafe { *(((self.0 << 3) as *const u64).add(1)) };
        Noun { raw }
    }
}

impl Ptr for Cell {
    fn as_ptr(&self) -> *const u64 {
        (self.0 << 3) as *const u64
    }

    // Peter: this fn replaces is_forward() and cell_forwarded_unchecked().
    fn forward_ptr(&self) -> Option<u64> {
        let head = unsafe { self.head().raw };
        if head & FORWARD_MASK == FORWARD {
            Some(head & PTR_MASK | INDIRECT)
        } else {
            None
        }
    }
}

/// Annotated 64-bit pointer.
#[derive(Clone, Copy)]
#[repr(C)]
union Noun {
    raw: u64,
    direct_atom: DirectAtom,
    indirect_atom: IndirectAtom,
    cell: Cell,
}

impl Noun {
    // Peter: this fn replaces direct().
    fn is_direct_atom(&self) -> bool {
        unsafe { self.raw & DIRECT_MASK == DIRECT }
    }

    // Peter: this fn replaces indirect_1().
    fn is_indirect_atom(&self) -> bool {
        unsafe { self.raw & INDIRECT_MASK == INDIRECT }
    }

    // Peter: this fn replaces is_cell().
    fn is_cell(&self) -> bool {
        unsafe { self.raw & CELL_MASK == CELL }
    }
}

impl Ptr for Noun {
    fn as_ptr(&self) -> *const u64 {
        unsafe { (self.raw << 3) as *const u64 }
    }

    fn forward_ptr(&self) -> Option<u64> {
        None
    }
}

/// Current direction of the stack
enum Polarity {
    /// Current frame is lowest in high memory
    East,
    /// Current frame is highest in low memory
    West,
}

/// Structure representing a Nock computational stack (1 per thread)
struct NockStack {
    sp: *mut u64,
    fp: *mut u64,
    polarity: Polarity,
    _map: MmapMut,
}

impl NockStack {
    /// Given the size *in noun-words*, memory-map space for a Nock stack.
    // Peter: this fn replaces map_nock_stack().
    fn new(size: usize) -> Result<Self, std::io::Error> {
        let bytesize = size * mem::size_of::<u64>();
        let mut map = MmapMut::map_anon(bytesize)?;
        unsafe {
            let fp: *mut u64 = map.as_mut_ptr() as *mut u64;
            let sp: *mut u64 = fp.add(2);
            // Peter: does it make more sense to store `size` at the base of the stack rather than
            // the end address of the stack?
            *fp = fp.add(size) as u64;
            Ok(Self {
                sp: sp,
                fp: fp,
                polarity: Polarity::West,
                _map: map,
            })
        }
    }

    // Peter: this fn replaces slot_west().
    fn slot_west(&self, slot: usize) -> *mut u64 {
        unsafe { self.fp.add(slot) }
    }

    // Peter: this fn replaces slot_east().
    fn slot_east(&self, slot: usize) -> *mut u64 {
        unsafe { self.fp.sub(slot + 1) }
    }

    /// Get a pointer to a slot in a frame
    // Peter: this fn replaces slot().
    fn slot(&self, slot: usize) -> *mut u64 {
        match self.polarity {
            Polarity::West => self.slot_west(slot),
            Polarity::East => self.slot_east(slot),
        }
    }

    /// Get a pointer to a local variable slot in a frame
    // Peter: this fn replaces local().
    fn local(&self, local: usize) -> *mut u64 {
        self.slot(local + 2)
    }

    // Peter: this fn replaces push_west().
    fn push_west(&mut self, slots: usize) {
        unsafe {
            let east_sp_new_fp: *mut u64 = *(self.slot_west(0)) as *mut u64;
            let new_east_sp: *mut u64 = east_sp_new_fp.sub(slots + 2);
            *(east_sp_new_fp.sub(1)) = self.sp as u64;
            *(east_sp_new_fp.sub(2)) = self.fp as u64;
            self.fp = east_sp_new_fp;
            self.sp = new_east_sp;
            self.polarity = Polarity::East;
        }
    }

    // Peter: this fn replaces push_east().
    fn push_east(&mut self, slots: usize) {
        unsafe {
            let west_sp_new_fp: *mut u64 = *(self.slot_east(0)) as *mut u64;
            let new_west_sp: *mut u64 = west_sp_new_fp.add(slots + 2);
            *(west_sp_new_fp) = self.sp as u64;
            *(west_sp_new_fp.add(1)) = self.fp as u64;
            self.fp = west_sp_new_fp;
            self.sp = new_west_sp;
            self.polarity = Polarity::West;
        }
    }

    /// Push a new frame
    // Peter: this fn replaces push().
    fn push(&mut self, slots: usize) {
        match self.polarity {
            Polarity::West => self.push_west(slots),
            Polarity::East => self.push_east(slots),
        }
    }

    // Peter: this fn replaces alloc_west().
    fn alloc_west(&mut self, size: usize) -> *mut u64 {
        unsafe {
            let base = self.sp;
            self.sp = self.sp.add(size);
            base
        }
    }

    // Peter: this fn replaces alloc_east().
    fn alloc_east(&mut self, size: usize) -> *mut u64 {
        unsafe {
            let base = self.sp.sub(size);
            self.sp = base;
            base
        }
    }

    /// Allocate on the stack
    // Peter: this fn replaces alloc().
    fn alloc(&mut self, size: usize) -> *mut u64 {
        match self.polarity {
            Polarity::West => self.alloc_west(size),
            Polarity::East => self.alloc_east(size),
        }
    }

    // Peter: this fn replaces copy_east().
    fn copy_east(&mut self, root: Noun) -> Noun {
        unsafe {
            let mut west_sp = *(self.slot_east(0)) as *mut u64;
            let lower_bound_inclusive: *const u64 = self.sp;
            let upper_bound_exclusive: *const u64 = self.fp;
            let mut copy_stack_top: *mut u64 = self.sp;
            let res = if root.is_direct_atom() {
                root
            } else if root.as_ptr() < lower_bound_inclusive {
                root
            } else if root.as_ptr() >= upper_bound_exclusive {
                root
            } else if root.is_indirect_atom() {
                let sz: usize = (root.indirect_atom.size() + 1) as usize;
                let base: *mut u64 = west_sp;
                west_sp = west_sp.add(sz);
                copy_nonoverlapping(root.as_mut_ptr(), base, sz);
                *(root.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                Noun {
                    raw: (base as u64) >> 3 | INDIRECT,
                }
            } else if root.is_cell() {
                let base: *mut u64 = west_sp;
                west_sp = west_sp.add(2);
                copy_stack_top = copy_stack_top.sub(4);
                *copy_stack_top = root.cell.head().raw;
                *(copy_stack_top.add(1)) = base as u64;
                *(copy_stack_top.add(2)) = root.cell.tail().raw;
                *(copy_stack_top.add(3)) = (base.add(1)) as u64;
                *(root.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                Noun {
                    raw: (base as u64) >> 3 | CELL,
                }
            } else {
                panic!("no tag matches");
            };
            loop {
                if (copy_stack_top as *const u64) == lower_bound_inclusive {
                    break;
                }
                let noun = Noun {
                    raw: *copy_stack_top,
                };
                let dest: *mut u64 = *(copy_stack_top.add(1)) as *mut u64;
                copy_stack_top = copy_stack_top.add(2);
                if noun.is_direct_atom() {
                    *dest = noun.raw
                } else if noun.as_ptr() < lower_bound_inclusive {
                    *dest = noun.raw
                } else if noun.as_ptr() >= upper_bound_exclusive {
                    *dest = noun.raw
                } else if noun.is_indirect_atom() {
                    match noun.indirect_atom.forward_ptr() {
                        Some(fwd) => *dest = fwd,
                        None => {
                            let sz: usize = (noun.indirect_atom.size() + 1) as usize;
                            let base: *mut u64 = west_sp;
                            west_sp = west_sp.add(sz);
                            copy_nonoverlapping(noun.as_mut_ptr(), base, sz);
                            *(noun.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                            *dest = (base as u64) >> 3 | INDIRECT;
                        }
                    }
                } else if noun.is_cell() {
                    match noun.cell.forward_ptr() {
                        Some(fwd) => *dest = fwd,
                        None => {
                            let base: *mut u64 = west_sp;
                            west_sp = west_sp.add(2);
                            copy_stack_top = copy_stack_top.sub(4);
                            *copy_stack_top = noun.cell.head().raw;
                            *(copy_stack_top.add(1)) = base as u64;
                            *(copy_stack_top.add(2)) = noun.cell.tail().raw;
                            *(copy_stack_top.add(3)) = (base.add(1)) as u64;
                            *(noun.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                            *dest = (base as u64) >> 3 | CELL;
                        }
                    }
                } else {
                    panic!("no tag matches");
                }
            }
            *(self.slot_east(0)) = west_sp as u64;
            res
        }
    }

    // Peter: this fn replaces copy_west().
    fn copy_west(&mut self, root: Noun) -> Noun {
        unsafe {
            let mut east_sp: *mut u64 = *(self.slot_west(0)) as *mut u64;
            let lower_bound_inclusive: *const u64 = self.fp;
            let upper_bound_exclusive: *const u64 = self.sp;
            let mut copy_stack_top: *mut u64 = self.sp;
            let res = if root.is_direct_atom() {
                root
            } else if root.as_ptr() < lower_bound_inclusive {
                root
            } else if root.as_ptr() >= upper_bound_exclusive {
                root
            } else if root.is_indirect_atom() {
                let sz: usize = (root.indirect_atom.size() + 1) as usize;
                east_sp = east_sp.sub(sz);
                let base: *mut u64 = east_sp;
                copy_nonoverlapping(root.as_mut_ptr(), base, sz);
                *(root.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                Noun {
                    raw: (base as u64) >> 3 | INDIRECT,
                }
            } else if root.is_cell() {
                east_sp = east_sp.sub(2);
                let base: *mut u64 = east_sp;
                copy_stack_top = copy_stack_top.add(4);
                *copy_stack_top = root.cell.head().raw;
                *(copy_stack_top.add(1)) = base as u64;
                *(copy_stack_top.add(2)) = root.cell.tail().raw;
                *(copy_stack_top.add(3)) = (base.add(1)) as u64;
                *(root.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                Noun {
                    raw: (base as u64) >> 3 | CELL,
                }
            } else {
                panic!("no tag matches")
            };
            loop {
                if (copy_stack_top as *const u64) == upper_bound_exclusive {
                    break;
                }
                let noun = Noun {
                    raw: *copy_stack_top,
                };
                let dest: *mut u64 = *(copy_stack_top.add(1)) as *mut u64;
                copy_stack_top = copy_stack_top.sub(2);
                if noun.is_direct_atom() {
                    *dest = noun.raw
                } else if noun.as_ptr() < lower_bound_inclusive {
                    *dest = noun.raw
                } else if noun.as_ptr() >= upper_bound_exclusive {
                    *dest = noun.raw
                } else if noun.is_indirect_atom() {
                    match noun.indirect_atom.forward_ptr() {
                        Some(fwd) => *dest = fwd,
                        None => {
                            let sz: usize = (noun.indirect_atom.size() + 1) as usize;
                            east_sp = east_sp.sub(sz);
                            let base: *mut u64 = east_sp;
                            copy_nonoverlapping(noun.as_mut_ptr(), base, sz);
                            *(noun.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                            *dest = (base as u64) >> 3 | INDIRECT;
                        }
                    }
                } else if noun.is_cell() {
                    match noun.cell.forward_ptr() {
                        Some(fwd) => *dest = fwd,
                        None => {
                            east_sp = east_sp.sub(2);
                            let base: *mut u64 = east_sp;
                            copy_stack_top = copy_stack_top.add(4);
                            *copy_stack_top = noun.cell.head().raw;
                            *(copy_stack_top.add(1)) = base as u64;
                            *(copy_stack_top.add(2)) = noun.cell.tail().raw;
                            *(copy_stack_top.add(3)) = (base.add(1)) as u64;
                            *(noun.as_mut_ptr()) = (base as u64) >> 3 | FORWARD;
                            *dest = (base as u64) >> 3 | CELL;
                        }
                    }
                } else {
                    panic!("no tag matches")
                }
            }
            *(self.slot_west(0)) = east_sp as u64;
            res
        }
    }

    // Peter: this fn replaces pop_west().
    fn pop_west(&mut self, root: Noun) -> Noun {
        unsafe {
            let res = self.copy_west(root);
            self.sp = *(self.slot_west(0)) as *mut u64;
            self.fp = *(self.slot_west(1)) as *mut u64;
            self.polarity = Polarity::East;
            res
        }
    }

    // Peter: this fn replaces pop_east().
    fn pop_east(&mut self, root: Noun) -> Noun {
        unsafe {
            let res = self.copy_east(root);
            self.sp = *(self.slot_east(0)) as *mut u64;
            self.fp = *(self.slot_east(1)) as *mut u64;
            self.polarity = Polarity::West;
            res
        }
    }

    // Peter: this fn replaces pop().
    fn pop(&mut self, root: Noun) -> Noun {
        match self.polarity {
            Polarity::East => self.pop_east(root),
            Polarity::West => self.pop_west(root),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_size() {
        // DirectAtom.
        {
            assert_eq!(
                std::mem::size_of::<u64>(),
                std::mem::size_of::<DirectAtom>()
            );
            let a = DirectAtom(107);
            assert_eq!(std::mem::size_of::<u64>(), std::mem::size_of_val(&a));
        }

        // IndirectAtom.
        {
            assert_eq!(
                std::mem::size_of::<u64>(),
                std::mem::size_of::<IndirectAtom>()
            );
            let a = IndirectAtom(110);
            assert_eq!(std::mem::size_of::<u64>(), std::mem::size_of_val(&a));
        }

        // Cell.
        {
            assert_eq!(std::mem::size_of::<u64>(), std::mem::size_of::<Cell>());
            let c = Cell(140);
            assert_eq!(std::mem::size_of::<u64>(), std::mem::size_of_val(&c));
        }

        // Noun.
        {
            assert_eq!(std::mem::size_of::<u64>(), std::mem::size_of::<Noun>());
            let n = Noun { raw: 242 };
            assert_eq!(std::mem::size_of::<u64>(), std::mem::size_of_val(&n));
        }
    }
}
