use memmap::MmapMut;
use std::mem;
use std::ptr::copy_nonoverlapping;

/// Tag bits for a direct atom
pub const DIRECT        : u64 = 0x0;

/// Tag mask for a direct atom (
pub const DIRECT_MASK   : u64 = 0x8000000000000000;

/// Maximum direct atom
pub const DIRECT_MAX    : u64 = 0x7FFFFFFFFFFFFFFF;

/// Tag bits for an indirect atom
pub const INDIRECT      : u64 = 0x8000000000000000;

/// Tag mask for an indirect atom
pub const INDIRECT_MASK : u64 = 0xC000000000000000;

/// Tag bits for a cell
pub const CELL          : u64 = 0xC000000000000000;

/// Tag mask for a cell
pub const CELL_MASK     : u64 = 0xE000000000000000;

/// Tag bits for a forwarding pointer
const FORWARD       : u64 = 0xE000000000000000;

/// Tag mask for a forwarding pointer
const FORWARD_MASK  : u64 = 0xE000000000000000;

/// Mask to extract a pointer if not shifting
pub const PTR_MASK       : u64 = 0x1FFFFFFFFFFFFFFF;

/// Current direction of the stack
enum Polarity {
    /// Current frame is lowest in high memory
    East,
    /// Current frame is highest in low memory
    West,
}

/// Structure representing a Nock computational stack (1 per thread)
pub struct NockStack {
    sp: *mut u64,
    fp: *mut u64,
    polarity: Polarity,
    _map: MmapMut,
}

/// Given the size *in noun-words*, memory-map space for a Nock stack.
pub fn map_nock_stack(size: usize) -> Result<NockStack,std::io::Error> {
    let bytesize = size * mem::size_of::<u64>();
    match MmapMut::map_anon(bytesize) {
        Ok(mut map) => {
            unsafe {
                let fp : *mut u64 = map.as_mut_ptr() as *mut u64;
                let sp : *mut u64 = fp.add(2);
                *fp = fp.add(size) as u64;
                Ok(
                    NockStack {
                        sp : sp,
                        fp : fp,
                        polarity : Polarity::West,
                        _map: map,
                    }
                )
            }
        },
        Err(e) => Err(e),
    }
}

unsafe fn slot_west(stack: &NockStack, slot: usize) -> *mut u64 {
    stack.fp.add(slot)
}

unsafe fn slot_east(stack: &NockStack, slot: usize) -> *mut u64 {
    stack.fp.sub(slot + 1)
}

/// Get a pointer to a slot in a frame
pub unsafe fn slot(stack: &NockStack, slot: usize) -> *mut u64 {
    match stack.polarity {
        Polarity::West => slot_west(stack, slot),
        Polarity::East => slot_east(stack, slot),
    }
}

/// Get a pointer to a local variable slot in a frame
pub unsafe fn local(stack: &NockStack, local: usize) -> *mut u64 {
    slot(stack, local + 2)
}

fn push_west(stack: &mut NockStack, slots: usize) {
    unsafe {
        let east_sp_new_fp : *mut u64 = *(slot_west(stack, 0)) as *mut u64;
        let new_east_sp : *mut u64 = east_sp_new_fp.sub(slots + 2);
        *(east_sp_new_fp.sub(1)) = stack.sp as u64;
        *(east_sp_new_fp.sub(2)) = stack.fp as u64;
        stack.fp = east_sp_new_fp;
        stack.sp = new_east_sp;
        stack.polarity = Polarity::East;
    }
}

fn push_east(stack: &mut NockStack, slots: usize) {
    unsafe {
        let west_sp_new_fp : *mut u64 = *(slot_east(stack, 0)) as *mut u64;
        let new_west_sp : *mut u64 = west_sp_new_fp.add(slots + 2);
        *(west_sp_new_fp) = stack.sp as u64;
        *(west_sp_new_fp.add(1)) = stack.fp as u64;
        stack.fp = west_sp_new_fp;
        stack.sp = new_west_sp;
        stack.polarity = Polarity::West;
    }
}

/// Push a new frame
pub fn push(stack: &mut NockStack, slots: usize) {
    match stack.polarity {
        Polarity::West => push_west(stack, slots),
        Polarity::East => push_east(stack, slots),
    }
}

fn alloc_west(stack: &mut NockStack, size: usize) -> *mut u64 {
    unsafe {
        let base = stack.sp;
        stack.sp = stack.sp.add(size);
        base
    }
}

fn alloc_east(stack: &mut NockStack, size: usize) -> *mut u64 {
    unsafe {
        let base = stack.sp.sub(size);
        stack.sp = base;
        base
    }
}

/// Allocate on the stack
pub fn alloc(stack: &mut NockStack, size: usize) -> *mut u64 {
    match stack.polarity {
        Polarity::West => alloc_west(stack, size),
        Polarity::East => alloc_east(stack, size),
    }
}

pub fn cell(stack: &mut NockStack, head: u64, tail: u64) -> u64 {
    let cell_dest = alloc(stack, 2);
    unsafe {
        *cell_dest = head;
        *(cell_dest.add(1)) = tail;
    }
    (cell_dest as u64) >> 3 | CELL
}

pub fn direct(atom: u64) -> u64 {
    if atom > DIRECT_MAX {
      panic!("Tried to make a direct atom larger than DIRECT_MAX");
    }
    atom
}

pub fn indirect_1(stack: &mut NockStack, atom: u64) -> u64 {
    let indirect_dest = alloc(stack, 2);
    unsafe {
       *indirect_dest = 8;
       *(indirect_dest.add(1)) = atom;
    }
    (indirect_dest as u64) >> 3 | INDIRECT
}

unsafe fn cell_ptr_unchecked(atom: u64) -> *mut u64 {
    (atom << 3) as *mut u64
}

unsafe fn cell_head_unchecked(atom: u64) -> u64 {
    *((atom << 3) as *const u64)
}

unsafe fn cell_tail_unchecked(atom: u64) -> u64 {
    *(((atom << 3) as *const u64).add(1))
}

/// Size in 64-bit words of an indirect atom
pub unsafe fn indirect_size_unchecked(atom: u64) -> u64 {
    *((atom << 3) as *const u64) << 3
}

pub unsafe fn indirect_data_unchecked(atom: u64) -> *const u64 {
    (indirect_ptr_unchecked(atom)).add(1)
}

unsafe fn indirect_ptr_unchecked(atom: u64) -> *mut u64 {
    (atom << 3) as *mut u64
}

pub fn is_direct(noun: u64) -> bool {
    noun & DIRECT_MASK == DIRECT
}

pub fn is_indirect(noun: u64) -> bool {
    noun & INDIRECT_MASK == INDIRECT
}

pub fn is_cell(noun: u64) -> bool {
    noun & CELL_MASK == CELL
}

fn is_forward(noun: u64) -> bool {
    noun & FORWARD_MASK == FORWARD
}

unsafe fn indirect_forwarded_unchecked(noun: u64) -> Option<u64> {
    let raw_sz : u64 = *(indirect_ptr_unchecked(noun));
    if is_forward(raw_sz) { Some(raw_sz & PTR_MASK | INDIRECT) }
    else { None }
}

unsafe fn cell_forwarded_unchecked(noun: u64) -> Option<u64> {
    let head : u64 = cell_head_unchecked(noun);
    if is_forward(head) { Some(head & PTR_MASK | CELL) }
    else { None }
}

unsafe fn copy_east(stack: &mut NockStack, root: u64) -> u64 {
    let mut west_sp : *mut u64 = *(slot_east(stack, 0)) as *mut u64;
    let lower_bound_inclusive : *const u64 = stack.sp;
    let upper_bound_exclusive : *const u64 = stack.fp;
    let mut copy_stack_top : *mut u64 = stack.sp;
    let res =
        if is_direct(root) { root }
        else if ((root << 3) as *const u64) < lower_bound_inclusive { root }
        else if ((root << 3) as *const u64) >= upper_bound_exclusive { root }
        else if is_indirect(root) {
            let sz : usize = (indirect_size_unchecked(root) + 1) as usize;
            let base : *mut u64 = west_sp;
            west_sp = west_sp.add(sz);
            copy_nonoverlapping(indirect_ptr_unchecked(root), base, sz);
            *(indirect_ptr_unchecked(root)) = (base as u64) >> 3 | FORWARD;
            (base as u64) >> 3 | INDIRECT
        } else if is_cell(root) {
            let base : *mut u64 = west_sp;
            west_sp = west_sp.add(2);
            copy_stack_top = copy_stack_top.sub(4);
            *copy_stack_top = cell_head_unchecked(root);
            *(copy_stack_top.add(1)) = base as u64;
            *(copy_stack_top.add(2)) = cell_tail_unchecked(root);
            *(copy_stack_top.add(3)) = (base.add(1)) as u64;
            *(cell_ptr_unchecked(root)) = (base as u64) >> 3 | FORWARD;
            (base as u64) >> 3 | CELL
        } else { panic!("no tag matches"); };
    loop {
        if (copy_stack_top as *const u64) == lower_bound_inclusive { break; }
        let noun : u64 = *copy_stack_top;
        let dest : *mut u64 = *(copy_stack_top.add(1)) as *mut u64;
        copy_stack_top = copy_stack_top.add(2);
        if is_direct(noun) { *dest = noun }
        else if ((noun << 3) as *const u64) < lower_bound_inclusive { *dest = noun }
        else if ((noun << 3) as *const u64) >= upper_bound_exclusive { *dest = noun }
        else if is_indirect(noun) {
            match indirect_forwarded_unchecked(noun) {
                Some(fwd) => { *dest = fwd },
                None => {
                    let sz : usize = (indirect_size_unchecked(noun) + 1) as usize;
                    let base : *mut u64 = west_sp;
                    west_sp = west_sp.add(sz);
                    copy_nonoverlapping(indirect_ptr_unchecked(noun), base, sz);
                    *(indirect_ptr_unchecked(noun)) = (base as u64) >> 3 | FORWARD;
                    *dest = (base as u64) >> 3 | INDIRECT;
                },
            }
        } else if is_cell(noun) {
            match cell_forwarded_unchecked(noun) {
                Some(fwd) => { *dest = fwd },
                None => {
                    let base : *mut u64 = west_sp;
                    west_sp = west_sp.add(2);
                    copy_stack_top = copy_stack_top.sub(4);
                    *copy_stack_top = cell_head_unchecked(noun);
                    *(copy_stack_top.add(1)) = base as u64;
                    *(copy_stack_top.add(2)) = cell_tail_unchecked(noun);
                    *(copy_stack_top.add(3)) = (base.add(1)) as u64;
                    *(cell_ptr_unchecked(noun)) = (base as u64) >> 3 | FORWARD;
                    *dest = (base as u64) >> 3 | CELL;
                }
            }
        } else { panic!("no tag matches"); }
    };
    *(slot_east(stack, 0)) = west_sp as u64;
    res
}

unsafe fn copy_west(stack: &mut NockStack, root: u64) -> u64 {
    let mut east_sp : *mut u64 = *(slot_west(stack, 0)) as *mut u64;
    let lower_bound_inclusive : *const u64 = stack.fp;
    let upper_bound_exclusive : *const u64 = stack.sp;
    let mut copy_stack_top : *mut u64 = stack.sp;
    let res =
        if is_direct(root) { root }
        else if ((root << 3) as *const u64) < lower_bound_inclusive { root }
        else if ((root << 3) as *const u64) >= upper_bound_exclusive { root }
        else if is_indirect(root) {
            let sz : usize = (indirect_size_unchecked(root) + 1) as usize;
            east_sp = east_sp.sub(sz);
            let base : *mut u64 = east_sp;
            copy_nonoverlapping(indirect_ptr_unchecked(root), base, sz);
            *(indirect_ptr_unchecked(root)) = (base as u64) >> 3 | FORWARD;
            (base as u64) >> 3 | INDIRECT
        } else if is_cell(root) {
            east_sp = east_sp.sub(2);
            let base : *mut u64 = east_sp;
            copy_stack_top = copy_stack_top.add(4);
            *copy_stack_top = cell_head_unchecked(root);
            *(copy_stack_top.add(1)) = base as u64;
            *(copy_stack_top.add(2)) = cell_tail_unchecked(root);
            *(copy_stack_top.add(3)) = (base.add(1)) as u64;
            *(cell_ptr_unchecked(root)) = (base as u64) >> 3 | FORWARD;
            (base as u64) >> 3 | CELL
        } else { panic!("no tag matches") };
    loop {
        if (copy_stack_top as *const u64) == upper_bound_exclusive { break; }
        let noun : u64 = *copy_stack_top;
        let dest : *mut u64 = *(copy_stack_top.add(1)) as *mut u64;
        copy_stack_top = copy_stack_top.sub(2);
        if is_direct(noun) { *dest = noun }
        else if ((noun << 3) as *const u64) < lower_bound_inclusive { *dest = noun }
        else if ((noun << 3) as *const u64) >= upper_bound_exclusive { *dest = noun }
        else if is_indirect(noun) {
            match indirect_forwarded_unchecked(noun) {
                Some(fwd) => { *dest = fwd },
                None => {
                    let sz : usize = (indirect_size_unchecked(noun) + 1) as usize;
                    east_sp = east_sp.sub(sz);
                    let base : *mut u64 = east_sp;
                    copy_nonoverlapping(indirect_ptr_unchecked(noun), base, sz);
                    *(indirect_ptr_unchecked(noun)) = (base as u64) >> 3 | FORWARD;
                    *dest = (base as u64) >> 3 | INDIRECT;
                },
            }
        } else if is_cell(noun) {
            match cell_forwarded_unchecked(noun) {
                Some(fwd) => { *dest = fwd },
                None => {
                    east_sp = east_sp.sub(2);
                    let base : *mut u64 = east_sp;
                    copy_stack_top = copy_stack_top.add(4);
                    *copy_stack_top = cell_head_unchecked(noun);
                    *(copy_stack_top.add(1)) = base as u64;
                    *(copy_stack_top.add(2)) = cell_tail_unchecked(noun);
                    *(copy_stack_top.add(3)) = (base.add(1)) as u64;
                    *(cell_ptr_unchecked(noun)) = (base as u64) >> 3 | FORWARD;
                    *dest = (base as u64) >> 3 | CELL;
                },
            }
        } else { panic!("no tag matches") }
    };
    *(slot_west(stack, 0)) = east_sp as u64;
    res
}

fn pop_west(stack: &mut NockStack, root: u64) -> u64 {
    unsafe {
       let res : u64 = copy_west(stack, root);
       stack.sp = *(slot_west(stack, 0)) as *mut u64;
       stack.fp = *(slot_west(stack, 1)) as *mut u64;
       stack.polarity = Polarity::East;
       res
    }
}

fn pop_east(stack: &mut NockStack, root: u64) -> u64 {
    unsafe {
        let res : u64 = copy_east(stack, root);
        stack.sp = *(slot_east(stack, 0)) as *mut u64;
        stack.fp = *(slot_east(stack, 1)) as *mut u64;
        stack.polarity = Polarity::West;
        res
    }
}

pub fn pop(stack: &mut NockStack, root: u64) -> u64 {
    match stack.polarity {
        Polarity::East => pop_east(stack, root),
        Polarity::West => pop_west(stack, root),
    }
}
