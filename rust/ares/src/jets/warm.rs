use crate::hamt::Hamt;
use crate::jets::cold::Batteries;
use crate::jets::Jet;
use crate::mem::{NockStack, Preserve};
use crate::noun::Noun;
use std::ptr::copy_nonoverlapping;

struct Warm(Hamt<WarmEntry>);

impl Preserve for Warm {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        self.0.preserve(stack);
    }
}

#[derive(Copy, Clone)]
struct WarmEntry(*mut WarmEntryMem);

struct WarmEntryMem {
    batteries: Batteries,
    jet: Jet,
    next: WarmEntry,
}

impl Preserve for WarmEntry {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if self.0.is_null() {
            return;
        }
        let mut ptr: *mut *mut WarmEntryMem = &mut self.0;
        loop {
            if stack.is_in_frame(*ptr) {
                (**ptr).batteries.preserve(stack);
                let dest_mem: *mut WarmEntryMem = stack.struct_alloc_in_previous_frame(1);
                copy_nonoverlapping(*ptr, dest_mem, 1);
                if (*dest_mem).next.0.is_null() {
                    break;
                };
                ptr = &mut ((*dest_mem).next.0);
            } else {
                break;
            }
        }
    }
}

impl Iterator for WarmEntry {
    type Item = (Batteries, Jet);
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            return None;
        }
        unsafe {
            let res = ((*(self.0)).batteries, (*(self.0)).jet);
            *self = (*(self.0)).next;
            Some(res)
        }
    }
}

impl Warm {
    pub fn find_jet(&mut self, stack: &mut NockStack, s: &mut Noun, f: &mut Noun) -> Option<Jet> {
        let warm_it = self.0.lookup(stack, f)?;
        for (batteries, jet) in warm_it {
            if batteries.matches(stack, *s) {
                return Some(jet);
            }
        }
        None
    }
}
