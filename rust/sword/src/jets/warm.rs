use crate::hamt::Hamt;
use crate::jets::cold::{Batteries, Cold};
use crate::jets::hot::Hot;
use crate::jets::Jet;
use crate::mem::{AllocResult, NockStack, Preserve};
use crate::noun::{Noun, Slots};
use std::ptr::{copy_nonoverlapping, null_mut};

/// key = formula
#[derive(Copy, Clone)]
pub struct Warm(Hamt<WarmEntry>);

impl Preserve for Warm {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        self.0.assert_in_stack(stack);
    }
    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        self.0.preserve(stack)
    }
}

#[derive(Copy, Clone)]
struct WarmEntry(*mut WarmEntryMem);

const WARM_ENTRY_NIL: WarmEntry = WarmEntry(null_mut());

struct WarmEntryMem {
    batteries: Batteries,
    jet: Jet,
    path: Noun, // useful for profiling/debugging
    next: WarmEntry,
}

impl Preserve for WarmEntry {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut cursor = *self;
        loop {
            stack.assert_struct_is_in(cursor.0, 1);
            (*cursor.0).batteries.assert_in_stack(stack);
            (*cursor.0).path.assert_in_stack(stack);
            if (*cursor.0).next.0.is_null() {
                break;
            };
            cursor = (*cursor.0).next;
        }
    }
    unsafe fn preserve(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        // TODO: Should this panic? What is this?
        if self.0.is_null() {
            return Ok(());
        }
        let mut ptr: *mut *mut WarmEntryMem = &mut self.0;
        loop {
            if stack.is_in_frame(*ptr) {
                (**ptr).batteries.preserve(stack)?;
                (**ptr).path.preserve(stack)?;
                let dest_mem: *mut WarmEntryMem = stack.struct_alloc_in_previous_frame(1)?;
                copy_nonoverlapping(*ptr, dest_mem, 1);
                *ptr = dest_mem;
                ptr = &mut ((*dest_mem).next.0);
                if (*dest_mem).next.0.is_null() {
                    break;
                };
            } else {
                break;
            }
        }
        Ok(())
    }
}

impl Iterator for WarmEntry {
    type Item = (Noun, Batteries, Jet);
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            return None;
        }
        unsafe {
            let res = ((*(self.0)).path, (*(self.0)).batteries, (*(self.0)).jet);
            *self = (*(self.0)).next;
            Some(res)
        }
    }
}

impl Warm {
    #[allow(clippy::new_without_default)]
    pub fn new(stack: &mut NockStack) -> AllocResult<Self> {
        Ok(Warm(Hamt::new(stack)?))
    }

    fn insert(
        &mut self,
        stack: &mut NockStack,
        formula: &mut Noun,
        path: Noun,
        batteries: Batteries,
        jet: Jet,
    ) -> AllocResult<()> {
        let current_warm_entry = self.0.lookup(stack, formula)?.unwrap_or(WARM_ENTRY_NIL);
        unsafe {
            let warm_entry_mem_ptr: *mut WarmEntryMem = stack.struct_alloc(1)?;
            *warm_entry_mem_ptr = WarmEntryMem {
                batteries,
                jet,
                path,
                next: current_warm_entry,
            };
            self.0 = self.0.insert(stack, formula, WarmEntry(warm_entry_mem_ptr))?;
        }
        Ok(())
    }

    pub fn init(stack: &mut NockStack, cold: &mut Cold, hot: &Hot) -> AllocResult<Self> {
        let mut warm = Self::new(stack)?;
        for (mut path, axis, jet) in *hot {
            let batteries_list = cold.find(stack, &mut path)?;
            for batteries in batteries_list {
                let mut batteries_tmp = batteries;
                let (battery, _parent_axis) = batteries_tmp
                    .next()
                    .expect("IMPOSSIBLE: empty battery entry in cold state");
                if let Ok(mut formula) = unsafe { (*battery).slot_atom(axis) } {
                    warm.insert(stack, &mut formula, path, batteries, jet)?;
                } else {
                    //  XX: need NockStack allocated string interpolation
                    // eprintln!("Bad axis {} into formula {:?}", axis, battery);
                    continue;
                }
            }
        }
        Ok(warm)
    }

    /// Walk through the linked list of WarmEntry objects and do a partial check
    /// against the subject using Batteries (walk to root of parent batteries).
    /// If there's a match, then we've found a valid jet.
    pub fn find_jet(
        &mut self,
        stack: &mut NockStack,
        s: &mut Noun,
        f: &mut Noun,
    ) -> AllocResult<Option<(Jet, Noun)>> {
        let warm_it = self.0.lookup(stack, f)?;
        if let Some(warm_entry) = warm_it {
            unsafe {
                let jet = (*warm_entry.0).jet;
                let path = (*warm_entry.0).path;
                if (*warm_entry.0).batteries.matches(stack, *s)? {
                    return Ok(Some((jet, path)));
                }
            }
        }
        Ok(None)
    }
}
