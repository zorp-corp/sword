use crate::hamt::{Hamt};
use crate::interpreter::slot_result;
use crate::mem::{unifying_equality, NockStack, Preserve};
use crate::noun::{Atom, DirectAtom, Noun, D, T};
use bitvec::prelude::BitSlice;
use std::ptr::copy_nonoverlapping;
use std::ptr::null_mut;

#[derive(Copy, Clone)]
pub struct Batteries(*mut BatteriesMem);

const NO_BATTERIES: Batteries = Batteries(null_mut());

#[derive(Copy, Clone)]
struct BatteriesMem {
    battery: Noun,
    parent_axis: Atom,
    parent_batteries: Batteries,
}

impl Preserve for Batteries {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut ptr: *mut *mut BatteriesMem = &mut self.0;
        loop {
            if stack.is_in_frame(*ptr) {
                (**ptr).parent_axis.preserve(stack);
                let dest_mem: *mut BatteriesMem = stack.struct_alloc_in_previous_frame(1);
                copy_nonoverlapping(*ptr, dest_mem, 1);
                if (*dest_mem).parent_batteries.0.is_null() {
                    break;
                };
                ptr = &mut ((*dest_mem).parent_batteries.0);
            } else {
                break;
            }
        }
    }
}

impl Iterator for Batteries {
    type Item = (*mut Noun, Atom);
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            None
        } else {
            unsafe {
                let res = (
                    &mut (*(self.0)).battery as *mut Noun,
                    (*(self.0)).parent_axis,
                );
                *self = (*(self.0)).parent_batteries;
                Some(res)
            }
        }
    }
}

impl Batteries {
    pub fn matches(self, stack: &mut NockStack, mut core: Noun) -> bool {
        for (battery, parent_axis) in self {
            if let Ok(d) = parent_axis.as_direct() {
                if d.data() == 0 {
                    if unsafe { !unifying_equality(stack, &mut core, battery) } {
                        return false;
                    };
                };
            };
            if let Ok(mut core_battery) = slot_result(core, BitSlice::from_element(&2u64)) {
                if unsafe { !unifying_equality(stack, &mut core, battery) } {
                    return false;
                };
                if let Ok(core_parent) = slot_result(core, parent_axis.as_bitslice()) {
                    core = core_parent;
                    continue;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }
}

#[derive(Copy, Clone)]
struct BatteriesList(*mut BatteriesListMem);

const BATTERIES_LIST_NIL: BatteriesList = BatteriesList(null_mut());

#[derive(Copy, Clone)]
struct BatteriesListMem {
    batteries: Batteries,
    next: BatteriesList,
}

impl Preserve for BatteriesList {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut ptr: *mut *mut BatteriesListMem = &mut self.0;
        loop {
            if stack.is_in_frame(*ptr) {
                (**ptr).batteries.preserve(stack);
                let dest_mem: *mut BatteriesListMem = stack.struct_alloc_in_previous_frame(1);
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

impl Iterator for BatteriesList {
    type Item = Batteries;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            None
        } else {
            unsafe {
                let mem = *(self.0);
                let res = mem.batteries;
                *self = mem.next;
                Some(res)
            }
        }
    }
}

impl BatteriesList {
    fn matches(self, stack: &mut NockStack, mut core: Noun) -> Option<Batteries> {
        for batteries in self {
            if batteries.matches(stack, core) {
                return Some(batteries);
            }
        }
        None
    }
}

#[derive(Copy, Clone)]
struct NounList(*mut NounListMem);

const NOUN_LIST_NIL: NounList = NounList(null_mut());

#[derive(Copy, Clone)]
struct NounListMem {
    element: Noun,
    next: NounList,
}

impl Preserve for NounList {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut ptr: *mut *mut NounListMem = &mut (*self).0;
        loop {
            if stack.is_in_frame(*ptr) {
                (**ptr).element.preserve(stack);
                let dest_mem: *mut NounListMem = stack.struct_alloc_in_previous_frame(1);
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

impl Iterator for NounList {
    type Item = *mut Noun;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            None
        } else {
            unsafe {
                let res = &mut (*(self.0)).element;
                *self = (*(self.0)).next;
                Some(res)
            }
        }
    }
}

pub struct Cold(*mut ColdMem);

struct ColdMem {
    /// key: outermost battery
    /// value: registered path to core
    battery_to_paths: Hamt<NounList>,
    /// Roots
    /// key: root noun
    /// value: root path
    root_to_paths: Hamt<NounList>,
    /// key: register path to core
    /// value: linked list of sequences of nested batteries
    path_to_batteries: Hamt<BatteriesList>,
}

impl Preserve for Cold {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        (*(self.0)).battery_to_paths.preserve(stack);
        (*(self.0)).root_to_paths.preserve(stack);
        (*(self.0)).path_to_batteries.preserve(stack);
        let new_dest: *mut ColdMem = stack.struct_alloc_in_previous_frame(1);
        copy_nonoverlapping(self.0, new_dest, 1);
        self.0 = new_dest;
    }
}

impl Cold {
    /// register a core, return a boolean of whether we actually needed to register (false ->
    /// already registered)
    ///
    /// XX TODO validate chum
    pub fn register(
        &mut self,
        stack: &mut NockStack,
        mut core: Noun,
        parent_axis: Atom,
        mut chum: Noun,
    ) -> Result<bool, ()> {
        let mut battery = slot_result(core, BitSlice::from_element(&2u64))?;
        let mut parent = slot_result(core, parent_axis.as_bitslice())?;

        unsafe {
            // Are we registering a root?
            if let Ok(parent_axis_direct) = parent_axis.as_direct() {
                if parent_axis_direct.data() == 0 {
                    let mut root_path = T(stack, &[chum, D(0)]);
                    if let Some(paths) = (*(self.0)).root_to_paths.lookup(stack, &mut core) {
                        for mut a_path in paths {
                            if unsafe { unifying_equality(stack, &mut root_path, a_path) } {
                                return Ok(false); // it's already in here
                            }
                        }
                    }
                    let batteries_mem_ptr: *mut BatteriesMem = stack.struct_alloc(1);
                    *batteries_mem_ptr = BatteriesMem {
                        battery: core,
                        parent_axis: DirectAtom::new_unchecked(0).as_atom(),
                        parent_batteries: NO_BATTERIES,
                    };

                    let current_batteries_list: BatteriesList = (*(self.0))
                        .path_to_batteries
                        .lookup(stack, &mut root_path)
                        .unwrap_or(BATTERIES_LIST_NIL);

                    let batteries_list_mem_ptr: *mut BatteriesListMem = stack.struct_alloc(1);
                    *batteries_list_mem_ptr = BatteriesListMem {
                        batteries: Batteries(batteries_mem_ptr),
                        next: current_batteries_list,
                    };

                    let current_paths_list: NounList = (*(self.0))
                        .root_to_paths
                        .lookup(stack, &mut core)
                        .unwrap_or(NOUN_LIST_NIL);

                    let paths_list_mem_ptr: *mut NounListMem = stack.struct_alloc(1);
                    *paths_list_mem_ptr = NounListMem {
                        element: root_path,
                        next: current_paths_list,
                    };

                    let cold_mem_ptr: *mut ColdMem = stack.struct_alloc(1);
                    *cold_mem_ptr = ColdMem {
                        battery_to_paths: (*(self.0)).battery_to_paths,
                        root_to_paths: (*(self.0)).root_to_paths.insert(
                            stack,
                            &mut core,
                            NounList(paths_list_mem_ptr),
                        ),
                        path_to_batteries: (*(self.0)).path_to_batteries.insert(
                            stack,
                            &mut root_path,
                            BatteriesList(batteries_list_mem_ptr),
                        ),
                    };

                    *self = Cold(cold_mem_ptr);
                    return Ok(true);
                }
            }

            // Check if we already registered this core
            if let Some(paths) = (*(self.0)).battery_to_paths.lookup(stack, &mut battery) {
                for path in paths {
                    if let Ok(path_cell) = (*path).as_cell() {
                        if unifying_equality(stack, &mut path_cell.head(), &mut chum) {
                            if let Some(batteries_list) =
                                (*(self.0)).path_to_batteries.lookup(stack, &mut *path)
                            {
                                if let Some(_batteries) = batteries_list.matches(stack, core) {
                                    return Ok(false);
                                }
                            }
                        }
                    }
                }
            }

            let mut parent_battery = slot_result(parent, BitSlice::from_element(&2u64))?;

            let mut ret: Result<bool, ()> = Err(()); // err until we actually found a parent

            let mut path_to_batteries = (*(self.0)).path_to_batteries;
            let mut battery_to_paths = (*(self.0)).battery_to_paths;
            let mut root_to_paths = (*(self.0)).root_to_paths;

            if let Some(paths) = battery_to_paths.lookup(stack, &mut parent_battery) {
                for mut a_path in paths {
                    // path is a reserved word lol
                    let battery_list = path_to_batteries
                        .lookup(stack, &mut *a_path)
                        .unwrap_or(BATTERIES_LIST_NIL);
                    if let Some(parent_batteries) = battery_list.matches(stack, core) {
                        let mut my_path = T(stack, &[chum, *a_path]);

                        let batteries_mem_ptr: *mut BatteriesMem = stack.struct_alloc(1);
                        *batteries_mem_ptr = BatteriesMem {
                            battery: battery,
                            parent_axis: parent_axis,
                            parent_batteries: parent_batteries,
                        };

                        let current_batteries_list = path_to_batteries
                            .lookup(stack, &mut my_path)
                            .unwrap_or(BATTERIES_LIST_NIL);
                        let batteries_list_mem_ptr: *mut BatteriesListMem = stack.struct_alloc(1);
                        *batteries_list_mem_ptr = BatteriesListMem {
                            batteries: Batteries(batteries_mem_ptr),
                            next: current_batteries_list,
                        };

                        let current_paths_list = battery_to_paths
                            .lookup(stack, &mut battery)
                            .unwrap_or(NOUN_LIST_NIL);
                        let paths_list_mem_ptr: *mut NounListMem = stack.struct_alloc(1);
                        *paths_list_mem_ptr = NounListMem {
                            element: my_path,
                            next: current_paths_list,
                        };

                        path_to_batteries = path_to_batteries.insert(
                            stack,
                            &mut my_path,
                            BatteriesList(batteries_list_mem_ptr),
                        );
                        battery_to_paths = battery_to_paths.insert(
                            stack,
                            &mut battery,
                            NounList(paths_list_mem_ptr),
                        );
                        ret = Ok(true);
                    }
                }
            };

            if let Some(paths) = root_to_paths.lookup(stack, &mut parent) {
                for mut a_path in paths {
                    // path is a reserved word lol
                    let battery_list = path_to_batteries
                        .lookup(stack, &mut *a_path)
                        .unwrap_or(BATTERIES_LIST_NIL);
                    if let Some(parent_batteries) = battery_list.matches(stack, core) {
                        let mut my_path = T(stack, &[chum, *a_path]);

                        let batteries_mem_ptr: *mut BatteriesMem = stack.struct_alloc(1);
                        *batteries_mem_ptr = BatteriesMem {
                            battery: battery,
                            parent_axis: parent_axis,
                            parent_batteries: parent_batteries,
                        };

                        let current_batteries_list = path_to_batteries
                            .lookup(stack, &mut my_path)
                            .unwrap_or(BATTERIES_LIST_NIL);
                        let batteries_list_mem_ptr: *mut BatteriesListMem = stack.struct_alloc(1);
                        *batteries_list_mem_ptr = BatteriesListMem {
                            batteries: Batteries(batteries_mem_ptr),
                            next: current_batteries_list,
                        };

                        let current_paths_list = battery_to_paths
                            .lookup(stack, &mut battery)
                            .unwrap_or(NOUN_LIST_NIL);
                        let paths_list_mem_ptr: *mut NounListMem = stack.struct_alloc(1);
                        *paths_list_mem_ptr = NounListMem {
                            element: my_path,
                            next: current_paths_list,
                        };

                        path_to_batteries = path_to_batteries.insert(
                            stack,
                            &mut my_path,
                            BatteriesList(batteries_list_mem_ptr),
                        );
                        battery_to_paths = battery_to_paths.insert(
                            stack,
                            &mut battery,
                            NounList(paths_list_mem_ptr),
                        );
                        ret = Ok(true);
                    }
                }
            };

            let cold_mem_ptr: *mut ColdMem = stack.struct_alloc(1);
            *cold_mem_ptr = ColdMem {
                battery_to_paths: battery_to_paths,
                root_to_paths: root_to_paths,
                path_to_batteries: path_to_batteries,
            };

            *self = Cold(cold_mem_ptr);
            ret
        }
    }
}

