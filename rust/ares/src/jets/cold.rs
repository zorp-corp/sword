use crate::hamt::Hamt;
use crate::mem::{unifying_equality, NockStack, Preserve};
use crate::noun::{Atom, DirectAtom, Noun, Slots, D, T};
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
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut cursor = *self;
        loop {
            stack.struct_is_in(cursor.0, 1);
            (*cursor.0).battery.assert_in_stack(stack);
            (*cursor.0).parent_axis.assert_in_stack(stack);
            if (*cursor.0).parent_batteries.0.is_null() {
                break;
            };
            cursor = (*cursor.0).parent_batteries;
        }
    }
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut ptr: *mut *mut BatteriesMem = &mut self.0;
        loop {
            if stack.is_in_frame(*ptr) {
                (**ptr).battery.preserve(stack);
                (**ptr).parent_axis.preserve(stack);
                let dest_mem: *mut BatteriesMem = stack.struct_alloc_in_previous_frame(1);
                copy_nonoverlapping(*ptr, dest_mem, 1);
                *ptr = dest_mem;
                ptr = &mut ((**ptr).parent_batteries.0);
                if (*dest_mem).parent_batteries.0.is_null() {
                    break;
                };
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
                    } else {
                        continue;
                    };
                };
            };
            if let Ok(mut core_battery) = core.slot(2) {
                if unsafe { !unifying_equality(stack, &mut core_battery, battery) } {
                    return false;
                };
                if let Ok(core_parent) = core.slot_atom(parent_axis) {
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
pub struct BatteriesList(*mut BatteriesListMem);

const BATTERIES_LIST_NIL: BatteriesList = BatteriesList(null_mut());

#[derive(Copy, Clone)]
struct BatteriesListMem {
    batteries: Batteries,
    next: BatteriesList,
}

impl Preserve for BatteriesList {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        if self.0.is_null() {
            return;
        }
        let mut cursor = *self;
        loop {
            stack.struct_is_in(cursor.0, 1);
            (*cursor.0).batteries.assert_in_stack(stack);
            if (*cursor.0).next.0.is_null() {
                break;
            };
            cursor = (*cursor.0).next;
        }
    }
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
                *ptr = dest_mem;
                ptr = &mut ((**ptr).next.0);
                if (*dest_mem).next.0.is_null() {
                    break;
                };
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
    fn matches(mut self, stack: &mut NockStack, core: Noun) -> Option<Batteries> {
        self.find(|&batteries| batteries.matches(stack, core))
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
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut cursor = *self;
        loop {
            stack.struct_is_in(cursor.0, 1);
            (*cursor.0).element.assert_in_stack(stack);
            if (*cursor.0).next.0.is_null() {
                break;
            };
            cursor = (*cursor.0).next;
        }
    }
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut ptr: *mut NounList = self;
        loop {
            if stack.is_in_frame((*ptr).0) {
                (*(*ptr).0).element.preserve(stack);
                let dest_mem: *mut NounListMem = stack.struct_alloc_in_previous_frame(1);
                copy_nonoverlapping((*ptr).0, dest_mem, 1);
                *ptr = NounList(dest_mem);
                ptr = &mut ((*(*ptr).0).next);
                if (*dest_mem).next.0.is_null() {
                    break;
                };
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
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.struct_is_in(self.0, 1);
        (*self.0).battery_to_paths.assert_in_stack(stack);
        (*self.0).root_to_paths.assert_in_stack(stack);
        (*self.0).path_to_batteries.assert_in_stack(stack);
    }
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
    pub fn new(stack: &mut NockStack) -> Self {
        let battery_to_paths = Hamt::new();
        let root_to_paths = Hamt::new();
        let path_to_batteries = Hamt::new();
        unsafe {
            let cold_mem_ptr: *mut ColdMem = stack.struct_alloc(1);
            *cold_mem_ptr = ColdMem {
                battery_to_paths,
                root_to_paths,
                path_to_batteries,
            };
            Cold(cold_mem_ptr)
        }
    }

    pub fn find(&mut self, stack: &mut NockStack, path: &mut Noun) -> BatteriesList {
        unsafe {
            (*(self.0))
                .path_to_batteries
                .lookup(stack, path)
                .unwrap_or(BATTERIES_LIST_NIL)
        }
    }

    /// register a core, return a boolean of whether we actually needed to register (false ->
    /// already registered)
    ///
    /// XX TODO validate chum
    #[allow(clippy::result_unit_err)]
    pub fn register(
        &mut self,
        stack: &mut NockStack,
        mut core: Noun,
        parent_axis: Atom,
        mut chum: Noun,
    ) -> Result<bool, ()> {
        let mut battery = core.slot(2)?;

        unsafe {
            // Are we registering a root?
            if let Ok(parent_axis_direct) = parent_axis.as_direct() {
                if parent_axis_direct.data() == 0 {
                    let mut root_path = T(stack, &[chum, D(0)]);
                    if let Some(paths) = (*(self.0)).root_to_paths.lookup(stack, &mut core) {
                        for a_path in paths {
                            if unifying_equality(stack, &mut root_path, a_path) {
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

            let mut parent = core.slot_atom(parent_axis)?;
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

            let mut parent_battery = parent.slot(2)?;

            let mut ret: Result<bool, ()> = Err(()); // err until we actually found a parent

            let mut path_to_batteries = (*(self.0)).path_to_batteries;
            let mut battery_to_paths = (*(self.0)).battery_to_paths;
            let root_to_paths = (*(self.0)).root_to_paths;

            if let Some(paths) = battery_to_paths.lookup(stack, &mut parent_battery) {
                for a_path in paths {
                    // path is a reserved word lol
                    let battery_list = path_to_batteries
                        .lookup(stack, &mut *a_path)
                        .unwrap_or(BATTERIES_LIST_NIL);
                    if let Some(parent_batteries) = battery_list.matches(stack, parent) {
                        let mut my_path = T(stack, &[chum, *a_path]);

                        let batteries_mem_ptr: *mut BatteriesMem = stack.struct_alloc(1);
                        *batteries_mem_ptr = BatteriesMem {
                            battery,
                            parent_axis,
                            parent_batteries,
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
                for a_path in paths {
                    // path is a reserved word lol
                    let battery_list = path_to_batteries
                        .lookup(stack, &mut *a_path)
                        .unwrap_or(BATTERIES_LIST_NIL);
                    if let Some(parent_batteries) = battery_list.matches(stack, parent) {
                        let mut my_path = T(stack, &[chum, *a_path]);

                        let batteries_mem_ptr: *mut BatteriesMem = stack.struct_alloc(1);
                        *batteries_mem_ptr = BatteriesMem {
                            battery,
                            parent_axis,
                            parent_batteries,
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
                battery_to_paths,
                root_to_paths,
                path_to_batteries,
            };

            *self = Cold(cold_mem_ptr);
            ret
        }
    }
}
