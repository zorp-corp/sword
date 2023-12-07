use crate::hamt::Hamt;
use crate::mem::{unifying_equality, NockStack, Preserve};
use crate::noun;
use crate::noun::{Atom, DirectAtom, Noun, Slots, D, T};
use crate::persist::{Persist, PMA};
use std::ptr::copy_nonoverlapping;
use std::ptr::null_mut;
use std::mem::size_of;

pub enum Error {
    NoParent,
    BadNock,
}

impl From<noun::Error> for Error {
    fn from(_: noun::Error) -> Self {
        Error::BadNock
    }
}

pub type Result = std::result::Result<bool, Error>;

// Batteries is a core hierarchy (e.g. a path of parent batteries to a root)
#[derive(Copy, Clone)]
pub struct Batteries(*mut BatteriesMem);

const NO_BATTERIES: Batteries = Batteries(null_mut());

#[derive(Copy, Clone)]
struct BatteriesMem {
    battery: Noun,
    parent_axis: Atom,
    parent_batteries: Batteries,
}

impl Persist for Batteries {
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize {
        let mut bytes = 0;
        let mut batteries = *self;

        loop {
            if batteries.0.is_null() { break; }
            if pma.contains(batteries.0, 1) { break; }
            bytes += size_of::<BatteriesMem>();
            bytes += (*batteries.0).battery.space_needed(stack, pma);
            bytes += (*batteries.0).parent_axis.space_needed(stack, pma);
            batteries = (*batteries.0).parent_batteries;
        }
        bytes
    }

    unsafe fn copy_to_buffer(
        &mut self,           
        stack: &mut NockStack,
        pma: &PMA,
        buffer: &mut *mut u8,
    ) {
        let mut dest = self;
        loop {
            if (*dest).0.is_null() { break; }
            if pma.contains((*dest).0, 1) { break; }

            let batteries_mem_ptr = *buffer as *mut BatteriesMem;
            copy_nonoverlapping((*dest).0, batteries_mem_ptr, 1);
            *buffer = batteries_mem_ptr.add(1) as *mut u8;

            (*batteries_mem_ptr).battery.copy_to_buffer(stack, pma, buffer);
            (*batteries_mem_ptr).parent_axis.copy_to_buffer(stack, pma, buffer);

            (*dest).0 = batteries_mem_ptr;
            dest = &mut (*(*dest).0).parent_batteries;
        }
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0 as u64
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Batteries(meta_handle as *mut BatteriesMem)
    }

}

impl Preserve for Batteries {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut cursor = *self;
        loop {
            stack.assert_struct_is_in(cursor.0, 1);
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
        let mut root_found: bool = false;

        for (battery, parent_axis) in self {
            if root_found {
                panic!("cold: core matched to root, but more data remains in path");
            }

            if let Ok(d) = parent_axis.as_direct() {
                if d.data() == 0 {
                    if unsafe { unifying_equality(stack, &mut core, battery) } {
                        root_found = true;
                        continue;
                    } else {
                        return false;
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

        if !root_found {
            panic!("cold: core matched exactly, but never matched root");
        }

        true
    }
}

// BatteriesList is a linked list of core hierarchies with an iterator; used to
// store all possible parent hierarchies for a core
#[derive(Copy, Clone)]
pub struct BatteriesList(*mut BatteriesListMem);

const BATTERIES_LIST_NIL: BatteriesList = BatteriesList(null_mut());

#[derive(Copy, Clone)]
struct BatteriesListMem {
    batteries: Batteries,
    next: BatteriesList,
}

impl Persist for BatteriesList {
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize {
        let mut bytes = 0;
        let mut list = *self;
        loop {
            if list.0.is_null() { break; }
            if pma.contains(list.0, 1) { break; }
            bytes += size_of::<BatteriesListMem>();
            bytes += (*list.0).batteries.space_needed(stack, pma);

            list = (*list.0).next;
        }
        bytes
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, pma: &PMA, buffer: &mut *mut u8) {
        let mut dest = self;

        loop {
            if (*dest).0.is_null() { break; }
            if pma.contains((*dest).0, 1) { break; }

            let list_mem_ptr = *buffer as *mut BatteriesListMem;
            copy_nonoverlapping((*dest).0, list_mem_ptr, 1);
            *buffer = list_mem_ptr.add(1) as *mut u8;
            (*dest).0 = list_mem_ptr;

            (*(*dest).0).batteries.copy_to_buffer(stack, pma, buffer);
            dest = &mut (*(*dest).0).next;
        }
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0 as u64
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        BatteriesList(meta_handle as *mut BatteriesListMem)
    }
}

impl Preserve for BatteriesList {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        if self.0.is_null() {
            return;
        }
        let mut cursor = *self;
        loop {
            stack.assert_struct_is_in(cursor.0, 1);
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

// NounList is a linked list of paths (path = list of nested core names) with an
// iterator; used to store all possible registered paths for a core
#[derive(Copy, Clone)]
struct NounList(*mut NounListMem);

const NOUN_LIST_NIL: NounList = NounList(null_mut());

#[derive(Copy, Clone)]
struct NounListMem {
    element: Noun,
    next: NounList,
}

impl Persist for NounList {
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize {
        let mut bytes: usize = 0;
        let mut list = *self;

        loop {
            if list.0.is_null() { break; }
            if pma.contains(list.0, 1) { break; }

            bytes += size_of::<NounListMem>();
            bytes += (*list.0).element.space_needed(stack, pma);

            list = (*list.0).next;
        }
        bytes
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, pma: &PMA, buffer: &mut *mut u8) {
        let mut dest = self;

        loop {
            if (*dest).0.is_null() { break; }
            if pma.contains((*dest).0, 1) { break; }

            let noun_list_mem_ptr = *buffer as *mut NounListMem;
            copy_nonoverlapping((*dest).0, noun_list_mem_ptr, 1);
            *buffer = noun_list_mem_ptr.add(1) as *mut u8;

            (*dest).0 = noun_list_mem_ptr;
            (*(*dest).0).element.copy_to_buffer(stack, pma, buffer);

            dest = &mut (*(*dest).0).next;
        }
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0 as u64
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        NounList(meta_handle as *mut NounListMem)
    }
}

impl Preserve for NounList {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        if self.0.is_null() {
            return;
        };
        let mut cursor = *self;
        loop {
            stack.assert_struct_is_in(cursor.0, 1);
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

#[derive(Copy, Clone)]
pub struct Cold(*mut ColdMem);

struct ColdMem {
    /// key: outermost battery (e.g. furthest battery from root for a core)
    /// value: possible registered paths for core
    ///
    /// Identical nock can exist in multiple places, so the outermost battery
    /// yields multiple paths. Instead of matching on the entire core in the Hamt
    /// (which would require iterating through every possible pair), we match
    /// the outermost battery to a path, then compare the core to the registered
    /// cores for that path.
    battery_to_paths: Hamt<NounList>,
    /// Roots
    /// key: root noun
    /// value: root path
    ///
    /// Just like battery_to_paths, but for roots (which refer to themselves as
    /// their parent).
    root_to_paths: Hamt<NounList>,
    /// key: registered path to core
    /// value: linked list of a sequence of nested batteries
    path_to_batteries: Hamt<BatteriesList>,
}

impl Persist for Cold {
    unsafe fn space_needed(&mut self, stack: &mut NockStack, pma: &PMA) -> usize {
        todo!()
    }

    unsafe fn copy_to_buffer(
        &mut self,
        stack: &mut NockStack,
        pma: &PMA,
        buffer: &mut *mut u8,
    ) {
        todo!()
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0 as u64
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Cold(meta_handle as *mut ColdMem)
    }
}

impl Preserve for Cold {
    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_struct_is_in(self.0, 1);
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
    pub fn is_null(&self) -> bool {
        unsafe {
            (*self.0).battery_to_paths.is_null()
                || (*self.0).battery_to_paths.is_null()
                || (*self.0).root_to_paths.is_null()
        }
    }

    pub fn new(stack: &mut NockStack) -> Self {
        let battery_to_paths = Hamt::new(stack);
        let root_to_paths = Hamt::new(stack);
        let path_to_batteries = Hamt::new(stack);
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

    /** Try to match a core directly to the cold state, print the resulting path if found
     */
    pub fn matches(&mut self, stack: &mut NockStack, core: &mut Noun) -> Option<Noun> {
        let mut battery = (*core).slot(2).ok()?;
        unsafe {
            let paths = (*(self.0)).battery_to_paths.lookup(stack, &mut battery)?;
            for path in paths {
                if let Some(batteries_list) =
                    (*(self.0)).path_to_batteries.lookup(stack, &mut (*path))
                {
                    if let Some(_batt) = batteries_list.matches(stack, *core) {
                        return Some(*path);
                    }
                }
            }
        };
        None
    }

    /// register a core, return a boolean of whether we actually needed to register (false ->
    /// already registered)
    ///
    /// XX: validate chum Noun as $chum
    #[allow(clippy::result_unit_err)]
    pub fn register(
        &mut self,
        stack: &mut NockStack,
        mut core: Noun,
        parent_axis: Atom,
        mut chum: Noun,
    ) -> Result {
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

            let mut battery = core.slot(2)?;
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

            // err until we actually found a parent
            let mut ret: Result = Err(Error::NoParent);

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
