#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]


extern "C" {
    #[doc = " Execute the given closure `f` within the memory arena between the\n `stack` and `alloc` pointers, with guard page protection. Write either\n `f`'s succesful result or a `guard_err` to the given `ret` pointer.\n\n Memory\n ------\n The free memory arena between the `stack` and `alloc` pointers is part of a\n NockStack frame, which may either face east or west. If the frame faces\n east, the `stack` pointer will be greater than the `alloc` pointer. If it\n faces west, the `stack` pointer will be less than the `alloc` pointer.\n\n All the pages in the memory arena are marked clean (`PROT_READ | PROT_WRITE`)\n by default, with the exception of a single guard page in the middle of the\n arena, which is marked with `PROT_NONE`.\n\n Guard\n -----\n This function protects the free memory arena between the `stack` and `alloc`\n pointers with a guard page. A guard page is simply a single page of memory\n which is marked with `PROT_NONE`. Since all other pages are marked clean by\n default, a SIGSEGV will only be raised if the `f` function attempts to write\n to the guard page. When it does, the signal handler will attempt to re-center\n the guard page in the remaining free space left in the arena. If there is no\n more free space, then memory exhaustion has occurred and the `guard_spent`\n error will be written to the `ret` pointer. The caller is then responsible\n for handling this error and aborting with a `bail:meme`."]
    pub fn guard(
        f: ::std::option::Option<
            unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void) -> *mut ::std::os::raw::c_void,
        >,
        arg: *mut ::std::os::raw::c_void,
        stack: *const ::std::os::raw::c_void,
        alloc: *const ::std::os::raw::c_void,
        ret: *mut *mut ::std::os::raw::c_void,
    );
}
pub const guard_err_guard_sound: guard_err = 0;
pub const guard_err_guard_armor: guard_err = 1;
pub const guard_err_guard_weird: guard_err = 2;
pub const guard_err_guard_spent: guard_err = 3;
pub const guard_err_guard_erupt: guard_err = 4;
pub type guard_err = ::std::os::raw::c_uint;