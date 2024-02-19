#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub const GUARD_SUCCESS:   u32 = guard_result_guard_success;
pub const GUARD_NULL:      u32 = guard_result_guard_null;
pub const GUARD_SIGNAL:    u32 = guard_result_guard_signal;
pub const GUARD_OOM:       u32 = guard_result_guard_oom;
pub const GUARD_MALLOC:    u32 = guard_result_guard_malloc;
pub const GUARD_MPROTECT:  u32 = guard_result_guard_mprotect;
pub const GUARD_SIGACTION: u32 = guard_result_guard_sigaction;
