#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub const GUARD_SOUND: u32 = guard_err_guard_sound; 
pub const GUARD_ARMOR: u32 = guard_err_guard_armor; 
pub const GUARD_WEIRD: u32 = guard_err_guard_weird;
pub const GUARD_SPENT: u32 = guard_err_guard_spent;
