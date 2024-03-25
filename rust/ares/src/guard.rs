use crate::interpreter::{Error, Mote, Result};
use crate::mem::NockStack;
use crate::noun::D;
use ares_guard::*;
use assert_no_alloc::permit_alloc;
use std::ffi::c_void;
use std::marker::PhantomData;

#[derive(Debug)]
pub enum GuardError {
    MemoryProtection,
    NullPointer,
    OutOfMemory,
    Setup,
    Unknown,
}

impl From<u32> for GuardError {
    fn from(value: u32) -> Self {
        match value {
            GUARD_NULL => Self::NullPointer,
            GUARD_OOM => Self::OutOfMemory,
            x if (x & GUARD_MPROTECT) != 0 => Self::MemoryProtection,
            x if (x & (GUARD_MALLOC | GUARD_SIGACTION)) != 0 => Self::Setup,
            _ => Self::Unknown,
        }
    }
}

pub struct CCallback<'closure> {
    pub function: unsafe extern "C" fn(*mut c_void) -> *mut c_void,
    pub input: *mut c_void,
    // without this it's too easy to accidentally drop the closure too soon
    _lifetime: PhantomData<&'closure mut c_void>,
}

impl<'closure> CCallback<'closure> {
    pub fn new<F>(closure: &'closure mut F) -> Self
    where
        F: FnMut() -> Result,
    {
        let function: unsafe extern "C" fn(*mut c_void) -> *mut c_void = Self::call_closure::<F>;

        Self {
            function,
            input: closure as *mut F as *mut c_void,
            _lifetime: PhantomData,
        }
    }

    unsafe extern "C" fn call_closure<F>(input: *mut c_void) -> *mut c_void
    where
        F: FnMut() -> Result,
    {
        let cb: &mut F = input.cast::<F>().as_mut().unwrap();
        let v = (*cb)();
        permit_alloc(|| {
            let v_box = Box::new(v);
            let v_ptr = Box::into_raw(v_box);
            v_ptr as *mut c_void
        })
    }
}

pub fn init_guard(stack: &NockStack) {
    unsafe {
        let start = stack.get_start();
        let end = start.add(stack.get_size());

        init(
            start as usize,
            end as usize,
            stack.get_stack_pointer_pointer() as *const usize,
            stack.get_alloc_pointer_pointer() as *const usize
        );
    }
}

pub fn call_with_guard<F: FnMut() -> Result>(
    closure: &mut F,
) -> Result {
    let cb = CCallback::new(closure);
    let mut ret_p: *mut c_void = std::ptr::null_mut();
    let ret_pp = &mut ret_p as *mut *mut c_void;

    unsafe {
        let res = guard(
            Some(cb.function as unsafe extern "C" fn(*mut c_void) -> *mut c_void),
            cb.input,
            ret_pp,
        );

        if res == 0 {
            permit_alloc(|| {
                let result_box = Box::from_raw(ret_p as *mut Result);
                *result_box
            })
        } else {
            let err = GuardError::from(res);
            match err {
                GuardError::OutOfMemory => Err(Error::NonDeterministic(Mote::Meme, D(0))),
                _ => {
                    panic!("serf: guard: unexpected error {:?} {}", err, res);
                }
            }
        }
    }
}
