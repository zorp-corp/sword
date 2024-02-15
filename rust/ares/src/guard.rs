use crate::interpreter::{interpret, Context, Error, Mote, Result};
use crate::noun::{Noun, D};
use ares_guard::*;
use assert_no_alloc::permit_alloc;
use std::convert::TryFrom;
use std::ffi::c_void;
use std::marker::PhantomData;

#[derive(Debug)]
pub enum GuardError {
    InvalidSignal,
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
            GUARD_SIGNAL => Self::InvalidSignal,
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

pub fn call_with_guard<F: FnMut() -> Result>(
    stack_pp: *const *const u64,
    alloc_pp: *const *const u64,
    closure: &mut F,
) -> Result {
    let cb = CCallback::new(closure);
    let mut ret_p: *mut c_void = std::ptr::null_mut();
    let ret_pp = &mut ret_p as *mut *mut c_void;

    unsafe {
        let res = guard(
            Some(cb.function as unsafe extern "C" fn(*mut c_void) -> *mut c_void),
            cb.input,
            stack_pp as *const usize,
            alloc_pp as *const usize,
            ret_pp,
        );

        if res == 0 {
            permit_alloc(|| {
                let result_box = Box::from_raw(ret_p as *mut Result);
                *result_box
            })
        } else {
            let err = GuardError::from(u32::try_from(res).unwrap());
            match err {
                GuardError::OutOfMemory => Err(Error::NonDeterministic(Mote::Meme, D(0))),
                _ => {
                    panic!("serf: guard: unexpected error {:?}", err);
                }
            }
        }
    }
}

pub fn interpret_with_guard(context: &mut Context, eve: Noun, lyf: Noun) -> Result {
    let stack_pp = context.stack.get_stack_pointer_pointer() as *const *const u64;
    let alloc_pp = context.stack.get_alloc_pointer_pointer() as *const *const u64;

    call_with_guard(stack_pp, alloc_pp, &mut || interpret(context, eve, lyf))
}
