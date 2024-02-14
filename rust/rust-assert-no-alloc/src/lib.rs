/* assert_no_alloc -- A custom Rust allocator allowing to temporarily disable
 * memory (de)allocations for a thread.
 *
 * Copyright (c) 2020 Florian Jung <flo@windfis.ch>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#![doc = include_str!("../README.md")]

use std::alloc::{System,GlobalAlloc,Layout};
use std::cell::Cell;

// check for mutually exclusive features.
#[cfg(all(feature = "disable_release", feature = "warn_release"))]
compile_error!("disable_release cannot be active at the same time with warn_release");


#[cfg(not(all(feature = "disable_release", not(debug_assertions))))] // if not disabled
thread_local! {
	static ALLOC_FORBID_COUNT: Cell<u32> = Cell::new(0);
	static ALLOC_PERMIT_COUNT: Cell<u32> = Cell::new(0);

	#[cfg(any( all(feature="warn_debug", debug_assertions), all(feature="warn_release", not(debug_assertions)) ))]
	static ALLOC_VIOLATION_COUNT: Cell<u32> = Cell::new(0);
}

#[cfg(all(feature = "disable_release", not(debug_assertions)))] // if disabled
pub fn assert_no_alloc<T, F: FnOnce() -> T> (func: F) -> T { // no-op
	func()
}

#[cfg(all(feature = "disable_release", not(debug_assertions)))] // if disabled
pub fn permit_alloc<T, F: FnOnce() -> T> (func: F) -> T { // no-op
	func()
}

#[cfg(not(all(feature = "disable_release", not(debug_assertions))))] // if not disabled
/// Calls the `func` closure, but forbids any (de)allocations.
///
/// If a call to the allocator is made, the program will abort with an error,
/// print a warning (depending on the `warn_debug` feature flag. Or ignore
/// the situation, when compiled in `--release` mode with the `disable_release`
///feature flag set (which is the default)).
pub fn assert_no_alloc<T, F: FnOnce() -> T> (func: F) -> T {
	// RAII guard for managing the forbid counter. This is to ensure correct behaviour
	// when catch_unwind is used
	struct Guard;
	impl Guard {
		fn new() -> Guard {
			ALLOC_FORBID_COUNT.with(|c| c.set(c.get()+1));
			Guard
		}
	}
	impl Drop for Guard {
		fn drop(&mut self) {
			ALLOC_FORBID_COUNT.with(|c| c.set(c.get()-1));
		}
	}


	#[cfg(any( all(feature="warn_debug", debug_assertions), all(feature="warn_release", not(debug_assertions)) ))] // if warn mode is selected
	let old_violation_count = violation_count();


	let guard = Guard::new(); // increment the forbid counter
	let ret = func();
	std::mem::drop(guard);    // decrement the forbid counter


	#[cfg(any( all(feature="warn_debug", debug_assertions), all(feature="warn_release", not(debug_assertions)) ))] // if warn mode is selected
	if violation_count() > old_violation_count {
		eprintln!("Tried to (de)allocate memory in a thread that forbids allocator calls!");
	}

	return ret;
}

#[cfg(not(all(feature = "disable_release", not(debug_assertions))))] // if not disabled
/// Calls the `func` closure, but ensures that the forbid and permit counters
/// are maintained accurately even if a longjmp originates and terminates
/// within the closure. If you longjmp over this function, we can't fix
/// anything about it.
pub fn ensure_alloc_counters<T, F: FnOnce() -> T> (func: F) -> T {
	let forbid_counter = ALLOC_FORBID_COUNT.with(|c| c.get());
	let permit_counter = ALLOC_PERMIT_COUNT.with(|c| c.get());

	let ret = func();

	ALLOC_FORBID_COUNT.with(|c| c.set(forbid_counter));
	ALLOC_PERMIT_COUNT.with(|c| c.set(permit_counter));

	return ret;
}

#[cfg(not(all(feature = "disable_release", not(debug_assertions))))] // if not disabled
/// Calls the `func` closure. Allocations are temporarily allowed, even if this
/// code runs inside of assert_no_alloc.
pub fn permit_alloc<T, F: FnOnce() -> T> (func: F) -> T {
	// RAII guard for managing the permit counter
	struct Guard;
	impl Guard {
		fn new() -> Guard {
			ALLOC_PERMIT_COUNT.with(|c| c.set(c.get()+1));
			Guard
		}
	}
	impl Drop for Guard {
		fn drop(&mut self) {
			ALLOC_PERMIT_COUNT.with(|c| c.set(c.get()-1));
		}
	}

	let guard = Guard::new(); // increment the forbid counter
	let ret = func();
	std::mem::drop(guard);    // decrement the forbid counter

	return ret;
}

#[cfg(any( all(feature="warn_debug", debug_assertions), all(feature="warn_release", not(debug_assertions)) ))] // if warn mode is selected
/// Returns the count of allocation warnings emitted so far.
///
/// Only available when the `warn_debug` or `warn release` features are enabled.
pub fn violation_count() -> u32 {
	ALLOC_VIOLATION_COUNT.with(|c| c.get())
}

#[cfg(any( all(feature="warn_debug", debug_assertions), all(feature="warn_release", not(debug_assertions)) ))] // if warn mode is selected
/// Resets the count of allocation warnings to zero.
///
/// Only available when the `warn_debug` or `warn release` features are enabled.
pub fn reset_violation_count() {
	ALLOC_VIOLATION_COUNT.with(|c| c.set(0));
}

pub fn reset_counters() {
	ALLOC_FORBID_COUNT.with(|c| c.set(0));
	ALLOC_PERMIT_COUNT.with(|c| c.set(0));

	#[cfg(any( all(feature="warn_debug", debug_assertions), all(feature="warn_release", not(debug_assertions)) ))]
	ALLOC_VIOLATION_COUNT.with(|c| c.set(0));
}




#[cfg(not(all(feature = "disable_release", not(debug_assertions))))] // if not disabled
/// The custom allocator that handles the checking.
///
/// To use this crate, you must add the following in your `main.rs`:
/// ```rust
/// use assert_no_alloc::*;
/// // ...
/// #[cfg(debug_assertions)]
/// #[global_allocator]
/// static A: AllocDisabler = AllocDisabler;
/// ```
pub struct AllocDisabler;

#[cfg(not(all(feature = "disable_release", not(debug_assertions))))] // if not disabled
impl AllocDisabler {
	fn check(&self, layout: Layout) {
		let forbid_count = ALLOC_FORBID_COUNT.with(|f| f.get());
		let permit_count = ALLOC_PERMIT_COUNT.with(|p| p.get());
		if forbid_count > 0 && permit_count == 0 {
			#[cfg(any( all(feature="warn_debug", debug_assertions), all(feature="warn_release", not(debug_assertions)) ))] // if warn mode is selected
			ALLOC_VIOLATION_COUNT.with(|c| c.set(c.get()+1));

			#[cfg(any( all(not(feature="warn_debug"), debug_assertions), all(not(feature="warn_release"), not(debug_assertions)) ))] // if abort mode is selected
			{
				#[cfg(all(feature = "log", feature = "backtrace"))]
				permit_alloc(|| log::error!("Memory allocation of {} bytes failed from:\n{:?}", layout.size(), backtrace::Backtrace::new()));
				#[cfg(all(feature = "log", not(feature = "backtrace")))]
				permit_alloc(|| log::error!("Memory allocation of {} bytes failed", layout.size()));

				#[cfg(all(not(feature = "log"), feature = "backtrace"))]
				permit_alloc(|| eprintln!("Allocation failure from:\n{:?}", backtrace::Backtrace::new()));

				// This handler can be overridden (although as of writing, the API to do so is still
				// unstable) so we must always call this even when the log feature is enabled
				std::alloc::handle_alloc_error(layout);
			}
		}
	}
}

#[cfg(not(all(feature = "disable_release", not(debug_assertions))))] // if not disabled
unsafe impl GlobalAlloc for AllocDisabler {
	unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
		self.check(layout);
		System.alloc(layout)
	}

	unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
		self.check(layout);
		System.dealloc(ptr, layout)
	}
}

/// Wrapper for objects whose Drop implementation shall be permitted
/// to (de)allocate.
///
/// Typical usage:
///
/// ```rust
/// let foo = PermitDrop::new(
///     permit_alloc(||
///         Box::new(...)
///     )
/// );
/// ```
///
/// Here, creation of the Box is guarded by the explicit `permit_alloc` call,
/// and destruction of the Box is guarded by PermitDrop. Neither creation nor
/// destruction will cause an assertion failure from within `assert_no_alloc`.
pub struct PermitDrop<T>(Option<T>);

impl<T> PermitDrop<T> {
	pub fn new(t: T) -> PermitDrop<T> {
		permit_alloc(|| {
			PermitDrop(Some(t))
		})
	}
}

impl<T> std::ops::Deref for PermitDrop<T> {
	type Target = T;
	fn deref(&self) -> &T { self.0.as_ref().unwrap() }
}

impl<T> std::ops::DerefMut for PermitDrop<T> {
	fn deref_mut(&mut self) -> &mut T { self.0.as_mut().unwrap() }
}

impl<I: Iterator> Iterator for PermitDrop<I> {
	type Item = I::Item;
	fn next(&mut self) -> Option<Self::Item> {
		(**self).next()
	}
}


impl<T> Drop for PermitDrop<T> {
	fn drop(&mut self) {
		let mut tmp = None;
		std::mem::swap(&mut tmp, &mut self.0);
		permit_alloc(|| {
			std::mem::drop(tmp);
		});
	}
}
