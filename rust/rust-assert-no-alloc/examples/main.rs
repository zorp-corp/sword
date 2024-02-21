use assert_no_alloc::*;

#[cfg(debug_assertions)]
#[global_allocator]
static A: AllocDisabler = AllocDisabler;

fn main() {
	println!("Alloc is allowed. Let's allocate some memory...");
	let mut vec_can_push = Vec::new();
	vec_can_push.push(42);

	println!();

	let fib5 = assert_no_alloc(|| {
		println!("Alloc is forbidden. Let's calculate something without memory allocations...");

		fn fib(n: u32) -> u32 {
			if n<=1 { 1 }
			else { fib(n-1) + fib(n-2) }
		}

		fib(5)
	});
	println!("\tSuccess, the 5th fibonacci number is {}", fib5);
	println!();

	assert_no_alloc(|| {
		println!("Alloc is forbidden. Let's allocate some memory...");
		let mut vec_cannot_push = Vec::new();
		vec_cannot_push.push(42); // panics
	});

	println!("This will not be executed if the above allocation has aborted.");
}
