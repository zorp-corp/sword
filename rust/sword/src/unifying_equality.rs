use crate::assert_acyclic;
use crate::assert_no_forwarding_pointers;
use crate::assert_no_junior_pointers;
use crate::mem::AllocResult;
use crate::mem::{NockStack, ALLOC, FRAME, STACK};
use crate::noun::Noun;
use crate::persist::{pma_contains, pma_dirty};
use either::Either::*;
use libc::{c_void, memcmp};

#[cfg(feature = "check_junior")]
#[macro_export]
macro_rules! assert_no_junior_pointers {
    ( $x:expr, $y:expr ) => {
        assert_no_alloc::permit_alloc(|| {
            assert!($x.no_junior_pointers($y));
        })
    };
}

#[cfg(not(feature = "check_junior"))]
#[macro_export]
macro_rules! assert_no_junior_pointers {
    ( $x:expr, $y:expr ) => {};
}

#[cfg(test)]
pub(crate) mod test {
    use crate::mem::NockStack;
    use crate::noun::Noun;

    /// Tests only, not part of the actual implementation. Use this outside of #[cfg(test)] and I will sic the linter on you.
    pub unsafe fn unifying_equality(stack: &mut NockStack, a: *mut Noun, b: *mut Noun) -> bool {
        super::unifying_equality(stack, a, b).expect("OOM error in test::unifying_equality")
    }
}

pub unsafe fn unifying_equality(stack: &mut NockStack, a: *mut Noun, b: *mut Noun) -> AllocResult<bool> {
    /* This version of unifying equality is not like that of vere.
     * Vere does a tree comparison (accelerated by pointer equality and short-circuited by mug
     * equality) and then unifies the nouns at the top level if they are equal.
     *
     * Here we recursively attempt to unify nouns. Pointer-equal nouns are already unified.
     * Disequal mugs again short-circuit the unification and equality check.
     *
     * Since we expect atoms to be normalized, direct and indirect atoms do not unify with each
     * other. For direct atoms, no unification is possible as there is no pointer involved in their
     * representation. Equality is simply direct equality on the word representation. Indirect
     * atoms require equality first of the size and then of the memory buffers' contents.
     *
     * Cell equality is tested (after mug and pointer equality) by attempting to unify the heads and tails,
     * respectively, of cells, and then re-testing. If unification succeeds then the heads and
     * tails will be pointer-wise equal and the cell itself can be unified. A failed unification of
     * the head or the tail will already short-circuit the unification/equality test, so we will
     * not return to re-test the pointer equality.
     *
     * When actually mutating references for unification, we must be careful to respect seniority.
     * A reference to a more junior noun should always be replaced with a reference to a more
     * senior noun, *never vice versa*, to avoid introducing references from more senior frames
     * into more junior frames, which would result in incorrect operation of the copier.
     */
    assert_acyclic!(*a);
    assert_acyclic!(*b);
    assert_no_forwarding_pointers!(*a);
    assert_no_forwarding_pointers!(*b);
    assert_no_junior_pointers!(stack, *a);
    assert_no_junior_pointers!(stack, *b);

    // If the nouns are already word-equal we have nothing to do
    if (*a).raw_equals(*b) {
        return Ok(true);
    };
    // If the nouns have cached mugs which are disequal we have nothing to do
    if let (Ok(a_alloc), Ok(b_alloc)) = ((*a).as_allocated(), (*b).as_allocated()) {
        if let (Some(a_mug), Some(b_mug)) = (a_alloc.get_cached_mug(), b_alloc.get_cached_mug()) {
            if a_mug != b_mug {
                return Ok(false);
            };
        };
    };
    stack.frame_push(0);
    *(stack.push::<(*mut Noun, *mut Noun)>()?) = (a, b);
    loop {
        if stack.stack_is_empty() {
            break;
        };
        let (x, y): (*mut Noun, *mut Noun) = *(stack.top());
        if (*x).raw_equals(*y) {
            stack.pop::<(*mut Noun, *mut Noun)>();
            continue;
        };
        if let (Ok(x_alloc), Ok(y_alloc)) = (
            // equal direct atoms return true for raw_equals()
            (*x).as_allocated(),
            (*y).as_allocated(),
        ) {
            if let (Some(x_mug), Some(y_mug)) = (x_alloc.get_cached_mug(), y_alloc.get_cached_mug())
            {
                if x_mug != y_mug {
                    break; // short-circuit, the mugs differ therefore the nouns must differ
                }
            };
            match (x_alloc.as_either(), y_alloc.as_either()) {
                (Left(x_indirect), Left(y_indirect)) => {
                    let x_as_ptr = x_indirect.to_raw_pointer();
                    let y_as_ptr = y_indirect.to_raw_pointer();
                    if x_indirect.size() == y_indirect.size()
                        && memcmp(
                            x_indirect.data_pointer() as *const c_void,
                            y_indirect.data_pointer() as *const c_void,
                            x_indirect.size() << 3,
                        ) == 0
                    {
                        let (_senior, junior) = senior_pointer_first(stack, x_as_ptr, y_as_ptr);
                        if x_as_ptr == junior {
                            if pma_contains(x, 1) {
                                pma_dirty(x, 1);
                            }
                            *x = *y;
                        } else {
                            if pma_contains(y, 1) {
                                pma_dirty(y, 1);
                            }
                            *y = *x;
                        }
                        stack.pop::<(*mut Noun, *mut Noun)>();
                        continue;
                    } else {
                        break;
                    }
                }
                (Right(x_cell), Right(y_cell)) => {
                    let x_as_ptr = x_cell.to_raw_pointer() as *const u64;
                    let y_as_ptr = y_cell.to_raw_pointer() as *const u64;
                    if x_cell.head().raw_equals(y_cell.head())
                        && x_cell.tail().raw_equals(y_cell.tail())
                    {
                        let (_senior, junior) = senior_pointer_first(stack, x_as_ptr, y_as_ptr);
                        if x_as_ptr == junior {
                            if pma_contains(x, 1) {
                                pma_dirty(x, 1);
                            }
                            *x = *y;
                        } else {
                            if pma_contains(y, 1) {
                                pma_dirty(y, 1);
                            }
                            *y = *x;
                        }
                        stack.pop::<(*mut Noun, *mut Noun)>();
                        continue;
                    } else {
                        /* THIS ISN'T AN INFINITE LOOP
                         * If we discover a disequality in either side, we will
                         * short-circuit the entire loop and reset the work stack.
                         *
                         * If both sides are equal, then we will discover pointer
                         * equality when we return and unify the cell.
                         */
                        *(stack.push::<(*mut Noun, *mut Noun)>()?) =
                            (x_cell.tail_as_mut(), y_cell.tail_as_mut());
                        *(stack.push::<(*mut Noun, *mut Noun)>()?) =
                            (x_cell.head_as_mut(), y_cell.head_as_mut());
                        continue;
                    }
                }
                (_, _) => {
                    break; // cells don't unify with atoms
                }
            }
        } else {
            break; // direct atom not raw equal, so short circuit
        }
    }
    stack.frame_pop();

    assert_acyclic!(*a);
    assert_acyclic!(*b);
    assert_no_forwarding_pointers!(*a);
    assert_no_forwarding_pointers!(*b);
    assert_no_junior_pointers!(stack, *a);
    assert_no_junior_pointers!(stack, *b);

    Ok((*a).raw_equals(*b))
}

unsafe fn senior_pointer_first(
    stack: &NockStack,
    a: *const u64,
    b: *const u64,
) -> (*const u64, *const u64) {
    let mut frame_pointer: *const u64 = stack.get_frame_pointer();
    let mut stack_pointer: *const u64 = stack.get_stack_pointer();
    let mut alloc_pointer: *const u64 = stack.get_alloc_pointer();
    let prev_stack_pointer = *(stack.prev_stack_pointer_pointer());

    let (mut high_pointer, mut low_pointer): (*const u64, *const u64) = if stack.is_west() {
        (prev_stack_pointer, alloc_pointer)
    } else {
        (alloc_pointer, prev_stack_pointer)
    };

    loop {
        if low_pointer.is_null() || high_pointer.is_null() {
            // we found the bottom of the stack; check entirety of the stack
            low_pointer = stack.get_start();
            high_pointer = stack.get_start().add(stack.get_size());
        }

        match (
            a < high_pointer && a >= low_pointer,
            b < high_pointer && b >= low_pointer,
        ) {
            (true, true) => {
                // both pointers are in the same frame, pick arbitrarily (lower in mem)
                break lower_pointer_first(a, b);
            }
            (true, false) => break (b, a), // a is in the frame, b is not, so b is senior
            (false, true) => break (a, b), // b is in the frame, a is not, so a is senior
            (false, false) => {
                // chase up the stack
                #[allow(clippy::comparison_chain)]
                // test to see if the frame under consideration is a west frame
                if stack_pointer < alloc_pointer {
                    stack_pointer = *(frame_pointer.sub(STACK + 1)) as *const u64;
                    alloc_pointer = *(frame_pointer.sub(ALLOC + 1)) as *const u64;
                    frame_pointer = *(frame_pointer.sub(FRAME + 1)) as *const u64;

                    // both pointers are in the PMA, pick arbitrarily (lower in mem)
                    if frame_pointer.is_null() {
                        break lower_pointer_first(a, b);
                    };

                    // previous allocation pointer
                    high_pointer = alloc_pointer;
                    // "previous previous" stack pointer. this is the other boundary of the previous allocation arena
                    low_pointer = *(frame_pointer.add(STACK)) as *const u64;
                } else if stack_pointer > alloc_pointer {
                    stack_pointer = *(frame_pointer.add(STACK)) as *const u64;
                    alloc_pointer = *(frame_pointer.add(ALLOC)) as *const u64;
                    frame_pointer = *(frame_pointer.add(FRAME)) as *const u64;

                    // both pointers are in the PMA, pick arbitrarily (lower in mem)
                    if frame_pointer.is_null() {
                        break lower_pointer_first(a, b);
                    };

                    // previous allocation pointer
                    low_pointer = alloc_pointer;
                    // "previous previous" stack pointer. this is the other boundary of the previous allocation arena
                    high_pointer = *(frame_pointer.sub(STACK + 1)) as *const u64;
                } else {
                    panic!("senior_pointer_first: stack_pointer == alloc_pointer");
                }
            }
        }
    }
}

fn lower_pointer_first(a: *const u64, b: *const u64) -> (*const u64, *const u64) {
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}
