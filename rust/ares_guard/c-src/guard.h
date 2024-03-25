#ifndef __GUARD_H__
#define __GUARD_H__

#include <setjmp.h>
#include <stdint.h>

/**
 * Error codes.
 */
typedef enum {
  guard_null,       // null stack or alloc pointer
  guard_signal,     // invalid signal
  guard_oom,        // out of memory
  guard_malloc,     // malloc error
  guard_mprotect,   // mprotect error
  guard_sigaction,  // sigaction error
} guard_err;

/**
 * @brief Initializes the guard page state with information about the memory
 * arena, which is later used to setup signal handlers for detecting OOM errors
 * and gracefully ceasing execution during interrupts.
 *
 * @param start_p   A pointer to the start of the memory arena
 * @param end_p     A pointer to the end of the memory arena
 * @param stack_pp  A pointer to the memory arena stack pointer location
 * @param alloc_pp  A pointer to the memory arena allocation pointer location
 */
void
init(
  const uintptr_t         start_p,
  const uintptr_t         end_p,
  const uintptr_t *const  stack_pp,
  const uintptr_t *const  alloc_pp
);

/**
 * @brief Executes the given callback function `f` within the memory arena 
 * with guard page protection. If `f`'s execution succeeds, its result is 
 * written to the return pointer `*ret`. If `f`'s execution triggers an
 * out of memory error or any other `guard_err`, the `guard_err` is
 * returned and `*ret` is left empty. In either case, cleanup is performed
 * before returning.
 * 
 * Definitions:
 * - A guard page is marked `PROT_NONE`.
 *
 * Assumptions:
 * - `NockStack` pages are marked `PROT_READ|PROT_WRITE` by default.
 * - All memory access patterns are outside-in.
 * - Callback functions are compatible with the C ABI.
 * - `NockStack` stack and allocation pointer locations are fixed.
 * - The caller is responsible for return value memory allocation.
 * - The caller is responsible for managing any external state the callback
 *   function may mutate.
 * - The callback function may be interrupted in the case of memory exhaustion
 *   or other `guard_err` error (failure to `mprotect`, `malloc`, etc.).
 * - `SIGSEGV` (`SIGBUS` on macOS) signals are expected to be raised only on
 *    guard page accesses.
 *
 * Invariants:
 * - A single guard page is installed and maintained in the approximate center
 *   until `crate::guard::call_with_guard` returns.
 * - A return value is only written to `*ret` on successful callback execution.
 * - A `guard_err` is returned.
 *
 * Enhancements:
 * - Use only a single, static jump buffer variable instead of a linked list.
 *   We currently use a linked list of jump buffers because we don't have a
 *   function for preserving stack traces across `crate::interpreter::interpret`
 *   calls.
 *
 * @param f The callback function to execute.
 * @param closure A pointer to the closure data for the callback function.
 * @param ret A pointer to a location where the callback's result can be stored.
 * 
 * @return 0 on callback success; otherwise `guard_err` error code.
 */
uint32_t
guard(
  void *(*f)(void *),
  void *closure,
  void **ret
);

#endif  // __GUARD_H__
