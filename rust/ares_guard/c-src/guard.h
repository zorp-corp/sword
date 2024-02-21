#ifndef __GUARD_H__
#define __GUARD_H__

#include <setjmp.h>
#include <stdint.h>

/**
 * Linked list stack of jump buffers.
 */
typedef struct GD_buflistnode GD_buflistnode;
struct GD_buflistnode {
  jmp_buf               buffer;
  GD_buflistnode *next;
};

/**
 * Return codes and flags.
 *
 * The flags are bitwise added to the errno of their respective errors.
 */
typedef enum {
  guard_null      = 1,          // null stack or alloc pointer
  guard_signal    = 2,          // invalid signal
  guard_oom       = 3,          // out of memory
  guard_malloc    = 0x10000000, // malloc error flag
  guard_mprotect  = 0x20000000, // mprotect error flag
  guard_sigaction = 0x40000000, // sigaction error flag
} guard_err;

/**
 * @brief Executes the given callback function `f` within the memory arena 
 * between the stack and allocation pointers pointed to by `s_pp` and `a_pp`,
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
 * - `SIGSEGV` signals are expected to be raised only on guard page accesses.
 *
 * Invariants:
 * - A single guard page is installed and maintained in the approximate center
 *   until `crate::guard::call_with_guard` returns.
 * - A return value is only written to `*ret` on successful callback execution.
 * - A `guard_err` is returned, excepting panics or negative assertions.
 *
 * Enhancements:
 * - Use only a single, static jump buffer variable instead of a linked list.
 *   We currently use a linked list of jump buffers because we don't have a
 *   function for preserving stack traces across `crate::interpreter::interpret`
 *   calls.
 *
 * @param f The callback function to execute.
 * @param closure A pointer to the closure data for the callback function.
 * @param s_pp A pointer to the stack pointer location.
 * @param a_pp A pointer to the allocation pointer location.
 * @param ret A pointer to a location where the callback's result can be stored.
 * 
 * @return A `guard_err` error code.
 */
guard_err
guard(
  void *(*f)(void *),
  void *closure,
  const uintptr_t *const s_pp,
  const uintptr_t *const a_pp,
  void **ret
);

#endif  // __GUARD_H__
