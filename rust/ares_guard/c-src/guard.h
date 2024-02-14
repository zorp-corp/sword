#ifndef __GUARD_H__
#define __GUARD_H__

#include <setjmp.h>
#include <stdint.h>

/**
 * Error codes and flags.
 *
 * The flags are bitwise added to the errno of their respective errors.
 */
typedef enum {
  guard_null      = 1,          // null stack or alloc pointer
  guard_signal,                 // invalid signal
  guard_oom,                    // OOM
  guard_malloc    = 0x10000000, // malloc error flag
  guard_mprotect  = 0x20000000, // mprotect error flag
  guard_sigaction = 0x40000000, // sigaction error flag
} guard_err;

typedef void *(*callback)(void *);

/**
 * Execute the given closure `f` within the memory arena between the
 * `stack` and `alloc` pointers, with guard page protection. Write either
 * `f`'s succesful result or a `guard_err` to the given `ret` pointer.
 *
 * Memory
 * ------
 * The free memory arena between the `stack` and `alloc` pointers is part of a
 * NockStack frame, which may either face east or west. If the frame faces
 * east, the `stack` pointer will be greater than the `alloc` pointer. If it
 * faces west, the `stack` pointer will be less than the `alloc` pointer.
 *
 * All the pages in the memory arena are marked clean (`PROT_READ | PROT_WRITE`)
 * by default, with the exception of a single guard page in the middle of the
 * arena, which is marked with `PROT_NONE`.
 *
 * Guard
 * -----
 * This function protects the free memory arena between the `stack` and `alloc`
 * pointers with a guard page. A guard page is simply a single page of memory
 * which is marked with `PROT_NONE`. Since all other pages are marked clean by
 * default, a SIGSEGV will only be raised if the `f` function attempts to write
 * to the guard page. When it does, the signal handler will attempt to re-center
 * the guard page in the remaining free space left in the arena. If there is no
 * more free space, then memory exhaustion has occurred and the `guard_spent`
 * error will be written to the `ret` pointer. The caller is then responsible
 * for handling this error and aborting with a `bail:meme`.
 */
int32_t
guard(
  callback f,
  void *closure,
  const uintptr_t *s_pp,
  const uintptr_t *a_pp,
  void **ret
);

#endif  // __GUARD_H__
