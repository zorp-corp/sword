#ifndef __GUARD_H__
#define __GUARD_H__
#include <sys/types.h>
#include <stdint.h>

/**
 * Execute the given closure `f` in a memory arena protected by a guard
 * page. The guard page is a single page of memory marked with `PROT_NONE`
 * via `mprotect`. The guard page is initially placed in the approximate
 * middle of the arena. When the closure attempts to write to the guard
 * page, a `SIGSEGV` signal is raised. The signal handler will then attempt
 * to move the guard page to the middle of the remaining free space in the
 * arena. If there is no more free space, then memory exhaustion has
 * occurred and the program will abort via a `bail:meme` error.
 */
void *guard(void *(*f)(void *), void *arg, void *stack, void *alloc);

#endif
