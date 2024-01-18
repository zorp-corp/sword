#ifndef __GUARD_H__
#define __GUARD_H__


/**
 * Execute the given closure `f` in the free memory space denoted by
 * the given `stack` and `alloc` pointers. If the current NockStack
 * frame faces west, the `stack` pointer will be less than the `alloc`
 * pointer. If it faces east, the `stack` pointer will be greater than
 * the `alloc` pointer.
 *
 * This function protects the memory space denoted by the `stack` and `alloc`
 * pointers with a guard page, which is a single page of memory inside of the
 * are which is marked with `PROT_NONE`. The rest of the pages in our memory
 * space are marked with `PROT_READ | PROT_WRITE` by default. This means that
 * the closure can read and write to the memory space, but if it attempts to
 * write to the guard page, a `SIGSEGV` signal will be raised. The signal
 * handler will then attempt to move the guard page to the middle of the
 * remaining free space in the arena. If there is no more free space, then
 * memory exhaustion has occurred and the function will return a value which
 * indicates that the caller should abort with a memory exhaustion error
 * (`bail:meme`).
 *
 * TODO: update and correct documentation.
 */
void guard(void *(*f)(void *), void *arg, void **stack, void **alloc, void **ret);

#endif
