#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "guard.h"


#define GD_PAGESZ 16384

static void *guard_p = NULL;
static void *stack_p = NULL;
static void *alloc_p = NULL;

typedef enum {
  guard_weird = 0, // strange state
  guard_armor = 1, // mprotect failure
  guard_spent = 2, // out of memory (bail:meme)
  guard_sound = 3  // job's done
} guard_err;


guard_err _focus_guard(void *si_addr) {
  if (stack_p == NULL || alloc_p == NULL) {
    return guard_weird;
  }

  // Install the guard page.
  if (guard_p == NULL) {
    guard_p = stack_p + ((alloc_p - stack_p) / 2);
    if (mprotect(guard_p, GD_PAGESZ, PROT_NONE) == -1) {
      return guard_armor;
    }
  }

  // Re-center the guard page if the fault address falls within it.
  if (si_addr >= guard_p && si_addr < guard_p + GD_PAGESZ) {
    // Unmark the old guard page.
    void *old_guard_pg = guard_p;
    if (mprotect(old_guard_pg, GD_PAGESZ, PROT_READ | PROT_WRITE) == -1) {
      return guard_armor;
    }

    // Place the new guard page in the center.
    guard_p = stack_p + ((alloc_p - stack_p) / 2);
    if (guard_p != old_guard_pg) {
      if (mprotect(guard_p, GD_PAGESZ, PROT_NONE) == -1) {
        return guard_armor;
      }
    } else {
      return guard_spent;
    }
  }

  return guard_sound;
}

void _sigsegv_handler(int sig, siginfo_t *si, void *unused) {
  _focus_guard(si->si_addr);
}

int _register_handler() {
  struct sigaction sa;

  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _sigsegv_handler;
  sigemptyset(&sa.sa_mask);

  return sigaction(SIGSEGV, &sa, NULL);
}

void guard(void *(*f)(void *), void *arg, void **stack, void **alloc, void **ret) {
  guard_err err;

  stack_p = *stack;
  alloc_p = *alloc;

  if (_register_handler() == -1) {
    err = guard_weird;
    goto fail;
  }

  *ret = f(arg);
  return;

fail:
  *ret = (void *) &err;
  return;
}
