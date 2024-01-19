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
  guard_sound = 0, // job's done
  guard_armor = 1, // mprotect
  guard_weird = 2, // strange state
  guard_spent = 3, // out of memory (bail:meme)
  guard_erupt = 4, // sigint
} guard_err_t;

volatile sig_atomic_t guard_err = guard_sound;


guard_err_t _focus_guard(void *si_addr) {
  // Check for strange situations.
  if (stack_p == NULL || alloc_p == NULL || si_addr == NULL) {
    return guard_weird;
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
    // TODO: Ensure the guard_p is page-aligned.
    // guard_p = (void *)((uintptr_t)guard_p & ~(GD_PAGESZ - 1));
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

void _signal_handler(int sig, siginfo_t *si, void *unused) {
  switch (sig) {
    case SIGSEGV:
      guard_err = _focus_guard(si->si_addr);
      break;
    case SIGINT:
      guard_err = guard_erupt;
      break;
    default:
      break;
  }
}

guard_err_t _register_handler() {
  guard_err_t err = guard_sound;
  struct sigaction sa;

  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _signal_handler;
  sa.sa_mask = 0;

  if (sigaction(SIGSEGV, &sa, NULL)) {
    err = guard_weird;
  }

  return guard_err;
}

void guard(void *(*f)(void *), void *arg, void **stack, void **alloc, void **ret) {
  guard_err_t err;
  stack_p = *stack;
  alloc_p = *alloc;

  if (_register_handler() != guard_sound) {
    err = guard_weird;
    goto fail;
  }

  if (guard_p == NULL) {
    guard_p = stack_p + ((alloc_p - stack_p) / 2);
    if (mprotect(guard_p, GD_PAGESZ, PROT_NONE) == -1) {
      err = guard_armor;
      goto fail;
    }
  }

  *ret = f(arg);

  if (guard_err != guard_sound) {
    goto fail;
  }

  return;

fail:
  *ret = (void *) &guard_err;
  return;
}
