#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "guard.h"


#define GD_PAGEBITS 14ULL
#define GD_PAGESIZE (1ULL << GD_PAGEBITS) /* 16K */

static void *guard_p = NULL;
static void *stack_p = NULL;
static void *alloc_p = NULL;

volatile sig_atomic_t err = guard_sound;


// Center the guard page.
guard_err _focus_guard() {
  // Check for strange situations.
  if (stack_p == NULL || alloc_p == NULL) {
    return guard_weird;
  }

  // Unmark the old guard page.
  void *old_guard_p = guard_p;
  if (mprotect(old_guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
    return guard_armor;
  }

  // Place the new guard page in the center.
  if (stack_p > alloc_p) {
    guard_p = stack_p - ((stack_p - alloc_p) / 2);
  }
  else if (stack_p < alloc_p) {
    guard_p = stack_p + ((alloc_p - stack_p) / 2);
  }
  else {
    return guard_weird;
  }
  guard_p = (void *)((uintptr_t)guard_p & ~(GD_PAGESIZE - 1));

  // Mark the new guard page.
  if (guard_p != old_guard_p) {
    if (mprotect(guard_p, GD_PAGESIZE, PROT_NONE) == -1) {
      return guard_armor;
    }
  } else {
    return guard_spent;
  }

  return guard_sound;
}

guard_err _slash_guard(void *si_addr) {
  if (si_addr >= guard_p && si_addr < guard_p + GD_PAGESIZE) {
    return _focus_guard();
  }

  return guard_weird;
}

void _signal_handler(int sig, siginfo_t *si, void *unused) {
  switch (sig) {
    case SIGSEGV:
      fprintf(stderr, "guard: caught sigsegv\r\n");
      err = _slash_guard(si->si_addr);
      break;
    case SIGINT:
      fprintf(stderr, "guard: caught sigint\r\n");
      err = guard_erupt;
      break;
    default:
      break;
  }
}

guard_err _register_handler() {
  struct sigaction sa;

  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _signal_handler;
  sa.sa_mask = 0;

  if (sigaction(SIGSEGV, &sa, NULL) || sigaction(SIGINT, &sa, NULL)) {
    return guard_weird;
  }

  return guard_sound;
}

void guard(void *(*f)(void *), void *arg, void *const *const stack, void *const *const alloc, void **ret) {
  stack_p = *stack;
  alloc_p = *alloc;

  if (_register_handler() != guard_sound) {
    err = guard_weird;
    goto fail;
  }

  fprintf(stderr, "guard: installing guard page\r\n");
  if (guard_p == NULL && (err = _focus_guard()) != guard_sound) {
    goto fail;
  }

  *ret = f(arg);

  if (err != guard_sound) {
    goto fail;
  }

  return;

fail:
  *ret = (void *) &err;
  return;
}
