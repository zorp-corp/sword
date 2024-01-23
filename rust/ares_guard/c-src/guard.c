#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "guard.h"


#define GD_PAGEBITS 14ULL
#define GD_PAGESIZE (1ULL << GD_PAGEBITS) /* 16K */

static uint64_t *guard_p = 0;
static uint64_t **stack = 0;
static uint64_t **alloc = 0;

volatile sig_atomic_t err = guard_sound;


// Center the guard page.
guard_err _focus_guard() {
  uint64_t *stack_p = *stack;
  uint64_t *alloc_p = *alloc;

  // Check for strange situations.
  if (stack_p == 0 || alloc_p == 0) {
    fprintf(stderr, "guard: stack or alloc pointer is null\r\n");
    return guard_weird;
  }

  // Unmark the old guard page.
  void *old_guard_p = guard_p;
  if (old_guard_p != 0 && mprotect(old_guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
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
  // fprintf(stderr, "guard: slash at %p\r\n", si_addr);
  // fprintf(stderr, "guard: guard at %p\r\n", (void *) guard_p);

  if (si_addr >= (void *)guard_p && si_addr < (void *)guard_p + GD_PAGESIZE) {
    return _focus_guard();
  }

  return guard_weird;
}

void _signal_handler(int sig, siginfo_t *si, void *unused) {
  switch (sig) {
    case SIGSEGV:
      err = _slash_guard(si->si_addr);
      break;
    case SIGINT:
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

  if (sigaction(SIGSEGV, &sa, 0) || sigaction(SIGINT, &sa, 0)) {
    return guard_weird;
  }

  return guard_sound;
}

void guard(void *(*f)(void *), void *arg, void *const *stack_pp, void *const *alloc_pp, void *ret) {
  stack = (uint64_t**) stack_pp;
  alloc = (uint64_t**) alloc_pp;

  fprintf(stderr, "guard: stack at %p\r\n", (void *) stack);
  fprintf(stderr, "guard: alloc at %p\r\n", (void *) alloc);

  fprintf(stderr, "guard: stack pointer at %p\r\n", (void *) *stack);
  fprintf(stderr, "guard: alloc pointer at %p\r\n", (void *) *alloc);

  if (guard_p == 0) {
    fprintf(stderr, "guard: installing guard page\r\n");
    guard_err install_err = _focus_guard();
    if (install_err != guard_sound) {
      fprintf(stderr, "guard: failed to install guard page\r\n");
      err = install_err;
      goto fail;
    }
  }
  fprintf(stderr, "guard: guard page installed at %p\r\n", (void *) guard_p);

  if (_register_handler() != guard_sound) {
    err = guard_weird;
    goto fail;
  }

  pthread_t thread;
  int thread_err;
  thread_err = pthread_create(&thread, NULL, f, arg);
  if (thread_err != 0) {
    err = guard_weird;
    goto fail;
  }

  while (err == guard_sound) {
    if (pthread_kill(thread, 0) != 0) {
      goto fail;
    }
    sleep(1);
  }

  void *thread_result;
  pthread_join(thread, &thread_result);
  *(void **)ret = thread_result;

  if (err != guard_sound) {
    goto fail;
  }

  if (mprotect(guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
    err = guard_armor;
    goto fail;
  }

  return;

fail:
  if (mprotect(guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
    fprintf(stderr, "guard: failed to remove guard page\r\n");
  }
  fprintf(stderr, "guard: error %d\r\n", err);
  *(void **)ret = (void *) &err;
  return;
}
