#include <setjmp.h>
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
static jmp_buf env_buffer;

volatile sig_atomic_t err = guard_sound;

// Center the guard page.
guard_err _focus_guard()
{
  uint64_t *stack_p = *stack;
  uint64_t *alloc_p = *alloc;

  // Check for strange situations.
  if (stack_p == 0 || alloc_p == 0) {
    // fprintf(stderr, "guard: stack or alloc pointer is null\r\n");
    return guard_weird;
  }

  // Unmark the old guard page.
  void *old_guard_p = guard_p;
  if (old_guard_p != 0
      && mprotect(old_guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
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
    // fprintf(stderr, "guard: weird; stack and alloc pointers are equal\r\n");
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

  // fprintf(stderr, "guard: installed guard page at %p\r\n", (void *) guard_p);

  return guard_sound;
}

guard_err _slash_guard(void *si_addr) {
  if (si_addr >= (void *)guard_p && si_addr < (void *)guard_p + GD_PAGESIZE) {
    // fprintf(stderr, "guard: slash in guard\r\n");
    return _focus_guard();
  }

  return guard_weird;
}

void _signal_handler(int sig, siginfo_t *si, void *unused)
{
  switch (sig) {
    case SIGSEGV:
      // fprintf(stderr, "guard: sigsegv at %p\r\n", si->si_addr);
      err = _slash_guard(si->si_addr);
      break;
    case SIGINT:
      // fprintf(stderr, "guard: sigint\r\n");
      err = guard_erupt;
      break;
    default:
      break;
  }

  if (err != guard_sound) {
    // fprintf(stderr, "guard: error %d; long jumping\r\n", err);
    longjmp(env_buffer, 1);
  }
}

guard_err _register_handler() {
  struct sigaction sa;

  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _signal_handler;
  sa.sa_mask = 0;

  // if (sigaction(SIGSEGV, &sa, 0) || sigaction(SIGINT, &sa, 0)) {
  if (sigaction(SIGSEGV, &sa, 0)) {
    return guard_weird;
  }

  // fprintf(stderr, "guard: registered handler\r\n");
  return guard_sound;
}

guard_err guard(
  void *(*f)(void *),
  void *user_data,
  void *const *stack_pp,
  void *const *alloc_pp,
  void *const *ret
)
{
  stack = (uint64_t**) stack_pp;
  alloc = (uint64_t**) alloc_pp;

  // fprintf(stderr, "guard: stack pointer at %p\r\n", (void *) *stack);
  // fprintf(stderr, "guard: alloc pointer at %p\r\n", (void *) *alloc);

  if (guard_p == 0) {
    guard_err install_err = _focus_guard();
    if (install_err != guard_sound) {
      // fprintf(stderr, "guard: failed to install guard page\r\n");
      err = install_err;
      goto fail;
    }
  }

  if (_register_handler() != guard_sound) {
    err = guard_weird;
    goto fail;
  }

  void *result;
  if (setjmp(env_buffer) == 0) {
    result = f(user_data);
  }
  else {
    if (err != guard_sound) {
      goto fail;
    }
  }

  *(void **)ret = result;

  // if (mprotect(guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
  //   err = guard_armor;
  //   goto fail;
  // }
  // fprintf(stderr, "guard: sound; uninstalled guard page\r\n");

  return guard_sound;

fail:
  if (mprotect(guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
    // fprintf(stderr, "guard: failed to uninstall guard page\r\n");
  }
  // fprintf(stderr, "guard: fail; uninstalled guard page\r\n");
  switch (err) {
    case guard_armor:
      // fprintf(stderr, "guard: armor error\r\n");
      break;
    case guard_weird:
      // fprintf(stderr, "guard: weird error\r\n");
      break;
    case guard_spent:
      // fprintf(stderr, "guard: spent error\r\n");
      break;
    case guard_erupt:
      // fprintf(stderr, "guard: erupt error\r\n");
      break;
  }
  return err;
}
