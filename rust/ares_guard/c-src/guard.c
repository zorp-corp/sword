#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#include "guard.h"


#define GD_PAGEBITS 14ULL
#define GD_PAGESIZE (1ULL << GD_PAGEBITS) /* 16K */

static uint64_t *guard_p = 0;
static jmp_buf env_buffer;
static void (*prev_sigsegv_handler)(int, siginfo_t *, void *);
static const uint64_t *(*low)(void *, void *) = 0;
static const uint64_t *(*high)(void *, void *) = 0;
static void *bounds = 0;
static void *context = 0;

// Center the guard page.
static guard_err
_focus_guard()
{
  const uint64_t *low_p = low(bounds, context);
  const uint64_t *high_p = high(bounds, context);

  if (low_p >= high_p ) {
    return guard_spent;
  }

  if (low_p == 0 || high_p == 0) {
    fprintf(stderr, "guard: low or high bound pointer is null\r\n");
    return guard_weird;
  }

  void *old_guard_p = guard_p;

  guard_p = (uint64_t *)low_p + ((high_p - low_p) / 2);
  guard_p = (uint64_t *)((uintptr_t)guard_p & ~(GD_PAGESIZE - 1));

  const bool same = old_guard_p == guard_p;
  const bool left = (high_p - low_p) > GD_PAGESIZE;
  if (same && !left) {
    fprintf(stderr, "guard: spent: %p; left: %u\r\n", guard_p, left);
    return guard_spent;
  }
  else {
    if (mprotect(guard_p, GD_PAGESIZE, PROT_NONE) == -1) {
      return guard_armor;
    }
  }

  if (old_guard_p != NULL && 
      mprotect(old_guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1)
  {
    return guard_armor;
  }

  return guard_sound;
}

static void
_signal_handler(int sig, siginfo_t *si, void *unused)
{
  if (sig != SIGSEGV) {
    fprintf(stderr, "guard: weird signal: %d\r\n", sig);
    return;
  }

  if (guard_p == NULL) {
    fprintf(stderr, "guard: no guard page\r\n");
    return;
  }

  if (si == NULL) {
    fprintf(stderr, "guard: no signal info\r\n");
    return;
  }

  if (si->si_addr >= (void *)guard_p && 
      si->si_addr <  (void *)guard_p + GD_PAGESIZE)
  {
    fprintf(stderr, "guard: hit: %p\r\n", si->si_addr);
    guard_err err = _focus_guard();
    if (err != guard_sound) {
      fprintf(stderr, "guard: jump\r\n");
      siglongjmp(env_buffer, err);
    }
  }
  else {
    fprintf(stderr, "guard: weird hit: %p\r\n", si->si_addr);
    return;
  }
}

static guard_err
_register_handler()
{
  struct sigaction sa;
  struct sigaction prev_sa;
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _signal_handler;
  sigemptyset(&sa.sa_mask);
  sigaddset(&(sa.sa_mask), SIGSEGV);

  if (sigaction(SIGSEGV, &sa, &prev_sa)) {
    fprintf(stderr, "guard: failed to register handler\r\n");
    return guard_weird;
  }
  prev_sigsegv_handler = prev_sa.sa_sigaction;

  return guard_sound;
}

guard_err
guard(
  void *(*work_f)(void *, void *),
  void *work_data,
  const uint64_t *(*low_f)(void *, void *),
  const uint64_t *(*high_f)(void *, void *),
  void *bounds_data,
  void *context_p,
  void *const *ret
)
{
  guard_err err;

  low = low_f;
  high = high_f;
  bounds= bounds_data;
  context = context_p;

  err = _focus_guard();
  if (guard_p == NULL && err != guard_sound && err != guard_spent) {
    fprintf(stderr, "guard: failed to install\r\n");
    goto fail;
  }

  if ((err = _register_handler()) != guard_sound) {
    fprintf(stderr, "guard: failed to register handler\r\n");
    goto fail;
  }

  err = sigsetjmp(env_buffer, 1);
  if (err == guard_start) {
    *(void **)ret = work_f(work_data, context_p);
    return guard_sound;
  }
  else {
    goto fail; 
  }

fail:
  // Restore the previous signal handler.
  {
    struct sigaction sa;
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = prev_sigsegv_handler;
    sigemptyset(&sa.sa_mask);
    sigaddset(&(sa.sa_mask), SIGSEGV);
    sigaction(SIGSEGV, &sa, NULL);
  }
  
  fprintf(stderr, "guard: fail: %d\r\n", err);
  return err;
}
