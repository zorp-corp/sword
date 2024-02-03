#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

#include "guard.h"


#define GD_PAGEBITS 14ULL
#define GD_PAGESIZE (1ULL << GD_PAGEBITS) /* 16K */

static uint64_t *guard_p = 0;
static jmp_buf env_buffer;
volatile sig_atomic_t err = guard_sound;
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

  // Check if we're spent already.
  if (low_p == high_p || low_p > high_p) {
    return guard_spent;
  }

  // Check for strange situations.
  if (low_p == 0 || high_p == 0) {
    fprintf(stderr, "guard: low or high bound pointer is null\r\n");
    return guard_weird;
  }

  // Unmark the old guard page.
  void *old_guard_p = guard_p;
  if (old_guard_p != 0
      && mprotect(old_guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
    return guard_armor;
  }

  // Place the new guard page in the low-aligned center.
  guard_p = (uint64_t *)low_p + ((high_p - low_p) / 2);
  guard_p = (uint64_t *)((uintptr_t)guard_p & ~(GD_PAGESIZE - 1));

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

static void
_signal_handler(int sig, siginfo_t *si, void *unused)
{
  switch (sig) {
    case SIGSEGV:
      if (si->si_addr >= (void *)guard_p && 
          si->si_addr < (void *)guard_p + GD_PAGESIZE)
      {
        fprintf(stderr, "guard: fault in guard: %p\r\n", si->si_addr);
        err = _focus_guard();
        break;
      }
      else {
        fprintf(stderr, "guard: fault outside guard %p\r\n", si->si_addr);
        if (NULL != prev_sigsegv_handler) {
          prev_sigsegv_handler(sig, si, unused);
          break;
        }
        else { 
          err = guard_weird;
        }
      }
      break;
    case SIGINT:
      fprintf(stderr, "guard: sigint\r\n");
      err = guard_erupt;
      break;
    default:
      break;
  }

  if (err != guard_sound) {
    fprintf(stderr, "guard: error %d; long jumping\r\n", err);
    siglongjmp(env_buffer, 1);
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

  // fprintf(stderr, "guard: registered handler\r\n");
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
  // Set globals.
  low = low_f;
  high = high_f;
  bounds= bounds_data;
  context = context_p;

  const uint64_t *low_p = low_f(bounds_data, context_p);
  const uint64_t *high_p = high_f(bounds_data, context_p);

  // fprintf(stderr, "guard: low: %p high: %p\r\n", (void *)low_p, (void *)high_p);

  const unsigned long free_mb = (unsigned long)(high_p - low_p) / 1024 / 1024;
  // fprintf(stderr, "guard: free space: %lu MB\r\n", free_mb);

  guard_err focus_err = _focus_guard();
  if (focus_err != guard_sound && focus_err != guard_spent) {
    fprintf(stderr, "guard: failed to install guard page\r\n");
    err = focus_err;
    goto fail;
  }

  if (_register_handler() != guard_sound) {
    err = guard_weird;
    goto fail;
  }

  void *result;
  if (sigsetjmp(env_buffer, 1) == 0) {
    result = work_f(work_data, context_p);
  }
  else {
    if (err != guard_sound) {
      goto fail;
    }
  }

  *(void **)ret = result;

  if (mprotect(guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1) {
    err = guard_armor;
    goto fail;
  }

  return guard_sound;

fail:
  if (guard_p != NULL && 
      mprotect(guard_p, GD_PAGESIZE, PROT_READ | PROT_WRITE) == -1)
  {
    fprintf(stderr, "guard: failed to uninstall guard page\r\n");
  }
  switch (err) {
    case guard_armor:
      fprintf(stderr, "guard: armor error\r\n");
      break;
    case guard_weird:
      fprintf(stderr, "guard: weird error\r\n");
      break;
    case guard_spent:
      fprintf(stderr, "guard: spent error\r\n");
      break;
    case guard_erupt:
      fprintf(stderr, "guard: erupt error\r\n");
      break;
  }
  return err;
}
