#include <assert.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "guard.h"

#define GD_PAGE_BITS              14ULL
#define GD_PAGE_SIZE              (1ULL << GD_PAGE_BITS)    // 16 KB
#define GD_PAGE_MASK              (GD_PAGE_SIZE - 1)
#define GD_PAGE_ROUND_DOWN(foo)   (foo & (~GD_PAGE_MASK))

static uintptr_t         guard_p = 0;
static const uintptr_t  *stack_pp = NULL;
static const uintptr_t  *alloc_pp = NULL;
static GD_buflistnode   *buffer_list = NULL;
static struct sigaction  prev_sigsegv_sa;
static struct sigaction  prev_sigbus_sa;

static guard_result
_prot_page(void *address, int prot)
{
  if (mprotect(address, GD_PAGE_SIZE, prot)) {
    fprintf(stderr, "guard: prot: mprotect error %d\r\n", errno);
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_mprotect | errno;
  }

  return 0;
}

static guard_result
_mark_page(void *address)
{
  return _prot_page(address, PROT_NONE);
}

static guard_result
_unmark_page(void *address)
{
  return _prot_page(address, PROT_READ | PROT_WRITE);
}

// Center the guard page.
// XX: could be a false positive if the new frame results in exact same guard page
//     solution: we only re-center from the signal handler
static guard_result
_focus_guard()
{
  uintptr_t stack_p = *stack_pp;
  uintptr_t alloc_p = *alloc_pp;
  uintptr_t old_guard_p = guard_p;
  uintptr_t new_guard_p;
  guard_result   err = 0;

  // fprintf(stderr, "guard: focus: stack pointer at %p\r\n", (void *)stack_p);
  // fprintf(stderr, "guard: focus: alloc pointer at %p\r\n", (void *)alloc_p);

  if (stack_p == 0 || alloc_p == 0) {
    fprintf(stderr, "guard: focus: stack or alloc pointer is null\r\n");
    return guard_null;
  } else if (stack_p == alloc_p) {
    // fprintf(stderr, "guard: focus: stack and alloc pointers equal\r\n");
    return guard_oom;
  }

  // fprintf(stderr, "guard: focus: old guard = %p\r\n", (void *)old_guard_p);

  // Compute new guard page
  // XX:  Should we also check for new_guard_p < min(stack_p, alloc_p)?
  new_guard_p = GD_PAGE_ROUND_DOWN((stack_p + alloc_p) / 2);
  // fprintf(stderr, "guard: focus: new guard = %p\r\n", (void *)new_guard_p);
  if (new_guard_p == old_guard_p) {
    // fprintf(stderr, "guard: focus: OOM\r\n");
    return guard_oom;
  }

  // Mark new guard page
  if ((err = _mark_page((void *)new_guard_p))) {
    fprintf(stderr, "guard: focus: mark error\r\n");
    return err;
  }

  // Update guard page tracker
  // fprintf(stderr, "guard: focus: installed guard at %p\r\n", (void *)new_guard_p);
  guard_p = new_guard_p;

  // Unmark the old guard page (if there is one)
  if (old_guard_p) {
    if ((err = _unmark_page((void *)old_guard_p))) {
      fprintf(stderr, "guard: focus: unmark error\r\n");
      return err;
    }
  }

  return 0;
}

static void
_signal_handler(int sig, siginfo_t *si, void *unused)
{
  uintptr_t sig_addr;
  guard_result   err = 0;

  assert(guard_p);

  // fprintf(stderr, "guard: handler: %d received\r\n", sig);

  if (sig != SIGSEGV && sig != SIGBUS) {
    fprintf(stderr, "guard: handler: invalid signal: %d\r\n", sig);
    assert(0);
  }

  sig_addr = (uintptr_t)si->si_addr;
  // fprintf(stderr, "guard: SIGSEGV address = %p\r\n", (void *)sig_addr);

  if (sig_addr >= guard_p &&
      sig_addr <  guard_p + GD_PAGE_SIZE)
  {
    // fprintf(stderr, "guard: hit: %p\r\n", si->si_addr);
    err = _focus_guard();
    if (err) {
      // fprintf(stderr, "guard: handler: focus error\r\n");
      siglongjmp(buffer_list->buffer, err);
    }
  }
  else {
    // fprintf(stderr, "guard: page at %p miss\r\n", (void *)guard_p);
    switch (sig) {
      case SIGSEGV: {
        if (prev_sigsegv_sa.sa_sigaction != NULL) {
          prev_sigsegv_sa.sa_sigaction(sig, si, unused);
        } else if (prev_sigsegv_sa.sa_handler != NULL) {
          prev_sigsegv_sa.sa_handler(sig);
        } else {
          assert(0);
        }
        break;
      }
      case SIGBUS: {
        if (prev_sigbus_sa.sa_sigaction != NULL) {
          prev_sigbus_sa.sa_sigaction(sig, si, unused);
        } else if (prev_sigbus_sa.sa_handler != NULL) {
          prev_sigbus_sa.sa_handler(sig);
        } else {
          assert(0);
        }
      }
    }
  }
}

static guard_result
_register_handler()
{
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _signal_handler;

  // XX should assert that prev_sigsegv_sa is uninitialized, but it's non-trivial
  if (sigaction(SIGSEGV, &sa, &prev_sigsegv_sa)) {
    fprintf(stderr, "guard: register: sigaction error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_sigaction | errno;
  }

  if (sigaction(SIGBUS, &sa, &prev_sigbus_sa)) {
    fprintf(stderr, "guard: register: sigaction error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_sigaction | errno;
  }

  return 0;
}

guard_result
guard(
  void *(*f)(void *),
  void *closure,
  const uintptr_t *const s_pp,
  const uintptr_t *const a_pp,
  void ** ret
) {
  GD_buflistnode  *new_buffer;
  guard_result err = 0;
  guard_result td_err = 0;

  // fprintf(stderr, "guard: stack pointer at %p\r\n", (void *)(*s_pp));
  // fprintf(stderr, "guard: alloc pointer at %p\r\n", (void *)(*a_pp));

  if (guard_p == 0) {
    assert(buffer_list == NULL);

    stack_pp = s_pp;
    alloc_pp = a_pp;

    // Initialize the guard page
    if ((err = _focus_guard())) {
      fprintf(stderr, "guard: initial focus error\r\n");
      goto exit;
    }

    // Register guard page signal handler
    if ((err = _register_handler())) {
      fprintf(stderr, "guard: registration error\r\n");
      goto clean;
    }
  } else {
    assert(buffer_list != NULL);
  }

  // Setup new longjmp buffer
  new_buffer = (GD_buflistnode *)malloc(sizeof(GD_buflistnode));
  if (new_buffer == NULL) {
    fprintf(stderr, "guard: malloc error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    err = guard_malloc | errno;
    goto skip;
  }
  new_buffer->next = buffer_list;
  buffer_list = new_buffer;

  // Run given closure
  // fprintf(stderr, "guard: run\r\n");
  if (!(err = sigsetjmp(buffer_list->buffer, 1))) {
    *ret = f(closure);
  }

  // Restore previous longjmp buffer
  buffer_list = buffer_list->next;
  free((void *)new_buffer);

skip:
  // If no more guarded closures, then...
  if (buffer_list == NULL) {
    // Remove new signal handlers
    if (sigaction(SIGSEGV, &prev_sigsegv_sa, NULL)) {
      fprintf(stderr, "guard: sigaction error\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      td_err = guard_sigaction | errno;

      if (!err) {
        err = td_err;
      }
    }
    if (sigaction(SIGBUS, &prev_sigbus_sa, NULL)) {
      fprintf(stderr, "guard: sigaction error\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      td_err = guard_sigaction | errno;

      if (!err) {
        err = td_err;
      }
    }

clean:
    // Unmark guard page
    assert(guard_p != 0);
    td_err = _unmark_page((void *)guard_p);
    if (td_err) {
      fprintf(stderr, "guard: unmark error\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      if (!err) {
        err = td_err;
      }
    }
    guard_p = 0;
  }

exit:
  // fprintf(stderr, "guard: return\r\n");
  return err;
}
