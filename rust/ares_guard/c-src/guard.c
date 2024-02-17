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

/**
 *  XX: documentation
 */
typedef struct _gs {
  uintptr_t         guard_p;
  const uintptr_t  *stack_pp;
  const uintptr_t  *alloc_pp;
  jmp_buf           env_buffer;
  struct sigaction  prev_sa;
} GuardState;

static GuardState *_guard_state = NULL;


static int32_t
_prot_page(void *address, int prot)
{
  if (mprotect(address, GD_PAGE_SIZE, prot)) {
    fprintf(stderr, "guard: prot: mprotect error %d\r\n", errno);
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_mprotect | errno;
  }

  return 0;
}

static int32_t
_mark_page(void *address)
{
  return _prot_page(address, PROT_NONE);
}

static int32_t
_unmark_page(void *address)
{
  return _prot_page(address, PROT_READ | PROT_WRITE);
}

/**
 * Center the guard page.
 */
static int32_t
_focus_guard(
  uintptr_t        *guard_pp,
  const uintptr_t   stack_p,
  const uintptr_t   alloc_p
) {
  // Check anomalous arguments
  if (stack_p == 0 || alloc_p == 0) {
    fprintf(stderr, "guard: focus: stack or alloc pointer is null\r\n");
    return guard_null;
  } else if (stack_p == alloc_p) {
    return guard_oom;
  }

  uintptr_t old_guard_p = *guard_pp;
  uintptr_t new_guard_p;
  int32_t   err = 0;

  // Unmark the old guard page (if there is one)
  if (old_guard_p) {
    if ((err = _unmark_page((void *)old_guard_p))) {
      fprintf(stderr, "guard: focus: unmark error, %p\r\n", (void *)old_guard_p);
      return err;
    }
  }

  // Compute new guard page
  // XX:  Should we also check for new_guard_p < min(stack_p, alloc_p)?
  new_guard_p = GD_PAGE_ROUND_DOWN((stack_p + alloc_p) / 2);
  if (new_guard_p == old_guard_p) {
    return guard_oom;
  }

  // Mark new guard page
  if ((err = _mark_page((void *)new_guard_p))) {
    fprintf(stderr, "guard: focus: mark error %p\r\n", (void *)new_guard_p);
    return err;
  }

  // Update guard page tracker
  *guard_pp = new_guard_p;
  
  return 0;
}

void
_signal_handler(int sig, siginfo_t *si, void *unused)
{
  uintptr_t sig_addr;
  int32_t   err = 0;

  assert(sig == SIGSEGV);

  sig_addr = (uintptr_t)si->si_addr;
  if (
    sig_addr >= _guard_state->guard_p && 
    sig_addr <  (_guard_state->guard_p + GD_PAGE_SIZE))
  {
    err = _focus_guard(
      &(_guard_state->guard_p),
      *(_guard_state->stack_pp),
      *(_guard_state->alloc_pp));
    if (err) {
      siglongjmp(_guard_state->env_buffer, err);
    }
  } else {
    struct sigaction prev_sa = _guard_state->prev_sa;

    if (prev_sa.sa_sigaction != NULL) {
      prev_sa.sa_sigaction(sig, si, unused);
    } else if (prev_sa.sa_handler != NULL) {
      prev_sa.sa_handler(sig);
    } else {
      // There should always be a default SIGSEGV handler
      assert(0);
    }
  }
}

int32_t
_register_handler(struct sigaction *prev_sa)
{
  struct sigaction sa;

  // Flag to use sa_sigaction
  sa.sa_flags = SA_SIGINFO;
  // Must use sa_sigaction; sa-handler takes signal handler as its only argument
  sa.sa_sigaction = _signal_handler;
  // Set mask of signals to ignore while running signal handler
  // XX:  By default the signal that triggered the signal handler is automatically added to the
  //      mask while it's being handled, so unless we plan to add more signals to this then I don't
  //      think it's necessary.
  // sigemptyset(&sa.sa_mask);
  // sigaddset(&(sa.sa_mask), SIGSEGV);

  // Set the new SIGSEGV handler, and save the old SIGSEGV handler (if any)
  if (sigaction(SIGSEGV, &sa, prev_sa)) {
    fprintf(stderr, "guard: register: sigaction error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_sigaction | errno;
  }

  return 0;
}

int32_t
_setup(
  GuardState **gs_p,
  const uintptr_t *const stack_pp,
  const uintptr_t *const alloc_pp
) {
  GuardState *gs;
  int32_t     err = 0;

  assert(*gs_p == NULL);

  // Setup guard page state
  *gs_p = (GuardState *)malloc(sizeof(GuardState));
  gs = *gs_p;
  if (gs == NULL) {
    fprintf(stderr, "guard: malloc error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_malloc | errno;
  }

  gs->guard_p = 0;
  gs->stack_pp = stack_pp;
  gs->alloc_pp = alloc_pp;

  // Initialize the guard page
  if ((err = _focus_guard(&(gs->guard_p), *stack_pp, *alloc_pp))) {
    return err;
  }

  // Register guard page signal handler
  if ((err = _register_handler(&(gs->prev_sa)))) {
    return err;
  }

  return 0;
}

int32_t
_teardown(GuardState** gs_p)
{
  int32_t err = 0;

  if (*gs_p != NULL) {
    GuardState *gs = *gs_p;

    if (gs->guard_p != 0) {
      err = _unmark_page((void *)gs->guard_p);
    }

    if (sigaction(SIGSEGV, &(gs->prev_sa), NULL)) {
      fprintf(stderr, "guard: teardown: sigaction error\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      err = guard_sigaction | errno;
    }

    free(gs);
    *gs_p = NULL;
  }

  return err;
}

int32_t
guard(
  callback f,
  void *closure,
  const uintptr_t *const stack_pp,
  const uintptr_t *const alloc_pp,
  void ** ret
) {
  int32_t err = 0;

  // Setup the guard page
  if ((err = _setup(&_guard_state, stack_pp, alloc_pp))) {
    goto done;
  }

  // Run given closure
  if (!(err = sigsetjmp(_guard_state->env_buffer, 1))) {
    *ret = f(closure);

    // Clean up
    err = _teardown(&_guard_state);

    return err;
  } else {
done:
    // Clean up
    _teardown(&_guard_state);

    return err;
  }
}
