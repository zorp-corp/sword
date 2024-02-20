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

#ifdef __APPLE__
  #define GD_SIGNAL   SIGBUS
#else
  #define GD_SIGNAL   SIGSEGV
#endif

typedef struct _GD_state GD_state;
struct _GD_state {
  uintptr_t         guard_p;
  const uintptr_t  *stack_pp;
  const uintptr_t  *alloc_pp;
  GD_buflistnode   *buffer_list;
  struct sigaction  prev_sa;
};

static GD_state *_guard_state = NULL;


static guard_err
_protect_page(void *address, int prot)
{
  if (mprotect(address, GD_PAGE_SIZE, prot)) {
    fprintf(stderr, "guard: prot: mprotect error %d\r\n", errno);
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_mprotect;
  }

  return 0;
}

// Center the guard page.
static guard_err
_focus_guard(GD_state *gs) {
  uintptr_t stack_p = *(gs->stack_pp);
  uintptr_t alloc_p = *(gs->alloc_pp);
  uintptr_t old_guard_p = _guard_state->guard_p;
  uintptr_t new_guard_p;
  guard_err err = 0;

  // Check anomalous arguments.
  if (stack_p == 0 || alloc_p == 0) {
    fprintf(stderr, "guard: focus: stack or alloc pointer is null\r\n");
    return guard_null;
  } else if (stack_p == alloc_p) {
    return guard_oom;
  }

  // Compute new guard page.
  new_guard_p = GD_PAGE_ROUND_DOWN((stack_p + alloc_p) / 2);
  if (new_guard_p == old_guard_p) {
    return guard_oom;
  }

  // Mark new guard page.
  if ((err = _protect_page((void *)new_guard_p, PROT_NONE))) {
    fprintf(stderr, "guard: focus: mark error\r\n");
    return err;
  }

  // Update guard page tracker.
  gs->guard_p = new_guard_p;

  // Unmark the old guard page if there is one.
  if (old_guard_p) {
    if ((err = _protect_page((void *)old_guard_p, PROT_READ | PROT_WRITE))) {
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
  guard_err err = 0;

  assert(_guard_state->guard_p);

  if (sig != GD_SIGNAL) {
    fprintf(stderr, "guard: handler: invalid signal: %d\r\n", sig);
    assert(0);
  }
  assert(sig == GD_SIGNAL);

  sig_addr = (uintptr_t)si->si_addr;

  if (sig_addr >= _guard_state->guard_p &&
      sig_addr <  _guard_state->guard_p + GD_PAGE_SIZE)
  {
    err = _focus_guard(_guard_state);
    if (err) {
      siglongjmp(_guard_state->buffer_list->buffer, err);
    }
  }
  else {
    if (_guard_state->prev_sa.sa_sigaction != NULL) {
      _guard_state->prev_sa.sa_sigaction(sig, si, unused);
    } else if (_guard_state->prev_sa.sa_handler != NULL) {
      _guard_state->prev_sa.sa_handler(sig);
    } else {
      assert(0);
    }
  }
}

// Registers the handler function for GD_SIGNAL.
static guard_err
_register_handler(GD_state *gs)
{
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _signal_handler;

  // Set the new handler and save the old if it exists.
  if (sigaction(GD_SIGNAL, &sa, &(gs->prev_sa))) {
    fprintf(stderr, "guard: register: sigaction error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_sigaction;
  }

  return 0;
}

guard_err
guard(
  void *(*f)(void *),
  void *closure,
  const uintptr_t *const s_pp,
  const uintptr_t *const a_pp,
  void **ret
) {
  GD_buflistnode  *new_buffer;
  guard_err err = 0;
  guard_err td_err = 0;

  // Setup guard state.
  if (_guard_state == NULL) {
    _guard_state = (GD_state *)malloc(sizeof(GD_state));
    if (_guard_state == NULL) {
      fprintf(stderr, "guard: malloc error\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      err = guard_malloc;
      goto skip;
    }
    _guard_state->guard_p = 0;
    _guard_state->stack_pp = s_pp;
    _guard_state->alloc_pp = a_pp;
  }
  
  if (_guard_state->guard_p == 0) {
    assert(_guard_state->buffer_list == NULL);

    // Initialize the guard page.
    if ((err = _focus_guard(_guard_state))) {
      fprintf(stderr, "guard: initial focus error\r\n");
      goto exit;
    }

    // Register guard page signal handler.
    if ((err = _register_handler(_guard_state))) {
      fprintf(stderr, "guard: registration error\r\n");
      goto clean;
    }
  } else {
    assert(_guard_state->buffer_list != NULL);
  }

  // Setup new longjmp buffer.
  new_buffer = (GD_buflistnode *)malloc(sizeof(GD_buflistnode));
  if (new_buffer == NULL) {
    fprintf(stderr, "guard: malloc error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    err = guard_malloc;
    goto skip;
  }
  new_buffer->next = _guard_state->buffer_list;
  _guard_state->buffer_list = new_buffer;

  // Run given closure.
  if (!(err = sigsetjmp(_guard_state->buffer_list->buffer, 1))) {
    *ret = f(closure);
  }

  // Restore previous longjmp buffer.
  _guard_state->buffer_list = _guard_state->buffer_list->next;
  free((void *)new_buffer);

skip:
  if (_guard_state->buffer_list == NULL) {
    if (sigaction(GD_SIGNAL, &_guard_state->prev_sa, NULL)) {
      fprintf(stderr, "guard: error replacing previous handler\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      td_err = guard_sigaction;

      if (!err) {
        err = td_err;
      }
    }

clean:
    // Unmark guard page.
    assert(_guard_state->guard_p != 0);
    td_err = _protect_page((void *)_guard_state->guard_p, PROT_READ | PROT_WRITE);
    if (td_err) {
      fprintf(stderr, "guard: unmark error\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      if (!err) {
        err = td_err;
      }
    }
    _guard_state->guard_p = 0;
    // free(_guard_state);
  }

exit:
  return err;
}
