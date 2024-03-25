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

/**
 * Linked list stack of jump buffers.
 */
typedef struct GD_buflistnode GD_buflistnode;
struct GD_buflistnode {
  jmp_buf         buffer;
  GD_buflistnode *next;
};

/**
 * Global guard page state.
 */
typedef struct GD_state GD_state;
struct GD_state {
  uintptr_t         guard_p;      // address of guard page
  uintptr_t         start_p;      // address of beginning of memory arena
  uintptr_t         end_p;        // address of end of memory arena
  const uintptr_t  *stack_pp;     // ptr to stack ptr
  const uintptr_t  *alloc_pp;     // ptr to alloc ptr
  GD_buflistnode   *buffer_list;  // linked list of longjmp buffers
  struct sigaction  prev_sa;      // original signal handler
};

static GD_state _gd_state = { 
  .guard_p      = 0,
  .start_p      = 0,
  .end_p        = 0,
  .stack_pp     = NULL,
  .alloc_pp     = NULL,
  .buffer_list  = NULL,
  .prev_sa      = { .sa_sigaction = NULL, .sa_flags = 0 },
};

static uint32_t
_protect_page(void *address, int prot)
{
  if (mprotect(address, GD_PAGE_SIZE, prot)) {
    fprintf(stderr, "guard: prot: mprotect error %d\r\n", errno);
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_mprotect ;
  }

  return 0;
}

// Center the guard page.
static uint32_t
_focus_guard(GD_state *gd)
{
  uintptr_t stack_p = *(gd->stack_pp);
  uintptr_t alloc_p = *(gd->alloc_pp);
  uintptr_t old_guard_p = (gd->guard_p);
  uintptr_t new_guard_p;
  uint32_t  err = 0;

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
  gd->guard_p = new_guard_p;

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
  uint32_t err = 0;

  assert(_gd_state.guard_p);
  if (sig != GD_SIGNAL) {
    fprintf(stderr, "guard: handler: invalid signal: %d\r\n", sig);
    assert(0);
  }

  sig_addr = (uintptr_t)si->si_addr;

  if (sig_addr >= _gd_state.guard_p &&
      sig_addr <  _gd_state.guard_p + GD_PAGE_SIZE)
  {
    err = _focus_guard(&_gd_state);
    if (err) {
      siglongjmp(_gd_state.buffer_list->buffer, err);
    }
  }
  else {
    struct sigaction prev_sa = _gd_state.prev_sa;

    if (prev_sa.sa_sigaction != NULL) {
      prev_sa.sa_sigaction(sig, si, unused);
    } else if (prev_sa.sa_handler != NULL) {
      prev_sa.sa_handler(sig);
    } else {
      // There should always be a default handler
      assert(0);
    }
  }
}

// Registers the handler function.
static uint32_t
_register_handler(GD_state *gd)
{
  struct sigaction sa;

  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = _signal_handler;

  if (sigaction(GD_SIGNAL, &sa, &(gd->prev_sa))) {
    fprintf(stderr, "guard: register: sigaction error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    return guard_sigaction;
  }

  return 0;
}

void
init(
  const uintptr_t         start_p,
  const uintptr_t         end_p,
  const uintptr_t *const  stack_pp,
  const uintptr_t *const  alloc_pp
) {
  _gd_state.start_p = start_p;
  _gd_state.end_p = end_p;
  _gd_state.stack_pp = stack_pp;
  _gd_state.alloc_pp = alloc_pp;
}

uint32_t
guard(
  void *(*f)(void *),
  void *closure,
  void **ret
) {
  GD_buflistnode *new_buffer;
  uint32_t        err = 0;
  uint32_t        td_err = 0;

  if (_gd_state.guard_p == 0) {
    assert(_gd_state.buffer_list == NULL);

    // Initialize the guard page.
    if ((err = _focus_guard(&_gd_state))) {
      fprintf(stderr, "guard: initial focus error\r\n");
      goto exit;
    }

    // Register guard page signal handler.
    if ((err = _register_handler(&_gd_state))) {
      fprintf(stderr, "guard: registration error\r\n");
      goto tidy;
    }
  } else {
    assert(_gd_state.buffer_list != NULL);
  }

  // Setup new longjmp buffer.
  new_buffer = (GD_buflistnode *)malloc(sizeof(GD_buflistnode));
  if (new_buffer == NULL) {
    fprintf(stderr, "guard: malloc error\r\n");
    fprintf(stderr, "%s\r\n", strerror(errno));
    err = guard_malloc;
    goto skip;
  }
  new_buffer->next = _gd_state.buffer_list;
  _gd_state.buffer_list = new_buffer;

  // Run given closure.
  if (!(err = sigsetjmp(_gd_state.buffer_list->buffer, 1))) {
    *ret = f(closure);
  }

  // Restore previous longjmp buffer.
  _gd_state.buffer_list = _gd_state.buffer_list->next;
  free((void *)new_buffer);

skip:
  if (_gd_state.buffer_list == NULL) {
    if (sigaction(GD_SIGNAL, &_gd_state.prev_sa, NULL)) {
      fprintf(stderr, "guard: error replacing sigsegv handler\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      td_err = guard_sigaction;

      if (!err) {
        err = td_err;
      }
    }

tidy:
    // Unmark guard page.
    assert(_gd_state.guard_p != 0);
    td_err = _protect_page((void *)_gd_state.guard_p, PROT_READ | PROT_WRITE);
    if (td_err) {
      fprintf(stderr, "guard: unmark error\r\n");
      fprintf(stderr, "%s\r\n", strerror(errno));
      if (!err) {
        err = td_err;
      }
    }
    _gd_state.guard_p = 0;
  }

exit:
  return err;
}
