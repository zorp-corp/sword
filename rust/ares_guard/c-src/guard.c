#include <signal.h>

#include "guard.h"


void *guard(void *(*f)(void *), void *arg, void *stack, void *alloc) {
  void* res = f(arg);
  return res;
}
