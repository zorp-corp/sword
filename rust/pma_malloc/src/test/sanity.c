#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../malloc.h"

//==============================================================================
// Functions
//==============================================================================

int
main(int argc, char** argv) {

  void *ptr_1;
  void *ptr_2;
  void *ptr_3;
  void *ptr_4;
  void *ptr_5;
  void *ptr_6;
  void *ptr_7;
  void *ptr_8;
  void *ptr_9;
  void *ptr_10;
  void *ptr_11;

  if (pma_init(argv[1])) {
    fprintf(stderr, "init not sane:\n");
    goto test_error;
  };

  ptr_1 = pma_malloc(8);
  ptr_2 = pma_malloc(16);
  ptr_3 = pma_malloc(32);
  ptr_4 = pma_malloc(64);
  ptr_5 = pma_malloc(128);
  ptr_6 = pma_malloc(256);
  ptr_7 = pma_malloc(512);
  ptr_8 = pma_malloc(1024);
  ptr_9 = pma_malloc(2048);
  ptr_10 = pma_malloc(4096);
  ptr_11 = pma_malloc(8192);

  if (pma_sync(1UL, 1UL)) {
    fprintf(stderr, "sync not sane:\n");
    goto test_error;
  };

  pma_free(ptr_1);
  pma_free(ptr_2);
  pma_free(ptr_3);
  pma_free(ptr_4);
  pma_free(ptr_5);
  pma_free(ptr_6);
  pma_free(ptr_7);
  pma_free(ptr_8);
  pma_free(ptr_9);
  pma_free(ptr_10);
  pma_free(ptr_11);

  if (pma_close(1UL, 2UL)) {
    fprintf(stderr, "sync not sane:\n");
    goto test_error;
  };

  printf("sane\n");

  return 0;

test_error:
  fprintf(stderr, "%s\n", strerror(errno));
  return -1;
}
