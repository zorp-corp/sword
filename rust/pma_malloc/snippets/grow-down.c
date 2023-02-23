#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#define PAGE_SIZE     4096

#define DEFAULT_TEXT  "PAYLOAD"

// 1. gcc -std=gnu11 grow-down.c -o grow-down
// 2. ./grow-down test.txt
int
main(int argc, char** argv) {

  int length = (PAGE_SIZE * 4);

  char *map = mmap(0, length, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_GROWSDOWN, 0, 0);
  if (map == -1) {
    fprintf(stderr, "mmap error\n");
    fprintf(stderr, "%s\n", strerror(errno));
    exit(-1);
  }

  for (int i = 0; i < 4; ++i) {
    char txt[2] = { i + 97, 0 };
    memcpy((map + (PAGE_SIZE * i)), (const char *)txt, 2);
    printf(".\n");
  }

  munmap(map, length);

  return 0;
}
