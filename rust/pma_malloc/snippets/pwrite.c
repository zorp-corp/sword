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

// 1. gcc -std=c11 mmap.c -o mmap
// 2. ./mmap test.txt 3
int
main(int argc, char** argv) {

  int fd = open(argv[1], O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  if (fd < 0) {
    fprintf(stderr, "%s\n", strerror(errno));
    exit(errno);
  }

  int length = (PAGE_SIZE * atoi(argv[2]));
  int pos = length - PAGE_SIZE;

  lseek(fd, (length - 1), SEEK_SET);
  write(fd, "", 1);

  char *map = mmap(0, PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd, pos);
  memcpy(map, DEFAULT_TEXT, strlen(DEFAULT_TEXT));
  msync(map, PAGE_SIZE, MS_SYNC);
  munmap(map, PAGE_SIZE);

  close(fd);
  return 0;
}
