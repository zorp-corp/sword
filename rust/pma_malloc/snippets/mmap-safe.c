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

// 1. gcc -std=c11 mmap-safe.c -o mmap
// 2. ./mmap test.txt
int
main(int argc, char** argv) {
  int fd = open(argv[1], O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  if (fd < 0) {
    fprintf(stderr, "%s\n", strerror(errno));
    exit(errno);
  }

  if (lseek(fd, (PAGE_SIZE - 1), SEEK_SET) < 0) {
    fprintf(stderr, "lseek error\n");
    close(fd);
    exit(-1);
  }

  if (write(fd, "", 1) != 1) {
    fprintf(stderr, "write error\n");
    close(fd);
    exit(-1);
  }

  char *map = mmap(0, PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (map == -1) {
    fprintf(stderr, "mmap error\n");
    close(fd);
    exit(-1);
  }

  printf("%p ; %s : %ld\n", map, DEFAULT_TEXT, strlen(DEFAULT_TEXT));

  memcpy(map, DEFAULT_TEXT, strlen(DEFAULT_TEXT));
  msync(map, PAGE_SIZE, MS_SYNC);
  munmap(map, PAGE_SIZE);

  close(fd);
  return 0;
}
