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

// 1. gcc -std=c11 mmap-resize.c -o mmap
// 2. ./mmap test.txt
int
main(int argc, char** argv) {
  // Create test file
  int fd = open(argv[1], O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);

  // Size file to one page
  lseek(fd, (PAGE_SIZE - 1), SEEK_SET);
  write(fd, "", 1);

  // mmap 3 pages
  char *map = mmap(0, (PAGE_SIZE * 3), PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

  // Write to first page
  memcpy(map, DEFAULT_TEXT, strlen(DEFAULT_TEXT));
  msync(map, (PAGE_SIZE * 3), MS_SYNC);

  // Resize file to 4 pages
  lseek(fd, ((PAGE_SIZE * 4) - 1), SEEK_SET);
  write(fd, "", 1);

  // Write to third page
  memcpy((map + (PAGE_SIZE * 2)), DEFAULT_TEXT, strlen(DEFAULT_TEXT));
  msync(map, (PAGE_SIZE * 3), MS_SYNC);

  // Clean up
  munmap(map, PAGE_SIZE);
  close(fd);
  return 0;
}
