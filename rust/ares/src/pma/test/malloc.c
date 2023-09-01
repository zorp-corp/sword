#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "../malloc.h"
#include "../includes/checksum.h"
#include "internals.h"

//==============================================================================
// CONFIGURABLE MACROS
//==============================================================================

#define TEST_PMA_SNAPSHOT_TEMPLATE  "test-snapshot-XXXXXX.bin"
#define TEST_PMA_SNAPSHOT_SUFFIX    4


//==============================================================================
// TYPES
//==============================================================================

typedef struct TestState TestState;
struct TestState {
  char *dir; // Directory in which to generate test files
};


//==============================================================================
// GLOBALS
//==============================================================================

TestState *_test_state = NULL;


//==============================================================================
// FORWARD DECLARATIONS
//==============================================================================

void test_pma_state_malloc_and_free(void);
void test_pma_extend_snapshot_file(void);
void test_pma_mark_page_dirty(void);
void test_pma_copy_page(void);
void test_pma_get_disk_dpage(void);
void test_pma_copy_dpage_cache(void);
void test_pma_get_cached_dpage(void);
void test_pma_copy_shared_page(void);
void test_pma_free_bytes(void);
void test_pma_free_pages(void);
void test_pma_get_new_pages(void);
void test_pma_get_new_page(void);
void test_pma_get_cached_pages(void);
void test_pma_malloc_single_page(void);
void test_pma_malloc_shared_page(void);
void test_pma_update_free_pages(void);
void test_pma_verify_checksum(void);
void test_pma_in_arena(void);
void test_pma_init(void);
void test_pma_sync(void);
void test_pma_load(void);


//==============================================================================
// MAIN & HELPERS
//==============================================================================

void
test_pma(char* test_dir) {
  // Set up test state
  _test_state = malloc(sizeof(TestState));
  _test_state->dir = test_dir;

  // Run tests
  test_pma_state_malloc_and_free();
  test_pma_extend_snapshot_file();
  test_pma_mark_page_dirty();
  test_pma_copy_page();
  test_pma_get_disk_dpage();
  test_pma_copy_dpage_cache();
  test_pma_get_cached_dpage();
  test_pma_copy_shared_page();
  test_pma_free_bytes();
  test_pma_free_pages();
  test_pma_get_new_pages();
  test_pma_get_new_page();
  test_pma_get_cached_pages();
  test_pma_malloc_single_page();
  test_pma_malloc_shared_page();
  test_pma_update_free_pages();
  test_pma_verify_checksum();
  test_pma_in_arena();
  test_pma_init();
  test_pma_sync();
  test_pma_load();

  // Clean up
  free(_test_state);

  // Done
  printf("Unit tests PASSED\n");
}

int
_generate_test_snapshot(char **filename) {
  size_t  dir_len;
  size_t  file_len;
  int     fd;

  dir_len = strlen(_test_state->dir);
  file_len = strlen(TEST_PMA_SNAPSHOT_TEMPLATE);

  *filename = malloc(dir_len + file_len + 1);
  strcpy(*filename, _test_state->dir);
  strcpy((*filename + dir_len), TEST_PMA_SNAPSHOT_TEMPLATE);
  assert(*filename);
  fd = mkstemps(*filename, TEST_PMA_SNAPSHOT_SUFFIX);
  assert(fd > 0);

  return fd;
}

void
_clean_up_test_snapshot(int fd, char *filename) {
  close(fd);
  unlink(filename);
  free(filename);
}


//==============================================================================
// TESTS
//==============================================================================

void
test_pma_state_malloc_and_free(void) {
  int res = -1;

  // pre state malloc
  assert(!_pma_state);

  // state malloc
  res = _pma_state_malloc();
  assert(!res);
  assert(_pma_state);
  assert(_pma_state->metadata);

  // try state malloc again
  res = _pma_state_malloc();
  assert(res == 1);

  // state free
  _pma_state_free();
  assert(!_pma_state);

  // try state free again
  _pma_state_free();

  // free metadata separately
  res = _pma_state_malloc();
  free(_pma_state->metadata);
  _pma_state->metadata = NULL;
  _pma_state_free();
}

void
test_pma_extend_snapshot_file(void) {
  struct stat   statbuf;
  uint64_t      multiplier;
  int           fd;
  int           ret;
  char         *filename = NULL;

  // Test 1: 0 multiplier
  ret = _pma_extend_snapshot_file(0);
  assert(ret == -1);

  // Test 2: massive multiplier
  ret = _pma_extend_snapshot_file(0xffffffff);
  assert(ret == -1);

  // Set up state & locals
  _pma_state_malloc();
  _pma_state->metadata->snapshot_size = 0;
  multiplier = 10;

  // Test 3: lseek fails; snapshot file doesn't exist
  ret = _pma_extend_snapshot_file(multiplier);
  assert(ret == -1);
  assert(errno == ESPIPE);

  // Set up fd
  errno = 0;
  fd = _generate_test_snapshot(&filename);
  close(fd);
  fd = open(filename, O_RDONLY);
  assert(fd > 0);
  _pma_state->snapshot_fd = fd;

  // Test 4: write fails; snapshot file read only
  errno = 0;
  ret = _pma_extend_snapshot_file(multiplier);
  assert(ret == -1);
  assert(errno == EBADF);
  close(fd);

  // Reset fd
  fd = open(filename, O_RDWR);
  assert(fd > 0);
  _pma_state->snapshot_fd = fd;

  // Test 5: Successful
  errno = 0;
  ret = _pma_extend_snapshot_file(multiplier);
  assert(ret == 0);
  assert(errno == 0);
  assert(fstat(fd, &statbuf) == 0);
  assert((uint64_t)statbuf.st_size == (multiplier * PMA_SNAPSHOT_RESIZE_INC));
  assert((uint64_t)statbuf.st_size == _pma_state->metadata->snapshot_size);

  // Clean up
  _clean_up_test_snapshot(fd, filename);
  _pma_state_free();
}

void
test_pma_mark_page_dirty(void) {
  PMADirtyPageEntry *dirty_page;

  // Set up state & locals
  _pma_state_malloc();
  _pma_state->metadata->num_dirty_pages = 10;
  dirty_page = (_pma_state->metadata->dirty_pages + 10);
  dirty_page->index     = 1;
  dirty_page->offset    = 2;
  dirty_page->num_pages = 3;
  dirty_page->status    = FREE;

  // Test 1: mark page dirty
  _pma_mark_page_dirty(4, 5, FIRST, 6);
  assert(_pma_state->metadata->num_dirty_pages == 11);
  assert(dirty_page->index == 4);
  assert(dirty_page->offset == 5);
  assert(dirty_page->num_pages == 6);
  assert(dirty_page->status == FIRST);

  // Clean up
  _pma_state_free();
}

void
test_pma_copy_page(void) {
  const uint64_t  page_uno_offset = 0;
  const uint64_t  page_dos_offset = PMA_PAGE_SIZE;
  const uint64_t  page_tre_offset = (2 * PMA_PAGE_SIZE);
  const uint64_t  file_size = (3 * PMA_PAGE_SIZE);
  const uint16_t  end_of_dpage_cache = (PMA_DPAGE_CACHE_SIZE - 1);
  ssize_t         bytes;
  const int       strlen = 6;
  int             fd;
  const char     *text_alpha = "ALPHA";
  const char     *text_bravo = "BRAVO";
  const char     *text_delta = "DELTA";
  char           *filename;
  char            text_test[6] = { 0 };
  void           *address;

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(12287 == lseek(fd, (file_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));
  assert(6 == pwrite(fd, text_alpha, strlen, 0));
  assert(6 == pwrite(fd, text_bravo, strlen, PMA_PAGE_SIZE));
  assert(6 == pwrite(fd, text_delta, strlen, (2 * PMA_PAGE_SIZE)));

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;

  _pma_state->metadata->dpage_cache = calloc(1, PMA_PAGE_SIZE);
  _pma_state->metadata->dpage_cache->tail = end_of_dpage_cache;
  _pma_state->metadata->dpage_cache->queue[end_of_dpage_cache] = 0;

  _pma_state->page_directory.entries = calloc(2, sizeof(PMAPageDirEntry));
  _pma_state->page_directory.entries[1].offset = page_dos_offset;

  // Set up address
  address = mmap(
      INDEX_TO_PTR(1),
      PMA_PAGE_SIZE,
      PROT_READ,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_dos_offset);
  assert(MAP_FAILED != address);

  // Test 1: copy page in backing file
  _pma_copy_page(address, page_tre_offset, FIRST, fd);
  assert(0 == _pma_state->metadata->dpage_cache->tail);
  assert(4096 == _pma_state->metadata->dpage_cache->queue[end_of_dpage_cache]);
  bytes = pread(fd, text_test, strlen, page_uno_offset);
  assert(6 == bytes);
  assert(0 == strcmp(text_alpha, text_test));
  bytes = pread(fd, text_test, strlen, page_dos_offset);
  assert(6 == bytes);
  assert(0 == strcmp(text_bravo, text_test));
  bytes = pread(fd, text_test, strlen, page_tre_offset);
  assert(6 == bytes);
  assert(0 == strcmp(text_bravo, text_test));

  // Clean up
  munmap(INDEX_TO_PTR(0), file_size);
  free(_pma_state->metadata->dpage_cache);
  _clean_up_test_snapshot(fd, filename);
  _pma_state_free();
}

void
test_pma_get_disk_dpage(void) {
  struct stat statbuf;
  uint64_t    init_size = 2 * PMA_PAGE_SIZE;
  uint64_t    next_offset;
  int         fd;
  char       *filename;

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->next_offset = init_size - PMA_PAGE_SIZE;
  _pma_state->metadata->snapshot_size = init_size;

  // Test 1: get next dpage without extending snapshot backing file
  next_offset = _pma_get_disk_dpage();
  assert(4096 == next_offset);
  assert(8192 == _pma_state->metadata->next_offset);

  // Test 2: failure to extend backing file
  next_offset = _pma_get_disk_dpage();
  assert(0 == next_offset);
  assert(8192 == _pma_state->metadata->next_offset);

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(8191 == lseek(fd, (init_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));
  _pma_state->snapshot_fd = fd;

  // Test 3: get next dpage after extending snapshot backing file
  next_offset = _pma_get_disk_dpage();
  assert(8192 == next_offset);
  assert(12288 == _pma_state->metadata->next_offset);
  assert(0 == fstat(fd, &statbuf));
  assert((uint64_t)statbuf.st_size == (PMA_SNAPSHOT_RESIZE_INC + init_size));

  // Clean up
  free(_pma_state->page_directory.entries);
  _clean_up_test_snapshot(fd, filename);
  _pma_state_free();
}

void
test_pma_copy_dpage_cache(void) {
  const uint64_t  page_uno_offset = PMA_PAGE_SIZE;
  const uint64_t  page_dos_offset = (2 * PMA_PAGE_SIZE);
  const uint64_t  page_tre_offset = (3 * PMA_PAGE_SIZE);
  const uint64_t  init_size = 4 * PMA_PAGE_SIZE;
  const uint64_t  test_code = 0xcafebabe8008135;
  uint64_t        data_buffer;
  ssize_t         bytes;
  int             fd = 0;
  char           *filename = NULL;

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(16383 == lseek(fd, (init_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;
  _pma_state->metadata->snapshot_size = init_size;
  _pma_state->metadata->dpage_cache = mmap(
      INDEX_TO_PTR(0),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_uno_offset);
  _pma_state->metadata->dpage_cache->dirty = 0;
  _pma_state->metadata->dpage_cache->size = 0;
  _pma_state->metadata->dpage_cache->head = 1;
  _pma_state->metadata->dpage_cache->tail = 2;
  _pma_state->metadata->dpage_cache->queue[0] = test_code;
  _pma_state->metadata->dpage_cache->queue[1] = page_dos_offset;
  _pma_state->page_directory.entries = malloc(sizeof(PMAPageDirEntry));
  _pma_state->page_directory.entries[0].offset = page_uno_offset;

  // Test 1: free page cache empty, getting new page fails
  _pma_state->metadata->next_offset = init_size;
  assert(_pma_copy_dpage_cache());

  // Test 2: free page cache empty, getting new page succeeds
  _pma_state->snapshot_fd = fd;
  _pma_state->metadata->next_offset = page_tre_offset;
  assert(0 == _pma_copy_dpage_cache());
  assert(16384 == _pma_state->metadata->next_offset);
  bytes = pread(fd, &data_buffer, 8, (page_tre_offset + 8));
  assert(8 == bytes);
  assert(0xcafebabe8008135 == data_buffer);

  // Reset dpage cache dirty bit
  _pma_state->metadata->dpage_cache->dirty = 0;

  // Test 3: free page cache has a page
  _pma_state->metadata->dpage_cache->size = 1;
  assert(0 == _pma_copy_dpage_cache());
  bytes = pread(fd, &data_buffer, 8, (page_dos_offset + 8));
  assert(8 == bytes);
  assert(0xcafebabe8008135 == data_buffer);

  // Clean up
  munmap(INDEX_TO_PTR(0), init_size);
  free(_pma_state->page_directory.entries);
  _clean_up_test_snapshot(fd, filename);
  _pma_state_free();
}

void
test_pma_get_cached_dpage(void) {

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->dpage_cache = calloc(1, sizeof(PMADPageCache));

  // Test 1: no pages in cache
  _pma_state->metadata->dpage_cache->dirty = 1;
  _pma_state->metadata->dpage_cache->size = 0;
  assert(0 == _pma_get_cached_dpage());
  
  // Test 2: only one page in cache and cache uncopied
  _pma_state->metadata->dpage_cache->dirty = 0;
  _pma_state->metadata->dpage_cache->size = 1;
  assert(0 == _pma_get_cached_dpage());

  // Test 3: successfully get page
  _pma_state->metadata->dpage_cache->dirty = 1;
  _pma_state->metadata->dpage_cache->size = 2;
  _pma_state->metadata->dpage_cache->head = 0;
  _pma_state->metadata->dpage_cache->tail = 1;
  _pma_state->metadata->dpage_cache->queue[0] = 0xcafebabe8008135;
  assert(0xcafebabe8008135 == _pma_get_cached_dpage());
  assert(1 == _pma_state->metadata->dpage_cache->size);
  assert(1 == _pma_state->metadata->dpage_cache->head);
  assert(1 == _pma_state->metadata->dpage_cache->tail);

  // Test 4: successfully get page & loop queue
  _pma_state->metadata->dpage_cache->head = PMA_DPAGE_CACHE_SIZE - 1;
  _pma_state->metadata->dpage_cache->queue[PMA_DPAGE_CACHE_SIZE - 1] = 0xdefaced0facade;
  assert(0xdefaced0facade == _pma_get_cached_dpage());
  assert(0 == _pma_state->metadata->dpage_cache->size);
  assert(0 == _pma_state->metadata->dpage_cache->head);
  assert(1 == _pma_state->metadata->dpage_cache->tail);

  // Clean up
  free(_pma_state->metadata->dpage_cache);
  _pma_state_free();
}

void
test_pma_copy_shared_page(void) {
  PMASharedPageHeader  *clean_shared_page;
  PMASharedPageHeader  *dirty_shared_page;
  ssize_t               bytes;
  const uint64_t        init_size = 4 * PMA_PAGE_SIZE;
  const uint64_t        page_nul_offset = 0;
  const uint64_t        page_uno_offset = PMA_PAGE_SIZE;
  const uint64_t        page_dos_offset = (2 * PMA_PAGE_SIZE);
  const uint64_t        page_tre_offset = (3 * PMA_PAGE_SIZE);
  const uint8_t         page_uno_size = 10;
  const uint8_t         page_dos_size = 20;
  uint8_t               data_buffer;
  int                   fd = 0;
  char                 *filename = NULL;

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(16383 == lseek(fd, (init_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;
  _pma_state->metadata->snapshot_size = init_size;
  _pma_state->metadata->dpage_cache = mmap(
      INDEX_TO_PTR(0),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_nul_offset);
  _pma_state->metadata->dpage_cache->dirty = 1;
  _pma_state->page_directory.entries = calloc(3, sizeof(PMAPageDirEntry));
  _pma_state->page_directory.entries[1].offset = page_uno_offset;
  _pma_state->page_directory.entries[1].status = SHARED;
  _pma_state->page_directory.entries[2].offset = page_dos_offset;
  _pma_state->page_directory.entries[2].status = SHARED;

  // Set up shared pages
  dirty_shared_page = mmap(
      INDEX_TO_PTR(1),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_uno_offset);
  dirty_shared_page->dirty = 1;
  dirty_shared_page->size = page_uno_size;

  clean_shared_page = mmap(
      INDEX_TO_PTR(2),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_dos_offset);
  clean_shared_page->dirty = 0;
  clean_shared_page->size = page_dos_size;

  // Test 1: don't copy if shared page already dirty
  assert(0 == _pma_copy_shared_page(dirty_shared_page));

  // Test 2: fail if a new dpage couldn't be acquired
  _pma_state->metadata->dpage_cache->size = 0;
  _pma_state->metadata->dpage_cache->head = 0;
  _pma_state->metadata->dpage_cache->tail = 0;
  assert(-1 == _pma_copy_shared_page(clean_shared_page));

  // Test 3: success
  _pma_state->snapshot_fd = fd;
  _pma_state->metadata->dpage_cache->size = 1;
  _pma_state->metadata->dpage_cache->tail = 1;
  _pma_state->metadata->dpage_cache->queue[0] = page_tre_offset;
  assert(0 == _pma_copy_shared_page(clean_shared_page));
  bytes = pread(fd, &data_buffer, 1, (page_uno_offset + 9));
  assert(1 == bytes);
  assert(10 == data_buffer);
  bytes = pread(fd, &data_buffer, 1, (page_dos_offset + 9));
  assert(1 == bytes);
  assert(20 == data_buffer);
  bytes = pread(fd, &data_buffer, 1, (page_tre_offset + 9));
  assert(1 == bytes);
  assert(20 == data_buffer);

  // Clean up
  free(_pma_state->page_directory.entries);
  munmap(PMA_SNAPSHOT_ADDR, init_size);
  _pma_state_free();
  _clean_up_test_snapshot(fd, filename);
}

void
test_pma_free_bytes(void) {
  PMASharedPageHeader  *shared_page_16;
  PMASharedPageHeader  *shared_page_64;
  PMASharedPageHeader  *shared_page_256;
  const uint64_t        init_size = 3 * PMA_PAGE_SIZE;
  const uint64_t        page_uno_offset = 0;
  const uint64_t        page_dos_offset = PMA_PAGE_SIZE;
  const uint64_t        page_tre_offset = (2 * PMA_PAGE_SIZE);
  const uint8_t         page_uno_size = 4;
  const uint8_t         page_dos_size = 6;
  const uint8_t         page_tre_size = 8;
  int                   fd = 0;
  int                   ret;
  char                 *filename = NULL;

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(12287 == lseek(fd, (init_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;
  _pma_state->metadata->snapshot_size = init_size;

  // Set up shared pages
  shared_page_16 = mmap(
      INDEX_TO_PTR(0),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_uno_offset);
  shared_page_16->dirty = 1;
  shared_page_16->size = page_uno_size;
  shared_page_16->free = 0;
  for (int i = 0; i < PMA_BITMAP_SIZE; ++i) {
    shared_page_16->bits[i] = 0;
  }

  shared_page_64 = mmap(
      INDEX_TO_PTR(1),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_dos_offset);
  shared_page_64->dirty = 1;
  shared_page_64->size = page_dos_size;
  shared_page_64->free = 0;
  for (int i = 0; i < PMA_BITMAP_SIZE; ++i) {
    shared_page_64->bits[i] = 0;
  }

  shared_page_256 = mmap(
      INDEX_TO_PTR(2),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      page_tre_offset);
  shared_page_256->dirty = 1;
  shared_page_256->size = page_tre_size;
  shared_page_256->free = 0;
  for (int i = 0; i < PMA_BITMAP_SIZE; ++i) {
    shared_page_256->bits[i] = 0;
  }

  // Test 1: free slot 0 of shared page with slot size 16
  ret = _pma_free_bytes((char*)shared_page_16 + sizeof(PMASharedPageHeader));
  assert(0 == ret);
  assert(1 == shared_page_16->free);
  assert(0x01 == shared_page_16->bits[0]);

  // Test 2: free slot 8 of shared page with slot size 64
  ret = _pma_free_bytes((char*)shared_page_64 + sizeof(PMASharedPageHeader) + 448);
  assert(0 == ret);
  assert(1 == shared_page_64->free);
  assert(0x80 == shared_page_64->bits[0]);

  // Test 3: free slot 15 of shared page with slot size 256
  ret = _pma_free_bytes((char*)shared_page_256 + sizeof(PMASharedPageHeader) + 3584);
  assert(0 == ret);
  assert(1 == shared_page_256->free);
  assert(0x40 == shared_page_256->bits[1]);

  // Test 4: failure when freeing an already free slot
  ret = _pma_free_bytes((char*)shared_page_16 + sizeof(PMASharedPageHeader));
  assert(-1 == ret);

  // Clean up
  munmap(PMA_SNAPSHOT_ADDR, init_size);
  _pma_state_free();
  _clean_up_test_snapshot(fd, filename);
}

void
test_pma_free_pages(void) {
  const uint64_t  init_size = 3 * PMA_PAGE_SIZE;
  const uint64_t  solo_page_offset = 0;
  const uint64_t  duo_page_offset = PMA_PAGE_SIZE;
  int             fd = 0;
  char           *filename = NULL;
  void           *solo_page;
  void           *duo_page;

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(12287 == lseek(fd, (init_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;
  _pma_state->metadata->snapshot_size = init_size;
  _pma_state->page_directory.entries = calloc(3, sizeof(PMAPageDirEntry));
  _pma_state->page_directory.entries[0].status = FIRST;
  _pma_state->page_directory.entries[0].offset = solo_page_offset;
  _pma_state->page_directory.entries[1].status = FIRST;
  _pma_state->page_directory.entries[1].offset = duo_page_offset;
  _pma_state->page_directory.entries[2].status = FOLLOW;
  _pma_state->page_directory.entries[2].offset = duo_page_offset + PMA_PAGE_SIZE;

  // Set up pages
  solo_page = mmap(
      INDEX_TO_PTR(0),
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      solo_page_offset);

  duo_page = mmap(
      INDEX_TO_PTR(1),
      2 * PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      duo_page_offset);

  // Test 1: fail when pointing to middle of page
  assert(-1 == _pma_free_pages(solo_page + 1));

  // Test 2: free single page allocation
  assert(0 == _pma_free_pages(solo_page));

  // test 3: free multi-page allocation
  assert(0 == _pma_free_pages(duo_page));

  // Clean up
  munmap(PMA_SNAPSHOT_ADDR, init_size);
  free(_pma_state->page_directory.entries);
  _pma_state_free();
  _clean_up_test_snapshot(fd, filename);
}

void
test_pma_get_new_pages(void) {
  const uint64_t  init_size = PMA_PAGE_SIZE;
  const uint64_t  num_pages = 2;
  int             fd = 0;
  char           *filename = NULL;
  void* const     address = PMA_SNAPSHOT_ADDR + PMA_PAGE_SIZE;
  void* const     arena_end = address + (2 * PMA_PAGE_SIZE);

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(4095 == lseek(fd, (init_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));

  // Set up state
  _pma_state_malloc();
  _pma_state->snapshot_fd = fd;
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;
  _pma_state->metadata->arena_end = address;
  _pma_state->metadata->snapshot_size = init_size;
  _pma_state->metadata->next_offset = init_size;

  // Test 1: allocate new pages
  assert(address == _pma_get_new_pages(num_pages));
  assert(12288 == _pma_state->metadata->next_offset);
  assert(arena_end == _pma_state->metadata->arena_end);

  // Clean Up
  munmap(address, num_pages * PMA_PAGE_SIZE);
  _pma_state_free();
  _clean_up_test_snapshot(fd, filename);
}

void
test_pma_get_new_page(void) {
  const uint64_t  init_size = 2 * PMA_PAGE_SIZE;
  const uint64_t  init_offset = PMA_PAGE_SIZE;
  int             fd = 0;
  char           *filename = NULL;
  void* const     address = PMA_SNAPSHOT_ADDR;
  void* const     arena_end = address + PMA_PAGE_SIZE;

  // Set up backing file
  fd = _generate_test_snapshot(&filename);
  assert(8191 == lseek(fd, (init_size - 1), SEEK_SET));
  assert(1 == write(fd, "", 1));

  // Set up state
  _pma_state_malloc();
  _pma_state->snapshot_fd = fd;
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;
  _pma_state->metadata->arena_end = address;
  _pma_state->metadata->snapshot_size = init_size;
  _pma_state->metadata->next_offset = init_offset;

  _pma_state->metadata->dpage_cache = calloc(1, sizeof(PMADPageCache));
  _pma_state->metadata->dpage_cache->size = 0;

  // Test 1: allocate new pages
  assert(address == _pma_get_new_page(FIRST));
  assert(8192 == _pma_state->metadata->next_offset);
  assert(arena_end == _pma_state->metadata->arena_end);

  // Clean Up
  munmap(address, PMA_PAGE_SIZE);
  free(_pma_state->metadata->dpage_cache);
  _pma_state_free();
  _clean_up_test_snapshot(fd, filename);
}

void
test_pma_get_cached_pages(void) {
  PMAPageRunCache  *test_0_cache;
  PMAPageRunCache  *test_1_cache;
  PMAPageRunCache  *test_2_cache;
  PMAPageRunCache  *test_3_cache;
  PMAPageRunCache  *test_4_cache;
  PMAPageRunCache  *test_5_cache;
  PMAPageRunCache  *wip_ptr;
  void             *address;

  // Set up state
  _pma_state_malloc();

  // Set up run caches for test
  test_0_cache = NULL;

  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x30000;
  wip_ptr->length = 6;
  wip_ptr->next = NULL;
  test_1_cache = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x20000;
  wip_ptr->length = 5;
  wip_ptr->next = test_1_cache;
  test_1_cache = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x10000;
  wip_ptr->length = 4;
  wip_ptr->next = test_1_cache;
  test_1_cache = wip_ptr;

  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x30000;
  wip_ptr->length = 6;
  wip_ptr->next = NULL;
  test_2_cache = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x20000;
  wip_ptr->length = 4;
  wip_ptr->next = test_2_cache;
  test_2_cache = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x10000;
  wip_ptr->length = 5;
  wip_ptr->next = test_2_cache;
  test_2_cache = wip_ptr;

  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x30000;
  wip_ptr->length = 4;
  wip_ptr->next = NULL;
  test_3_cache = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x20000;
  wip_ptr->length = 5;
  wip_ptr->next = test_3_cache;
  test_3_cache = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x10000;
  wip_ptr->length = 6;
  wip_ptr->next = test_3_cache;
  test_3_cache = wip_ptr;

  test_4_cache = calloc(1, sizeof(PMAPageRunCache));
  test_4_cache->page = 0x40000;
  test_4_cache->length = 2;
  // Invalid pointer; used to confirm that we stop searching when we find exact run
  test_4_cache->next = 0x8fffffffffffffff;

  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x50000;
  wip_ptr->length = 3;
  wip_ptr->next = NULL;
  test_5_cache = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMAPageRunCache));
  wip_ptr->page = 0x99000;
  wip_ptr->length = 1;
  wip_ptr->next = test_5_cache;
  test_5_cache = wip_ptr;

  // Test 0: page run cache empty
  _pma_state->free_page_runs = test_0_cache;
  address = _pma_get_cached_pages(2);
  assert(NULL == address);

  // Test 1: find run bigger than requested, by two pages, at the very beginning
  _pma_state->free_page_runs = test_1_cache;
  address = _pma_get_cached_pages(2);
  assert(0x10000 == address);
  assert(2 == _pma_state->free_page_runs->length);
  assert(0x12000 == _pma_state->free_page_runs->page);
  assert(5 == _pma_state->free_page_runs->next->length);
  assert(0x20000 == _pma_state->free_page_runs->next->page);
  assert(6 == _pma_state->free_page_runs->next->next->length);
  assert(0x30000 == _pma_state->free_page_runs->next->next->page);
  assert(NULL == _pma_state->free_page_runs->next->next->next);

  // Test 2: find run bigger than request, by two pages, in the middle
  _pma_state->free_page_runs = test_2_cache;
  address = _pma_get_cached_pages(2);
  assert(0x20000 == address);
  assert(5 == _pma_state->free_page_runs->length);
  assert(0x10000 == _pma_state->free_page_runs->page);
  assert(2 == _pma_state->free_page_runs->next->length);
  assert(0x22000 == _pma_state->free_page_runs->next->page);
  assert(6 == _pma_state->free_page_runs->next->next->length);
  assert(0x30000 == _pma_state->free_page_runs->next->next->page);
  assert(NULL == _pma_state->free_page_runs->next->next->next);

  // Test 3: find run bigger than requested, by two pages, at the very end
  _pma_state->free_page_runs = test_3_cache;
  address = _pma_get_cached_pages(2);
  assert(0x30000 == address);
  assert(6 == _pma_state->free_page_runs->length);
  assert(0x10000 == _pma_state->free_page_runs->page);
  assert(5 == _pma_state->free_page_runs->next->length);
  assert(0x20000 == _pma_state->free_page_runs->next->page);
  assert(2 == _pma_state->free_page_runs->next->next->length);
  assert(0x32000 == _pma_state->free_page_runs->next->next->page);
  assert(NULL == _pma_state->free_page_runs->next->next->next);

  // Test 4: find exactly sized run, as only entry in cache, and stop looking
  _pma_state->free_page_runs = test_4_cache;
  address = _pma_get_cached_pages(2);
  assert(0x40000 == address);
  assert(0x8fffffffffffffff == _pma_state->free_page_runs);

  // Test 5: find run bigger than request, by a single page
  _pma_state->free_page_runs = test_5_cache;
  address = _pma_get_cached_pages(2);
  assert(0x50000 == address);
  assert(1 == _pma_state->free_page_runs->length);
  assert(0x99000 == _pma_state->free_page_runs->page);
  assert(NULL == _pma_state->free_page_runs->next);
  assert(0x52000 == _pma_state->free_pages->page);
  assert(NULL == _pma_state->free_pages->next);

  // Clean up
  while (test_1_cache != NULL) {
    wip_ptr = test_1_cache;
    test_1_cache = test_1_cache->next;
    free(wip_ptr);
  }
  while (test_2_cache != NULL) {
    wip_ptr = test_2_cache;
    test_2_cache = test_2_cache->next;
    free(wip_ptr);
  }
  while (test_3_cache != NULL) {
    wip_ptr = test_3_cache;
    test_3_cache = test_3_cache->next;
    free(wip_ptr);
  }
  free(_pma_state->free_pages);
  free(_pma_state->free_page_runs);
  _pma_state_free();
}

void
test_pma_malloc_single_page(void) {
  PMASinglePageCache *wip_ptr;
  
  // Set up state
  _pma_state_malloc();

  // Set up free page cache
  wip_ptr = calloc(1, sizeof(PMASinglePageCache));
  wip_ptr->page = 0x20000;
  wip_ptr->next = NULL;
  _pma_state->free_pages = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMASinglePageCache));
  wip_ptr->page = 0x10000;
  wip_ptr->next = _pma_state->free_pages;
  _pma_state->free_pages = wip_ptr;

  // Test 1: get page from free page cache
  assert(0x10000 == _pma_malloc_single_page(FIRST));
  assert(0x20000 == _pma_state->free_pages->page);
  assert(NULL == _pma_state->free_pages->next);

  // Case when no pages in free page cache tested by test_pma_get_new_page

  // Clean up
  free(_pma_state->free_pages);
  _pma_state_free();
}

void
test_pma_malloc_shared_page(void) {
  PMASinglePageCache *free_pages;
  PMASinglePageCache *wip_ptr;
  const uint64_t      mmap_size = 2 * PMA_PAGE_SIZE;
  const uint8_t       test_1_bucket_size = 0;
  const uint8_t       test_2_bucket_size = 0;
  const uint8_t       test_3_bucket_size = 6;
  void               *shared_pages;

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;
  _pma_state->metadata->snapshot_size = PMA_PAGE_SIZE;
  _pma_state->metadata->next_offset = PMA_PAGE_SIZE;
  _pma_state->free_pages = NULL;

  _pma_state->metadata->dpage_cache = calloc(1, sizeof(PMADPageCache));
  _pma_state->metadata->dpage_cache->size = 0;

  // Set up shared pages
  shared_pages = mmap(
      PMA_SNAPSHOT_ADDR,
      mmap_size,
      PROT_READ | PROT_WRITE,
      MAP_ANONYMOUS | MAP_PRIVATE,
      -1,
      0);
  assert(MAP_FAILED != shared_pages);

  // Set up free page cache
  wip_ptr = calloc(1, sizeof(PMASinglePageCache));
  wip_ptr->page = (shared_pages + PMA_PAGE_SIZE);
  wip_ptr->next = NULL;
  free_pages = wip_ptr;
  wip_ptr = calloc(1, sizeof(PMASinglePageCache));
  wip_ptr->page = shared_pages;
  wip_ptr->next = free_pages;
  free_pages = wip_ptr;

  // Test 1: could not allocate page
  assert(-1 == _pma_malloc_shared_page(test_1_bucket_size));

  // Test 2: 16 byte slots
  _pma_state->free_pages = free_pages;
  assert(0 == _pma_malloc_shared_page(test_2_bucket_size));
  assert(NULL != _pma_state->metadata->shared_pages[test_2_bucket_size]);
  assert(1 == _pma_state->metadata->shared_pages[test_2_bucket_size]->dirty);
  assert(4 == _pma_state->metadata->shared_pages[test_2_bucket_size]->size);
  assert(253 == _pma_state->metadata->shared_pages[test_2_bucket_size]->free);
  for (uint8_t i = 0; i < PMA_BITMAP_SIZE; ++i) {
    assert(PMA_EMPTY_BITMAP == _pma_state->metadata->shared_pages[test_2_bucket_size]->bits[i]);
  }
  assert(NULL != _pma_state->free_pages);
  assert((shared_pages + PMA_PAGE_SIZE) == _pma_state->free_pages->page);
  assert(NULL == _pma_state->free_pages->next);

  // Test 3: 1024 byte slots
  assert(0 == _pma_malloc_shared_page(test_3_bucket_size));
  assert(NULL != _pma_state->metadata->shared_pages[test_3_bucket_size]);
  assert(1 == _pma_state->metadata->shared_pages[test_3_bucket_size]->dirty);
  assert(10 == _pma_state->metadata->shared_pages[test_3_bucket_size]->size);
  assert(3 == _pma_state->metadata->shared_pages[test_3_bucket_size]->free);
  for (uint8_t i = 0; i < PMA_BITMAP_SIZE; ++i) {
    assert(PMA_EMPTY_BITMAP == _pma_state->metadata->shared_pages[test_3_bucket_size]->bits[i]);
  }
  assert(NULL == _pma_state->free_pages);

  // Clean up
  munmap(shared_pages, mmap_size);
  _pma_state_free();
}

void
test_pma_update_free_pages(void) {
  PMADirtyPageEntry test_1_dirty_pages[2];
  PMADirtyPageEntry test_2_dirty_page;
  PMADirtyPageEntry test_3_dirty_page;

  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = PMA_SNAPSHOT_ADDR;

  // Set up dirty pages
  test_1_dirty_pages[0].index = 1;
  test_1_dirty_pages[0].num_pages = 1;
  test_1_dirty_pages[0].status = SHARED;
  test_1_dirty_pages[1].index = 1;
  test_1_dirty_pages[1].num_pages = 2;
  test_1_dirty_pages[1].status = FIRST;

  test_2_dirty_page.index = 2;
  test_2_dirty_page.num_pages = 1;
  test_2_dirty_page.status = FREE;

  test_3_dirty_page.index = 3;
  test_3_dirty_page.num_pages = 2;
  test_3_dirty_page.status = FREE;

  // Test 1: all dirty pages have non-free status
  assert(0 == _pma_update_free_pages(2, test_1_dirty_pages));
  assert(NULL == _pma_state->free_pages);
  assert(NULL == _pma_state->free_page_runs);

  // Test 2: add single page to free page cache
  assert(0 == _pma_update_free_pages(1, &test_2_dirty_page));
  assert(NULL != _pma_state->free_pages);
  assert(INDEX_TO_PTR(2) == _pma_state->free_pages->page);
  assert(NULL == _pma_state->free_pages->next);

  // Test 3: add multiple free pages to free page runs cache
  assert(0 == _pma_update_free_pages(1, &test_3_dirty_page));
  assert(NULL != _pma_state->free_page_runs);
  assert(INDEX_TO_PTR(3) == _pma_state->free_page_runs->page);
  assert(2 == _pma_state->free_page_runs->length);
  assert(NULL == _pma_state->free_page_runs->next);

  // Clean up
  free(_pma_state->free_pages);
  free(_pma_state->free_page_runs);
  _pma_state_free();
}

void
test_pma_verify_checksum(void) {
  PMAMetadata fake_metadata_page;

  // Set up state
  _pma_state_malloc();

  // Test 1: good checksum
  fake_metadata_page.checksum = 0;
  fake_metadata_page.checksum = crc_32(
      (unsigned char *)(&fake_metadata_page),
      PMA_PAGE_SIZE);
  assert(1 == _pma_verify_checksum(&fake_metadata_page));

  // Test 2: bad checksum
  fake_metadata_page.checksum = 0xbaddecaf;
  assert(0 == _pma_verify_checksum(&fake_metadata_page));

  // Clean up
  _pma_state_free();
}

void
test_pma_in_arena(void) {
  // Set up state
  _pma_state_malloc();
  _pma_state->metadata->arena_start = 0x7fffffff;
  _pma_state->metadata->arena_end = 0x80000001;

  // Test 1: before arena start
  assert(0 == pma_in_arena(0x10000000));

  // Test 2: equal to arena start
  assert(1 == pma_in_arena(0x7fffffff));

  // Test 3: in arena
  assert(1 == pma_in_arena(0x80000000));

  // Test 4: equal to arena end
  assert(0 == pma_in_arena(0x80000001));

  // Test 5: after arena end
  assert(0 == pma_in_arena(0xffffffff));

  // Clean up
  _pma_state_free();
}

void
test_pma_init(void) {
  struct stat page_dir_statbuf;
  struct stat page_dir_statbuf_v;
  struct stat snapshot_statbuf;
  struct stat snapshot_statbuf_v;
  size_t      dir_len;
  uint32_t    checksum;
  char       *page_dir_path;
  char       *snapshot_path;

  // Set up
  dir_len = strlen(_test_state->dir);

  page_dir_path = malloc(dir_len + 15);
  sprintf(page_dir_path, "%s/%s/%s", _test_state->dir, PMA_DEFAULT_DIR_NAME, PMA_PAGE_DIR_FILENAME);

  snapshot_path = malloc(dir_len + 15);
  sprintf(snapshot_path, "%s/%s/%s", _test_state->dir, PMA_DEFAULT_DIR_NAME, PMA_SNAPSHOT_FILENAME);

  // Test 1: successful initialization
  assert(0 == pma_init(_test_state->dir));

  fstat(_pma_state->page_dir_fd, &page_dir_statbuf);
  stat(page_dir_path, &page_dir_statbuf_v);
  assert(page_dir_statbuf_v.st_dev == page_dir_statbuf.st_dev);
  assert(page_dir_statbuf_v.st_ino == page_dir_statbuf.st_ino);

  fstat(_pma_state->snapshot_fd, &snapshot_statbuf);
  stat(snapshot_path, &snapshot_statbuf_v);
  assert(snapshot_statbuf_v.st_dev == snapshot_statbuf.st_dev);
  assert(snapshot_statbuf_v.st_ino == snapshot_statbuf.st_ino);

  assert(0x400000 == page_dir_statbuf.st_size);
  assert(0x40000000 == snapshot_statbuf.st_size);

  assert(NULL == _pma_state->free_pages);
  assert(NULL == _pma_state->free_page_runs);
  assert(0 == _pma_state->meta_page_offset);

  assert(0x400000 == _pma_state->page_directory.size);
  assert(1 == _pma_state->page_directory.next_index);
  assert(FIRST == _pma_state->page_directory.entries[0].status);
  assert(8192 == _pma_state->page_directory.entries[0].offset);

  assert(0xBADDECAFC0FFEE00 == _pma_state->metadata->magic_code);
  assert(1 == _pma_state->metadata->version);
  assert(0 == _pma_state->metadata->epoch);
  assert(0 == _pma_state->metadata->event);
  assert(0 == _pma_state->metadata->root);
  assert(0x10000 == _pma_state->metadata->arena_start);
  assert(0x11000 == _pma_state->metadata->arena_end);
  assert(12288 == _pma_state->metadata->next_offset);
  assert(0x10000 == _pma_state->metadata->dpage_cache);
  assert(0 == _pma_state->metadata->dpage_cache->dirty);
  assert(0 == _pma_state->metadata->dpage_cache->size);
  assert(0 == _pma_state->metadata->dpage_cache->head);
  assert(0 == _pma_state->metadata->dpage_cache->tail);
  assert(0 == _pma_state->metadata->num_dirty_pages);
  assert(0 == _pma_state->metadata->dirty_pages[0].index);
  assert(0 == _pma_state->metadata->dirty_pages[0].offset);
  assert(0 == _pma_state->metadata->dirty_pages[0].num_pages);

  checksum = _pma_state->metadata->checksum;
  _pma_state->metadata->checksum = 0;
  assert(checksum == crc_32((unsigned char*)_pma_state->metadata, PMA_PAGE_SIZE));

  // Clean up
  munmap(_pma_state->metadata->arena_start, _pma_state->metadata->snapshot_size);
  munmap(_pma_state->page_directory.entries, PMA_MAXIMUM_DIR_SIZE);

  _pma_state_free();

  unlink(snapshot_path);
  free(snapshot_path);

  unlink(page_dir_path);
  free(page_dir_path);
}

void
test_pma_sync(void) {
  PMAMetadata          *metadata_page_1;
  PMAMetadata          *metadata_page_2;
  PMASharedPageHeader  *shared_page_16b;
  size_t                dir_len;
  char                 *page_dir_path;
  char                 *snapshot_path;

  // Set up
  dir_len = strlen(_test_state->dir);

  page_dir_path = malloc(dir_len + 15);
  sprintf(page_dir_path, "%s/%s/%s", _test_state->dir, PMA_DEFAULT_DIR_NAME, PMA_PAGE_DIR_FILENAME);

  snapshot_path = malloc(dir_len + 15);
  sprintf(snapshot_path, "%s/%s/%s", _test_state->dir, PMA_DEFAULT_DIR_NAME, PMA_SNAPSHOT_FILENAME);

  pma_init(_test_state->dir);
  _pma_state->metadata->epoch = 1;
  _pma_state->metadata->event = 1;

  // Test 1: good event, bad epoch
  assert(-1 == pma_sync(0, 2, 0));

  // Test 2: good epoch, bad event
  assert(-1 == pma_sync(1, 0, 0));

  // Test 3: successful sync
  _pma_state->metadata->epoch = 0;
  _pma_state->metadata->event = 0;

  pma_malloc(16);
  assert(1 == _pma_state->metadata->num_dirty_pages);

  assert(0 == pma_sync(1, 2, 3));
  assert(1 == _pma_state->metadata->epoch);
  assert(2 == _pma_state->metadata->event);
  assert(3 == _pma_state->metadata->root);
  assert(0x12000 == _pma_state->metadata->arena_end);
  assert(0x11000 == _pma_state->metadata->shared_pages[0]);
  assert(NULL == _pma_state->metadata->shared_pages[1]);
  assert(NULL == _pma_state->metadata->shared_pages[2]);
  assert(NULL == _pma_state->metadata->shared_pages[3]);
  assert(NULL == _pma_state->metadata->shared_pages[4]);
  assert(NULL == _pma_state->metadata->shared_pages[5]);
  assert(NULL == _pma_state->metadata->shared_pages[6]);
  assert(0x10000 == _pma_state->metadata->dpage_cache);
  assert(0 == _pma_state->metadata->num_dirty_pages);
  assert(16384 == _pma_state->metadata->next_offset);

  metadata_page_1 = mmap(
      NULL,
      PMA_PAGE_SIZE,
      PROT_READ,
      MAP_SHARED,
      _pma_state->snapshot_fd,
      0);
  metadata_page_2 = mmap(
      NULL,
      PMA_PAGE_SIZE,
      PROT_READ,
      MAP_SHARED,
      _pma_state->snapshot_fd,
      4096);
  shared_page_16b = mmap(
      NULL,
      PMA_PAGE_SIZE,
      PROT_READ,
      MAP_SHARED,
      _pma_state->snapshot_fd,
      12288);
  
  assert(metadata_page_1->magic_code == _pma_state->metadata->magic_code);
  assert(metadata_page_1->checksum == _pma_state->metadata->checksum);
  assert(metadata_page_1->version == _pma_state->metadata->version);
  assert(metadata_page_1->epoch == _pma_state->metadata->epoch);
  assert(metadata_page_1->event == _pma_state->metadata->event);
  assert(metadata_page_1->root == _pma_state->metadata->root);
  assert(metadata_page_1->arena_start == _pma_state->metadata->arena_start);
  assert(metadata_page_1->arena_end == _pma_state->metadata->arena_end);
  assert(metadata_page_1->dpage_cache == _pma_state->metadata->dpage_cache);
  assert(metadata_page_1->snapshot_size == _pma_state->metadata->snapshot_size);
  assert(metadata_page_1->next_offset == _pma_state->metadata->next_offset);

  assert(1 == metadata_page_1->num_dirty_pages);
  assert(1 == metadata_page_1->dirty_pages[0].index);
  assert(12288 == metadata_page_1->dirty_pages[0].offset);
  assert(1 == metadata_page_1->dirty_pages[0].num_pages);
  assert(SHARED == metadata_page_1->dirty_pages[0].status);

  assert(0 == metadata_page_2->epoch);
  assert(0 == metadata_page_2->event);
  assert(0 == metadata_page_2->root);
  assert(0x11000 == metadata_page_2->arena_end);
  assert(NULL == metadata_page_2->shared_pages[0]);
  assert(NULL == metadata_page_2->shared_pages[1]);
  assert(NULL == metadata_page_2->shared_pages[2]);
  assert(NULL == metadata_page_2->shared_pages[3]);
  assert(NULL == metadata_page_2->shared_pages[4]);
  assert(NULL == metadata_page_2->shared_pages[5]);
  assert(NULL == metadata_page_2->shared_pages[6]);
  assert(0x10000 == metadata_page_2->dpage_cache);
  assert(0 == metadata_page_2->num_dirty_pages);
  assert(12288 == metadata_page_2->next_offset);

  assert(NULL == shared_page_16b->next);
  assert(0 == shared_page_16b->dirty);
  assert(4 == shared_page_16b->size);
  assert(252 == shared_page_16b->free);

  // Clean up
  munmap(metadata_page_1, PMA_PAGE_SIZE);
  munmap(metadata_page_2, PMA_PAGE_SIZE);

  munmap(_pma_state->metadata->arena_start, _pma_state->metadata->snapshot_size);
  munmap(_pma_state->page_directory.entries, PMA_MAXIMUM_DIR_SIZE);

  _pma_state_free();

  unlink(snapshot_path);
  free(snapshot_path);

  unlink(page_dir_path);
  free(page_dir_path);
}

void
test_pma_load(void) {
  PMARootState    res;
  size_t          dir_len;
  const uint64_t  bad_code = 0x600DDECAFC0FFEE0;
  const uint64_t  old_event = 0;
  const uint32_t  bad_checksum = 0;
  const uint32_t  bad_version = 1337;
  int             snapshot_fd;
  char           *bin_path;
  char           *page_dir_path;
  char           *snapshot_path;

  // Set up
  dir_len = strlen(_test_state->dir);

  bin_path = malloc(dir_len + 6);
  sprintf(bin_path, "%s/%s", _test_state->dir, PMA_DEFAULT_DIR_NAME);

  page_dir_path = malloc(dir_len + 15);
  sprintf(page_dir_path, "%s/%s", bin_path, PMA_PAGE_DIR_FILENAME);

  snapshot_path = malloc(dir_len + 15);
  sprintf(snapshot_path, "%s/%s", bin_path, PMA_SNAPSHOT_FILENAME);

  // Test 1: dir doesn't exist
  rmdir(bin_path);
  res = pma_load(_test_state->dir);
  assert(0 == res.epoch);
  assert(0 == res.event);
  assert(0 == res.root);
  assert(2 == errno);
  errno = 0;

  // Test 2: snapshot doesn't exist
  pma_init(_test_state->dir);
  assert(0 == pma_close(0, 1, 0));
  unlink(snapshot_path);
  
  res = pma_load(_test_state->dir);
  assert(0 == res.epoch);
  assert(0 == res.event);
  assert(0 == res.root);
  assert(2 == errno);

  errno = 0;
  _pma_state_free();
  unlink(page_dir_path);

  // Test 3: page directory doesn't exist
  pma_init(_test_state->dir);
  assert(0 == pma_close(0, 1, 0));
  unlink(page_dir_path);
  
  res = pma_load(_test_state->dir);
  assert(0 == res.epoch);
  assert(0 == res.event);
  assert(0 == res.root);
  assert(2 == errno);

  errno = 0;
  _pma_state_free();
  unlink(snapshot_path);

  // Test 4: bad magic code
  pma_init(_test_state->dir);
  assert(0 == pma_close(0, 1, 0));
  snapshot_fd = open(snapshot_path, PMA_NEW_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  assert(0 < snapshot_fd);
  pwrite(snapshot_fd, &bad_code, 8, 0);

  res = pma_load(_test_state->dir);
  assert(0 == res.epoch);
  assert(0 == res.event);
  assert(0 == res.root);
  assert(EILSEQ == errno);

  errno = 0;
  close(snapshot_fd);
  unlink(snapshot_path);
  unlink(page_dir_path);

  // Test 5: bad version
  pma_init(_test_state->dir);
  assert(0 == pma_close(0, 1, 0));
  snapshot_fd = open(snapshot_path, PMA_NEW_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  assert(0 < snapshot_fd);
  pwrite(snapshot_fd, &bad_version, 4, 12);

  res = pma_load(_test_state->dir);
  assert(0 == res.epoch);
  assert(0 == res.event);
  assert(0 == res.root);
  assert(EILSEQ == errno);

  errno = 0;
  close(snapshot_fd);
  unlink(snapshot_path);
  unlink(page_dir_path);

  // Test 6: both metadata pages have invalid checksum
  pma_init(_test_state->dir);
  assert(0 == pma_close(0, 1, 0));
  snapshot_fd = open(snapshot_path, PMA_NEW_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  assert(0 < snapshot_fd);
  pwrite(snapshot_fd, &bad_checksum, 4, 8);
  pwrite(snapshot_fd, &bad_checksum, 4, (PMA_PAGE_SIZE + 8));

  res = pma_load(_test_state->dir);
  assert(0 == res.epoch);
  assert(0 == res.event);
  assert(0 == res.root);
  assert(EILSEQ == errno);

  errno = 0;
  close(snapshot_fd);
  unlink(snapshot_path);
  unlink(page_dir_path);

  // Test 7: first metadata page is newer but has bad checksum
  pma_init(_test_state->dir);
  assert(0 == pma_close(1, 2, 3));
  snapshot_fd = open(snapshot_path, PMA_NEW_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  assert(0 < snapshot_fd);
  pwrite(snapshot_fd, &bad_checksum, 4, 8);
  pwrite(snapshot_fd, &old_event, 8, (PMA_PAGE_SIZE + 24));

  res = pma_load(_test_state->dir);
  assert(0 == res.epoch);
  assert(0 == res.event);
  assert(0 == res.root);
  assert(0 == _pma_state->meta_page_offset);

  assert(0 == pma_close(4, 4, 4));
  close(snapshot_fd);
  unlink(snapshot_path);
  unlink(page_dir_path);

  // Test 8: second metadata page is newer
  pma_init(_test_state->dir);
  assert(0 == pma_sync(1, 2, 3));
  assert(0 == pma_close(4, 5, 6));

  res = pma_load(_test_state->dir);
  assert(4 == res.epoch);
  assert(5 == res.event);
  assert(6 == res.root);
  assert(0 == _pma_state->meta_page_offset);

  assert(0 == pma_close(7, 8, 9));
  close(snapshot_fd);
  unlink(snapshot_path);
  unlink(page_dir_path);

  // Clean up
  free(bin_path);
  free(snapshot_path);
  free(page_dir_path);
}
