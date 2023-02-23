/**
 * ----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <phk@FreeBSD.ORG> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Poul-Henning Kamp
 * ----------------------------------------------------------------------------
 */
#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "includes/checksum.h"
#include "malloc.h"

//==============================================================================
// CONFIGURABLE MACROS
//==============================================================================

/**
 * PMA_PAGE_SIZE = 1 << PMA_PAGE_SHIFT
 *
 * Should be configured to native page size.
 */
#define PMA_PAGE_SHIFT        12U

/**
 * PMA_MIN_ALLOC_SIZE = 1 << PMA_MIN_ALLOC_SHIFT
 *
 * Note that types/sizes in SharedPageHeader are currently hardcoded to this
 * value being 4.
 */
#define PMA_MIN_ALLOC_SHIFT   4U

/**
 * How many bits per bitmap element. Change only if not 8 bits/byte
 */
#define PMA_BITMAP_BITS       (8 * sizeof(uint8_t))

/**
 * Automatic disk flush dirty page limit
 *
 * TODO
 */
//#define PMA_FLUSH_PAGE_LIMIT  128

/**
 * Automatic disk flush event limit
 *
 * TODO
 */
//#define PMA_FLUSH_EVENT_LIMIT 128

/**
 * Abort on non-fatal error
 */
#define PMA_ABORT             0

//==============================================================================
// AUTO MACROS (do not manually configure)
//==============================================================================

/**
 * Number bytes per page
 */
#define PMA_PAGE_SIZE         (1UL << PMA_PAGE_SHIFT)

/**
 * A mask for the offset of an address inside a page
 */
#define PMA_PAGE_MASK         (PMA_PAGE_SIZE - 1)

/**
 * Minimum size of an allocation in bytes
 *
 * If this is too small, it's too much work to manage small allocations.
 */
#define PMA_MIN_ALLOC_SIZE    (1U << PMA_MIN_ALLOC_SHIFT)

/**
 * PMA_MAX_SHARED_ALLOC = 1 << PMA_MAX_SHARED_SHIFT
 *
 * Should be log_2 of 1/4 of page size. Also the number of buckets in the array
 * of shared page pointers.
 */
#define PMA_MAX_SHARED_SHIFT  (PMA_PAGE_SHIFT - 2U)

/**
 * Max slot size (in bytes) for shared page allocations
 *
 * In the original phk_malloc code, this was set to 1/2 the size of a page.
 * However, since shared page metadata is stored as a header inside the page
 * itself, an allocation of 1/2 a page will use a full page anyway. Therefore,
 * the limit is set to 1/4 of a page to remove the overhead of dealing with
 * the shared page header for a page containing a single allocation.
 */
#define PMA_MAX_SHARED_ALLOC  (1UL << PMA_MAX_SHARED_SHIFT)

/**
 * Round address down to beginning of containing page
 */
#define PAGE_ROUND_DOWN(foo)  (foo & (~PMA_PAGE_MASK))

/**
 * Round address up to beginning of next page
 */
#define PAGE_ROUND_UP(foo)    ((foo + PMA_PAGE_MASK) & (~PMA_PAGE_MASK))

/**
 * Convert pointer to index in page directory
 */
#define PTR_TO_INDEX(foo)     ((((uint64_t)foo) - ((uint64_t)_pma_state->metadata.arena_start)) >> PMA_PAGE_SHIFT)

/**
 * TODO - documentation
 */
#define INDEX_TO_PTR(foo)     (void *)((char *)_pma_state->metadata.arena_start + (foo * PMA_PAGE_SIZE))

/**
 * Flags to use for all mmap operations, excluding initial metadata page mapping
 *
 * We don't care to what memory the metadata pages are mapped, so long as it's
 * before the memory arena, because we track it in the PMA process itself.
 * However, to retain consistent pointers between ship shutdown & relaunch, we
 * want all memory arena mmap mappings to go to the exact address to which we
 * tell them. Another mapping already existing at one of those addresses is a
 * fatal error.
 *
 * For more info, see https://www.man7.org/linux/man-pages/man2/mmap.2.html.
 */
#define PMA_MMAP_FLAGS        (MAP_SHARED | MAP_FIXED_NOREPLACE)

/**
 * Magic code that identifies a file as an event snapshot file
 */
#define PMA_MAGIC_CODE        0xBADDECAFC0FFEE00  // i.e. all decaf coffee

/**
 * Version of the persistent memory arena which created an event snapshot (in
 * case of breaking changes)
 */
#define PMA_DATA_VERSION      1

/**
 * Representation of an empty byte for a byte in a bitmap (1 = empty, 0 = full)
 */
#define PMA_EMPTY_BITMAP      0xFF

/**
 * See SharedPageHeader for explanation
 */
#define PMA_BITMAP_SIZE       27

/**
 * TODO
 */
#define PMA_DPAGE_CACHE_SIZE  511

/**
 *
 *    (4096 - (20 * 8)) / 24 = 164
 *
 * TODO
 */
#define PMA_DIRTY_PAGE_LIMIT  164

/**
 * Default settings for new PMA backing files
 *
 * See https://www.man7.org/linux/man-pages/man2/chmod.2.html for more info
 * about individual flags.
 *
 * Start with a page directory big enough to hold 1 GiB of data:
 *
 *    1 GiB = 262144 page entries
 *    (up to) 16 bytes per page dir entry
 *    4096 / 16 = 256 entries per page
 *    262144 / 256 = 1024 pages
 *    1024 * 4096 = 4194304 bytes
 *
 * Maximum size of page directory = 340 GiB
 */
#define PMA_SNAPSHOT_FILENAME "snap.bin"
#define PMA_PAGE_DIR_FILENAME "page.bin"
#define PMA_DEFAULT_DIR_NAME  ".bin"
#define PMA_FILE_FLAGS        (O_RDWR | O_CREAT)
#define PMA_DIR_PERMISSIONS   (S_IRWXU | S_IRWXG | S_IRWXO)
#define PMA_FILE_PERMISSIONS  (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define PMA_INIT_SNAP_SIZE    1073741824
#define PMA_INIT_DIR_SIZE     4194304

// TODO
#define PMA_MAXIMUM_DIR_SIZE  365072220160

// TODO
#define PMA_SNAPSHOT_ADDR     0x10000

/**
 * 4 GiB in bytes
 *
 * TODO: documentation
 */
#define PMA_SNAP_RESIZE_INC   4294967296

/**
 * Logging helper macros
 */
#define INIT_ERROR            { err_line = __LINE__; goto init_error; }
#define LOAD_ERROR            { err_line = __LINE__; goto load_error; }
#define ERROR(foo)            _pma_error(foo, address, __LINE__)
#define WARNING(foo)          _pma_warning(foo, address, __LINE__)

//==============================================================================
// TYPES
//==============================================================================

/**
 * Page statuses used in page directory
 */
typedef enum _pma_page_status_t {
  UNALLOCATED,
  FREE,
  SHARED,
  FIRST,
  FOLLOW
} PageStatus;

/**
 * Directory entry for a page in virtual memory
 */
typedef struct _pma_page_dir_entry_t {
  uint64_t    offset; // Offset for page in backing file
  PageStatus  status; // Status of page
} PageDirEntry;

/**
 * Directory of pages in virtual memory
 */
typedef struct _pma_page_dir_t {
  uint64_t      size;         // Number of slots currently supported by page directory
  uint64_t      next_index;   // Index of next open slot in (makes it easier to resize)
  PageDirEntry *entries;      // Address to start of page directory as an array of entries
} PageDir;

/**
 * Shared allocation page
 *
 * A shared page is an array of slots of a single size. The metadata for each
 * page is stored as a header within the page itself.
 *
 * On a 64-bit system, the alignment of this struct is 8. This is relevant to
 * the currently hard-coded values for simplifying how slots work. The ideal
 * size of a hard-coded bitmap, given the number of slots available in a page
 * after subtracting the header, is 32 bytes:
 *
 *    X = max # slots in page (16-byte slots)
 *    (4096 - (11 + ceil(X/8))) > 16X
 *    (4096 - (11 + (X/8) + 1)) > 16X
 *                   4084 - X/8 > 16X
 *                    32672 - X > 128X
 *                        32672 > 129X
 *                       253.27 > X
 *                            X = 253
 *    bitmap bytes = ceil(253 div 8) = ceil(31.625) = 32
 *
 * However, the alignment adds padding bytes in between the scalar and array
 * struct members. In this case, this doesn't affect the total number of
 * available slots, but it could if the members of the SharedPageHeader change.
 */
typedef struct _pma_shared_page_t {
  struct _pma_shared_page_t  *next;      // Next shared page; forms a stack as additional pages of the same slot size are allocated
  uint8_t                     dirty;
//   void                      *page;    // Pointer to the first data slot      // TODO: but then why does shift exist?
  uint8_t                     size;      // Slot size for this page = 2^size
//  uint8_t                     shift;   // Byte shift for actual slot data     // TODO: don't need this if we hardcode min size as min alloc as 16U
//  uint8_t                     total;   // Total number of slots in page       // TODO: can't actually find a use for this info, other than sanity asserts
  uint8_t                     free;      // Number of free slots in page
  uint8_t                     bits[32];  // Bitmap of which slots are free
} SharedPageHeader;

/**
 * TODO: Documentation
 */
typedef struct _pma_dirty_page_entry_t {
  uint64_t    index;      // TODO
  uint64_t    offset;     // TODO
  uint32_t    num_pages;  // Total number of pages marked dirty
  PageStatus  status;     // TODO
} DirtyPageEntry;

/**
 * Free page run node
 *
 * Nodes form a bidirectional linked list storing info about runs of free pages
 * (pages mmapped to disk, but with nothing allocated on them).
 *
 * TODO: Documentation
 */
typedef struct _pma_single_page_cache_t {
  struct _pma_single_page_cache_t  *next;   // Next node in list
  void                             *page;   // Pointer to free page
} SinglePageCache;

/**
 * Free page run node
 *
 * Nodes form a bidirectional linked list storing info about runs of free pages
 * (pages mmapped to disk, but with nothing allocated on them).
 *
 * TODO: Documentation
 */
typedef struct _pma_page_run_cache_t {
  struct _pma_page_run_cache_t *next;     // Next node in list
  void                         *page;     // Pointer to start of page run
  uint64_t                      length;   // Number of pages in run
} PageRunCache;

/**
 * Free dpage run node
 *
 * Nodes form a bidirectional linked list storing info about runs of free dpages
 * (page-sized blocks already allocated to the snapshot file on disk but with no
 * memory mmapped to them).
 *
 * It's possible to simplify this cache by turning it into a stack of individual
 * free dpages. However, since multi-page allocations will *never* move,
 * allocating them in a single block not only simplifies the malloc algorithm,
 * but also allows us to take advantage of locality caching: typically, when the
 * OS experiences a page miss, the OS/hardware will fetch not just the missing
 * page, but also several of the following (nearby?) pages.
 *
 * TODO: Documentation
 * TODO: size needs to be updated during metadata sync; it stores the "safe" size of the entries
 */
typedef struct _pma_free_dpage_cache_t {
  uint16_t  dirty;                        // Has dpage cache already been copied to a new page with PROT_WRITE
  uint16_t  size;                         // Number of entries in queue
  uint16_t  head;                         // Index of front of queue
  uint16_t  tail;                         // Index of back of queue
  uint64_t  queue[PMA_DPAGE_CACHE_SIZE];  // Cache of free dpages as queue
} DPageCache;

/**
 * Persistent Memory Arena/event snapshot metadata
 */
typedef struct _pma_metadata_t {
  uint64_t          magic_code;       // Stamp identifying a file as a New Mars PMA file
  uint32_t          checksum;         // TODO
  uint32_t          version;          // Version of Vere (New Mars?) used to produce the backing file
  uint64_t          epoch;            // Epoch ID of the most recently processed event
  uint64_t          event;            // ID of the most recently processed event
  void             *arena_start;      // Beginning of mapped address space
  void             *arena_end;        // End of mapped address space (first address beyond mapped range)
  SharedPageHeader *shared_pages[PMA_MAX_SHARED_SHIFT]; // Shared allocation pages
  DPageCache       *dpage_cache;      // TODO
  uint64_t          snapshot_size;    // TODO
  uint64_t          next_offset;      // TODO
  uint8_t           num_dirty_pages;  // TODO
  DirtyPageEntry    dirty_pages[PMA_DIRTY_PAGE_LIMIT];  // Array of changes not yet synced to page directory (exactly 164)
} Metadata;

/**
 * Struct containing global data used by PMA
 *
 * TODO: Documentation
 */
typedef struct _pma_global_state_t {
  Metadata          metadata;         // Metadata for current status of event snapshot
  uint64_t          meta_page_offset; // TODO
  PageDir           page_directory;   // TODO
  int               snapshot_fd;      // File descriptor for PMA backing file
  int               page_dir_fd;      // TODO
  SinglePageCache  *free_pages;       // TODO
  PageRunCache     *free_page_runs;   // TODO
} State;

//==============================================================================
// FORWARD DECLARATIONS
//==============================================================================

int       _pma_verify_checksum(Metadata *meta_page);
int       _pma_sync_dirty_pages(int fd, uint8_t num_dirty_pages, DirtyPageEntry *dirty_pages);
int       _pma_write_page_status(int fd, uint64_t index, PageStatus status);
int       _pma_write_page_offset(int fd, uint64_t index, uint64_t offset);
int       _pma_update_free_pages(uint8_t num_dirty_pages, DirtyPageEntry *dirty_pages);
void     *_pma_malloc_bytes(size_t size);
int       _pma_malloc_shared_page(uint8_t bucket);
void     *_pma_malloc_pages(size_t size);
void     *_pma_malloc_single_page(PageStatus status);
void     *_pma_malloc_multi_pages(uint64_t num_pages);
void     *_pma_get_cached_pages(uint64_t num_pages);
void     *_pma_get_new_page(PageStatus status);
void     *_pma_get_new_pages(uint64_t num_pages);
void      _pma_free_pages(void *address);
void      _pma_free_bytes(void *address);
int       _pma_copy_shared_page(void *address);
uint64_t  _pma_get_single_dpage(void);
uint64_t  _pma_get_cached_dpage(void);
int       _pma_copy_dpage_cache(void);
uint64_t  _pma_get_disk_dpage(void);
void      _pma_copy_page(void *address, uint64_t offset, PageStatus status, int fd);  // TODO: return pass/fail boolean?
void      _pma_mark_page_dirty(uint64_t index, uint64_t offset, PageStatus status, uint32_t num_pages);
int       _pma_extend_snapshot_file(uint64_t multiplier);
void      _pma_error(const char *p, void *a, int l);
void      _pma_warning(const char *p, void *a, int l);

//==============================================================================
// GLOBALS
//==============================================================================

State *_pma_state = NULL;

//==============================================================================
// PUBLIC FUNCTIONS
//==============================================================================

int
pma_init(const char *path) {
  DIR      *dir;
  char     *filepath;
  void     *meta_pages;
  void     *page_dir;
  uint64_t  meta_bytes;
  int       err;
  int       err_line;
  int       page_dir_fd = 0;
  int       snapshot_fd = 0;

  //
  // Set up
  //

  // Only init once
  if (_pma_state != NULL) {
    return 0;
  }

  // Precompute metadata and page directory sizes in bytes
  meta_bytes = 2 * PMA_PAGE_SIZE;

  // Allocate memory for state
  _pma_state = malloc(sizeof(State));

  //
  // Create backing files
  //

  // Initialize dir and file path buffer
  filepath = malloc(
      strlen(path) + 1 +
      strlen(PMA_DEFAULT_DIR_NAME) + 1 +
      strlen(PMA_SNAPSHOT_FILENAME) + 1);

  // Create input directory, if necessary
  dir = opendir(path);
  if (dir == NULL) {
    // Error if opening dir failed for reason other than it doesn't exist
    if (ENOENT != errno) INIT_ERROR;

    // Error if creating dir failed
    if (mkdir(path, PMA_DIR_PERMISSIONS)) INIT_ERROR;
  }

  // Create file path for dir of backing files
  sprintf(filepath, "%s/%s", path, PMA_DEFAULT_DIR_NAME);

  // Create dir for backing files
  if (mkdir(filepath, PMA_DIR_PERMISSIONS)) INIT_ERROR;

  // Create backing file for snapshot
  sprintf(filepath, "%s/%s/%s", path, PMA_DEFAULT_DIR_NAME, PMA_SNAPSHOT_FILENAME);
  snapshot_fd = open(filepath, PMA_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  if (snapshot_fd == -1) INIT_ERROR;

  // Create backing file for page directory
  sprintf(filepath, "%s/%s/%s", path, PMA_DEFAULT_DIR_NAME, PMA_PAGE_DIR_FILENAME);
  page_dir_fd = open(filepath, PMA_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  if (page_dir_fd == -1) INIT_ERROR;

  //
  // Set initial sizes for backing files
  //

  // Set initial size of snapshot file
  err = lseek(snapshot_fd, (PMA_INIT_SNAP_SIZE - 1), SEEK_SET);
  if (err == -1) INIT_ERROR;
  err = write(snapshot_fd, "", 1);
  if (err != 1) INIT_ERROR;

  // Set initial size of page directory
  err = lseek(page_dir_fd, (PMA_INIT_DIR_SIZE - 1), SEEK_SET);
  if (err == -1) INIT_ERROR;
  err = write(page_dir_fd, "", 1);
  if (err != 1) INIT_ERROR;

  //
  // Initialize snapshot and page directory
  //

  /*
   * The following links are useful for understanding the layout of virtual memory for a Linux process:
   *    https://www.sobyte.net/post/2022-08/linux-virtual-memory/
   *    https://blog.holbertonschool.com/hack-the-virtual-memory-malloc-the-heap-the-program-break/
   *        Chapters 2 & 3
   *
   * Practically, on my machine, this translates to the following virtual memory layout:
   *    - ???   = 0x0000 0000 0000  -  0x0000 0000 ffff      64 KiB
   *    - empty = 0x0000 0001 0000  -  0x559f ffff ffff     ~85 TiB
   *    - data  = 0x55a0 0000 0000  -  0x560f ffff ffff     448 GiB
   *    - heap  = 0x5610 0000 0000  -  0x7f3f ffff ffff     ~41 TiB
   *    - libs  = 0x7f40 0000 0000  -  0x7f9f ffff ffff     384 GiB
   *    - stack = 0x7fa0 0000 0000  -  0x7ffb ffff ffff     368 GiB
   *    - vdso  = 0x7ffc 0000 0000  -  0x7fff ffff ffff      16 GiB
   * Note that these address ranges are rough approximations and the sizes are vastly larger for sections like 'data'
   * and 'vdso' than the actual memory section for the process because I'm documenting the range in which the section
   * can be found. Identical Linux processes will not have identical memory layouts due to Address Space Layout
   * Randomization.
   *
   * Without explicit arguments, calls to mmap will return addresses in the above 'stack' range, and successive calls
   * will grow down. I presume that this is due to the implementation of this proposal: https://lwn.net/Articles/91829/
   *
   * Given these circumstances, probably the easiest things to do are:
   *  1.  mmap the snapshot to a low address (i.e. 0x1 0000) so that it can use all of the available space before the
   *      'data' section
   *  2.  mmap the page directory using its maximum possible size (at least on Linux, it's okay to mmap a file to more
   *      pages than it actually occupies and have it grow into the space). Doing so on eliminates the need to ever
   *      resize the mapping using mremap.
   *  3.  mmap the page directory without a location hint. How big is this mmap? Given the above estimate of virtual
   *      memory available to the snapshot (85 TiB) and the ratio of snapshot size to page directory size (256:1), this
   *      mapping would be 340 GiB in size. Even assuming the kernel were not smart enough to work around the linked
   *      libs, this is still small enough to fit into the stack, according to the above memory section size estimates.
   */

  // Init metadata pages
  meta_pages = mmap(
      NULL,
      meta_bytes,
      PROT_READ | PROT_WRITE,
      MAP_SHARED,
      snapshot_fd,
      0);
  if (meta_pages == MAP_FAILED) INIT_ERROR;

  // Init page directory
  page_dir = mmap(
      NULL,
      PMA_MAXIMUM_DIR_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED,
      page_dir_fd,
      0);
  if (page_dir == MAP_FAILED) INIT_ERROR;

  //
  // Setup metadata
  //

  // Initialize simple metadata state
  _pma_state->metadata.magic_code = PMA_MAGIC_CODE;
  _pma_state->metadata.checksum   = 0;
  _pma_state->metadata.version    = PMA_DATA_VERSION;
  _pma_state->metadata.epoch      = 0;
  _pma_state->metadata.event      = 0;

  // Initialize shared pages stacks
  for(uint8_t i = 0; i < PMA_MAX_SHARED_SHIFT; ++i) {
    _pma_state->metadata.shared_pages[i] = NULL;
  }

  // Initialize dirty page array
  for(uint8_t i = 0; i < PMA_DIRTY_PAGE_LIMIT; ++i) {
    _pma_state->metadata.dirty_pages[i].index     = 0;
    _pma_state->metadata.dirty_pages[i].offset    = 0;
    _pma_state->metadata.dirty_pages[i].num_pages = 0;
  }
  _pma_state->metadata.num_dirty_pages = 0;

  // Initialize snapshot page info
  _pma_state->metadata.snapshot_size  = PMA_INIT_SNAP_SIZE;
  _pma_state->metadata.next_offset    = meta_bytes;

  // Initialize arena start pointer
  _pma_state->metadata.arena_start  = (void *)PMA_SNAPSHOT_ADDR;

  // Manually allocate a page for the dpage cache
  _pma_state->metadata.dpage_cache = mmap(
      _pma_state->metadata.arena_start,
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED_NOREPLACE,
      snapshot_fd,
      meta_bytes);
  if (_pma_state->metadata.dpage_cache == MAP_FAILED) INIT_ERROR;

  // Initialize arena end pointer
  _pma_state->metadata.arena_end = (void*)((char*)_pma_state->metadata.arena_start + PMA_PAGE_SIZE);

  // Setup initial dpage cache values
  _pma_state->metadata.dpage_cache->dirty = 0;
  _pma_state->metadata.dpage_cache->size  = 0;
  _pma_state->metadata.dpage_cache->head  = 0;
  _pma_state->metadata.dpage_cache->tail  = 0;

  //
  // Setup page directory
  //

  _pma_state->page_directory.size       = PMA_INIT_DIR_SIZE;
  _pma_state->page_directory.next_index = 1;
  _pma_state->page_directory.entries    = (PageDirEntry *)page_dir;

  // First page used by dpage cache
  _pma_state->page_directory.entries[0].status = FIRST;

  //
  // Setup transient state
  //

  // Replace the first metadata page, since they're identical
  _pma_state->meta_page_offset = 0;

  // Initialize file descriptors
  _pma_state->snapshot_fd = snapshot_fd;
  _pma_state->page_dir_fd = page_dir_fd;

  // Initialize free page caches
  _pma_state->free_pages      = NULL;
  _pma_state->free_page_runs  = NULL;

  //
  // Sync initial PMA state to disk
  //

  // Sync dpage cache
  err = msync(
      _pma_state->metadata.dpage_cache,
      PMA_PAGE_SIZE,
      MS_SYNC);
  if (err) INIT_ERROR;

  // Sync page directory
  err = msync(
      (void *)_pma_state->page_directory.entries,
      PMA_PAGE_SIZE,
      MS_SYNC);
  if (err) INIT_ERROR;

  // Compute checksum for metadata
  _pma_state->metadata.checksum = crc_32(
      (const unsigned char *)(&(_pma_state->metadata)),
      PMA_PAGE_SIZE);

  // Copy and sync metadata to both buffers
  memcpy(
    meta_pages,
    (const void *)(&(_pma_state->metadata)),
    PMA_PAGE_SIZE);
  memcpy(
    (void *)((Metadata*)meta_pages + 1),
    (const void *)(&(_pma_state->metadata)),
    PMA_PAGE_SIZE);
  if (msync(meta_pages, meta_bytes, MS_SYNC)) INIT_ERROR;

  // Remove PROT_WRITE permissions from snapshot and page directory
  if (mprotect(meta_pages, meta_bytes, PROT_READ)) INIT_ERROR;
  if (mprotect(_pma_state->metadata.dpage_cache, PMA_PAGE_SIZE, PROT_READ)) INIT_ERROR;
  if (mprotect(page_dir, PMA_PAGE_SIZE, PROT_READ)) INIT_ERROR;

  //
  // Done
  //

  // Clean up
  free((void*)filepath);
  munmap(meta_pages, meta_bytes);

  return 0;

init_error:
  err = errno;
  fprintf(stderr, "(L%d) Initialization error: %s\n", err_line, strerror(errno));

  munmap(meta_pages, meta_bytes);
  munmap(page_dir, PMA_INIT_DIR_SIZE);
  if (snapshot_fd) close(snapshot_fd);
  if (page_dir_fd) close(page_dir_fd);
  free((void*)filepath);

  exit(err);
}

int
pma_load(const char *path) {
  Metadata     *newer_page;
  Metadata     *older_page;
  char         *filepath;
  void         *address;
  void         *meta_pages;
  uint64_t      index;
  uint64_t      meta_bytes;
  int           err;
  int           err_line;
  int           page_dir_fd = 0;
  int           snapshot_fd = 0;

  //
  // Set up
  //

  // Only init once
  if (_pma_state != NULL) {
    return 0;
  }

  // Precompute metadata and page directory sizes in bytes
  meta_bytes = 2 * PMA_PAGE_SIZE;

  // Allocate memory for state
  _pma_state = malloc(sizeof(State));

  //
  // Create backing files
  //

  // Initialize dir and file path buffer
  filepath = malloc(
      strlen(path) + 1 +
      strlen(PMA_DEFAULT_DIR_NAME) + 1 +
      strlen(PMA_SNAPSHOT_FILENAME) + 1);

  // Open backing file for snapshot
  sprintf(filepath, "%s/%s/%s", path, PMA_DEFAULT_DIR_NAME, PMA_SNAPSHOT_FILENAME);
  snapshot_fd = open(filepath, PMA_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  if (snapshot_fd == -1) LOAD_ERROR;

  // Open backing file for page directory
  sprintf(filepath, "%s/%s/%s", path, PMA_DEFAULT_DIR_NAME, PMA_PAGE_DIR_FILENAME);
  page_dir_fd = open(filepath, PMA_FILE_FLAGS, PMA_FILE_PERMISSIONS);
  if (page_dir_fd == -1) LOAD_ERROR;

  //
  // Verify file can be loaded
  //

  // Read magic code
  err = read(snapshot_fd, (void*)(&_pma_state->metadata.magic_code), sizeof(uint64_t));
  if ((err != -1) || (_pma_state->metadata.magic_code != PMA_MAGIC_CODE)) {
    errno = EILSEQ; // TODO: EPROTO instead?
    LOAD_ERROR;
  }

  // Load metadata pages
  meta_pages = mmap(
      NULL,
      meta_bytes,
      PROT_READ,
      MAP_SHARED,
      snapshot_fd,
      0);
  if (meta_pages == MAP_FAILED) LOAD_ERROR;

  // Determine newer metadata page
  newer_page = (Metadata*)meta_pages;
  older_page = (Metadata*)((char*)meta_pages + PMA_PAGE_SIZE);
  if (
      (newer_page->epoch > older_page->epoch) ||
      ((newer_page->epoch == older_page->epoch) && (newer_page->event > older_page->event))) {
    newer_page = older_page;
    older_page = (Metadata*)meta_pages;
  }

  // Verify checksum for either page
  if (!_pma_verify_checksum(newer_page)) {
    if (_pma_verify_checksum(older_page)) {
      newer_page = older_page;
    } else {
      errno = EILSEQ;
      LOAD_ERROR;
    }
  }

  // Next page replaced is the older of the two pages
  _pma_state->meta_page_offset = (newer_page == meta_pages) ? PMA_PAGE_SIZE : 0;

  // Update page directory using metadata dirty page list
  err = _pma_sync_dirty_pages(page_dir_fd, _pma_state->metadata.num_dirty_pages, _pma_state->metadata.dirty_pages);
  if (err) LOAD_ERROR;

  _pma_state->metadata.num_dirty_pages = 0;

  //
  // Load page directory
  //

  // mmap page directory
  _pma_state->page_directory.entries = mmap(
      NULL,
      PMA_MAXIMUM_DIR_SIZE,
      PROT_READ,
      MAP_SHARED,
      page_dir_fd,
      0);
  if (_pma_state->page_directory.entries == MAP_FAILED) LOAD_ERROR;

  //
  // Map pages and compute free page caches
  //

  index = 0;
  while (1) {
    struct stat   st;
    uint64_t      count = 1;

    switch (_pma_state->page_directory.entries[index].status) {
      case UNALLOCATED:
        ++index;
        continue;

      case FREE:
        // While pages have FREE status AND are contiguous on disk, scan forward
        ++index;
        while (
            _pma_state->page_directory.entries[index].status == FREE &&
            _pma_state->page_directory.entries[index].offset == (_pma_state->page_directory.entries[index - 1].offset + PMA_PAGE_SIZE)) {
          ++count;
          ++index;
        }

        // Add to appropriate free page cache
        if (count == 1) {
          SinglePageCache *free_page = (SinglePageCache *)malloc(sizeof(SinglePageCache));

          // Add it to the single-page cache
          free_page->next = _pma_state->free_pages;
          free_page->page = INDEX_TO_PTR(index - 1);
          _pma_state->free_pages = free_page;

        } else {
          PageRunCache *page_run = (PageRunCache *)malloc(sizeof(SinglePageCache));

          page_run->next = _pma_state->free_page_runs;
          page_run->page = INDEX_TO_PTR(index - count);
          page_run->length = count;
          _pma_state->free_page_runs = page_run;
        }

        // Map free pages (they're expected to be mapped but read only)
        address = mmap(
            INDEX_TO_PTR(index - count),
            (PMA_PAGE_SIZE * count),
            PROT_READ,
            MAP_SHARED | MAP_FIXED_NOREPLACE,
            page_dir_fd,
            _pma_state->page_directory.entries[index - count].offset);

        continue;

      case SHARED:
        // Map immediately
        address = mmap(
            INDEX_TO_PTR(index),
            PMA_PAGE_SIZE,
            PROT_READ,
            MAP_SHARED | MAP_FIXED_NOREPLACE,
            page_dir_fd,
            _pma_state->page_directory.entries[index].offset);
        if (address == MAP_FAILED) LOAD_ERROR;

        ++index;

        continue;

      case FIRST:
        // While pages have FOLLOW status, scan forward
        ++index;
        while (_pma_state->page_directory.entries[index].status == FOLLOW) {
          // XX
          assert(_pma_state->page_directory.entries[index].offset == (_pma_state->page_directory.entries[index - 1].offset + PMA_PAGE_SIZE));

          ++count;
          ++index;
        }

        // mmap entire block
        address = mmap(
            INDEX_TO_PTR(index - count),
            (count * PMA_PAGE_SIZE),
            PROT_READ,
            MAP_SHARED | MAP_FIXED_NOREPLACE,
            page_dir_fd,
            _pma_state->page_directory.entries[index - count].offset);
        if (address == MAP_FAILED) LOAD_ERROR;

        continue;

      case FOLLOW:
        // FOLLOW pages should be passed over correctly by FIRST case
      default:
        fprintf(stderr, "Index %lu invalid\n", index);
        errno = EINVAL;
        LOAD_ERROR;
    }

    // Get next free index
    _pma_state->page_directory.next_index = index;

    // Get total number of indices
    fstat(page_dir_fd, &st);
    _pma_state->page_directory.size = ((st.st_size / sizeof(PageDirEntry)) - 1);

    break;
  }

  //
  // Done
  //

  // XX: Check version number, possibly upgrade

  // Clean up
  munmap(meta_pages, meta_bytes);
  free((void*)filepath);

  return 0;

load_error:
  err = errno;
  fprintf(stderr, "(L%d) Error loading from %s: %s\n", err_line, path, strerror(errno));

  munmap(meta_pages, meta_bytes);
  munmap(_pma_state->page_directory.entries, PMA_MAXIMUM_DIR_SIZE);
  munmap(_pma_state->metadata.arena_start, ((uint64_t)_pma_state->metadata.arena_end - (uint64_t)_pma_state->metadata.arena_start));
  if (snapshot_fd) close(snapshot_fd);
  if (page_dir_fd) close(page_dir_fd);
  free((void*)filepath);

  exit(err);
}

int
pma_close(uint64_t epoch, uint64_t event) {
  // Sync changes to disk
  if (pma_sync(epoch, event)) {
    return -1;
  }

  // Unmap page directory
  munmap(_pma_state->page_directory.entries, PMA_MAXIMUM_DIR_SIZE);

  // Unmap snapshot
  munmap(_pma_state->metadata.arena_start, _pma_state->metadata.snapshot_size);

  // Close file descriptors
  close(_pma_state->page_dir_fd);
  close(_pma_state->snapshot_fd);

  return 0;
}

void *
pma_malloc(size_t size) {
  void *result = NULL;

  /* MALLOC_LOCK */

  if (!size) {
    /* MALLOC_UNLOCK */
    return result;
  } else if ((size + PMA_PAGE_SIZE) < size) {   // Check for overflow
    errno = ENOMEM;
  } else if (size <= PMA_MAX_SHARED_ALLOC) {
    result = _pma_malloc_bytes(size);
  } else {
    result = _pma_malloc_pages(size);
  }

  /* MALLOC_UNLOCK */

  // TODO: Document that this could be NULL but errno should be set
  return result;
}

void
pma_free(void *address) {
  uint64_t  index;

  // TODO: This is legal for POSIX free, but would this ever happen for pma_free?
  if (address == NULL) return;

  // TODO: Should these error?
  if (address < _pma_state->metadata.arena_start) {
    WARNING("address too low to make sense");
    return;
  }
  if (address >= _pma_state->metadata.arena_end) {
    WARNING("address too high to make sense");
    return;
  }
  if (address >= _pma_state->metadata.arena_end) {
    WARNING("address was never allocated");
    return;
  }

  index = PTR_TO_INDEX(address);

  switch (_pma_state->page_directory.entries[index].status) {
    case UNALLOCATED:
      // Something has definitely gone wrong if an address between arena_start
      // and arena_end, with an index between 0 and next_free_index is
      // unallocated
      ERROR("address marked unallocated");
      break;

    case FREE:
      WARNING("address already free");
      break;

    case SHARED:
      _pma_free_bytes(address);
      break;

    case FIRST:
      _pma_free_pages(address);
      break;

    case FOLLOW:
      ERROR("address points to middle of multi-page allocation");
      break;
  }
}

int
pma_sync(uint64_t epoch, uint64_t event) {
  DPageCache *dpage_cache = _pma_state->metadata.dpage_cache;
  ssize_t     bytes_out;
  int         err;

  // Epoch & event may only increase
  if (
      (epoch < _pma_state->metadata.epoch) ||
      ((epoch == _pma_state->metadata.epoch) && (event <= _pma_state->metadata.event))) {
    errno = EINVAL;
    goto sync_error;
  }

  // Clear dpage cache dirty bit and compute new size
  if (dpage_cache->dirty) {
    dpage_cache->dirty = 0;
    dpage_cache->size = (dpage_cache->tail - dpage_cache->head);
  }

  // Sync dirty pages
  for (uint8_t i = 0; i < _pma_state->metadata.num_dirty_pages; ++i) {
    void     *address = INDEX_TO_PTR(_pma_state->metadata.dirty_pages[i].index);
    uint64_t  bytes = (_pma_state->metadata.dirty_pages[i].num_pages * PMA_PAGE_SIZE);

    // Clear dirty bit for shared pages
    if (_pma_state->metadata.dirty_pages[i].status == SHARED) {
      ((SharedPageHeader*)address)->dirty = 0;
    }

    err = msync(address, bytes, MS_SYNC);
    if (err) goto sync_error;

    if (mprotect(address, bytes, PROT_READ)) goto sync_error;
  }

  // Compute checksum
  _pma_state->metadata.epoch = epoch;
  _pma_state->metadata.event = event;
  _pma_state->metadata.checksum = 0;
  _pma_state->metadata.checksum = crc_32(
      (const unsigned char *)(&(_pma_state->metadata)),
      PMA_PAGE_SIZE);

  // Sync metadata
  bytes_out = pwrite(
      _pma_state->snapshot_fd,
      (const void *)(&(_pma_state->metadata)),
      PMA_PAGE_SIZE,
      _pma_state->meta_page_offset);
  if (bytes_out != PMA_PAGE_SIZE) goto sync_error;

  _pma_state->meta_page_offset = _pma_state->meta_page_offset ? 0 : PMA_PAGE_SIZE;

  // Sync dirty pages in page directory
  err = _pma_sync_dirty_pages(
      _pma_state->page_dir_fd,
      _pma_state->metadata.num_dirty_pages,
      _pma_state->metadata.dirty_pages);
  if (err) goto sync_error;

  // Update free page caches
  err = _pma_update_free_pages(_pma_state->metadata.num_dirty_pages, _pma_state->metadata.dirty_pages);
  if (err) goto sync_error;

  // Reset dirty page array
  _pma_state->metadata.num_dirty_pages = 0;

  // TODO: Should sync failure be a fatal error?
  return 0;

sync_error:
  return -1;
}

//==============================================================================
// PRIVATE FUNCTIONS
//==============================================================================

int
_pma_verify_checksum(Metadata *meta_page) {
  uint32_t checksum;

  // Copy metadata in advance of using it, since: 1) we expect the checksum to be valid; 2) we need to set the value of
  // the checksum in the metadata to 0.
  memcpy(
      (void*)(&(_pma_state->metadata)),
      (const void *)meta_page,
      PMA_PAGE_SIZE);

  // Since we're computing the checksum on the object which itself includes the checksum, we treat the checksum as 0.
  _pma_state->metadata.checksum = 0;

  // Compute checksum
  checksum = crc_32(
      (const unsigned char *)(&(_pma_state->metadata)),
      PMA_PAGE_SIZE);

  // Compare checksums
  return (checksum == _pma_state->metadata.checksum);
}

int
_pma_sync_dirty_pages(int fd, uint8_t num_dirty_pages, DirtyPageEntry *dirty_pages) {
  PageStatus  cont_status;
  uint64_t    init_offset;
  uint64_t    index;

  for (uint8_t i = 0; i < num_dirty_pages; ++i) {
    cont_status = (dirty_pages[i].status == FIRST) ? FOLLOW : dirty_pages[i].status;
    init_offset = dirty_pages[i].offset;
    index = dirty_pages[i].index;

    if (_pma_write_page_status(fd, index, dirty_pages[i].status)) goto sync_error;
    // Offset of 0 is code for "leave it alone"
    if (init_offset) {
      if (_pma_write_page_offset(fd, index, init_offset)) goto sync_error;
    }

    // The offset on disk doesn't actually matter for the continuation pages of a multi-page allocation, but it does
    // matter for free page runs: just because two page runs are contiguous in memory, it doesn't mean they are
    // contiguous on disk. An order of events like:
    //
    //    [multi-page allocation] -> [shared-page allocation] -> [multi-page allocation]
    //
    // could produce a situation where the two multi-page allocations are adjacent in memory, but separated by one
    // page on disk (because of copy-on-write using a new dpage during the shared page allocation).
    for (uint32_t j = 1; j < dirty_pages[i].num_pages; ++j) {
      // XX
      assert((dirty_pages[i].status == FIRST) || (cont_status == FREE));

      if (_pma_write_page_status(fd, (index + j), cont_status)) goto sync_error;
      // Offset of 0 is code for "leave it alone"
      if (init_offset) {
        if (_pma_write_page_offset(fd, index, (init_offset + (j * PMA_PAGE_SIZE)))) goto sync_error;
      }
    }
  }

  return 0;

sync_error:
  return -1;
}

int
_pma_write_page_status(int fd, uint64_t index, PageStatus status) {
  ssize_t bytes_out;

  bytes_out = pwrite(
      fd,
      (const void *)&status,
      sizeof(PageStatus),
      ((index * sizeof(PageDirEntry)) + sizeof(uint64_t)));

  if (bytes_out != sizeof(PageStatus)) {
    return -1;
  }

  return 0;
}

int
_pma_write_page_offset(int fd, uint64_t index, uint64_t offset) {
  ssize_t bytes_out;

  bytes_out = pwrite(
      fd,
      (const void *)&offset,
      sizeof(uint64_t),
      (index * sizeof(PageDirEntry)));

  if (bytes_out != sizeof(uint64_t)) {
    return -1;
  }

  return 0;
}

int
_pma_update_free_pages(uint8_t num_dirty_pages, DirtyPageEntry *dirty_pages) {
  SinglePageCache  *free_page;
  PageRunCache     *page_run;

  // TODO: Pull out common code between here and pma_load
  for (uint8_t i = 0; i < num_dirty_pages; ++i) {
    if (dirty_pages[i].status != FREE) continue;

    if (dirty_pages[i].num_pages > 1) {
      page_run = (PageRunCache *)malloc(sizeof(SinglePageCache));
      if (page_run == NULL) return -1;

      page_run->next = _pma_state->free_page_runs;
      page_run->page = INDEX_TO_PTR(dirty_pages[i].index);
      page_run->length = dirty_pages[i].num_pages;
      _pma_state->free_page_runs = page_run;

    } else {
      free_page = (SinglePageCache *)malloc(sizeof(SinglePageCache));
      if (free_page == NULL) return -1;

      free_page->next = _pma_state->free_pages;
      free_page->page = INDEX_TO_PTR(dirty_pages[i].index);
      _pma_state->free_pages = free_page;
    }
  }

  return 0;
}

/**
 * Allocate memory within a shared allocation page.
 *
 * @param size  Size in bytes to allocate (must be <= 1/4 page)
 *
 * @return  The address of the newly allocated memory
 */
void *
_pma_malloc_bytes(size_t size)
{
  SharedPageHeader *shared_page;
  uint16_t          i, slot_size;
  uint8_t           bucket, byte, bit;

  // XX
  assert(size <= PMA_MAX_SHARED_ALLOC);

  // Don't bother with anything less than the minimum allocation size
  if (size < PMA_MIN_ALLOC_SIZE) {
    size = PMA_MIN_ALLOC_SIZE;
  }

  // Find the right bucket
  bucket = 0;
  i = size - 1;
  while (i >>= 1) bucket++;
  slot_size = (1 << (bucket + 1));

  // Search for a shared page with open slots
  shared_page = _pma_state->metadata.shared_pages[bucket];
  while ((shared_page != NULL) && (shared_page->free == 0)) {
    shared_page = shared_page->next;
  }

  // Make a new shared page if necessary
  if (shared_page == NULL) {
    if (_pma_malloc_shared_page(bucket)) {
      return NULL;
    }

    shared_page = _pma_state->metadata.shared_pages[bucket];

  } else {
    if (_pma_copy_shared_page((void *)shared_page)) {
      return NULL;
    }
  }

  // XX
  assert(shared_page->free);

  // Find first empty slot using bitmap (1 = empty, 0 = full)
  byte = 0;
  while (shared_page->bits[byte] == 0) {
    // XX
    assert(byte < PMA_BITMAP_SIZE);
    ++byte;
  }
  i = shared_page->bits[byte];
  bit = 0;
  while (~i & 1U) {
    i >>= 1;
    ++bit;
  }

  // Mark slot full
  shared_page->bits[byte] -= (1 << bit);
  --(shared_page->free);

  // Return slot
  return (void *)(
      (char *)shared_page +
      (sizeof(SharedPageHeader)) +
      (slot_size * ((PMA_BITMAP_BITS * byte) + bit)));
}

/**
 * Allocate a new shared allocation page.
 *
 * @param bucket  Into which bucket in the shared allocation pages array the new
 *                page will go (which also corresponds to the size of the slots
 *                in the page)
 *
 * @return  0 success
 *         -1 failure
 */
int
_pma_malloc_shared_page(uint8_t bucket)
{
  SharedPageHeader *shared_page;

  // Get a new writeable page
  shared_page = (SharedPageHeader *)_pma_malloc_single_page(SHARED);
  if (shared_page == NULL) {
    return -1;
  }

  // Initialize header for shared page
  shared_page->dirty = 1;
  shared_page->size = (bucket + 1);
  shared_page->free = ((PMA_PAGE_SIZE - sizeof(SharedPageHeader)) / (1 << bucket));
  for (uint8_t i = 0; i < PMA_BITMAP_SIZE; ++i) {
    shared_page->bits[i] = PMA_EMPTY_BITMAP;
  }

  // Add new shared page to top of stack
  shared_page->next = _pma_state->metadata.shared_pages[bucket];
  _pma_state->metadata.shared_pages[bucket] = shared_page;

  return 0;
}

/**
 * Allocate memory for a large object in one or more pages.
 *
 * @param size  Size in bytes to allocate (must be > 1/4 page)
 *
 * @return  The address of the newly allocated memory
 */
void *
_pma_malloc_pages(size_t size)
{
  void     *address;
  uint64_t  num_pages;

  // XX
  assert(size > PMA_MAX_SHARED_ALLOC);

  // Round size up to nearest page boundary
  size = PAGE_ROUND_UP(size);
  num_pages = size >> PMA_PAGE_SHIFT;

  if (num_pages == 1) {
    address = _pma_malloc_single_page(FIRST);
  } else {
    address = _pma_malloc_multi_pages(num_pages);
  }

  return address;
}

void *
_pma_malloc_single_page(PageStatus status) {
  void             *address;
  SinglePageCache  *free_page = _pma_state->free_pages;

  // Get an existing free page from cache, if available
  if (free_page != NULL) {
    address = free_page->page;
    _pma_state->free_pages = free_page->next;
    free((void *)free_page);

    // Make the page writeable
    mprotect(address, PMA_PAGE_SIZE, (PROT_READ | PROT_WRITE));

    // Add page to dirty list
    _pma_mark_page_dirty(PTR_TO_INDEX(address), 0, status, 1);
  } else {
    // Otherwise, allocate a new page
    address = _pma_get_new_page(status);
  }

  // XX
  assert((((uint64_t)address) % PMA_PAGE_SIZE) == 0);

  return address;
}

void *
_pma_malloc_multi_pages(uint64_t num_pages) {
  void *address;

  address = _pma_get_cached_pages(num_pages);
  if (!address) {
    address = _pma_get_new_pages(num_pages);
  }

  return address;
}

void *
_pma_get_cached_pages(uint64_t num_pages) {
  PageRunCache *page_run_cache = _pma_state->free_page_runs;
  PageRunCache *prev_page_run  = NULL;
  PageRunCache *valid_page_run = NULL;
  void         *address = NULL;

  // Do a pass looking for an exactly-sized run. While doing this, also record the smallest run still big enough to fit
  // our data.
  while (page_run_cache != NULL) {
    uint64_t run_length = page_run_cache->length;

    if (run_length == num_pages) {
      valid_page_run = page_run_cache;
      break;

    } else if (run_length > num_pages ) {
      if ((valid_page_run == NULL) || (valid_page_run->length > run_length)) {
        valid_page_run = page_run_cache;
      }
    }

    prev_page_run = page_run_cache;
    page_run_cache = page_run_cache->next;
  }

  //  If run found...
  if (valid_page_run != NULL) {
    // Use it
    address = valid_page_run->page;

    // If run larger than necessary by two pages...
    if (valid_page_run->length > (num_pages + 1)) {
      // Reduce it
      valid_page_run->page += (num_pages * PMA_PAGE_SIZE);
      valid_page_run->length -= num_pages;

    // Otherwise...
    } else {
      // Update cache pointers: we're going to use the whole run or we're going
      // to move the remaining page to the single-page cache. Either way, we're
      // going to free the run object.
      prev_page_run->next = valid_page_run->next;

      // If there's a page left...
      if (valid_page_run->length == (num_pages + 1)) {
        SinglePageCache *trailing_page = (SinglePageCache *)malloc(sizeof(SinglePageCache));

        // Add it to the single-page cache
        trailing_page->next = _pma_state->free_pages;
        trailing_page->page = ((char *)address + (num_pages * PMA_PAGE_SIZE));
        _pma_state->free_pages = trailing_page;
      }

      free((void *)valid_page_run);
    }

    // Make pages writeable
    mprotect(address, (num_pages * PMA_PAGE_SIZE), (PROT_READ | PROT_WRITE));

    // Add pages to dirty list
    _pma_mark_page_dirty(PTR_TO_INDEX(address), 0, FIRST, num_pages);
  }

  return address;
}

void *
_pma_get_new_page(PageStatus status) {
  void     *address;
  uint64_t  offset;

  // Get a dpage to which to map the address
  offset = _pma_get_single_dpage();
  if (!offset) {
    return NULL;
  }

  // Try to map next open memory address to dpage
  address = mmap(
      _pma_state->metadata.arena_end,
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED_NOREPLACE,
      _pma_state->snapshot_fd,
      offset);
  if (address == MAP_FAILED) {
    address = _pma_state->metadata.arena_end;
    ERROR("mmap failed");
  }

  // XX
  assert(address == _pma_state->metadata.arena_end);

  // Record PMA expansion
  _pma_state->metadata.arena_end += PMA_PAGE_SIZE;

  // Add page to dirty list
  _pma_mark_page_dirty(PTR_TO_INDEX(address), offset, status, 1);

  return address;
}

void *
_pma_get_new_pages(uint64_t num_pages) {
  void     *address;
  uint64_t  bytes = (num_pages * PMA_PAGE_SIZE);
  uint64_t  offset = _pma_state->metadata.next_offset;
  uint64_t  size = _pma_state->metadata.snapshot_size;
  uint64_t  new_size = (offset + bytes);

  // Get new dpages. Extend snapshot backing file first, if necessary.
  if (new_size >= size) {
    // Multi-page allocations maybe larger than the snapshot resize increment
    uint64_t multiplier = ((new_size - size) / PMA_SNAP_RESIZE_INC) + 1;

    // Fail if snapshot file couldn't be extended
    if (_pma_extend_snapshot_file(multiplier)) return NULL;
  }

  // Try to map dpages to address
  address = mmap(
      _pma_state->metadata.arena_end,
      bytes,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED_NOREPLACE,
      _pma_state->snapshot_fd,
      offset);
  if (address == MAP_FAILED) {
    address = _pma_state->metadata.arena_end;
    ERROR("mmap failed");
  }

  // XX
  assert(address == _pma_state->metadata.arena_end);

  // Update offset of next open dpage
  _pma_state->metadata.next_offset += bytes;
  _pma_state->metadata.arena_end += bytes;

  // Add allocated pages to dirty list
  _pma_mark_page_dirty(PTR_TO_INDEX(address), offset, FIRST, num_pages);

  return address;
}

/**
 * Deallocate one or more pages of allocated memory.
 *
 * @param address   Address of block to deallocated
 *
 * @return  Abort if address does not point to start of page
 */
void
_pma_free_pages(void *address) {

  uint32_t index = PTR_TO_INDEX(address);
  uint32_t num_pages = 0;

  if ((uint64_t)address & PMA_PAGE_MASK) {
    ERROR("address does not point to the root of a page");
    return;
  }

  // XX
  assert(_pma_state->page_directory.entries[index].status == FIRST);

  // Count number of pages in allocation
  do {
    ++num_pages;
  } while (_pma_state->page_directory.entries[index + num_pages].status == FOLLOW);

  // Mark pages dirty
  _pma_mark_page_dirty(index, 0, FREE, num_pages);
}

/**
 * Deallocate a block of memory in a shared allocation page.
 *
 * @param address   Address of block to deallocated
 */
void
_pma_free_bytes(void *address) {
  SharedPageHeader *header = (SharedPageHeader *)((uint64_t)address & (~PMA_PAGE_MASK));
  uint8_t           slot = ((((uint64_t)address & PMA_PAGE_MASK) - sizeof(SharedPageHeader)) / (1 << header->size));
  uint8_t           byte = slot / PMA_BITMAP_BITS;
  uint8_t           bit = slot % PMA_BITMAP_BITS;

  // Copy-on-write
  _pma_copy_shared_page((void *)header);

  if (header->bits[byte] & (1 << bit)) {
    WARNING("bucketized address already free");
    return;
  }

  header->bits[byte] += (1 << bit);
  ++header->free;
}

int
_pma_copy_shared_page(void *address) {
  SharedPageHeader *shared_page;
  uint64_t          offset;

  // Check if page has already been copied
  shared_page = (SharedPageHeader*)address;
  if (shared_page->dirty) {
    return 0;
  }

  offset = _pma_get_single_dpage();
  if (!offset) {
    return -1;
  }

  // Make sure dpage cache is writeable
  if (!_pma_state->metadata.dpage_cache->dirty) {
    if (_pma_copy_dpage_cache()) ERROR("dpage cache copy failed");
  }

  // Copy page
  _pma_copy_page(address, offset, SHARED, _pma_state->snapshot_fd);

  // Mark page dirty so it isn't copied again
  shared_page->dirty = 1;

  return 0;
}

uint64_t
_pma_get_single_dpage(void) {
  uint64_t offset;

  // Get a cached dpage, if one is available
  offset = _pma_get_cached_dpage();
  if (!offset) {
    // Otherwise, get a new dpage from disk
    offset = _pma_get_disk_dpage();
  }

  // XX
  assert((offset % PMA_PAGE_SIZE) == 0);

  return offset;
}

uint64_t
_pma_get_cached_dpage(void) {
  uint64_t offset;
  uint16_t dirty  = _pma_state->metadata.dpage_cache->dirty;
  uint16_t size   = _pma_state->metadata.dpage_cache->size;
  uint16_t head   = _pma_state->metadata.dpage_cache->head;

  // If the cache is empty, or there's only one page in the cache and the cache
  // hasn't been touched yet, then exit early. If the cache hasn't been touched
  // yet, we'll need to copy-on-write the cache as well, so if there's only one
  // page, don't even bother.
  if ((size == 0) || ((size == 1) && !dirty)) {
    return 0;
  }

  // Special copy-on-write for dpage cache
  if (!dirty) {
    if (_pma_copy_dpage_cache()) {
      return 0;
    }
  }

  // TODO: macros for dealing with cache?
  // Pop page off queue
  offset = _pma_state->metadata.dpage_cache->queue[head];
  _pma_state->metadata.dpage_cache->size -= 1;
  _pma_state->metadata.dpage_cache->head = ((head + 1) % PMA_DPAGE_CACHE_SIZE);

  // XX
  assert(_pma_state->metadata.dpage_cache->size != -1);

  return offset;
}

int
_pma_copy_dpage_cache(void) {
  void     *address;
  uint64_t  offset;
  uint16_t  dirty  = _pma_state->metadata.dpage_cache->dirty;
  uint16_t  size   = _pma_state->metadata.dpage_cache->size;
  uint16_t  head   = _pma_state->metadata.dpage_cache->head;

  // Sanity check
  // TODO: throw warning?
  if (dirty) return 0;

  address = (void *)_pma_state->metadata.dpage_cache;

  // If pages available in cache...
  if (size) {
    // Use a page from the cache and record that it was used afterwards
    offset = _pma_state->metadata.dpage_cache->queue[head];

    _pma_copy_page(address, offset, FIRST, _pma_state->snapshot_fd);

    _pma_state->metadata.dpage_cache->size -= 1;
    _pma_state->metadata.dpage_cache->head = ((head + 1) % PMA_DPAGE_CACHE_SIZE);

  } else {
    // Otherwise, get a brand new page from disk
    offset = _pma_get_disk_dpage();
    if (!offset) return -1;

    _pma_copy_page(address, offset, FIRST, _pma_state->snapshot_fd);
  }

  // Mark dpage cache dirty (aka writeable)
  _pma_state->metadata.dpage_cache->dirty = 1;

  return 0;
}

uint64_t
_pma_get_disk_dpage(void) {
  uint64_t offset = _pma_state->metadata.next_offset;
  uint64_t size = _pma_state->metadata.snapshot_size;

  // Get a new dpage. Extend snapshot backing file first, if necessary.
  if (offset == size) {
    // Fail if snapshot file couldn't be extended
    if (_pma_extend_snapshot_file(1)) return 0;
  }

  // Update offset of next open dpage
  _pma_state->metadata.next_offset += PMA_PAGE_SIZE;

  return offset;
}

void
_pma_copy_page(void *address, uint64_t offset, PageStatus status, int fd) {
  void     *new_address;
  ssize_t   bytes_out;
  uint64_t  index = PTR_TO_INDEX(address);
  uint16_t  tail = _pma_state->metadata.dpage_cache->tail;

  bytes_out = pwrite(fd, address, PMA_PAGE_SIZE, offset);
  if (bytes_out != PMA_PAGE_SIZE) ERROR("pwrite failed");

  // TODO: is munmap needed here?
  new_address = mmap(
      address,
      PMA_PAGE_SIZE,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_FIXED,
      fd,
      offset);
  if (new_address == MAP_FAILED) ERROR(strerror(errno));

  // XX
  assert(new_address == address);

  // Add previous dpage to cache
  // Note: the dpage cache should always be writeable here, either because the dpage cache is the page we just copied,
  // or because it was made writeable in advance by _pma_copy_shared_page
  _pma_state->metadata.dpage_cache->queue[tail] = _pma_state->page_directory.entries[index].offset;
  _pma_state->metadata.dpage_cache->tail = ((tail + 1) % PMA_DPAGE_CACHE_SIZE);

  // Add page to dirty page list
  _pma_mark_page_dirty(index, offset, status, 1);
}

void
_pma_mark_page_dirty(uint64_t index, uint64_t offset, PageStatus status, uint32_t num_pages) {
  DirtyPageEntry *dirty_page = (DirtyPageEntry *)_pma_state->metadata.dirty_pages;

  // TODO: check for dirty page overflow
  dirty_page += _pma_state->metadata.num_dirty_pages++;

  dirty_page->index     = index;
  dirty_page->offset    = offset;
  dirty_page->status    = status;
  dirty_page->num_pages = num_pages;
}

int
_pma_extend_snapshot_file(uint64_t multiplier) {
  int err;

  // Update size in metadata
  _pma_state->metadata.snapshot_size += (multiplier * PMA_SNAP_RESIZE_INC);

  // Extend snapshot file
  err = lseek(_pma_state->snapshot_fd, (_pma_state->metadata.snapshot_size - 1), SEEK_SET);
  if (err == -1) return -1;
  err = write(_pma_state->snapshot_fd, "", 1);
  if (err != 1) return -1;

  return 0;
}

/**
 * Log error to console and exit.
 *
 * @param s   Error message
 * @param p   Address which caused error
 * @param l   Line number
 *
 * @return  Always aborts
 */
void
_pma_error(const char *s, void *p, int l) {
   fprintf(stderr, "ERROR %d: %p - %s\n", l, p, s);

   abort();
}

/**
 * Log warning message to console.
 *
 * @param s   Error message
 * @param p   Address which caused error
 * @param l   Line number
 *
 * @return  Abort if warnings should be treated as errors
 */
void
_pma_warning(const char *s, void *p, int l) {
   if (PMA_ABORT) _pma_error(s, p, l);

   fprintf(stderr, "WARNING %d: %p - %s\n", l, p, s);
}
