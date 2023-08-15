#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>


//==============================================================================
// MACROS
//==============================================================================

#define PMA_PAGE_SHIFT        12U
#define PMA_MIN_ALLOC_SHIFT   4U
#define PMA_BITMAP_BITS       (8 * sizeof(uint8_t))
#define PMA_SNAPSHOT_RESIZE_INC 0x100000000
#define PMA_PAGE_SIZE         (1UL << PMA_PAGE_SHIFT)
#define PMA_PAGE_MASK         (PMA_PAGE_SIZE - 1)
#define PMA_MIN_ALLOC_SIZE    (1U << PMA_MIN_ALLOC_SHIFT)
#define PMA_MAX_SHARED_SHIFT  (PMA_PAGE_SHIFT - 2U)
#define PMA_MAX_SHARED_ALLOC  (1UL << PMA_MAX_SHARED_SHIFT)
#define PMA_SHARED_BUCKETS    (PMA_MAX_SHARED_SHIFT - PMA_MIN_ALLOC_SHIFT + 1)
#define PAGE_ROUND_DOWN(foo)  (foo & (~PMA_PAGE_MASK))
#define PAGE_ROUND_UP(foo)    ((foo + PMA_PAGE_MASK) & (~PMA_PAGE_MASK))
#define PTR_TO_INDEX(foo)     ((((uint64_t)(foo)) - ((uint64_t)_pma_state->metadata->arena_start)) >> PMA_PAGE_SHIFT)
#define INDEX_TO_PTR(foo)     (void *)((char *)_pma_state->metadata->arena_start + ((foo) * PMA_PAGE_SIZE))
#ifdef __linux__
  #define PMA_MMAP_FLAGS        (MAP_SHARED | MAP_FIXED_NOREPLACE)
#else
  #define PMA_MMAP_FLAGS        (MAP_SHARED | MAP_FIXED)
#endif
#define PMA_MAGIC_CODE        0xBADDECAFC0FFEE00  // i.e. all decaf coffee
#define PMA_DATA_VERSION      1
#define PMA_EMPTY_BITMAP      0xFF
#define PMA_BITMAP_SIZE       32
#define PMA_DPAGE_CACHE_SIZE  ((PMA_PAGE_SIZE - sizeof(PMADPageCache)) / sizeof(uint64_t))
#define PMA_DIRTY_PAGE_LIMIT  164
#define PMA_SNAPSHOT_FILENAME "snap.bin"
#define PMA_PAGE_DIR_FILENAME "page.bin"
#define PMA_DEFAULT_DIR_NAME  ".bin"
#define PMA_NEW_FILE_FLAGS    (O_RDWR | O_CREAT)
#define PMA_LOAD_FILE_FLAGS   (O_RDWR
#define PMA_DIR_PERMISSIONS   (S_IRWXU | S_IRWXG | S_IRWXO)
#define PMA_FILE_PERMISSIONS  (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define PMA_INIT_SNAP_SIZE    0x40000000
#define PMA_INIT_DIR_SIZE     0x400000
#define PMA_MAXIMUM_DIR_SIZE  0x5500000000
#ifdef __linux__
  #define PMA_SNAPSHOT_ADDR     0x10000
#else
  #define PMA_SNAPSHOT_ADDR     0x28000000000
#endif
#define PMA_MAX_DISK_FILE_SIZE  0x100000000000
#define PMA_MAX_RESIZE_FACTOR   (PMA_MAX_DISK_FILE_SIZE / PMA_SNAPSHOT_RESIZE_INC)


//==============================================================================
// TYPES
//==============================================================================

enum PMAPageStatus {
  UNALLOCATED,
  FREE,
  SHARED,
  FIRST,
  FOLLOW
};
typedef enum PMAPageStatus PMAPageStatus;

typedef struct PMAPageDirEntry PMAPageDirEntry;
struct PMAPageDirEntry {
  uint64_t      offset;
  PMAPageStatus status;
};

typedef struct PMAPageDir PMAPageDir;
struct PMAPageDir {
  uint64_t         size;
  uint64_t         next_index;
  PMAPageDirEntry *entries;
};

typedef struct PMASharedPageHeader PMASharedPageHeader;
struct PMASharedPageHeader {
  struct PMASharedPageHeader *next;
  uint8_t                     dirty;
  uint8_t                     size;
  uint8_t                     free;
  uint8_t                     bits[PMA_BITMAP_SIZE];
};

typedef struct PMADirtyPageEntry PMADirtyPageEntry;
struct PMADirtyPageEntry {
  uint64_t      index;
  uint64_t      offset;
  uint32_t      num_pages;
  PMAPageStatus status;
};

typedef struct PMASinglePageCache PMASinglePageCache;
struct PMASinglePageCache {
  PMASinglePageCache *next;
  void               *page;
};

typedef struct PMAPageRunCache PMAPageRunCache;
struct PMAPageRunCache {
  PMAPageRunCache *next;
  void            *page;
  uint64_t         length;
};

typedef struct PMADPageCache PMADPageCache;
struct PMADPageCache {
  uint8_t   dirty;
  uint16_t  size;
  uint16_t  head;
  uint16_t  tail;
  uint64_t  queue[];
};

typedef struct PMAMetadata PMAMetadata;
struct PMAMetadata {
  uint64_t             magic_code;
  uint32_t             checksum;
  uint32_t             version;
  uint64_t             epoch;
  uint64_t             event;
  uint64_t             root;
  void                *arena_start;
  void                *arena_end;
  PMASharedPageHeader *shared_pages[PMA_SHARED_BUCKETS];
  PMADPageCache       *dpage_cache;
  uint64_t             snapshot_size;
  uint64_t             next_offset;
  uint8_t              num_dirty_pages;
  uint64_t             padding[2];
  PMADirtyPageEntry    dirty_pages[PMA_DIRTY_PAGE_LIMIT];
};
static_assert(sizeof(PMAMetadata) == PMA_PAGE_SIZE, "PMAMetadata must be a page in length");

typedef struct PMAState PMAState;
struct PMAState {
  PMAMetadata        *metadata;
  uint64_t            meta_page_offset;
  PMAPageDir          page_directory;
  int                 snapshot_fd;
  int                 page_dir_fd;
  PMASinglePageCache *free_pages;
  PMAPageRunCache    *free_page_runs;
};


//==============================================================================
// GLOBALS
//==============================================================================

extern PMAState *_pma_state;


//==============================================================================
// FUNCTIONS
//==============================================================================

int       _pma_verify_checksum(PMAMetadata *meta_page);
int       _pma_sync_dirty_pages(int fd, uint8_t num_dirty_pages, PMADirtyPageEntry *dirty_pages);
int       _pma_write_page_status(int fd, uint64_t index, PMAPageStatus status);
int       _pma_write_page_offset(int fd, uint64_t index, uint64_t offset);
int       _pma_update_free_pages(uint8_t num_dirty_pages, PMADirtyPageEntry *dirty_pages);
void     *_pma_malloc_bytes(size_t size);
int       _pma_malloc_shared_page(uint8_t bucket);
void     *_pma_malloc_pages(size_t size);
void     *_pma_malloc_single_page(PMAPageStatus status);
void     *_pma_malloc_multi_pages(uint64_t num_pages);
void     *_pma_get_cached_pages(uint64_t num_pages);
void     *_pma_get_new_page(PMAPageStatus status);
void     *_pma_get_new_pages(uint64_t num_pages);
int       _pma_free_pages(void *address);
int       _pma_free_bytes(void *address);
int       _pma_copy_shared_page(void *address);
uint64_t  _pma_get_single_dpage(void);
uint64_t  _pma_get_cached_dpage(void);
int       _pma_copy_dpage_cache(void);
uint64_t  _pma_get_disk_dpage(void);
void      _pma_copy_page(void *address, uint64_t offset, PMAPageStatus status, int fd);
void      _pma_mark_page_dirty(uint64_t index, uint64_t offset, PMAPageStatus status, uint32_t num_pages);
int       _pma_extend_snapshot_file(uint32_t multiplier);
void      _pma_warning(const char *p, void *a, int l);
void      _pma_state_free(void);
int       _pma_state_malloc(void);
