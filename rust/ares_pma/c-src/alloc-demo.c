#include "btree.h"
#include "btree.c"

#include <stdlib.h>
#include <stdio.h>
#include <sys/random.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>


/* ;;: determining alloc size constant:

   target total alloc size: 1T = 1 << 40

   each alloc: 128M = 128 << 20 = 16#8000000
   total allocs: 1T / 128M = 8192 = 16#2000

   each alloc: 32M = 32 << 20 = 16#2000000
   total allocs: 1T / 32M = 32768 = 16#8000

   ** let's do 32M for now **

   sizeof(WAL_node) is 28 b

   at 16#8000 total allocs, need at least 16#E0000 b = 16#380 K of
   storage <1M total. 10x to 10M and just hardcode it.
*/


#define PMA_PATH "./pma"
#define WAL_PATH PMA_PATH "/wal"

/* WAL size in bytes. Fixed at 10M. */
#define WAL_SIZ_b (10ULL << 20)

#define EVENTNUM_IDX 0
#define TOTALSIZ_IDX 1
static uint64_t eventnum = 0;
static uint64_t total_allocsz_b = 0;

typedef unsigned char BYTE;


static inline size_t
_B2PAGES(size_t x)
/* return number of pages required to store x bytes */
{
  x += BT_PAGESIZE - 1;
  x &= ~(BT_PAGESIZE - 1);
  return x >> BT_PAGEBITS;
}
/* the opposite of b2pages */
#define _P2BYTES(x) ((size_t)(x) << BT_PAGEBITS)

#ifndef DEMO_NOPRINT
#define DEMO_PRINTF(fmt, ...)                                              \
        fprintf(stderr, "%s:%d " fmt "\n", __func__, __LINE__, __VA_ARGS__)
#define DEMO_PUTS(arg)	DEMO_PRINTF("%s", arg)
#else
#define DEMO_PRINTF(...) (void*)0;
#define DEMO_PUTS(x) (void*)0;
#endif



typedef struct WAL_node WAL_node;
struct WAL_node {
  BYTE     *loc;                /* pointer to alloc */
  size_t    siz_b;              /* size of alloc */
  uint32_t  chk;                /* crc32 checksum of alloc */
  uint64_t  event;              /* event number in which alloc was made */
};

typedef struct WAL_state WAL_state;
struct WAL_state {
  int fd;                       /* WAL backing file fd */
  WAL_node *head;               /* WAL head */
  WAL_node *tail;               /* WAL current node (bump allocation) */
};


static WAL_node *
WAL_node_new(WAL_state *wal)
/* allocate a new WAL node */
{
  assert(wal->tail);

  /* return the current head and bump the pointer */
  return wal->tail++;
}

static WAL_state *
WAL_open(BT_state *state)
/* open/create write-ahead log and store head at wal_head */
{
  WAL_state *wal_state = calloc(1, sizeof *wal_state);
  int isnew = 0;
  WAL_node *wal_tail = 0;

  DEMO_PUTS("Opening write-ahead log at " WAL_PATH);

  /* open/create WAL */
  wal_state->fd = open(WAL_PATH, O_RDWR);
  if (wal_state->fd == -1) {
    /* create new WAL */
    wal_state->fd = open(WAL_PATH, O_CREAT | O_RDWR, 0644);
    isnew = 1;
  }
  if (wal_state->fd == -1) {
    DPRINTF("Error opening WAL: %s", strerror(errno));
    abort();
  }
  if (ftruncate(wal_state->fd, WAL_SIZ_b) != 0) {
    DPRINTF("Failed to ftruncate WAL: %s", strerror(errno));
    abort();
  }

  /* mmap the WAL */
  wal_state->head = mmap(NULL,
                  WAL_SIZ_b,
                  PROT_READ | PROT_WRITE,
                  MAP_SHARED,
                  wal_state->fd,
                  0);
  if (wal_state->head == MAP_FAILED) {
    DPRINTF("Failed to mmap WAL: %s", strerror(errno));
    close(wal_state->fd);
    abort();
  }

  /* a new WAL should be zero filled */
  if (isnew) {
    ZERO(wal_state->head, WAL_SIZ_b);
    wal_state->tail = wal_state->head;
    return wal_state;
  }

  /* find the tail */
  wal_tail = wal_state->head;
  for (; wal_tail->loc != 0; wal_tail++) {
    /* break if next WAL node's eventnum is higher than last synced */
    if ((wal_tail+1)->event >= eventnum) {
      wal_tail += 1;
      size_t used_siz_b = (uintptr_t)(wal_tail - wal_state->head) * sizeof(WAL_node);
      ZERO(wal_tail, WAL_SIZ_b - used_siz_b);
      break;
    }
  }
  wal_state->tail = wal_tail;

  return wal_state;
}

static void
WAL_validate(WAL_state *wal)
{
  uint32_t chk;
  DEMO_PUTS("Validating write-ahead log");

  size_t i = 0;
  size_t print_at = 500;
  size_t nmemb = wal->tail - wal->head;

  for (WAL_node *w = wal->head
         ; w < wal->tail
         ; w++, i++) {
    if (i % print_at == 0) {
      DEMO_PRINTF("Validated %zu / %zu entries", i, nmemb);
    }

    assert(w->loc != 0);
    chk = crc_32(w->loc, w->siz_b);

    if (chk != w->chk) {
      DPRINTF("Wal validation failed: addr: %p, eventnum: %"PRIu64", current event num: %"PRIu64", stored chk: 0x%"PRIX32", calculated chk: 0x%"PRIX32,
              w->loc, w->event, eventnum, w->chk, chk);
      /* fprintf(stderr, "FAIL: addr: %p, eventnum: %"PRIu64"\n", w->loc, w->event); */
      abort();
    }
    /* else { */
    /*   fprintf(stderr, "SUCC: addr: %p, eventnum: %"PRIu64"\n", w->loc, w->event); */
    /* } */
  }
  DEMO_PRINTF("Finished. Validated %zu / %zu entries", i, nmemb);
}

static void
WAL_sync(WAL_state *wal)
{
  assert(wal->head);
  msync(wal->head, WAL_SIZ_b, MS_SYNC);
}

static void
WAL_close(WAL_state *wal)
{
  WAL_sync(wal);
  close(wal->fd);
  ZERO(wal, sizeof *wal);
}


void
d_alloc_random(BT_state *state, WAL_state *wal_state, size_t siz_b)
{
  uint32_t chk = 0;
  size_t siz_p;
  BYTE *p = 0;
  WAL_node *w = 0;

  siz_p = _B2PAGES(siz_b);
  p = bt_malloc(state, siz_p);
  if (getrandom(p, siz_b, 0) == -1) {
    DPRINTF("getrandom call failed with %s", strerror(errno));
    abort();
  }
  chk = crc_32(p, siz_b);

  w = WAL_node_new(wal_state);
  w->loc = p;
  w->siz_b = siz_b;
  w->chk = chk;
  w->event = eventnum;

  total_allocsz_b += siz_b;

  /* DEMO_PRINTF("Alloced random blob of data with chk: 0x%" PRIX32, chk); */
}

void
d_sync(BT_state *state, WAL_state *wal)
/* sync the btree and store next event number in new tree */
{
  DEMO_PRINTF("Syncing at eventnum: %"PRIu64, eventnum);
  /* store current alloc size */
  bt_meta_set(state, TOTALSIZ_IDX, total_allocsz_b);
  /* sync btree */
  bt_sync(state);
  /* bump event num */
  eventnum += 1;
  /* store new eventnum */
  bt_meta_set(state, EVENTNUM_IDX, eventnum);
  DEMO_PUTS("Sync completed");
}


int main(int argc, char *argv[])
{
#define IGNORE_VALIDATION 0
#if 0
#define ALLOC_SIZ_b (32ULL << 10)             /* 32K */
#define TARGET_TOTAL_ALLOCSZ_B (100ULL << 20) /* 100M */
#else
#define ALLOC_SIZ_b (32ULL << 20)             /* 32M */
#define TARGET_TOTAL_ALLOCSZ_B (1ULL << 40)   /* 1T */
#endif

  BT_state *state = 0;
  WAL_state *wal_state = 0;
  size_t sync_at = 500;
  size_t print_at = 100;

  /* create pma directory */
  if (mkdir(PMA_PATH, 0774) == -1
      && errno != EEXIST) {
    DPRINTF("== failed to create PMA directory: %s", strerror(errno));
  }

  /* initialize state and open PMA */
  bt_state_new(&state);
  assert(SUCC(bt_state_open(state, PMA_PATH, 0, 0644)));

  eventnum = bt_meta_get(state, EVENTNUM_IDX);
  eventnum += 1;
  bt_meta_set(state, EVENTNUM_IDX, eventnum);
  total_allocsz_b = bt_meta_get(state, TOTALSIZ_IDX);

  /* open write-ahead log */
  wal_state = WAL_open(state);

  if (!IGNORE_VALIDATION) {
    WAL_validate(wal_state);
  }

  /* main loop */
  DEMO_PUTS("Starting random allocation loop");

  size_t siz_g, siz_m, siz_k;
  for (size_t i = 0
         ; total_allocsz_b < TARGET_TOTAL_ALLOCSZ_B
         ; i++) {
    siz_k = (total_allocsz_b >> 10) & 0x3FF;
    siz_m = (total_allocsz_b >> 20) & 0x3FF;
    siz_g = total_allocsz_b >> 30;

    if (i && i % sync_at == 0) {
      d_sync(state, wal_state);
    }
    if (i % print_at == 0) {
      DEMO_PRINTF("Total alloc size: %zuG %zuM %zuK", siz_g, siz_m, siz_k);
    }

    d_alloc_random(state, wal_state, ALLOC_SIZ_b);
  }

  DEMO_PUTS("Finished writing random data. Closing pma...");
  d_sync(state, wal_state);
  bt_state_close(state);
  WAL_close(wal_state);

  return 0;
}
/*
d_sync:236 Syncing at eventnum: 59
d_sync:245 Sync completed
main:302 Total alloc size: 921G 896M 0K
main:302 Total alloc size: 925G 0M 0K
main:302 Total alloc size: 928G 128M 0K
main:302 Total alloc size: 931G 256M 0K
main:302 Total alloc size: 934G 384M 0K
d_sync:236 Syncing at eventnum: 60
d_sync:245 Sync completed
main:302 Total alloc size: 937G 512M 0K
main:302 Total alloc size: 940G 640M 0K
main:302 Total alloc size: 943G 768M 0K
main:302 Total alloc size: 946G 896M 0K
main:302 Total alloc size: 950G 0M 0K
d_sync:236 Syncing at eventnum: 61
d_sync:245 Sync completed
main:302 Total alloc size: 953G 128M 0K
main:302 Total alloc size: 956G 256M 0K
main:302 Total alloc size: 959G 384M 0K
main:302 Total alloc size: 962G 512M 0K
main:302 Total alloc size: 965G 640M 0K
Segmentation fault (core dumped)
*/
