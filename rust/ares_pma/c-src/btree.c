#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>

#include "lib/checksum.h"

typedef uint32_t pgno_t;        /* a page number */
typedef uint32_t vaof_t;        /* a virtual address offset */
typedef uint32_t flag_t;
typedef unsigned char BYTE;
typedef unsigned long ULONG;

//// ===========================================================================
////                              tmp tmp tmp tmp tmp
/* ;;: remove -- for debugging */
/*
  bp(X) where X is false will raise a SIGTRAP. If the process is being run
  inside a debugger, this can be caught and ignored. It's equivalent to a
  breakpoint. If run without a debugger, it will dump core, like an assert
*/
#ifdef DEBUG
#if defined(__i386__) || defined(__x86_64__)
#define bp(x) do { if(!(x)) __asm__ volatile("int $3"); } while (0)
#elif defined(__thumb__)
#define bp(x) do { if(!(x)) __asm__ volatile(".inst 0xde01"); } while (0)
#elif defined(__aarch64__)
#define bp(x) do { if(!(x)) __asm__ volatile(".inst 0xd4200000"); } while (0)
#elif defined(__arm__)
#define bp(x) do { if(!(x)) __asm__ volatile(".inst 0xe7f001f0"); } while (0)
#else
STATIC_ASSERT(0, "debugger break instruction unimplemented");
#endif
#else
#define bp(x) ((void)(0))
#endif

/* coalescing of memory freelist currently prohibited since we haven't
   implemented coalescing of btree nodes (necessary) */
#define CAN_COALESCE 0
/* ;;: remove once confident in logic and delete all code dependencies on
     state->node_freelist */
#define USE_NLIST 1
#if USE_NLIST
/* ;;: obviously this should be removed once we've fully switched over to the
     nlist. And calls to _node_alloc should be updated to calls to _bt_nalloc */
#define _node_alloc(...) _bt_nalloc(__VA_ARGS__)
#endif

#define ZERO(s, n) memset((s), 0, (n))

#define S7(A, B, C, D, E, F, G) A##B##C##D##E##F##G
#define S6(A, B, C, D, E, F, ...) S7(A, B, C, D, E, F, __VA_ARGS__)
#define S5(A, B, C, D, E, ...) S6(A, B, C, D, E, __VA_ARGS__)
#define S4(A, B, C, D, ...) S5(A, B, C, D, __VA_ARGS__)
#define S3(A, B, C, ...) S4(A, B, C, __VA_ARGS__)
#define S2(A, B, ...) S3(A, B, __VA_ARGS__)
#define S(A, ...) S2(A, __VA_ARGS__)

#define KBYTES(x) ((size_t)(x) << 10)
#define MBYTES(x) ((size_t)(x) << 20)
#define GBYTES(x) ((size_t)(x) << 30)
#define TBYTES(x) ((size_t)(x) << 40)
#define PBYTES(x) ((size_t)(x) << 50)

/* 4K page in bytes */
#define P2BYTES(x) ((size_t)(x) << 14)
/* the opposite of P2BYTES */
#define B2PAGES(x) ((size_t)(x) >> 14)


#define __packed        __attribute__((__packed__))
#define UNUSED(x) ((void)(x))

#ifdef DEBUG
# define DPRINTF(fmt, ...)                                              \
        fprintf(stderr, "%s:%d " fmt "\n", __func__, __LINE__, __VA_ARGS__)
#else
# define DPRINTF(fmt, ...)	((void) 0)
#endif
#define DPUTS(arg)	DPRINTF("%s", arg)
#define TRACE(...) DPUTS("")

#define BT_SUCC 0
#define SUCC(x) ((x) == BT_SUCC)


#define BT_MAPADDR  ((void *) S(0x1000,0000,0000))

/* convert addr offset to raw address */
#define OFF2ADDR(x) ((void *)((uintptr_t)(BT_MAPADDR) + (x)))
/* convert raw memory address to offset */
#define ADDR2OFF(a) ((vaof_t)((uintptr_t)(a) - (uintptr_t)BT_MAPADDR))

#define BT_PAGEBITS 14ULL
#define BT_PAGEWORD 32ULL
#define BT_PAGESIZE (1ULL << BT_PAGEBITS) /* 16K */
#define BT_NUMMETAS 2                     /* 2 metapages */
#define BT_ADDRSIZE (BT_PAGESIZE << BT_PAGEWORD)
#define PMA_GROW_SIZE (BT_PAGESIZE * 1024)

#define BT_NOPAGE 0

/*
  FO2BY: file offset to byte
  get byte INDEX into pma map from file offset
*/
#define FO2BY(fo)                               \
  ((uint64_t)(fo) << BT_PAGEBITS)

/*
  BY2FO: byte to file offset
  get pgno from byte INDEX into pma map
*/
#define BY2FO(p)                                \
  ((pgno_t)((p) >> BT_PAGEBITS))

/*
  FO2PA: file offset to page
  get a reference to a BT_page from a file offset
*/
#define FO2PA(map, fo)                          \
  ((BT_page *)&(map)[FO2BY(fo)])

/* NMEMB: number of members in array, a */
#define NMEMB(a)                                \
  (sizeof(a[0]) / sizeof(a))

#define offsetof(st, m) \
    __builtin_offsetof(st, m)


//// ===========================================================================
////                                  btree types

/*
  btree page header. all pages share this header. Though for metapages, you can
  expect it to be zeroed out.
*/
typedef struct BT_pageheader BT_pageheader;
struct BT_pageheader {
  uint8_t  dirty[256];          /* dirty bit map */
} __packed;

/*
  btree key/value data format

/*
  BT_dat is used to provide a view of the data section in a BT_page where data is
  stored like:
        va  fo  va  fo
  bytes 0   4   8   12

  The convenience macros given an index into the data array do the following:
  BT_dat_lo(i) returns ith   va (low addr)
  BT_dat_hi(i) returns i+1th va (high addr)
  BT_dat_fo(i) returns ith file offset
*/
typedef union BT_dat BT_dat;
union BT_dat {
  vaof_t va;                    /* virtual address offset */
  pgno_t fo;                    /* file offset */
};

/* like BT_dat but when a struct is more useful than a union */
typedef struct BT_kv BT_kv;
struct BT_kv {
  vaof_t va;
  pgno_t fo;
};

/* ;;: todo, perhaps rather than an index, return the data directly and typecast?? */
#define BT_dat_lo(i) ((i) * 2)
#define BT_dat_fo(i) ((i) * 2 + 1)
#define BT_dat_hi(i) ((i) * 2 + 2)

#define BT_dat_lo2(I, dat)
#define BT_dat_fo2(I, dat)
#define BT_dat_hi2(I, dat)

/* BT_dat_maxva: pointer to highest va in page data section */
#define BT_dat_maxva(p)                         \
  ((void *)&(p)->datd[BT_dat_lo(BT_DAT_MAXKEYS)])

/* BT_dat_maxfo: pointer to highest fo in page data section */
#define BT_dat_maxfo(p)                         \
  ((void *)&(p)->datd[BT_dat_fo(BT_DAT_MAXVALS)])

#define BT_DAT_MAXBYTES (BT_PAGESIZE - sizeof(BT_pageheader))
#define BT_DAT_MAXENTRIES  (BT_DAT_MAXBYTES / sizeof(BT_dat))
#define BT_DAT_MAXKEYS (BT_DAT_MAXENTRIES / 2)
/* #define BT_DAT_MAXKEYS 10 */
#define BT_DAT_MAXVALS BT_DAT_MAXKEYS
static_assert(BT_DAT_MAXENTRIES % 2 == 0);

/*
   all pages in the memory arena consist of a header and data section
*/
typedef struct BT_page BT_page;
struct BT_page {
  BT_pageheader head;           /* ;;: TODO remove header and store all header data in BT_meta */
  union {                       /* data section */
    BT_dat      datd[BT_DAT_MAXENTRIES]; /* union view */
    BT_kv       datk[0];                 /* struct view */
    BYTE        datc[0];                 /* byte-level view */
  };
};
static_assert(sizeof(BT_page) == BT_PAGESIZE);
static_assert(BT_DAT_MAXBYTES % sizeof(BT_dat) == 0);

#define BT_MAGIC   0xBADDBABE
#define BT_VERSION 1
/*
   a meta page is like any other page, but the data section is used to store
   additional information
*/
#define BLK_BASE_LEN0 (MBYTES(2) - (BT_PAGESIZE * BT_NUMMETAS))
#define BLK_BASE_LEN1 (BLK_BASE_LEN0 * 4)
#define BLK_BASE_LEN2 (BLK_BASE_LEN1 * 4)
#define BLK_BASE_LEN3 (BLK_BASE_LEN2 * 4)
#define BLK_BASE_LEN4 (BLK_BASE_LEN3 * 4)
#define BLK_BASE_LEN5 (BLK_BASE_LEN4 * 4)
#define BLK_BASE_LEN6 (BLK_BASE_LEN5 * 4)
#define BLK_BASE_LEN7 (BLK_BASE_LEN6 * 4)
typedef struct BT_meta BT_meta;
struct BT_meta {
  uint32_t  magic;
  uint32_t  version;
  pgno_t    last_pg;            /* last page used in file */
  uint32_t  _pad0;
  uint64_t  txnid;
  void     *fix_addr;           /* fixed addr of btree */

  pgno_t   blk_base[8];         /* block base array for striped node partition */

  /* ;;: for the blk_base array, code may be simpler if this were an array of
       BT_page *. */

  uint8_t  blk_cnt;             /* currently highest valid block base */
  uint8_t  depth;               /* tree depth */
/* #define BP_DIRTY ((uint8_t)0x01) /\* ;;: TODO remove dirty flag *\/ */
#define BP_META  ((uint8_t)0x02)
  uint8_t  flags;
  uint8_t  _pad1;
  pgno_t   root;
  /* ;;: confirm: shouldn't the checksum actually follow the roots array? */
  uint32_t chk;                 /* checksum */
  /* 64bit alignment manually checked */
  uint64_t roots[];             /* for usage by ares */

  /* ;;: TODO: ensure the crc_32 checksum cannot be zero */

} __packed;
static_assert(sizeof(BT_meta) <= BT_DAT_MAXBYTES);

/* the length of the metapage up to but excluding the checksum */
#define BT_META_LEN (offsetof(BT_meta, chk))

#define BT_roots_bytelen (sizeof(BT_meta) - offsetof(BT_meta, roots))

typedef struct BT_mlistnode BT_mlistnode;
struct BT_mlistnode {
  void *va;                     /* virtual address */
  size_t sz;                    /* size in pages */
  BT_mlistnode *next;           /* next freelist node */
};

typedef struct BT_nlistnode BT_nlistnode;
struct BT_nlistnode {
  BT_page *va;                  /* virtual address */
  size_t sz;                    /* size in pages */
  BT_nlistnode *next;           /* next freelist node */
};

typedef struct BT_flistnode BT_flistnode;
struct BT_flistnode {
  pgno_t pg;                    /* pgno - an offset in the persistent file */
  size_t sz;                    /* size in pages */
  BT_flistnode *next;           /* next freelist node */
};

/* macro to access the metadata stored in a page's data section */
#define METADATA(p) ((BT_meta *)(void *)(p)->datc)

typedef struct BT_state BT_state;
struct BT_state {
  uint16_t      flags;          /* ;;: rem */
  int           data_fd;
  int           meta_fd;        /* ;;: confident can be removed because we're not explicitly calling write() */
  char         *path;
  ULONG         branch_page_cnt; /* ;;: rem */
  ULONG         leaf_page_cnt;   /* ;;: rem */
  void         *fixaddr;
  BYTE         *map;
  BT_page      *node_freelist;
  BT_meta      *meta_pages[2];  /* double buffered */
  /* ;;: note, while meta_pages[which]->root stores a pgno, we may want to just
       store a pointer to root in state in addition to avoid a _node_find on it
       every time it's referenced */
  /* BT_page      *root; */
  off_t         file_size;      /* the size of the pma file in bytes */
  pgno_t        frontier;       /* last non-free page in use by pma (exclusive) */
  unsigned int  which;          /* which double-buffered db are we using? */
  BT_nlistnode *nlist;          /* node freelist */
  BT_mlistnode *mlist;          /* memory freelist */
  BT_flistnode *flist;          /* pma file freelist */
  /* ;;: for deletion coalescing:

     when freeing data, push onto the pending flist and mlist. When pushing onto
     the mlist, you can preemptively coalesce. You don't need to coalesce at all
     in the pending flist.

     when inserting and coalescing, if you can free a node then push onto the
     pending nlist

  */

  BT_flistnode *pending_flist;
  BT_nlistnode *pending_nlist;
};

/*
  ;;: wrt to frontier: if you need to allocate space for data, push the frontier
     out by that amount allocated. If you're allocating a new stripe, push it to
     the end of that stripe.
*/


//// ===========================================================================
////                            btree internal routines

static void _bt_printnode(BT_page *node); /* ;;: tmp */
static int
_bt_insertdat(vaof_t lo, vaof_t hi, pgno_t fo,
              BT_page *parent, size_t childidx); /* ;;: tmp */

#define BT_MAXDEPTH 4           /* ;;: todo derive it */
typedef struct BT_findpath BT_findpath;
struct BT_findpath {
  BT_page *path[BT_MAXDEPTH];
  size_t idx[BT_MAXDEPTH];
  uint8_t depth;
};

/* _node_get: get a pointer to a node stored at file offset pgno */
static BT_page *
_node_get(BT_state *state, pgno_t pgno)
{
  /* TODO: eventually, once we can store more than 2M of nodes, this will need
     to reference the meta page's blk_base array to determine where a node is
     mapped. i.e:

  - receive pgno
  - find first pgno in blk_base that exceeds pgno : i
  - sector that contains node is i-1
  - appropriately offset into i-1th fixed size partition: 2M, 8M, 16M, ...

  */

  /* for now, this works because the 2M sector is at the beginning of both the
     memory arena and pma file
  */
  if (pgno <= 1) return 0;      /* no nodes stored at 0 and 1 (metapages) */
  /* TODO: when partition striping is implemented, a call beyond the furthest
     block base should result in the allocation of a new block base */
  assert((pgno * BT_PAGESIZE) < MBYTES(2));
  return FO2PA(state->map, pgno);
}

/* ;;: I don't think we should need this if _node_alloc also returns a disc offset */
static pgno_t
_fo_get(BT_state *state, BT_page *node)
{
  uintptr_t vaddr = (uintptr_t)node;
  uintptr_t start = (uintptr_t)state->map;
  return BY2FO(vaddr - start);
}

#ifndef USE_NLIST
static BT_page *                /* ;;: change to return both a file and node offset as params to function. actual return value is error code */
_node_alloc(BT_state *state)
{
  /* TODO: will eventually need to walk a node freelist that allocs space for
     the striped node partitions. Since this is unimplemented, just allocating
     space from first 2M */

  /* ;;: when node freelist is implemented, will we need to return the file
       offset of the node as well? This is important for splitting where we
       allocate a new node and need to store its file offset in the parent's
       data index */
  size_t width = (BYTE *)state->node_freelist - state->map;
  assert(width < MBYTES(2));
  /* ;;: todo confirm data sections are zeroed */
  /* ZERO(state->node_freelist, BT_PAGESIZE); */
  return ++state->node_freelist;
}
#endif

static BT_page *
_bt_nalloc(BT_state *state)
/* allocate a node in the node freelist */
{
  BT_nlistnode **n = &state->nlist;

  for (; *n; n = &(*n)->next) {
    /* ;;: this assert is temporary. When partition striping is
         implemented. Rather than assert, conditionally check if we're at the
         end of the current stripe. If so, allocate a new region and append that
         to the freelist. */
    size_t width = (BYTE *)state->nlist - state->map;
    assert(width < MBYTES(2));
    /* perfect fit */
    if ((*n)->sz == 1) {
      BT_page *ret;
      ret = (*n)->va;
      *n = (*n)->next;
      return ret;
    }
    /* larger than necessary: shrink the node */
    if ((*n)->sz > 1) {
      BT_page *ret;
      ret = (*n)->va;
      (*n)->sz -= 1;
      (*n)->va = (*n)->va + 1;
      return ret;
    }
  }
}

/* ;;: from our usage, _node_cow no longer needs to take indirect pointer to
     newnode. We don't ever do anything with it */
static int
_node_cow(BT_state *state, BT_page *node, BT_page **newnode, pgno_t *pgno)
{
  BT_page *ret = _node_alloc(state);
  memcpy(ret->datk, node->datk, sizeof node->datk[0] * BT_DAT_MAXENTRIES);
  *pgno = _fo_get(state, ret);
  *newnode = ret;
  return BT_SUCC;
}

/* binary search a page's data section for a va. Returns a pointer to the found BT_dat */
static void *
_bt_bsearch(BT_page *page, vaof_t va)
{
  /* ;;: todo: actually bsearch rather than linear */
  for (BT_kv *kv = &page->datk[0]; kv <= BT_dat_maxva(page); kv++) {
    if (kv->va == va)
      return kv;
  }

  return 0;
}

static size_t
_bt_childidx(BT_page *node, vaof_t lo, vaof_t hi)
/* looks up the child index in a parent node. If not found, return is
   BT_DAT_MAXKEYS */
{
  size_t i = 0;
  for (; i < BT_DAT_MAXKEYS - 1; i++) {
    vaof_t llo = node->datk[i].va;
    vaof_t hhi = node->datk[i+1].va;
    if (llo <= lo && hhi >= hi)
      return i;
  }
  return BT_DAT_MAXKEYS;
}

/* ;;: find returns a path to nodes that things should be in if they are there. */
/* a leaf has a meta page depth eq to findpath depth */
static int
_bt_find2(BT_state *state,
          BT_page *node,
          BT_findpath *path,
          uint8_t maxdepth,
          vaof_t lo,
          vaof_t hi)
{
  /* ;;: meta node stores depth (node or leaf?)
     look at root node and binsearch BT_dats where low is <= lo and high is >= hi
     If at depth of metapage (a leaf), then done
     otherwise grab node, increment depth, save node in path
  */
  if (path->depth > maxdepth)
    return ENOENT;

  assert(node != 0);

  size_t i;
  if ((i = _bt_childidx(node, lo, hi)) == BT_DAT_MAXKEYS)
    return ENOENT;

  if (path->depth == maxdepth) {
    path->idx[path->depth] = i;
    path->path[path->depth] = node;
    return BT_SUCC;
  }
  /* then branch */
  else {
    pgno_t fo = node->datk[i].fo;
    BT_page *child = _node_get(state, fo);
    path->idx[path->depth] = i;
    path->path[path->depth] = node;
    path->depth++;
    return _bt_find2(state, child, path, maxdepth, lo, hi);
  }
}

static void
_bt_root_new(BT_page *root)
{
  root->datk[0].va = 0;
  root->datk[0].fo = 0;
  root->datk[1].va = UINT32_MAX;
  root->datk[1].fo = 0;
}

static int
_bt_find(BT_state *state, BT_findpath *path, vaof_t lo, vaof_t hi)
{
  path->depth = 1;
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  uint8_t maxdepth = meta->depth;
  return _bt_find2(state, root, path, maxdepth, lo, hi);
}

static int
_bt_findpath_is_root(BT_findpath *path)
{
  assert(path != 0);
  return path->depth == 0;
}

/* _bt_numkeys: find next empty space in node's data section. Returned as
   index into node->datk. If the node is full, return is BT_DAT_MAXKEYS */
static size_t
_bt_numkeys(BT_page *node)
{
  size_t i = 1;
  for (; i < BT_DAT_MAXKEYS; i++) {
    if (node->datk[i].va == 0) break;
  }
  return i;
}

static int
_bt_datshift(BT_page *node, size_t i, size_t n)
/* shift data segment at i over by n KVs */
{
  assert(i+n < BT_DAT_MAXKEYS); /* check buffer overflow */
  size_t siz = sizeof node->datk[0];
  size_t bytelen = (BT_DAT_MAXKEYS - i - n) * siz;
  memmove(&node->datk[i+n], &node->datk[i], bytelen);
  ZERO(&node->datk[i], n * siz);
  return BT_SUCC;
}

/* _bt_split_datcopy: copy right half of left node to right node */
static int
_bt_split_datcopy(BT_page *left, BT_page *right)
{
  size_t mid = BT_DAT_MAXKEYS / 2;
  size_t bytelen = mid * sizeof(left->datk[0]);
  /* copy rhs of left to right */
  memcpy(right->datk, &left->datk[mid], bytelen);
  /* zero rhs of left */
  ZERO(&left->datk[mid], bytelen); /* ;;: note, this would be unnecessary if we stored node.N */
  /* the last entry in left should be the first entry in right */
  left->datk[mid].va = right->datk[0].va;

  return BT_SUCC;
}

static int
_bt_ischilddirty(BT_page *parent, size_t child_idx)
{
  assert(child_idx < 2048);
  uint8_t flag = parent->head.dirty[child_idx >> 3];
  return flag & (1 << (child_idx & 0x7));
}

/* ;;: todo: name the 0x8 and 4 literals and/or generalize */
static int
_bt_dirtychild(BT_page *parent, size_t child_idx)
{
  assert(child_idx < 2048);
  /* although there's nothing theoretically wrong with dirtying a dirty node,
     there's probably a bug if we do it since a we only dirty a node when it's
     alloced after a split or CoWed */
  assert(!_bt_ischilddirty(parent, child_idx));
  uint8_t *flag = &parent->head.dirty[child_idx >> 3];
  *flag |= 1 << (child_idx & 0x7);
  return BT_SUCC;
}

static int
_bt_cleanchild(BT_page *parent, size_t child_idx)
{
  assert(_bt_ischilddirty(parent, child_idx));
  uint8_t *flag = &parent->head.dirty[child_idx >> 3];
  *flag ^= 1 << (child_idx & 0x7);
  return BT_SUCC;
}

/* ;:: assert that the node is dirty when splitting */
static int
_bt_split_child(BT_state *state, BT_page *parent, size_t i, pgno_t *newchild)
{
  /* ;;: todo: better error handling */
  /* ;;: todo: assert parent and left is dirty */
  int rc = BT_SUCC;
  size_t N;
  BT_page *left = _node_get(state, parent->datk[i].fo);
  BT_page *right = _node_alloc(state);
  if (right == 0)
    return ENOMEM;
  if (!SUCC(rc = _bt_split_datcopy(left, right)))
    return rc;

  /* adjust high address of left node in parent */
  N = _bt_numkeys(left);
  /* parent->datk[i+1].va = left->datk[N-1].va; /\* ;;: is this necessary? *\/ */

  /* insert reference to right child into parent node */
  N = _bt_numkeys(right);
  vaof_t lo = right->datk[0].va;
  vaof_t hi = right->datk[N-1].va;

  _bt_insertdat(lo, hi, _fo_get(state, right), parent, i);

  /* dirty right child */
  size_t ridx = _bt_childidx(parent, lo, hi);
  assert(ridx == i+1);          /* 0x100000020100;;: tmp? */
  _bt_dirtychild(parent, ridx);

  /* ;;: fix this */
  *newchild = _fo_get(state, right);

  return BT_SUCC;
}

/* ;;: since we won't be rebalancing on delete, but rather on insert, you should add rebalance logic to _bt_insert2 which checks the degree of a node and rebalances if less than minimum */

static int
_bt_rebalance(BT_state *state, BT_page *node)
{
  return 255;
}

/* insert lo, hi, and fo in parent's data section for childidx */
static int
_bt_insertdat(vaof_t lo, vaof_t hi, pgno_t fo,
              BT_page *parent, size_t childidx)
{
  DPRINTF("BEFORE INSERT lo %" PRIu32 " hi %" PRIu32 " fo %" PRIu32, lo, hi, fo);
  /* _bt_printnode(parent); */

  /* ;;: TODO confirm this logic is appropriate for branch nodes. (It /should/
       be correct for leaf nodes) */
  vaof_t llo = parent->datk[childidx].va;
  vaof_t hhi = parent->datk[childidx+1].va;

  /* duplicate */
  if (llo == lo && hhi == hi) {
    parent->datk[childidx].fo = fo;
    return BT_SUCC;
  }

  if (llo == lo) {
    _bt_datshift(parent, childidx + 1, 1);
    vaof_t oldfo = parent->datk[childidx].fo;
    parent->datk[childidx].fo = fo;
    parent->datk[childidx+1].va = hi;
    parent->datk[childidx+1].fo = oldfo + (hi - llo);
  }
  else if (hhi == hi) {
    _bt_datshift(parent, childidx + 1, 1);
    parent->datk[childidx+1].va = lo;
    parent->datk[childidx+1].fo = fo;
  }
  else {
    _bt_datshift(parent, childidx + 1, 2);
    parent->datk[childidx+1].va = lo;
    parent->datk[childidx+1].fo = fo;
    parent->datk[childidx+2].va = hi;
    pgno_t lfo = parent->datk[childidx].fo;
    vaof_t lva = parent->datk[childidx].va;
    parent->datk[childidx+2].fo = (lfo == 0)
      ? 0
      : lfo + (hi - lva);
  }

  DPUTS("AFTER INSERT");
  /* _bt_printnode(parent); */
  return BT_SUCC;
}


//// ===========================================================================
////                           wip - deletion coalescing

/* ;;: todo: rename routines */

int
_bt_delco_1pass_0(BT_state *state, vaof_t lo, vaof_t hi,
                  BT_page *node, uint8_t depth, uint8_t maxdepth)
{
  /* Perform a dfs search on all ranges that fall within lo and hi */

  /* ;;: we can't use bt_childidx because the range of lo-hi may overlap ofc */
  size_t loidx = 0;
  size_t hiidx = 0;

  /* first find the entry that matches lo */
  size_t i;
  for (i = 0; i < BT_DAT_MAXKEYS-1; i++) {
    vaof_t llo = node->datk[i].va;
    if (llo <= lo) {
      loidx = i;
      break;
    }
  }

  /* and then the entry that matches hi */
  for (; i < BT_DAT_MAXKEYS-1; i++) {
    vaof_t hhi = node->datk[i].va;
    if (hhi >= hi) {
      hiidx = hi;
      break;
    }
  }

  /* node->datk[loidx] - node->datk[hiidx] are the bounds on which to perform
     the dfs */
  for (i = loidx; i < hiidx; i++) {
    vaof_t llo = node->datk[i].va;
    pgno_t pg = node->datk[i].va;

    /* if at the leaf level, terminate with failure if pg is not free */
    if (depth == maxdepth) {
      if (pg != 0) return 1;
      else continue;
    }

    /* otherwise, dfs the child node */
    BT_page *child = _node_get(state, pg);
    if (!SUCC(_bt_delco_1pass_0(state, lo, hi, child, depth+1, maxdepth)))
      return 1;
  }

  /* whether we're at a leaf or a branch, by now all pages corresponding to the
     hi-lo range must be free */
  return BT_SUCC;
}

/* ;;: since this is called by another recursive function _bt_delco that first
     finds if a split exists, this /could/ take a pgno to avoid unnecessarily
     rewalking the tree. not a big deal though as is. */
static int
_bt_delco_1pass(BT_state *state, vaof_t lo, vaof_t hi)
/* returns true if the leaves in the given range are all free (pgno of 0). false
   otherwise. This must be the case for an insert into an overlapping range to
   succeed */
{
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  return _bt_delco_1pass_0(state, lo, hi, root, 1, meta->depth);
}

static void
_pending_nlist_insert(BT_state *state, pgno_t nodepg)
{
  /* ;;: todo: need to account for a null head */
  BT_nlistnode *head = state->pending_nlist;
  BT_page *va = _node_get(state, nodepg);

  /* we don't need to account for a freelist node's size because we aren't
     coalescing the pending freelists */
  while (head->next) {
    if (head->next->va > va)
      break;
    head = head->next;
  }

  /* head->next is either null or has a higher address than va */
  BT_nlistnode *new = calloc(1, sizeof new);
  new->next = head->next;
  new->sz = 1;
  new->va = va;

  head->next = new;
}

static void
_pending_nlist_clear(BT_state *state)
{
  /* there's no need for a pending freelist "pop" routine as we only clear nodes
     from it after all have been merged with the real freelists */
  BT_nlistnode *prev = state->pending_nlist;
  BT_nlistnode *next = prev->next;
  while (prev) {
    free(prev);
    prev = next;
    next = next->next;
  }
  state->pending_nlist = 0;
}

static BT_nlistnode *
_nlist_find(BT_nlistnode *head, BT_page *va)
/* find a node  */
{

}

static void
_pending_nlist_merge(BT_state *state)
/* merge state->pending_nlist with state->nlist. To be called when syncing */
{
  BT_nlistnode *src_head = state->pending_nlist;
  BT_nlistnode *dst_head = state->nlist;

  while (src_head) {
    /* ;;: todo refactor */
    while (dst_head) {
      BT_page *dst_va = dst_head->va;
      BT_page *src_va = src_head->va;
      if (dst_head->va <= src_head->va
          && dst_head->va + dst_head->sz >= src_head->va) {
        /* found node in nlist that fits node in pending nlist */

        dst_head->sz += 1;
        break;
      }
      else if (dst_head->va + dst_head->sz < src_head->va
               && dst_head->next->va > src_head->va) {
        /* pending nlist node belongs between two nlist nodes */
        BT_nlistnode *new = calloc(1, sizeof *new);
        memcpy(new, src_head, sizeof *src_head);
        new->sz = 1;
        new->va = src_head->va;
        /* insert */
        new->next = dst_head->next;
        dst_head->next = new;
        break;
      }
      dst_head = dst_head->next;
    }
    if (!dst_head) {
      /* need to track prev */
    }


    src_head = src_head->next;
  }

  _pending_nlist_clear(state);
}


/* ;;: todo move shit around */
static void
_bt_delco_droptree2(BT_state *state, pgno_t nodepg, uint8_t depth, uint8_t maxdepth)
{
  /* branch */
  if (depth != maxdepth) {
    BT_page *node = _node_get(state, nodepg);
    for (size_t i = 0; i < BT_DAT_MAXKEYS; i++) {
      BT_kv entry = node->datk[i];
      if (entry.fo == 0)
        break;                  /* done */
      _bt_delco_droptree2(state, entry.fo, depth+1, maxdepth);
    }
  }

  _pending_nlist_insert(state, nodepg);
}

static void
_bt_delco_droptree(BT_state *state, pgno_t nodepg, uint8_t depth)
{
  /* completely drop a tree. Assume that all leaves under the tree are free
     (pgno = 0) */
  assert(nodepg >= 2);
  BT_meta *meta = state->meta_pages[state->which];
  return _bt_delco_droptree2(state, nodepg, depth, meta->depth);
}

static void
_bt_delco_trim_rsubtree_lhs2(BT_state *state, vaof_t lo, vaof_t hi,
                            pgno_t nodepg, uint8_t depth, uint8_t maxdepth)
{
  BT_page *node = _node_get(state, nodepg);
  size_t hiidx = 0;

  /* find hi idx of range */
  size_t i;
  for (i = 0; i < BT_DAT_MAXKEYS-1; i++) {
    vaof_t hhi = node->datk[i].va;
    if (hhi >= hi) {
      hiidx = i;
      break;
    }
  }

  /* set the lo address of datk[hiidx] to hi */
  node->datk[hiidx-1].va = hi;

  /* drop the subtrees left of the range */
  if (depth != maxdepth) {
    for (i = 0; i < hiidx-1; i++) {
      pgno_t childpg = node->datk[i].fo;
      if (childpg == 0)
        break;
      _bt_delco_droptree(state, childpg, depth+1);
    }
  }

  /* memmove the buffer so the found range is the first in the node */
  BYTE *dst = (BYTE *)&node->datk[0].va;
  BYTE *src = (BYTE *)&node->datk[hiidx-1].va;
  BYTE *end = (BYTE *)&node->datk[BT_DAT_MAXKEYS-1].fo;
  size_t len = end - src;

  memmove(dst, src, len);

  /* ;;: TODO add temporary asserts for testing? */

  /* and now zero the moved range */
  ZERO(dst+len, end-(dst+len));

  /* done if this is a leaf */
  if (depth == maxdepth)
    return;
  /* otherwise, recur on subtree */
  pgno_t rsubtree = node->datk[hiidx].fo;
  return _bt_delco_trim_rsubtree_lhs2(state, lo, hi, rsubtree, depth+1, maxdepth);
}

static void
_bt_delco_trim_rsubtree_lhs(BT_state *state, vaof_t lo, vaof_t hi,
                            pgno_t nodepg, uint8_t depth)
{
  BT_meta *meta = state->meta_pages[state->which];
  return _bt_delco_trim_rsubtree_lhs2(state, lo, hi, nodepg, depth, meta->depth);
}

static void
_bt_delco_trim_lsubtree_rhs2(BT_state *state, vaof_t lo, vaof_t hi,
                            pgno_t nodepg, uint8_t depth, uint8_t maxdepth)
{
  BT_page *node = _node_get(state, nodepg);
  size_t loidx = 0;

  /* find low idx of range */
  size_t i;
  for (i = 0; i < BT_DAT_MAXKEYS-1; i++) {
    vaof_t llo = node->datk[i].va;
    if (llo <= lo) {
      loidx = i;
      break;
    }
  }

  /* set the hi address of datk[loidx] to hi */
  node->datk[loidx+1].va = hi;

  /* drop the subtrees right of the range */
  if (depth != maxdepth) {
    /* recur and droptree for branches */
    for (i = loidx+1; i < BT_DAT_MAXKEYS-1; i++) {
      pgno_t childpg = node->datk[i].fo;
      if (childpg == 0)
        break;
      _bt_delco_droptree(state, childpg, depth+1);
    }
  }

  /* always zero rhs whether node is a leaf or a branch */
  BYTE *beg = (BYTE *)&node->datk[loidx+1].fo;
  BYTE *end = (BYTE *)&node->datk[BT_DAT_MAXKEYS-1].fo;
  size_t len = end - beg;

  ZERO(beg, len);
  /* ;;: this won't zero the last fo, but that should be fine. remove the assert
       when you're confident it /is/ fine */
  assert(node->datk[BT_DAT_MAXKEYS-1].fo == 0);

  /* done if this is a leaf */
  if (depth == maxdepth)
    return;
  /* otherwise, recur on the left subtree */
  pgno_t lsubtree = node->datk[loidx].fo;
  return _bt_delco_trim_lsubtree_rhs2(state, lo, hi, lsubtree, depth+1, maxdepth);
}

static void
_bt_delco_trim_lsubtree_rhs(BT_state *state, vaof_t lo, vaof_t hi,
                            pgno_t nodepg, uint8_t depth)
{
  BT_meta *meta = state->meta_pages[state->which];
  return _bt_delco_trim_lsubtree_rhs2(state, lo, hi, nodepg, depth, meta->depth);
}

static void
_bt_delco(BT_state *state, vaof_t lo, vaof_t hi,
          pgno_t nodepg, uint8_t depth, uint8_t maxdepth)
{
  /* ;;: "find_internal_splits" in the original algorithm */
  BT_page *node = _node_get(state, nodepg);
  size_t N = _bt_numkeys(node);

  size_t loidx = 0;
  size_t hiidx = 0;
  pgno_t lsubtree = 0;
  pgno_t rsubtree = 0;

  /* find low idx of range */
  for (size_t i = 0; i < BT_DAT_MAXKEYS-1; i++) {
    vaof_t llo = node->datk[i].va;
    if (llo <= lo) {
      loidx = i;
      break;
    }
  }

  /* find high idx of range */
  for (size_t i = loidx; i < BT_DAT_MAXKEYS-1; i++) {
    vaof_t hhi = node->datk[i].va;
    if (hhi >= hi) {
      assert(i > 0);
      hiidx = i - 1;
      break;
    }
  }

  /* non-split range and at leaf. done */
  if (depth == maxdepth
      && hiidx == loidx) {
    return;
  }

  lsubtree = node->datk[loidx].fo;
  rsubtree = node->datk[hiidx].fo;

  if (depth < maxdepth) {
    /* guarantee path is dirty by CoWing node if not */

    /* ;;: refactor? code duplication?? */
    if (!_bt_ischilddirty(node, loidx)) {
      BT_page *child = _node_get(state, lsubtree);
      BT_page *new;
      pgno_t newpg;
      _node_cow(state, child, &new, &newpg);
      lsubtree = node->datk[loidx].fo = newpg;
      _bt_dirtychild(node, loidx);
    }

    if (!_bt_ischilddirty(node, hiidx)) {
      BT_page *child = _node_get(state, rsubtree);
      BT_page *new;
      pgno_t newpg;
      _node_cow(state, child, &new, &newpg);
      rsubtree = node->datk[hiidx].fo = newpg;
      _bt_dirtychild(node, hiidx);
    }
  }

  /* non-split range, recurse to child tree */
  if (hiidx == loidx) {
    pgno_t childpg = node->datk[loidx].fo;
    _bt_delco(state, lo, hi, childpg, depth+1, maxdepth);
  }

  /* split range discovered */
  if (hiidx > loidx) {
    /* run first pass to guarantee range is completely free */
    if (!SUCC(_bt_delco_1pass(state, lo, hi))) {
      /* attempted insert on split range that cannot be coalesced */
      assert(0);
    }

    /* set leftmost boundary va to hi */
    node->datk[loidx+1].va = hi;

    /* set the lo side of the right boundary to hi */
    node->datk[hiidx].va = hi;

    /* drop all trees between the two subtrees */
    for (size_t i = loidx+1; i < hiidx; i++) {
      pgno_t childpg = node->datk[i].fo;
      _bt_delco_droptree(state, childpg, depth);
    }

    /* move buffer */
    BYTE *dst = (BYTE *)&node->datk[loidx+1].va;
    BYTE *src = (BYTE *)&node->datk[hiidx].va;
    BYTE *end = (BYTE *)&node->datk[BT_DAT_MAXKEYS-1].fo;
    size_t len = end - src;
    memmove(dst, src, len);
    ZERO(dst+len, end-(dst+len));

    /* trim left subtree then trim right subtree */
    _bt_delco_trim_lsubtree_rhs(state, lo, hi, lsubtree, depth+1);
    _bt_delco_trim_rsubtree_lhs(state, lo, hi, rsubtree, depth+1);

    /* done */
    return;
  }
}

/* ;;: todo, update meta->depth when we add a row. Should this be done in
     _bt_rebalance? */
static int
_bt_insert2(BT_state *state, vaof_t lo, vaof_t hi, pgno_t fo,
        BT_page *node, size_t depth)
{
  /* ;;: to be written in such a way that node is guaranteed both dirty and
       non-full */

  /* ;;: remember:
     - You need to CoW+dirty a node when you insert a non-dirty node.
     - You need to insert into a node when:
       - It's a leaf
       - It's a branch and you CoWed the child
     - Hence, all nodes in a path to a leaf being inserted into need to already
     be dirty or explicitly Cowed. Splitting doesn't actually factor into this
     decision afaict.
  */

  assert(node);

  int rc = 255;
  size_t N = 0;
  size_t childidx = _bt_childidx(node, lo, hi);
  assert(childidx != BT_DAT_MAXKEYS);
  BT_meta *meta = state->meta_pages[state->which];

  if (depth < meta->depth) {
    pgno_t childpgno = node->datk[childidx].fo;
    BT_page *child = _node_get(state, childpgno);
    N = _bt_numkeys(child);
  }

  /* nullcond: node is a leaf */
  if (meta->depth == depth) {
    /* guaranteed non-full and dirty by n-1 recursive call, so just insert */
    return _bt_insertdat(lo, hi, fo, node, childidx);
  }

  /* do we need to CoW the child node? */
  if (!_bt_ischilddirty(node, childidx)) {
    BT_page *newchild;
    pgno_t pgno;
    _node_cow(state, node, &newchild, &pgno);
    node->datk[childidx].fo = pgno;
    _bt_dirtychild(node, childidx);
  }

  /* do we need to split the child node? */
  if (N >= BT_DAT_MAXKEYS - 2) {
      pgno_t rchild_pgno;
      if (!SUCC(rc = _bt_split_child(state, node, childidx, &rchild_pgno)))
        return rc;

      /* since we split the child's data, recalculate the child idx */
      /* ;;: note, this can be simplified into a conditional i++ */
      childidx = _bt_childidx(node, lo, hi);

  }

  /* the child is now guaranteed non-full (split) and dirty. Recurse */
  BT_page *child = _node_get(state, node->datk[childidx].fo);
  return _bt_insert2(state, lo, hi, fo, child, depth+1);
}

static int
_bt_insert(BT_state *state, vaof_t lo, vaof_t hi, pgno_t fo)
/* handles CoWing/splitting of the root page since it's special cased. Then
   passes the child matching hi/lo to _bt_insert2 */
{
  int rc;

  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);

  /* the root MUST be dirty (zero checksum in metapage) */
  assert(meta->chk == 0);

  size_t N = _bt_numkeys(root);

  /* perform deletion coalescing (and preemptively guarantee path is dirty) if
     inserting a non-zero (non-free) page */
  if (fo != 0) {
    _bt_delco(state, lo, hi, meta->root, 1, meta->depth);
  }

  /* CoW root's child if it isn't already dirty */
  size_t childidx = _bt_childidx(root, lo, hi);
  assert(childidx != BT_DAT_MAXKEYS); /* ;;: this should catch the case of
                                           improperly inserting into a split
                                           range. Should we do it earlier or
                                           differently? */
  if (meta->depth > 1
      && !_bt_ischilddirty(root, childidx)) {
    BT_page *child = _node_get(state, root->datk[childidx].fo);
    BT_page *newchild;
    pgno_t  newchildpg;
    _node_cow(state, child, &newchild, &newchildpg);
    root->datk[childidx].fo = newchildpg;
    _bt_dirtychild(root, childidx);
  }

  /* before calling into recursive insert, handle root splitting since it's
     special cased (2 allocs) */
  if (N >= BT_DAT_MAXKEYS - 2) { /* ;;: remind, fix all these conditions to be - 2 */
    pgno_t pg = 0;

    /* the old root is now the left child of the new root */
    BT_page *left = root;
    BT_page *right = _node_alloc(state);
    BT_page *rootnew = _node_alloc(state);

    /* split root's data across left and right nodes */
    _bt_split_datcopy(left, right);
    /* save left and right in new root's .data */
    pg = _fo_get(state, left);
    rootnew->datk[0].fo = pg;
    rootnew->datk[0].va = 0;
    pg = _fo_get(state, right);
    rootnew->datk[1].fo = pg;
    rootnew->datk[1].va = right->datk[0].va;
    rootnew->datk[2].va = UINT32_MAX;
    /* dirty new root's children */
    _bt_dirtychild(rootnew, 0);
    _bt_dirtychild(rootnew, 1);
    /* update meta page information. (root and depth) */
    pg = _fo_get(state, rootnew);
    meta->root = pg;
    meta->depth += 1;
    root = rootnew;
  }

  /*
    meta is dirty
    root is dirty and split if necessary
    root's child in insert path is dirty and split if necessary
    finally, recurse on child
  */
  return _bt_insert2(state, lo, hi, fo, root, 1);
  /* return _bt_insert2(state, lo, hi, fo, child, 1); */
}

/* ;;: wip */
/* ;;: inspired by lmdb's MDB_pageparent. While seemingly unnecessary for
     _bt_insert, this may be useful for _bt_delete when we implement deletion
     coalescing */
typedef struct BT_ppage BT_ppage;
struct BT_ppage {
  BT_page *node;
  BT_page *parent;
};

static int
_bt_delete(BT_state *state, vaof_t lo, vaof_t hi)
{
  /* ;;: tmp, implement coalescing of zero ranges and merging/rebalancing of
       nodes */
  return _bt_insert(state, lo, hi, 0);
}

static int
_mlist_new(BT_state *state)
{
  /* implemented separate from _mlist_read since _mlist_read uses lo va == 0 to
     stop parsing node's data. This, however, is a valid starting condition when
     freshly creating the btree */

  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  assert(root->datk[0].fo == 0);

  vaof_t lo = root->datk[0].va;
  vaof_t hi = root->datk[1].va;
  size_t len = B2PAGES(hi - lo);

  BT_mlistnode *head = calloc(1, sizeof *head);

  head->next = 0;
  head->sz = len;
  head->va = OFF2ADDR(lo);

  state->mlist = head;

  return BT_SUCC;
}

static int
_flist_grow(BT_state *state, BT_flistnode *space)
/* growing the flist consists of expanding the backing persistent file, pushing
   that space onto the disk freelist, and updating the dimension members in
   BT_state */
{
  /* ;;: I don't see any reason to grow the backing file non-linearly, but we
       may want to adjust the size of the amount grown based on performance
       testing. */
  if (-1 == lseek(state->data_fd, state->file_size + PMA_GROW_SIZE, SEEK_SET))
    return errno;
  if (-1 == write(state->data_fd, "", 1))
    return errno;


  /* find the last node in the disk freelist */
  BT_flistnode *tail = state->flist;
  for (; tail->next; tail = tail->next)
    ;

  pgno_t lastpgfree = tail->pg + tail->sz;

  /* ;;: TODO, make sure you are certain of this logic. Further, add assertions
       regarding relative positions of state->file_size, state->frontier, and
       lastpgfree

       we MAY call into this routine even if there is freespace on the end
       because it's possible that freespace isn't large enough. We may also call
       into this routine when the frontier exceeds the last free pg because
       that's just how freelists work. ofc, frontier should never exceed
       file_size. what other assertions??

  */

  /* if the frontier (last pg in use) is less than the last page free, we should
     coalesce the new node with the tail. */
  if (state->frontier <= lastpgfree) {
    tail->sz += PMA_GROW_SIZE;
  }
  /* otherwise, a new node needs to be allocated */
  else {
    BT_flistnode *new = calloc(1, sizeof *new);
    /* since the frontier exceeds the last pg free, new freespace should
       naturally be allocated at the frontier */
    new->pg = state->frontier;
    new->sz = PMA_GROW_SIZE;
    tail->next = new;
  }

  /* finally, update the file size */
  state->file_size += PMA_GROW_SIZE;

  return BT_SUCC;
}

static int
_flist_new(BT_state *state)
{
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  assert(root->datk[0].fo == 0);

  vaof_t lo = root->datk[0].va;
  vaof_t hi = root->datk[1].va;
  size_t len = B2PAGES(hi - lo);

  BT_flistnode *head = calloc(1, sizeof *head);

  head->next = 0;
  head->sz = len;
  head->pg = PMA_GROW_SIZE; /* ;;: should we invoke logic to expand the backing file
                          here? probably. implement it */ /*  */
  state->flist = head;

  return BT_SUCC;
}

#if USE_NLIST
static int
_nlist_new(BT_state *state)
#define NLIST_PG_START 2        /* the third page */
{
  BT_meta *meta = state->meta_pages[state->which];
  BT_nlistnode *head = calloc(1, sizeof *head);

  /* the size of a new node freelist is just the first stripe length */
  head->sz = BLK_BASE_LEN0;
  head->va = &((BT_page *)state->map)[BT_NUMMETAS];
  head->next = 0;

  state->nlist = head;

  return BT_SUCC;
}

static BT_nlistnode *
_nlist_read_prev(BT_nlistnode *head, BT_nlistnode *curr)
{
  /* find nlist node preceding curr and return it */
  BT_nlistnode *p, *n;
  p = head;
  n = head->next;
  for (; n; p = n, n = n->next) {
    if (n == curr)
      return p;
  }
  return 0;
}

/* TODO this is a pretty bad algorithm in terms of time complexity. It should be
   fixed, but isn't necessary now as our nlist is quite small. You may want to
   consider making nlist doubly linked or incorporate a sort and merge step. */
static int
_nlist_read2(BT_state *state, BT_page *node, uint8_t maxdepth,
             BT_nlistnode *head, uint8_t depth)
/* recursively walk all nodes in the btree. Allocating new nlist nodes when a
   node is found to be in a stripe unaccounted for. For each node found,
   split/shrink the appropriate node to account for the allocated page */
{
  BT_nlistnode *p, *n;
  p = head;
  n = head->next;

  /* find the nlist node that fits the current btree node */
  for (; n; p = n, n = n->next) {
    if (p->va <= node && p->va + p->sz > node)
      break;
  }

  /* if the nlist node is only one page wide, it needs to be freed */
  if (p->sz == 1) {
    BT_nlistnode *prev = _nlist_read_prev(head, p);
    prev->next = p->next;
    free(p);
    goto e;
  }

  /* if the btree node resides at the end of the nlist node, just shrink it */
  BT_page *last = p->va + p->sz - 1;
  if (last == node) {
    p->sz -= 1;
    goto e;
  }

  /* if the btree node resides at the start of the nlist node, likewise shrink
     it and update the va */
  if (p->va == node) {
    p->sz -= 1;
    p->va += 1;
    goto e;
  }

  /* otherwise, need to split the current nlist node */
  BT_nlistnode *right = calloc(1, sizeof *right);
  size_t lsz = node - p->va;
  size_t rsz = (p->va + p->sz) - node;
  /* remove 1 page from the right nlist node's size to account for the allocated
     btree node */
  rsz -= 1;
  assert(lsz > 0 && rsz > 0);

  /* update the size of the left node. And set the size and va of the right
     node. Finally, insert the new nlist node into the nlist. */
  p->sz = lsz;
  right->sz = rsz;
  right->va = node + 1;
  right->next = p->next;
  p->next = right;

 e:
  /* if at a leaf, we're finished */
  if (depth == maxdepth) {
    return BT_SUCC;
  }

  /* otherwise iterate over all child nodes, recursively constructing the
     list */
  int rc = BT_SUCC;
  for (size_t i = 0; i < BT_DAT_MAXKEYS; i++) {
    BT_kv kv = node->datk[i];
    BT_page *child = _node_get(state, node->datk[i].fo);
    if (!child) continue;
    if (!SUCC(rc = _nlist_read2(state,
                                child,
                                maxdepth,
                                head,
                                depth+1)))
      return rc;
  }

  /* all children traversed */
  return BT_SUCC;
}

static int
_nlist_read(BT_state *state)
{
  /* ;;: this should theoretically be simpler than _mlist_read. right? We can
     derive the stripes that contain nodes from the block base array stored in
     the metapage. What else do we need to know? -- the parts of each stripe
     that are free or in use. How can we discover that?

     1) Without storing any per-page metadata, we could walk the entire tree
     from the root. Check the page number of the node. And modify the freelist
     accordingly.

     2) If we stored per-page metadata, this would be simpler. Linearly traverse
     each stripe and check if the page is BT_NODE or BT_FREE.

     -- are there downsides to (2)? The only advantage to this would be quicker
        startup. So for now, going to traverse all nodes and for each node,
        traverse the nlist and split it appropriately.
  */

  int rc = BT_SUCC;
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);

  /* ;;: since partition striping isn't implemented yet, simplifying code by
     assuming all nodes reside in the 2M region */
  BT_nlistnode *head = calloc(1, sizeof *head);
  head->sz = BLK_BASE_LEN0;
  head->va = &((BT_page *)state->map)[BT_NUMMETAS];
  head->next = 0;

  if (!SUCC(rc = _nlist_read2(state, root, meta->depth, head, 1)))
    return rc;

  state->nlist = head;

  return rc;
}
#endif

static BT_mlistnode *
_mlist_read2(BT_state *state, BT_page *node, uint8_t maxdepth, uint8_t depth)
{
  /* leaf */
  if (depth == maxdepth) {
    BT_mlistnode *head, *prev;
    head = prev = calloc(1, sizeof *head);

    size_t i = 0;
    BT_kv *kv = &node->datk[i];
    while (i < BT_DAT_MAXKEYS - 1) {
#if CAN_COALESCE
      /* free and contiguous with previous mlist node: merge */
      if (kv->fo == 0
          && ADDR2OFF(prev->va) + P2BYTES(prev->sz) == kv->va) {
        vaof_t hi = node->datk[i+1].va;
        vaof_t lo = kv->va;
        size_t len = B2PAGES(hi - lo);
        prev->sz += len;
      }
      /* free but not contiguous with previous mlist node: append new node */
      else if (kv->fo == 0) {
#endif
        BT_mlistnode *new = calloc(1, sizeof *new);
        vaof_t hi = node->datk[i+1].va;
        vaof_t lo = kv->va;
        size_t len = B2PAGES(hi - lo);
        new->sz = len;
        new->va = OFF2ADDR(lo);
        prev->next = new;
        prev = new;
#if CAN_COALESCE
      }
#endif

      kv = &node->datk[++i];
    }
    return head;
  }

  /* branch */
  size_t i = 0;
  BT_mlistnode *head, *prev;
  head = prev = 0;
  for (; i < BT_DAT_MAXKEYS; ++i) {
    BT_kv kv = node->datk[i];
    if (kv.fo == BT_NOPAGE)
      continue;
    BT_page *child = _node_get(state, kv.fo);
    BT_mlistnode *new = _mlist_read2(state, child, maxdepth, depth+1);
    if (head == 0) {
      head = prev = new;
    }
    else {
      /* just blindly append and unify the ends afterward */
      prev->next = new;
    }
  }
  return 0;
}

static int
_mlist_read(BT_state *state)
{
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  uint8_t maxdepth = meta->depth;
  BT_mlistnode *head = _mlist_read2(state, root, maxdepth, 1);

  /*
    trace the full freelist and unify nodes one last time
    NB: linking the leaf nodes would make this unnecessary
  */
#if CAN_COALESCE
  BT_mlistnode *p = head;
  BT_mlistnode *n = head->next;
  while (n) {
    size_t llen = P2BYTES(p->sz);
    uintptr_t laddr = (uintptr_t)p->va;
    uintptr_t raddr = (uintptr_t)n->va;
    /* contiguous: unify */
    if (laddr + llen == raddr) {
      p->sz += n->sz;
      p->next = n->next;
      free(n);
    }
  }
#endif

  state->mlist = head;
  return BT_SUCC;
}

static int
_mlist_delete(BT_state *state)
{
  BT_mlistnode *head, *prev;
  head = prev = state->mlist;
  while (head->next) {
    prev = head;
    head = head->next;
    free(prev);
  }
  state->mlist = 0;
  return BT_SUCC;
}

static void
_flist_split(BT_flistnode *head, BT_flistnode **left, BT_flistnode **right)
/* split flist starting at head into two lists, left and right at the midpoint
   of head */
{
  assert(head != 0);
  BT_flistnode *slow, *fast;
  slow = head; fast = head->next;

  while (fast) {
    fast = fast->next;
    if (fast) {
      slow = slow->next;
      fast = fast->next;
    }
  }

  *left = head;
  *right = slow->next;
  slow->next = 0;
}

static BT_flistnode *
_flist_merge2(BT_flistnode *l, BT_flistnode *r)
/* returns the furthest node in l that has a pg less than the first node in r */
{
  assert(l);
  assert(r);

  BT_flistnode *curr, *prev;
  prev = l;
  curr = l->next;

  while (curr) {
    if (curr->pg < r->pg) {
      prev = curr;
      curr = curr->next;
    }
  }

  if (prev->pg < r->pg)
    return prev;

  return 0;
}

static BT_flistnode *
_flist_merge(BT_flistnode *l, BT_flistnode *r)
/* merge two sorted flists, l and r and return the sorted result */
{
  BT_flistnode *head;

  if (!l) return r;
  if (!r) return l;

  while (l && r) {
    if (l->next == 0) {
      l->next = r;
      break;
    }
    if (r->next == 0) {
      break;
    }

    BT_flistnode *ll = _flist_merge2(l, r);
    BT_flistnode *rnext = r->next;
    /* insert head of r into appropriate spot in l */
    r->next = ll->next;
    ll->next = r;
    /* adjust l and r heads */
    l = ll->next;
    r = rnext;
  }

  return head;
}

BT_flistnode *
_flist_mergesort(BT_flistnode *head)
{
  if (head == 0 || head->next == 0)
    return head;

  BT_flistnode *l, *r;
  _flist_split(head, &l, &r);

  /* ;;: todo, make it non-recursive. Though, shouldn't matter as much here
       since O(log n). merge already non-recursive */
  _flist_mergesort(l);
  _flist_mergesort(r);

  return _flist_merge(l, r);
}

BT_flistnode *
_flist_read2(BT_state *state, BT_page *node, uint8_t maxdepth, uint8_t depth)
{
  /* leaf */
  if (depth == maxdepth) {
    BT_flistnode *head, *prev;
    head = prev = calloc(1, sizeof(*head));

    /* ;;: fixme the head won't get populated in this logic */
    size_t i = 0;
    BT_kv *kv = &node->datk[i];
    while (i < BT_DAT_MAXKEYS - 1) {
      /* Just blindly append nodes since they aren't guaranteed sorted */
      BT_flistnode *new = calloc(1, sizeof *new);
      vaof_t hi = node->datk[i+1].va;
      vaof_t lo = kv->va;
      size_t len = B2PAGES(hi - lo);
      pgno_t fo = kv->fo;
      new->sz = len;
      new->pg = fo;
      prev->next = new;
      prev = new;

      kv = &node->datk[++i];
    }
    return head;
  }

  /* branch */
  size_t i = 0;
  BT_flistnode *head, *prev;
  head = prev = 0;
  for (; i < BT_DAT_MAXKEYS; ++i) {
    BT_kv kv = node->datk[i];
    if (kv.fo == BT_NOPAGE)
      continue;
    BT_page *child = _node_get(state, kv.fo);
    BT_flistnode *new = _flist_read2(state, child, maxdepth, depth+1);
    if (head == 0) {
      head = prev = new;
    }
    else {
      /* just blindly append and unify the ends afterward */
      prev->next = new;
    }
  }
  return 0;
}

static int
_flist_read(BT_state *state)
{
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  uint8_t maxdepth = meta->depth;
  BT_flistnode *head = _flist_read2(state, root, maxdepth, 0);
  /* ;;: infinite loop with proper starting depth of 1. -- fix that! */
  /* BT_flistnode *head = _flist_read2(state, root, maxdepth, 1); */

  if (head == 0)
    return BT_SUCC;

  /* sort the freelist */
  _flist_mergesort(head);

  /* merge contiguous regions after sorting */
  BT_flistnode *p = head;
  BT_flistnode *n = head->next;
  while (n) {
    size_t llen = p->sz;
    pgno_t lfo = p->pg;
    pgno_t rfo = n->pg;
    /* contiguous: unify */
    if (lfo + llen == rfo) {
      p->sz += n->sz;
      p->next = n->next;
      free(n);
    }
  }

  state->flist = head;
  return BT_SUCC;
}

static int
_flist_delete(BT_state *state)
{
  BT_flistnode *head, *prev;
  head = prev = state->flist;
  while (head->next) {
    prev = head;
    head = head->next;
    free(prev);
  }
  state->flist = 0;
  return BT_SUCC;
}

#define CLOSE_FD(fd)                            \
  do {                                          \
    close(fd);                                  \
    fd = -1;                                    \
  } while(0)

/* TODO: move to lib */
static uint32_t
nonzero_crc_32(void *dat, size_t len)
{
  unsigned char nonce = 0;
  uint32_t chk = crc_32(dat, len);

  do {
    if (nonce > 8)
      abort();
    chk = update_crc_32(chk, nonce++);
  } while (chk == 0);

  return chk;
}

static int
_bt_state_meta_which(BT_state *state, int *which)
{
  BT_meta *m1 = state->meta_pages[0];
  BT_meta *m2 = state->meta_pages[1];
  *which = -1;

  if (m1->flags == 0) {
    /* first is dirty */
    *which = 1;
  }
  else if (m2->flags == 0) {
    /* second is dirty */
    *which = 0;
  }
  else if (m1->txnid > m2->txnid) {
    /* first is most recent */
    *which = 0;
  }
  else if (m1->txnid < m2->txnid) {
    /* second is most recent */
    *which = 1;
  }
  else {
    /* invalid state */
    return EINVAL;
  }

  /* checksum the metapage found and abort if checksum doesn't match */
  BT_meta *meta = state->meta_pages[*which];
  uint32_t chk = nonzero_crc_32(meta, BT_META_LEN);
  if (chk != meta->chk) {
    abort();
  }

  return BT_SUCC;
}

static int
_bt_state_read_header(BT_state *state)
{
  /* TODO: actually read the header and copy the data to meta when we implement
     persistence */
  BT_page metas[2];
  int rc, len, which;
  BT_meta *m1, *m2;

  /* pma already exists, parse metadata file */
  m1 = state->meta_pages[0];
  m2 = state->meta_pages[1];

  /* ;;: TODO, need to store last page in use by pma in both metadata pages. choose the frontier after _bt_state_meta_which and store it in state */
  TRACE();

  if ((len = pread(state->data_fd, metas, BT_PAGESIZE*2, 0))
      != BT_PAGESIZE*2) {
    /* new pma */
    return ENOENT;
  }

  /* validate magic */
  if (m1->magic != BT_MAGIC) {
    DPRINTF("metapage 0x%pX inconsistent magic: 0x%" PRIX32, m1, m1->magic);
    return EINVAL;
  }
  if (m2->magic != BT_MAGIC) {
    DPRINTF("metapage 0x%pX inconsistent magic: 0x%" PRIX32, m2, m2->magic);
    return EINVAL;
  }

  /* validate flags */
  if (m1->flags & BP_META != BP_META) {
    DPRINTF("metapage 0x%pX missing meta page flag", m1);
    return EINVAL;
  }
  if (m2->flags & BP_META != BP_META) {
    DPRINTF("metapage 0x%pX missing meta page flag", m2);
    return EINVAL;
  }

  /* validate binary version */
  if (m1->version != BT_VERSION) {
    DPRINTF("version mismatch on metapage: 0x%pX, metapage version: %" PRIu32 ", binary version %u",
            m1, m1->version, BT_VERSION);
    return EINVAL;
  }

  /* validate binary version */
  if (m2->version != BT_VERSION) {
    DPRINTF("version mismatch on metapage: 0x%pX, metapage version: %" PRIu32 ", binary version %u",
            m2, m2->version, BT_VERSION);
    return EINVAL;
  }

  if (!SUCC(rc = _bt_state_meta_which(state, &which)))
    return rc;

  state->which = which;

  return BT_SUCC;
}

static int
_bt_state_meta_new(BT_state *state)
#define INITIAL_ROOTPG 2
{
  BT_page *p1, *p2, *root;
  BT_meta meta = {0};
  int rc, pagesize;

  TRACE();

  /* ;;: HERE HERE HERE: call node_alloc */
  root = _node_alloc(state);
  _bt_root_new(root);

  pagesize = sizeof *p1;

  /* initialize meta struct */
  meta.magic = BT_MAGIC;
  meta.version = BT_VERSION;
  meta.last_pg = 1;
  meta.txnid = 0;
  meta.fix_addr = BT_MAPADDR;
  meta.blk_cnt = 1;
  meta.depth = 1;
  meta.flags = BP_META;
  meta.root = _fo_get(state, root);
  assert(meta.root == INITIAL_ROOTPG); /* ;;: remove?? */

  /* initialize the block base array */
  meta.blk_base[0] = BT_NUMMETAS + 1;

  /* initialize the metapages */
  p1 = &((BT_page *)state->map)[0];
  p2 = &((BT_page *)state->map)[1];

  /* copy the metadata into the metapages */
  memcpy(METADATA(p1), &meta, sizeof meta);
  /* ;;: todo, should the second metapage actually share a .root with the
       first?? */
  memcpy(METADATA(p2), &meta, sizeof meta);

  return BT_SUCC;
}

static int
_bt_state_load(BT_state *state)
{
  int rc;
  int new = 0;
  BT_page *p;
  struct stat stat;

  TRACE();

  if (!SUCC(rc = _bt_state_read_header(state))) {
    if (rc != ENOENT) return rc;
    DPUTS("creating new db");
    state->file_size = PMA_GROW_SIZE;
    new = 1;
  }

  state->map = mmap(BT_MAPADDR,
                    BT_ADDRSIZE,
                    PROT_READ | PROT_WRITE,
                    MAP_FIXED | MAP_SHARED,
                    state->data_fd,
                    0);

  p = (BT_page *)state->map;
  state->meta_pages[0] = METADATA(p);
  state->meta_pages[0] = METADATA(p + 1);

#ifndef USE_NLIST
  state->node_freelist = &((BT_page *)state->map)[3]; /* begin allocating nodes
                                                       on third page (first two
                                                       are for metadata) -- this
                                                       was quite dumb. This is
                                                       the fourth page of
                                                       course. But it worked,
                                                       because in _bt_root_new
                                                       we use the third page
                                                       without calling the
                                                       allocation function */
#endif

  /* new db, so populate metadata */
  if (new) {
    /* ;;: move this logic to _flist_new */
    if (-1 == lseek(state->data_fd, state->file_size, SEEK_SET))
      return errno;
    if (-1 == write(state->data_fd, "", 1))
      return errno;

    state->file_size = PMA_GROW_SIZE;

#if USE_NLIST
    /* ;;: necessary to call this before _bt_state_meta_new */
    assert(SUCC(_nlist_new(state)));
#endif

    if (!SUCC(rc = _bt_state_meta_new(state))) {
      munmap(state->map, BT_ADDRSIZE);
      return rc;
    }
  }
  else {
    if (fstat(state->data_fd, &stat) != 0)
      return errno;

    state->file_size = stat.st_size;
  }

  if (new) {
    assert(SUCC(_mlist_new(state)));
    assert(SUCC(_flist_new(state)));
  }
  else {
    assert(SUCC(_mlist_read(state)));
    assert(SUCC(_flist_read(state)));
#if USE_NLIST
    /* ;;: this might need to be re-ordered given that _nlist_new needs to be
         called before _bt_state_meta_new. Haven't thought about it yet. */
    assert(SUCC(_nlist_read(state)));
#endif
  }

  return BT_SUCC;
}

/* ;;: TODO, when persistence has been implemented, _bt_falloc will probably
     need to handle extension of the file with appropriate striping. i.e. if no
     space is found on the freelist, save the last entry, expand the file size,
     and set last_entry->next to a new node representing the newly added file
     space */
static pgno_t
_bt_falloc(BT_state *state, size_t pages)
{
  /* walk the persistent file freelist and return a pgno with sufficient
     contiguous space for pages */
  BT_flistnode **n = &state->flist;
  pgno_t ret = 0;

  /* first fit */
  /* ;;: is there any reason to use a different allocation strategy for disk? */
  for (; *n; n = &(*n)->next) {
    /* perfect fit */
    if ((*n)->sz == pages) {
      pgno_t ret;
      ret = (*n)->pg;
      *n = (*n)->next;
      return ret;
    }
    /* larger than necessary: shrink the node */
    if ((*n)->sz > pages) {
      pgno_t ret;
      ret = (*n)->pg;
      (*n)->sz -= pages;
      (*n)->pg = (*n)->pg + pages;
      return ret;
    }
  }

  return 0;
}

static int
_bt_sync_hasdirtypage(BT_state *state, BT_page *node)
/* ;;: could be more efficiently replaced by a gcc vectorized builtin */
{
  for (size_t i = 0; i < NMEMB(node->head.dirty); i++) {
    if (node->head.dirty[i] != 0)
      return 1;
  }

  return 0;
}

static int
_bt_sync_leaf(BT_state *state, BT_page *node)
{
  /* msync all of a leaf's data that is dirty. The caller is expected to sync
     the node itself and mark it as clean in the parent. */
  pgno_t pg;
  size_t i = 0;

  for (size_t i = 0; i < BT_DAT_MAXKEYS-1; i++) {
    if (!_bt_ischilddirty(node, i))
      continue;                 /* not dirty. nothing to do */

    /* ;;: we don't actually need the page, do we? */
    /* pgno_t pg = node->datk[i].fo; */
    vaof_t lo = node->datk[i].va;
    vaof_t hi = node->datk[i+1].va;
    size_t bytelen = hi - lo;
    void *addr = OFF2ADDR(lo);

    /* sync the page */
    if (msync(addr, bytelen, MS_SYNC))
      return errno;

    /* and clean the dirty bit */
    _bt_cleanchild(node, i);
  }

  /* ;;: all data pages synced. should we now sync the node as well? No, I think
       that should be the caller's responsibility */

  /* ;;: it is probably faster to scan the dirty bit set and derive the datk idx
     rather than iterate over the full datk array and check if it is dirty. This
     was simpler to implement for now though. */
  /* while (_bt_sync_hasdirtypage(state, node)) { */
  /*   ... */
  /* } */

  return BT_SUCC;
}

static int
_bt_sync_meta(BT_state *state)
/* syncs the metapage and performs necessary checksumming. Additionally, flips
   the which */
{
  BT_meta *meta = state->meta_pages[state->which];
  BT_meta *newmeta;
  uint32_t chk;
  int newwhich;

  /* checksum the metapage */
  chk = nonzero_crc_32(meta, BT_META_LEN);
  /* ;;: todo: guarantee the chk cannot be zero */

  meta->chk = chk;

  /* sync the metapage */
  if (msync(meta, sizeof(BT_page), MS_SYNC))
    return errno;

  /* zero the new metapage's checksum */
  newwhich = state->which ? 0 : 1;
  newmeta = state->meta_pages[newwhich];
  newmeta->chk = 0;

  /* copy over metapage to new metapage excluding the checksum */
  memcpy(newmeta, meta, BT_META_LEN);

  /* CoW a new root since the root referred to by the metapage should always be
     dirty */
  BT_page *root, *newroot;
  pgno_t newrootpg;
  root = _node_get(state, newmeta->root);
  if (!SUCC(_node_cow(state, root, &newroot, &newrootpg)))
    abort();

  newmeta->root = newrootpg;

  /* finally, switch the metapage we're referring to */
  state->which = newwhich;

  return BT_SUCC;
}

static int
_bt_sync(BT_state *state, BT_page *node, uint8_t depth, uint8_t maxdepth)
/* recursively syncs the subtree under node. The caller is expected to sync node
   itself and mark it clean. */
{
  int rc = 0;

  /* leaf */
  if (depth == maxdepth) {
    _bt_sync_leaf(state, node);
    return BT_SUCC;
  }

  /* do dfs */
  for (size_t i = 0; i < BT_DAT_MAXKEYS-1; i++) {
    if (!_bt_ischilddirty(node, i))
      continue;                 /* not dirty. nothing to do */

    BT_page *child = _node_get(state, node->datk[i].fo);

    /* recursively sync the child's data */
    if (rc = _bt_sync(state, child, depth+1, maxdepth))
      return rc;

    /* sync the child node */
    if (msync(child, sizeof(BT_page), MS_SYNC))
      return errno;

    /* clean the child */
    _bt_cleanchild(node, i);
  }

  return BT_SUCC;
}


//// ===========================================================================
////                            btree external routines

int
bt_state_new(BT_state **state)
{
  TRACE();

  BT_state *s = calloc(1, sizeof *s);
  s->meta_fd = s->data_fd = -1;
  s->fixaddr = BT_MAPADDR;
  *state = s;
  return BT_SUCC;
}

#define DATANAME "/data.pma"
int
bt_state_open(BT_state *state, const char *path, ULONG flags, mode_t mode)
{
  int oflags, rc;
  char *dpath;

  TRACE();
  UNUSED(flags);

  oflags = O_RDWR | O_CREAT;
  dpath = malloc(strlen(path) + sizeof(DATANAME));
  if (!dpath) return ENOMEM;
  sprintf(dpath, "%s" DATANAME, path);

  if (mkdir(path, 0774) == -1)
    return errno;

  if ((state->data_fd = open(dpath, oflags, mode)) == -1)
    return errno;

  if (!SUCC(rc = _bt_state_load(state)))
    goto e;

  /* ;;: this may be entirely unnecessary */
  oflags |= O_DSYNC;            /* see man 2 open */
  if ((state->meta_fd = open(dpath, oflags, mode)) == -1) {
    rc = errno;
    goto e;
  }

  state->path = strdup(dpath);

 e:
  /* cleanup FDs stored in state if anything failed */
  if (!SUCC(rc)) {
    if (state->data_fd != -1) CLOSE_FD(state->data_fd);
    if (state->meta_fd != -1) CLOSE_FD(state->meta_fd);
  }

  free(dpath);
  return rc;
}

int
bt_state_close(BT_state *state)
{
  int rc;
  if (state->data_fd != -1) CLOSE_FD(state->data_fd);
  if (state->meta_fd != -1) CLOSE_FD(state->meta_fd);

  _mlist_delete(state);
  _flist_delete(state);

  /* ;;: wip delete the file because we haven't implemented persistence yet */
  if (!SUCC(rc = remove(state->path)))
    return rc;

  return BT_SUCC;
}

void *
bt_malloc(BT_state *state, size_t pages)
{
  BT_mlistnode **n = &state->mlist;
  void *ret = 0;
  /* first fit */
  for (; *n; n = &(*n)->next) {
    /* perfect fit */
    if ((*n)->sz == pages) {
      ret = (*n)->va;
      *n = (*n)->next;
      break;
    }
    /* larger than necessary: shrink the node */
    if ((*n)->sz > pages) {
      ret = (*n)->va;
      (*n)->sz -= pages;
      (*n)->va = (BT_page *)(*n)->va + pages;
      break;
    }
  }

  pgno_t pgno = _bt_falloc(state, pages);
  bp(pgno != 0);
  _bt_insert(state,
             ADDR2OFF(ret),
             ADDR2OFF(ret) + P2BYTES(pages),
             pgno);

  bp(ret != 0);
  return ret;
}

void
bt_free(BT_state *state, void *lo, void *hi)
{
  vaof_t looff = ADDR2OFF(lo);
  vaof_t hioff = ADDR2OFF(hi);
  _bt_insert(state, looff, hioff, 0);
  /* ;;: and now add freespace to state->flist. coalescing when you do so */
}

int
bt_sync(BT_state *state)
{
  /* as is often the case, handling the metapage/root is a special case, which
     is done here. Syncing any other page of the tree is done in _bt_sync */
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  int rc = 0;

  if (rc = _bt_sync(state, root, 1, meta->depth))
    return rc;

  /* sync the root page */
  if (msync(root, sizeof(BT_page), MS_SYNC))
    return errno;

  /* then sync the metapage */
  if (rc = _bt_sync_meta(state))
    return rc;

  return BT_SUCC;
}

uint64_t
bt_meta_get(BT_state *state, size_t idx)
{
  BT_meta *meta = state->meta_pages[state->which];
  assert((uintptr_t)&meta->roots[idx] - (uintptr_t)&meta <= sizeof *meta);
  return meta->roots[idx];
}

void
bt_meta_set(BT_state *state, size_t idx, uint64_t val)
{
  BT_meta *meta = state->meta_pages[state->which];
  assert((uintptr_t)&meta->roots[idx] - (uintptr_t)&meta <= sizeof *meta);
  meta->roots[idx] = val;
}


//// ===========================================================================
////                                    tests

/* ;;: obv this should be moved to a separate file */
static void
_sham_sync_clean(BT_page *node)
{
  for (uint8_t *dit = &node->head.dirty[0]
         ; dit < &node->head.dirty[sizeof(node->head.dirty) - 1]
         ; dit++) {
    *dit = 0;
  }
}

static void
_sham_sync2(BT_state *state, BT_page *node, uint8_t depth, uint8_t maxdepth)
{
  if (depth == maxdepth) return;

  /* clean node */
  _sham_sync_clean(node);

  /* then recurse and clean all children with DFS */
  size_t N = _bt_numkeys(node);
  for (size_t i = 1; i < N; ++i) {
    BT_kv kv = node->datk[i];
    pgno_t childpg = kv.fo;
    BT_page *child = _node_get(state, childpg);
    _sham_sync2(state, child, depth+1, maxdepth);
  }
}

static void
_sham_sync(BT_state *state)
{
  /* walk the tree and unset the dirty bit from all pages */
  BT_meta *meta = state->meta_pages[state->which];
  BT_page *root = _node_get(state, meta->root);
  meta->chk = nonzero_crc_32(meta, BT_META_LEN);
  _sham_sync2(state, root, 1, meta->depth);
}

static void
_bt_printnode(BT_page *node)
{
  printf("node: %p\n", node);
  printf("data: \n");
  for (size_t i = 0; i < BT_DAT_MAXKEYS; ++i) {
    if (i && node->datk[i].va == 0)
      break;
    printf("[%5zu] %10x %10x\n", i, node->datk[i].va, node->datk[i].fo);
  }
}

static void
_test_nodeinteg(BT_state *state, BT_findpath *path,
                vaof_t lo, vaof_t hi, pgno_t pg)
{
  size_t childidx = 0;
  BT_page *parent = 0;

  assert(SUCC(_bt_find(state, path, lo, hi)));
  parent = path->path[path->depth];
  /* _bt_printnode(parent); */
  childidx = path->idx[path->depth];
  assert(parent->datk[childidx].fo == pg);
  assert(parent->datk[childidx].va == lo);
  assert(parent->datk[childidx+1].va == hi);
}

int main(int argc, char *argv[])
{
  BT_state *state;
  BT_findpath path = {0};
  int rc = 0;


//// ===========================================================================
////                                  test0 wip

  /* deletion coalescing */
  bt_state_new(&state);
  assert(SUCC(bt_state_open(state, "./pmatest", 0, 0644)));

  /* enable coalescing of the memory freelist */
#undef  CAN_COALESCE
#define CAN_COALESCE 1

  /* ;;: disabling for now as I don't have an answer to the "how to find the hi
       address on a bt_free call so that _bt_delete can be called" question */
#if 0
  void *t0a = bt_malloc(state, 10);
  void *t0b = bt_malloc(state, 10);
  bt_free(state, t0a);
  bt_free(state, t0b);
  /* memory freelist got coallesced. next malloc call should find the same range
     and result in attempting to insert a range that overlaps a non-coallesced
     region */
  void *t0ab = bt_malloc(state, 20);
  /* t0a should have the same address as t0ab */
  assert(t0a == t0ab);
#endif

  /* ;;: can still suitably test by calling insert and delete routines directly */
  _bt_insert(state, 0x1000, 0x4000, 4);
  _bt_insert(state, 0x4000, 0x8000, 4);
  _bt_delete(state, 0x1000, 0x4000);
  _bt_delete(state, 0x4000, 0x8000);
  _bt_insert(state, 0x1000, 0x7000, 7);


  //// ===========================================================================
  ////                                    test1

  bt_state_new(&state);
  assert(SUCC(bt_state_open(state, "./pmatest", 0, 0644)));
  void * xxx = bt_malloc(state, 10); /* tmp - testing malloc logic */


  /* splitting tests. Insert sufficient data to force splitting. breakpoint before
     that split is performed */

  /* the hhi == hi case for more predictable splitting math */
  vaof_t lo = 10;
  /* vaof_t hi = BT_DAT_MAXKEYS * 4; */
  vaof_t hi = 0xDEADBEEF;
  pgno_t pg = 1;                /* dummy value */
  for (size_t i = 0; i < BT_DAT_MAXKEYS * 4; ++i) {
    /* if (i % (BT_DAT_MAXKEYS - 2) == 0) */
    /*   bp(0);                    /\* breakpoint on split case *\/ */
    _bt_insert(state, lo, hi, pg);
    _test_nodeinteg(state, &path, lo, hi, pg);
    lo++; pg++;
  }

  int which = state->which;
  /* sham sync and re-run insertions */
  _sham_sync(state);
  for (size_t i = 0; i < BT_DAT_MAXKEYS * 4; ++i) {
    _bt_insert(state, lo, hi, pg);
    _test_nodeinteg(state, &path, lo++, hi, pg++);
  }
  assert(which != state->which);

  assert(SUCC(bt_state_close(state)));



//// ===========================================================================
////                                    test2

  assert(SUCC(bt_state_open(state, "./pmatest", 0, 644)));
  _mlist_read(state);
  _flist_read(state);

  /* varieties of insert */

  /* 2.1 exact match */
  lo = 0x10;
  hi = 0x20;
  pg = 0xFFFFFFFF;

  bp(0);
  _bt_insert(state, lo, hi, pg);
  _bt_insert(state, lo, hi, pg);

  /* ;;: you should also probably assert the data is laid out in datk at you expect */
  _test_nodeinteg(state, &path, lo, hi, pg);

  _bt_delete(state, lo, hi);

  /* 2.2 neither bounds match */
  bp(0);
  _bt_insert(state, lo, hi, pg);
  _bt_insert(state, lo+2, hi-2, pg-1);

  _test_nodeinteg(state, &path, lo, hi, pg);
  _test_nodeinteg(state, &path, lo+2, hi-2, pg-1);

  _bt_delete(state, lo, hi);
  _bt_delete(state, lo+2, hi-2);

  /* 2.3 space to right */
  bp(0);
  _bt_insert(state, lo, hi, pg);
  _bt_insert(state, lo, hi-2, pg-1);

  _test_nodeinteg(state, &path, lo, hi, pg);
  _test_nodeinteg(state, &path, lo, hi-2, pg-1);

  _bt_delete(state, lo, hi);
  _bt_delete(state, lo, hi-2);

  /* 2.4 space to left */
  bp(0);

  _bt_insert(state, lo, hi, pg);
  _bt_insert(state, lo+2, hi, pg-1);

  _test_nodeinteg(state, &path, lo, hi, pg);
  _test_nodeinteg(state, &path, lo+2, hi, pg-1);

  _bt_delete(state, lo, hi);
  _bt_delete(state, lo+2, hi);

  assert(SUCC(bt_state_close(state)));

  return 0;
}


/* ;;:

   1) checksum m1
   2) sync m1
   3) zero m2
   4) copy all of m1 to m2 excluding m1

   The current dirty metapage should have a zero checksum so that it happens to
   be synced by the OS, it won't be valid.

*/

/* ;;:

   Check if root page is dirty from metapage. if not, exit sync

   Create a queue of dirty pages.

   BFS the tree. Add root page. Add all pages in dirty bit set. Advance read
   head to next page (index 1) and do the same until read head and write head
   are equal.

   queue consists of pairs of memory address and length.

   if length field is zero, we'll msync length 1 page. -- which means this is a
   node. if when iterating over queue, we find a zero length entry, then add
   that node's dirty page.

   ---

   this /was/ the initial plan after some discussion. But after further
   discussion, we can actually do a depth first search. To make implementation
   even more simple, we can do an iterative dfs where we start from the root
   each time. Why? Because the bulk of time to execute is going to be disc
   io.

   after each msync of a page, descend to the deepest dirty page. msync that
   page. set that page's dirty bit in the parent to non-dirty. repeat. once
   you're at the root page and there are no dirty bits set, sync the
   root. Finally, sync the metapage (with checksumming).

 */
