#include "btree.h"
#include "btree.c"

#include <stdlib.h>
#include <stdio.h>

static void
_test_nodeinteg(BT_state *state, BT_findpath *path,
                vaof_t lo, vaof_t hi, pgno_t pg)
{
  size_t childidx = 0;
  BT_page *parent = 0;

  assert(SUCC(_bt_find(state, path, lo, hi)));
  parent = path->path[path->depth];
  childidx = path->idx[path->depth];
  assert(parent->datk[childidx].fo == pg);
  assert(parent->datk[childidx].va == lo);
  assert(parent->datk[childidx+1].va == hi);
}

static size_t
_mlist_sizep(BT_mlistnode *head)
/* calculate the size of the mlist in pages */
{
  size_t sz = 0;
  while (head) {
    sz += head->sz;
    head = head->next;
  }
  return sz;
}

static size_t
_flist_sizep(BT_flistnode *head)
/* calculate the size of the flist in pages */
{
  size_t sz = 0;
  while (head) {
    sz += head->sz;
    head = head->next;
  }
  return sz;
}

int main(int argc, char *argv[])
{
  DPUTS("PMA Tests");

  BT_state *state1;
  BT_findpath path = {0};
  int rc = 0;


  DPUTS("== test 1: insert");

  bt_state_new(&state1);
  if (mkdir("./pmatest1", 0774) == -1)
    return errno;
  assert(SUCC(bt_state_open(state1, "./pmatest1", 0, 0644)));

#define LOWEST_ADDR 0x200000;
  vaof_t lo = LOWEST_ADDR;
  vaof_t hi = 0xDEADBEEF;
  pgno_t pg = 1;                /* dummy value */
  for (size_t i = 0; i < BT_DAT_MAXKEYS * 4; ++i) {
    _bt_insert(state1, lo, hi, pg);
    _test_nodeinteg(state1, &path, lo, hi, pg);
    lo++; pg++;
  }

  bt_state_close(state1);


  DPUTS("== test 2: malloc");
  BT_state *state2;

  bt_state_new(&state2);
  if (mkdir("./pmatest2", 0774) == -1)
    return errno;
  assert(SUCC(bt_state_open(state2, "./pmatest2", 0, 0644)));

  void *t2a = bt_malloc(state2, 10);
  bt_free(state2, t2a, (BT_page*)t2a + 10);
  void *t2b = bt_malloc(state2, 10);
  /* should have pulled the same pointer due to eager mlist coalescing */
  assert(t2a == t2b);
  ZERO(&path, sizeof path);
  _bt_find(state2, &path, addr2off(t2b), addr2off((BT_page *)t2b + 10));
#define T2P1_PRNT0 (path.path[path.depth])
#define T2P1_CIDX0 (path.idx[path.depth])
#define T2P1_CIDX1 (path.idx[path.depth] + 1)
  /* check length as represented in btree */
  assert(T2P1_PRNT0->datk[T2P1_CIDX1].va
         - T2P1_PRNT0->datk[T2P1_CIDX0].va
         == 10);
  bt_free(state2, t2b, (BT_page*)t2b + 10);
  ZERO(&path, sizeof path);
  _bt_find(state2, &path, addr2off(t2b), addr2off((BT_page *)t2b + 10));
  /* fo should be zero (free) */
  assert(path.path[path.depth]->datk[path.idx[path.depth]].fo == 0);
  /* should invoke deletion coalescing - 10 page free range in btree */
  void *t2c = bt_malloc(state2, 20);

  bt_state_close(state2);


  DPUTS("== test 3: ephemeral structure restoration");
  BT_state *state3;

  bt_state_new(&state3);
  if (mkdir("./pmatest3", 0774) == -1)
    return errno;
  assert(SUCC(bt_state_open(state3, "./pmatest3", 0, 0644)));

  typedef struct lohi_pair lohi_pair;
  struct lohi_pair
  {
    BT_page *lo;
    BT_page *hi;
  };

#define ITERATIONS 1000
#define MAXALLOCPG 0xFF
  lohi_pair allocs[ITERATIONS] = {0};
  size_t alloc_sizp = 0;
  size_t flist_sizp = _flist_sizep(state3->flist);
  size_t mlist_sizp = _mlist_sizep(state3->mlist);
  for (size_t i = 0; i < ITERATIONS; i++) {
    /* malloc a random number of pages <= 256 and store in the allocs array */
    int pages = random();
    pages &= MAXALLOCPG;
    pages += 1;
    allocs[i].lo = bt_malloc(state3, pages);
    allocs[i].hi = allocs[i].lo + pages;
    alloc_sizp += pages;
    /* validate size changes to mlist and flist */
    assert(_flist_sizep(state3->flist)
           == (flist_sizp - alloc_sizp));
    assert(_mlist_sizep(state3->mlist)
           == (mlist_sizp - alloc_sizp));
  }

  /* sync the state */
  /* bt_sync(state3); */

  /* TODO: close and reopen state. validate ephemeral structures */

  flist_sizp = _flist_sizep(state3->flist);
  mlist_sizp = _mlist_sizep(state3->mlist);
  alloc_sizp = 0;
  for (size_t i = 0; i < ITERATIONS / 2; i++) {
    /* free half of the allocations */
    bt_free(state3, allocs[i].lo, allocs[i].hi);
    alloc_sizp += allocs[i].hi - allocs[i].lo;
    /* validate size changes to mlist */
    assert(_mlist_sizep(state3->mlist)
           == (mlist_sizp + alloc_sizp));
  }

  /* resync the state */
  bt_sync(state3);

  bt_state_close(state3);

  bt_state_new(&state3);

  assert(SUCC(bt_state_open(state3, "./pmatest3", 0, 0644)));
  /* TODO: close and reopen state. validate ephemeral structures */

  return 0;
}
