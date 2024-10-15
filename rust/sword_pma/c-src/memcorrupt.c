#include "btest-overrides.h"
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
    size_t sz_p = addr2off(head->hi) - addr2off(head->lo);
    sz += sz_p;
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
    size_t sz_p = head->hi - head->lo;
    sz += sz_p;
    head = head->next;
  }
  return sz;
}

static BT_mlistnode *
_mlist_copy(BT_state *state)
{
  BT_mlistnode *head = state->mlist;
  BT_mlistnode *ret, *prev;
  ret = prev = calloc(1, sizeof *ret);
  memcpy(ret, head, sizeof *head);
  ret->next = 0;
  head = head->next;
  while (head) {
    BT_mlistnode *copy = calloc(1, sizeof *copy);
    memcpy(copy, head, sizeof *head);
    prev->next = copy;
    prev = copy;
    head = head->next;
  }
  return ret;
}

static BT_nlistnode *
_nlist_copy(BT_state *state)
{
  BT_nlistnode *head = state->nlist;
  BT_nlistnode *ret, *prev;
  ret = prev = calloc(1, sizeof *ret);
  memcpy(ret, head, sizeof *head);
  ret->next = 0;
  head = head->next;
  while (head) {
    BT_nlistnode *copy = calloc(1, sizeof *copy);
    memcpy(copy, head, sizeof *head);
    prev->next = copy;
    prev = copy;
    head = head->next;
  }
  return ret;
}

static BT_flistnode *
_flist_copy(BT_state *state)
{
  BT_flistnode *head = state->flist;
  BT_flistnode *ret, *prev;
  ret = prev = calloc(1, sizeof *ret);
  memcpy(ret, head, sizeof *head);
  ret->next = 0;
  head = head->next;
  while (head) {
    BT_flistnode *copy = calloc(1, sizeof *copy);
    memcpy(copy, head, sizeof *head);
    prev->next = copy;
    prev = copy;
    head = head->next;
  }
  return ret;
}

static int
_mlist_eq(BT_mlistnode *l, BT_mlistnode *r)
{
  while (l && r) {
    if (l->lo != r->lo)
      bp(0);
    if (l->hi != r->hi)
      bp(0);
    l = l->next; r = r->next;
  }
  if (l == 0 && r == 0)
    return 1;
  bp(0);
}

static int
_nlist_eq(BT_nlistnode *l, BT_nlistnode *r)
{
  while (l && r) {
    if (l->lo != r->lo)
      bp(0);
    if (l->hi != r->hi)
      bp(0);
    l = l->next; r = r->next;
  }
  if (l == 0 && r == 0)
    return 1;
  bp(0);
}

static int
_flist_eq(BT_flistnode *l, BT_flistnode *r)
{
  while (l && r) {
    if (l->lo != r->lo)
      bp(0);
    if (l->hi != r->hi)
      bp(0);
    l = l->next; r = r->next;
  }
  if (l == 0 && r == 0)
    return 1;
  bp(0);
}

int main(int argc, char *argv[])
{
  DPRINTF("PMA Max Storage: %lld", ((uint64_t)UINT32_MAX * BT_PAGESIZE) - BLK_BASE_LEN_TOTAL);
  DPUTS("PMA Tests");

  BT_findpath path = {0};
  int rc = 0;

  /*
     test 3 pairs poorly with an overridden BT_DAT_MAKEYS=10 leading to huge
     persistent file growth. Disabling for now. Is there some way we can easily
     override these values at a per-test level without altering a code?
  */

  BT_page *testnode = calloc(0, sizeof *testnode);
  for (size_t i = 0; i <= 10; i++) {
    assert(!_bt_ischilddirty(testnode, i));
  }
  for (size_t i = 0; i <= 10; i++) {
    _bt_dirtychild(testnode, i);
    assert(_bt_ischilddirty(testnode, i));
  }
  for (size_t i = 0; i <= 10; i++) {
    assert(_bt_ischilddirty(testnode, i));
  }
  assert(!_bt_ischilddirty(testnode, 11));


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

/* #define ITERATIONS 1000 */
#define ITERATIONS 40
#define MAXALLOCPG 0xF
  lohi_pair allocs[ITERATIONS] = {0};
  int should_free[ITERATIONS] = {0};
  size_t alloc_sizp = 0;
  size_t mlist_sizp = _mlist_sizep(state3->mlist);
  BT_meta *meta = state3->meta_pages[state3->which];
  BT_page *root = _node_get(state3, meta->root);
  size_t N;

  /* ;;: testing huge alloc for ted */
  /* bp(0); */
  /* void *foo = bt_malloc(state3, 0x40000); */
  
  bp(0);
  for (size_t i = 0; i < ITERATIONS; i++) {
    /* malloc a random number of pages <= 16 and store in the allocs array */
    int pages = random();
    pages &= MAXALLOCPG;
    pages |= 1;
    allocs[i].lo = bt_malloc(state3, pages);
    allocs[i].hi = allocs[i].lo + pages;
    should_free[i] = random() % 2;
    alloc_sizp += pages;
  }

  /* close/reopen */
#if 1
  /* a sync is not sufficient to repro the sigsegv. So issue most likely in pma
     restore path */
  bp(0);
  bt_state_close(state3);
  bt_state_new(&state3);
  assert(SUCC(bt_state_open(state3, "./pmatest3", 0, 0644)));
#else
  bt_sync(state3);
#endif

  /* free allocations according to the values of should_free[i] */
  bp(0);
  for (size_t i = 0; i < ITERATIONS; i++) {
    if (should_free[i]) {
      bt_free(state3, allocs[i].lo, allocs[i].hi);
    }
  }

  /* sync before cloning ephemeral data structures and closing the pma in order
     to ensure the pending freelists are already merged (otherwise the clone
     will differ from the reconstructed) */
  bp(0);
  bt_sync(state3);

  /* copy ephemeral data structures */
  BT_mlistnode *mlist_copy = _mlist_copy(state3);
  BT_nlistnode *nlist_copy = _nlist_copy(state3);
  BT_flistnode *flist_copy = _flist_copy(state3);

  /* close/reopen */
  bt_state_close(state3);
  bt_state_new(&state3);
  assert(SUCC(bt_state_open(state3, "./pmatest3", 0, 0644)));

  /* assert clones structures are equivalent to those reconstructed */
  bp(0);
  assert(_mlist_eq(mlist_copy, state3->mlist));
  assert(_nlist_eq(nlist_copy, state3->nlist));
  assert(_flist_eq(flist_copy, state3->flist));

  meta = state3->meta_pages[state3->which];
  BT_meta metacopy = {0};
  memcpy(&metacopy, meta, sizeof metacopy);

  bt_state_close(state3);
  bt_state_new(&state3);
  assert(SUCC(bt_state_open(state3, "./pmatest3", 0, 0644)));

  /* compare for equality copies of ephemeral structures with restored ephemeral
     structures */
  meta = state3->meta_pages[state3->which];
  /* ;;: fixme */
  /* assert(meta->root == metacopy.root); */
  /* assert(_mlist_eq(mlist_copy, state3->mlist)); */
  /* assert(_nlist_eq(nlist_copy, state3->nlist)); */
  /* assert(_flist_eq(flist_copy, state3->flist)); */

  bt_state_close(state3);

  return 0;
}
