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

  BT_state *state1;
  BT_findpath path = {0};
  int rc = 0;


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

#if 0
  /*
     test 3 pairs poorly with an overridden BT_DAT_MAKEYS=10 leading to huge
     persistent file growth. Disabling for now. Is there some way we can easily
     override these values at a per-test level without altering a code?
  */


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
  BT_meta *meta = state3->meta_pages[state3->which];
  BT_page *root = _node_get(state3, meta->root);
  size_t N;
  for (size_t i = 0; i < ITERATIONS; i++) {
    /* malloc a random number of pages <= 256 and store in the allocs array */
    int pages = random();
    pages &= MAXALLOCPG;
    pages += 1;
    allocs[i].lo = bt_malloc(state3, pages);
    allocs[i].hi = allocs[i].lo + pages;
    alloc_sizp += pages;
    /* validate size changes to mlist and flist */
    /* ;;: no longer a valid comparison since the flist may have grown */
    /* assert(_flist_sizep(state3->flist) */
    /*        == (flist_sizp - alloc_sizp)); */
    assert(_mlist_sizep(state3->mlist)
           == (mlist_sizp - alloc_sizp));
    N = _bt_numkeys(root);
    /* assert(root->datk[N-2].fo == 0); */
  }

  /* sync the state */
  /* bt_sync(state3); */

  /* TODO: close and reopen state. validate ephemeral structures */

  flist_sizp = _flist_sizep(state3->flist);
  mlist_sizp = _mlist_sizep(state3->mlist);
  alloc_sizp = 0;
  /* for (size_t i = 0; i < ITERATIONS / 2; i++) { */
  /*   /\* free half of the allocations *\/ */
  /*   bt_free(state3, allocs[i].lo, allocs[i].hi); */
  /*   alloc_sizp += allocs[i].hi - allocs[i].lo; */
  /*   /\* validate size changes to mlist *\/ */
  /*   assert(_mlist_sizep(state3->mlist) */
  /*          == (mlist_sizp + alloc_sizp)); */
  /* } */

  /* copy ephemeral structures */
  BT_mlistnode *mlist_copy = _mlist_copy(state3);
  BT_nlistnode *nlist_copy = _nlist_copy(state3);
  BT_flistnode *flist_copy = _flist_copy(state3);
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

#endif


  DPUTS("== test 4: backing file extension");
  BT_state *state4;

  bt_state_new(&state4);
  if (mkdir("./pmatest4", 0774) == -1)
    return errno;
  assert(SUCC(bt_state_open(state4, "./pmatest4", 0, 0644)));

#define PMA_INITIAL_SIZE_p PMA_GROW_SIZE_p
  BYTE *t4a = bt_malloc(state4, PMA_GROW_SIZE_p * 2);
  BYTE *t4b = t4a;
  for (size_t i = 0; i < PMA_GROW_SIZE_b * 2; i++) {
    *t4b++ = rand();
  }

  assert(state4->file_size_p == PMA_INITIAL_SIZE_p + PMA_GROW_SIZE_p * 2);
  /* given the allocation pattern the head of the flist should also be the
     tail. The hi page here should match the file size */
  assert(state4->flist->hi == state4->file_size_p);

  bt_state_close(state4);

  bt_state_new(&state4);

  assert(SUCC(bt_state_open(state4, "./pmatest4", 0, 0644)));

  assert(state4->file_size_p == PMA_INITIAL_SIZE_p + PMA_GROW_SIZE_p * 2);
  assert(state4->flist->next->hi == state4->file_size_p);


  DPUTS("== test 5: partition striping");
  BT_state *state5;

  bt_state_new(&state5);
  if (mkdir("./pmatest5", 0774) == -1)
    return errno;
  assert(SUCC(bt_state_open(state5, "./pmatest5", 0, 0644)));


#define NODEPART0_MAX_NODES B2PAGES(BLK_BASE_LEN0_b)

  /* the maximum number of allocations is dependent on the allocation
     pattern. So this is approximate, but that's fine */
#define NODEPART0_MAX_ALLOCS (NODEPART0_MAX_NODES * BT_DAT_MAXKEYS)

  /* ;;: ^ this is 254016, is that right? In that case, we may need to
       artificially decrease the partition sizes. We could also decrease the
       page size. Oh wait, why don't we just decrease the maxkeys constant? */

  for (size_t i = 0; i < NODEPART0_MAX_ALLOCS; i++) {
    bt_malloc(state5, 1);
  }

  /* we should be using the second partition */
  pgno_t t4partoff0 = state5->meta_pages[state5->which]->blk_base[1];
  assert(t4partoff0 != 0);

  /* close and reopen the state */
  bt_state_close(state5);
  bt_state_new(&state5);
  assert(SUCC(bt_state_open(state5, "./pmatest5", 0, 0644)));

  /* the partition offset should be the same */
  assert(t4partoff0 == state5->meta_pages[state5->which]->blk_base[1]);

  return 0;
}
