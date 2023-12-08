#include "btree.h"
#include "btree.c"


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

int main(int argc, char *argv[])
{
  DPUTS("PMA Tests");

  BT_state *state1;
  BT_findpath path = {0};
  int rc = 0;


  DPUTS("== test 1: insert");

  bt_state_new(&state1);

  assert(SUCC(bt_state_open(state1, "./pmatest1", 0, 0644)));

#define LOWEST_ADDR 0x200000;
  vaof_t lo = LOWEST_ADDR;
  vaof_t hi = 0xDEADBEEF;
  pgno_t pg = 1;                /* dummy value */
  for (size_t i = 0; i < BT_DAT_MAXKEYS * 4; ++i) {
    DPRINTF("== i: %zu", i);
    _bt_insert(state1, lo, hi, pg);
    _test_nodeinteg(state1, &path, lo, hi, pg);
    lo++; pg++;
  }

  bt_state_close(state1);


  DPUTS("== test 2: malloc");
  BT_state *state2;

  bt_state_new(&state2);
  assert(SUCC(bt_state_open(state2, "./pmatest2", 0, 0644)));

  void *t2a = bt_malloc(state2, 10);
  bt_free(state2, t2a, (BT_page*)t2a + 10);
  void *t2b = bt_malloc(state2, 10);
  /* should have pulled the same pointer due to eager mlist coalescing */
  assert(t2a == t2b);
  ZERO(&path, sizeof path);
  _bt_find(state2, &path, addr2off(t2b), addr2off((BT_page *)t2b + 10));
  bt_free(state2, t2b, (BT_page*)t2b + 10);
  ZERO(&path, sizeof path);
  _bt_find(state2, &path, addr2off(t2b), addr2off((BT_page *)t2b + 10));
  /* should invoke deletion coalescing - 10 page free range in btree */
  void *t2c = bt_malloc(state2, 20);

  return 0;
}
