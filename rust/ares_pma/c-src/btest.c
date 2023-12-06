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
  /* _bt_printnode(parent); */
  childidx = path->idx[path->depth];
  assert(parent->datk[childidx].fo == pg);
  assert(parent->datk[childidx].va == lo);
  assert(parent->datk[childidx+1].va == hi);
}

int main(int argc, char *argv[])
{
  DPUTS("PMA Tests");

  BT_state *state;
  BT_findpath path = {0};
  int rc = 0;

  bt_state_new(&state);

  
  DPUTS("== test 1: insert");
  assert(SUCC(bt_state_open(state, "./pmatest", 0, 0644)));

  vaof_t lo = 10;
  vaof_t hi = 0xDEADBEEF;
  pgno_t pg = 1;                /* dummy value */
  for (size_t i = 0; i < BT_DAT_MAXKEYS * 4; ++i) {
    /* if (i % (BT_DAT_MAXKEYS - 2) == 0) */
    /*   bp(0);                    /\* breakpoint on split case *\/ */
    _bt_insert(state, lo, hi, pg);
    _test_nodeinteg(state, &path, lo, hi, pg);
    lo++; pg++;
  }

  return 0;
}
