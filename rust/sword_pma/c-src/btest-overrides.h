#ifndef __BTEST_OVERRIDES_H__
#define __BTEST_OVERRIDES_H__

/*
  For overriding constants defined in btree.h and btree.c in tests. This isn't
  very flexible though because these can't be overriden on per-test basis. Maybe
  there is some other solution if necessary
*/

#undef BT_DAT_MAXKEYS
#define BT_DAT_MAXKEYS 10

/* maxdepth expanded because when BT_DAT_MAXKEYS is shrunk, the depth of the
   btree can grow higher than usual */
#undef BT_MAXDEPTH
#define BT_MAXDEPTH 10

#endif
