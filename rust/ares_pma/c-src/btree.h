#ifndef __BTREE_H__
#define __BTREE_H__
#include <sys/stat.h>
#include <stdint.h>
typedef unsigned long ULONG;

//// ===========================================================================
////                            btree external routines

int bt_state_new(BT_state **state);

int bt_state_open(BT_state *state, const char *path, ULONG flags, mode_t mode);

int bt_state_close(BT_state *state);

void * bt_malloc(BT_state *state, size_t pages);

void bt_free(BT_state *state, void *lo, void *hi);

int bt_sync(BT_state *state);

uint64_t bt_meta_get(BT_state *state, size_t idx);

void bt_meta_set(BT_state *state, size_t idx, uint64_t val);

#endif
