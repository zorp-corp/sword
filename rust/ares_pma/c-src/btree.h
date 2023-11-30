#ifndef __BTREE_H__
#define __BTREE_H__
#include <sys/types.h>
#include <stdint.h>

struct BT_state;
typedef struct BT_state BT_state;

typedef unsigned long ULONG;

//// ===========================================================================
////                            btree external routines

/**
 * instantiate an opaque BT_state handle
 */
int bt_state_new(BT_state **state);

/**
 * Open the persistent state or create if one doesn't exist
 */
int bt_state_open(BT_state *state, const char *path, ULONG flags, mode_t mode);

/**
 * Close the persistent state
 */
int bt_state_close(BT_state *state);

/**
 * Allocate persistent memory space
 */
void * bt_malloc(BT_state *state, size_t pages);

/**
 * Free persistent memory space
 */
void bt_free(BT_state *state, void *lo, void *hi);

/**
 * Sync a snapshot of the persistent memory to disk
 * This will **exit the process** on failure to avoid data corruption
 */
int bt_sync(BT_state *state);

/**
 * Get a metadata entry
 */
uint64_t bt_meta_get(BT_state *state, size_t idx);

/**
 * Set a metadata entry
 */
void bt_meta_set(BT_state *state, size_t idx, uint64_t val);

/**
 * Give the allocation range in the btree that a pointer lives in
 */
int bt_range_of(BT_state *state, void *p, void **lo, void **hi);

/**
 * Ensure a region of memory is "dirty" i.e. can be mutated
 *
 * A successful call to bt_dirty ensures that the memory range can be mutated
 * until the next call to `bt_sync()`
 */
int bt_dirty(BT_state *state, void *lo, void *hi);

/**
 * Given a pointer, give the containing region of allocated memory, or the next
 * highest if the pointer is to free memory
 */
int bt_next_alloc(BT_state *state, void *p, void **lo, void **hi);

/**
 * Return the memory bounds of the persistent-memory B-tree
 */
void bt_bounds(BT_state *state, void **lo, void **hi);

/**
 * Return whether a pointer is within the persistent-memory B-tree
 */
int bt_inbounds(BT_state *state, void *p);

#endif
