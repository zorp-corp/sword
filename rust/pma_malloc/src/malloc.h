/**
 * Persistent Memory Arena for the New Mars Nock virtualization engine.
 */

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

//==============================================================================
// PROTOTYPES
//==============================================================================

/**
 * Struct returned from pma_load()
 */
typedef struct _pma_root_state_t {
  uint64_t  epoch;            // Epoch ID of the most recently processed event
  uint64_t  event;            // ID of the most recently processed event
  uint64_t  root;             // Root after most recent event
} RootState;

/**
 * Initialize a brand new PMA environment and event snapshot
 *
 * @param path  File directory in which to create backing files for snapshot and page directory
 *
 * @return  0 success
 * @return -1 failure
 */
int
pma_init(const char *path);

/**
 * TODO
 */
RootState
pma_load(const char *path);

/**
 * TODO
 */
int
pma_close(uint64_t epoch, uint64_t event, uint64_t root);

/**
 * Allocate a new block of memory in the PMA
 *
 * @param size  Size in bytes to allocate
 *
 * @return  The address of the newly allocated memory
 */
void *
pma_malloc(size_t size);

/**
 * Deallocate an existing block of memory in the PMA
 *
 * @param address   Address of block to deallocated
 *
 * @return  Abort if address is unallocated or in the middle of a multi-page
 *          allocation
 */
void
pma_free(void *address);

/**
 * TODO
 */
int
pma_sync(uint64_t epoch, uint64_t event, uint64_t root);

/**
 * True if the address is in the PMA
 */
bool
pma_in_arena(void *address);
