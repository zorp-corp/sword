/**
 * Persistent Memory Arena for the New Mars Nock virtualization engine.
 */

#pragma once

#include <stddef.h>
#include <stdint.h>

//==============================================================================
// PROTOTYPES
//==============================================================================

/**
 * Initialize a brand new PMA environment and event snapshot
 *
 * @param path  File directory in which to create backing files for snapshot and
 *              page directory
 *
 * @return  0   success
 * @return  -1  failure; errno set to error code
 */
int
pma_init(const char *path);

/**
 * Load an existing PMA environment and event snapshot
 *
 * @param path  File directory from which to load the backing files for the
 *              snapshot and page directory
 *
 * @return  0   success
 * @return  -1  failure; errno set to error code
 */
int
pma_load(const char *path);

/**
 * Safely unload the PMA after syncing changes to PMA state
 *
 * @param epoch Epoch of latest event successfully applied to state snapshot
 * @param event Event number of latest event successfully applied to state
 *              snapshot
 *
 * @return  0   success
 * @return  -1  failure; errno set to error code
 */
int
pma_close(uint64_t epoch, uint64_t event);

/**
 * Allocate a new block of memory in the PMA
 *
 * @param size  Size in bytes to allocate
 *
 * @return  NULL    failure; errno set to error code
 * @return  void*   address of the newly allocated memory
 */
void *
pma_malloc(size_t size);

/**
 * Deallocate an existing block of memory in the PMA
 *
 * @param address   Address of block to deallocated
 *
 * @return  0   success
 * @return  -1  failure; errno set to error code
 */
int
pma_free(void *address);

/**
 * Sync changes to PMA state
 *
 * @param epoch Epoch of latest event successfully applied to state snapshot
 * @param event Event number of latest event successfully applied to state
 *              snapshot
 *
 * @return  0   success
 * @return  -1  failure; errno set to error code
 */
int
pma_sync(uint64_t epoch, uint64_t event);
