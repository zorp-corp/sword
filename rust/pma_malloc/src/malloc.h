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
int
pma_load(const char *path);

/**
 * TODO
 */
int
pma_close(uint64_t epoch, uint64_t event);

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
pma_sync(uint64_t epoch, uint64_t event);
