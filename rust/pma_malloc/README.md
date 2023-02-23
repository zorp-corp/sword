# New Mars Persistent Memory Arena Allocator

This is a repo for a C implementation of the New Mars Persistence Memory Arena (New Mars PMA) allocator, as originally
designed by [Edward Amsden](https://github.com/eamsden).

## Reference Code Sources

- `phk_malloc` source pulled from
  [here](https://github.com/emeryberger/Malloc-Implementations/blob/master/allocators/phkmalloc/phkmalloc.c)
- LMDB source pulled from [here](https://github.com/LMDB/lmdb/tree/mdb.master/libraries/liblmdb)

## Preamble

Before reading the design for this repo, you should familiarize yourself with the following documents:
- [Ed Amsden's New Mars PMA design doc](https://github.com/urbit/new-mars/blob/master/docs/persistence.md)
- [LMDB whitepaper](http://www.lmdb.tech/media/20120829-LinuxCon-MDB-txt.pdf)
- [PHK malloc whitepaper](https://papers.freebsd.org/1998/phk-malloc.files/phk-malloc-paper.pdf)
- [PHK's "Notes from the Architect"](https://varnish-cache.org/docs/trunk/phk/notes.html#)

## Context

### B+ Trees

One of the key differences between the New Mars PMA and its LMDB source inspiration is the attempt to avoid using
[B+ Trees](https://en.wikipedia.org/wiki/B+_tree). B+ Trees are incredibly useful for databases and filesystems, and
LMDB is no exception. However, although the New Mars PMA can also be thought of as a database, the constraints on its
usage are entirely different from those of LMDB.

LMDB is a single-level key-value store. LMDB users are expected to interact with data through the LMDB interface. As
such, LMDB prioritises search operations. Since LMDB data is mutable, read and write transactions must be ordered, and
"old" database states need to be tracked. This combination of requirements (search and intermediary states) makes B+
Trees a natural fit: `O(log n)` search and the ability to "clone" the tree several times by copying `O(log n)` nodes
per clone.

Additionally, the data LMDB stores is not self-referential (entries do not reference other entries). Since data does not
reference other data and is accessed indirectly through the LMDB interface, pages are free to refer to each other by
page number relative to the origin. This eliminates the need for fixed address mappings, which eliminates the need for a
page directory. Factoring in the scrambled nature of the data (a result of mutable data and copy-on-write mechanics), a
structure using nodes such as a B+ tree appears a natural fit.

The New Mars PMA, on the other hand, is quite different. The PMA is a single-level object store. "Users" of the PMA are
expected to use allocation addresses directly. Furthermore, the data stored in the PMA is potentially self-referential.
These constraints exclude page number references, as they necessitate fixed memory addresses. This further necessitates
the use of a page directory which can track how disk pages map to memory addresses. Given this set of alternative
constraints, the B+ Tree is no longer a natural fit; the only remaining benefit is that a Tree structure simplifies
copy-on-write semantics. Therefore, the below New Mars PMA design seeks to use Arrays wherever possible, to benefit from
`O(1)` access/insertion and avoid the complexity of dealing with B+ Tree nodes.

### Page Directory

The above-mentioned constraints on the New Mars PMA, particularly the introduction of a page directory, introduce new
challenges. The double-buffer design of the metadata pages requires that either:
1. All changes, including those to metadata, follow copy-on-write semantics
2. All changes are applied simultaneously during commit

For LMDB, this is simple as both the database and a portion of the metadata (specifically, the free page cache) are
stored as B+ Trees, and therefore incur only an `O(log n)` cost to copy-on-write the entire walk from root to node.
However, for the New Mars PMA, this means that either:
1. The page directory and free page cache must be implemented using B+ Trees
2. The page directory and free page cache (or the changes to them) must find their way into the metadata

Consider the following: suppose that our PMA syncs the page directory and the metadata in two separate operations, and
that the page directory is implemented as an Array. We processed an event that has resulted in pulling a page out of the
free page cache, and then performed an `msync` operation on the dirty page. Now it's time to update the metadata and
page directory:
- If we update the page directory first, but crash before we can update the metadata, then we'll mark a page which
  should be free as occupied. Though this does not result in data loss, it introduces a memory leak that is persistent
  across sessions and cannot be detected, even by defragmentation.
- If we update the metadata first, but crash before we can update the page directory, then we'll fail to mark an
  occupied page as not free. In this particular example, that's okay: the same "free" page will be selected as we
  attempt to reprocess the event, and we'll write the same data to it. However, in the generic case where an event
  results in an arbitrary pattern of allocate and free operations (or in the case where multiple events are processed
  between commits), this is a major bug which will lead to data loss.
- Copy-on-write semantics are not an option for the page directory, as the entire implementation hinges on the Array
  structure of memory addresses.

A great deal of the following section is devoted to the discussion of alternative solutions (i.e. ones not using B+
Trees) to resolve these problems.

## Design

This section is a discussion of possible implementation variants for the design proposed by Ed, and how it relates to
the LMDB and `phk_malloc` source material.

### Static Mapping

Static address mapping exists as an option in LMDB, but it is an absolute necessity for the New Mars PMA. The reason for
the necessity of static mapping as a feature has already been discussed above, and its consequences will be further
discussed below.

### Single-Level Store

This is a core element of LMDB, and it is a core element of the New Mars PMA as well. OS developers and hardware
manufacturers have spent an enormous amount of effort to optimize the transfer and retention of data between memory and
disk, using tools such as paging, virtual memory, and unified buffer caching. We should structure the New Mars PMA to
maximally leverage these advanced tools. As a single-level store, our application doesn't know or care about the
hardware distinction between memory and disk, so long as the OS makes loom data available to us whenever necessary.
Therefore, just like LMDB, the New Mars PMA will allocate all data to a file-backed memory mapping which persists
across sessions.

### Metadata Double-Buffering

As in LMDB, the New Mars PMA backing file will use double-buffered metadata pages: the first two pages of the backing
file will be reserved for metadata. On commit, the older of the two pages will be overwritten with the new metadata.
The metadata pages will include a checksum to verify that no crashes occurred while writing the metadata page itself.

### Copy-on-Write

This is another core element of LMDB and another core element of the New Mars PMA. All allocations to the PMA which
would modify an existing page instead modify a copy. Disk/memory pages are tracked using a page directory. Though the
OS may write-through changes from memory to disk at any time, the metadata pages and page directory remain untouched
until events are committed. This means that the event snapshot can never be corrupted by a system crash.

### Shared Allocation Pages

Shared allocation pages are a feature from `phk_malloc`: to reduce the effects of internal fragmentation, "small"
allocations are rounded up to the nearest power of 2 and allocated in a similarly-sized slot within a page that acts as
an Array of such slots. Every slot in a page is of the same size. In addition, there is a declared minimum allocation
size (also a power of 2): any allocation smaller than the minimum allocation size is rounded up to meet the minimum
size, as otherwise the performance cost of managing very small allocations becomes a burden.

In `phk_malloc`, "small" allocations are 1/2 of a page or smaller. However, since `phk_malloc` stores shared page
metadata as a header within the page, any allocation of 1/2 a page is effectively granted a full page anyway, but with
the added burdens of the shared page metadata and shared page allocation process. For this reason, the New Mars PMA
instead considers a "small" allocation to be one that is 1/4 of a page or smaller.

In addition, `phk_malloc` has a variable-size header, since the bitmap which stores the slot info depends on the number
of slots, and the number of slots depends on the size of the slots. The News Mars PMA instead uses a fixed-size header
which always allocates enough bitmap space to fit the maximum number of minimum-sized slots. Though this results in
slightly fewer slots for some shared allocation pages, it greatly simplifies the code that interacts with shared
allocation pages (the most commonly used type of page).

### Thread Safety

The New Mars PMA can guarantee thread safety for an arbitrary number of readers without the reader table design of LMDB.
New Mars does not support data modification: data can only be allocated, accessed, or destroyed. This restriction
significantly simplifies the range of possible states when compared with LMDB. Since data cannot be modified, the idea
of a reader needing to see an "older" version of some data does not exist. During a write operation, existing data is
always preserved through the copy-on-write mechanic. There is no concept of needing to access data after it has been
freed: the assumption is that whatever New Mars process interacts with the PMA will be responsible for managing the
lifecycle of data (via address tracing or reference counting), meaning that by definition data cannot be needed after it
has been deallocated.

#### Notes

A natural consequence of there being only three operations in the New Mars PMA (allocate, deallocate, and sync) is that
only shared allocation data pages are ever copied: the only reason to copy a data page is to write to it, and the only
data pages that can be written to multiple times are shared allocation pages. An allocation of one or more data pages is
never copied or moved between allocation and deletion.

#### Possible Extensions

The New Mars PMA is currently limited to one writer thread. The most likely method of extending the existing design to
multiple writers would be a global writer lock around the metadata state.

It's possible that in the future, opportunities for improving PMA efficiency are discovered that require "modifying"
data: deleting one noun and writing another of identical size. If this ever occurs, this should be an explicit command
available to New Mars through the PMA, so that the PMA can enforce (potentially strict) restrictions about how to handle
this operation in a thread-safe manner.

### Page Directory

As described above, dealing with the page directory introduces the following dilemma:
1. All changes, including those to metadata, must follow copy-on-write semantics; OR
2. All changes are applied simultaneously during commit

#### Solution 1: B+ Trees and Copy-on-Write

The first possible solution is to implement the page directory as a B+ Tree. Since A B+ Tree is a key-value store, we
can define the keys as the page memory address (as a page offset from the root `mmap` address) and the value to be the
offset (in pages) on disk.

A change to any entry in the page directory requires copy-on-write for all nodes in the walk from root to leaf. However,
given the massive branching factor of an entire page acting as a node, this is actually a minuscule number of nodes
relative to the entire page directory (a B+ Tree page directory of depth 5 would be able to manage ~16 TiB of
addressable space). Furthermore, if there are multiple allocations between syncs, the number of nodes touched per
"transaction" will go down.

However, this solution introduces a new problem for the New Mars PMA with which LMDB did not have to deal: since the
page directory manages all memory, including that of the page directory itself, it must be the second thing loaded when
resuming ship execution (after the metadata pages). But since the page directory manages itself, it must be loaded to
determine how to load it! The solution for LMDB is that the entire backing file is loaded in one `mmap` operation,
allowing the nodes to refer to each other by page number (offset from origin). The solution for the PMA is that each
node must not only store the address of its child nodes, but also their offset on disk as well, so that it can load them
during startup.

**Performance:**

B+ Trees have `O(log n)` search and `O(log n)` insertion. The cost of updating the page directory is `O(n * log m)`,
where `n` is the number of updates between syncs and `m` is the total number of pages managed by the directory (however,
as `n` increases, the cost approaches `O(m)`). Like all Tree-based implementations, this one does not benefit from disk
locality when caching.

#### Solution 2: Store Dirty Page Info in Metadata Page

The second possible solution is to store dirty page info (i.e. the pending updates to the page directory and free page
cache) inside the metadata page itself. The metadata object is allocated an entire page, but only uses a very small
fraction of it. If the remainder of the metadata page were treated as an Array of dirty page info, then the following
update algorithm is possible:
- Process (de)allocation events; adds pages to the dirty page cache
- Directly before sync, compute the checksum of the metadata page
- Write metadata and checksum
- Process the dirty page cache in order; clear it when done

By storing a limited number of updates to the page directory (estimated around ~160 entries), we can apply the changes
to the metadata and page directory "simultaneously". Updating the page directory becomes a post-sync/startup operation.

However, this design begs the question, "What do we do once the dirty page cache is full?" One option is to error out.
Another option is to force a sync. For comets and renegades, it's even possible to include an option which never syncs,
necessitating a breach on crash/shutdown. Regardless, this is a setting which can be made configurable.

#### Page Directory as Array

Implementing the page directory as an Array has a few variants of its own. The question that requires answer is whether
the page directory should be a regular PMA allocation, i.e. should the page directory manage itself. The first option is
to have the page directory manage itself. In this case, it's treated like any other allocation, just particularly large.
The second option is to have the page directory exist outside the rest of the allocations. For example, it could live in
a separate `mmap` file, and its address in memory can "grow down", away from the metadata pages and the start of the
PMA.

**Performance:**

Regardless of which Array variant is used, the performance is the same: Arrays have `O(1)` lookup. When the size of an
Array is doubled on resize, the cost of insertion is also `O(1)` (amortized). The cost of updating the page directory is
linear in the number of updates: `O(n)`. All Array-based implementations benefit from disk locality when caching.

### Free Page Cache

Dealing with the free page cache is very similar to dealing with the page directory. Like the page directory, the free
page cache is effectively an extension of the metadata. We cannot write to the free page cache between commits, since
we have no idea when the OS may write through changes to disk and leave the snapshot in an inconsistent state. We also
cannot reuse newly freed pages, since this will overwrite data that we may need in the event of a crash. This situation
results in basically the same dilemma as we encountered when dealing with the page directory: either the free page cache
must follow copy-on-write semantics, or it must be updated "simultaneously" with the other metadata.

#### Solution 1: B+ Trees and Copy-on-Write

Just as with the page directory, we can choose to manage the free page cache as a Tree. Again, the entire walk from root
to leaf will need copy-on-write semantics. However, given the much smaller number of elements in the free page cache,
this is a linear cost. Unlike the page directory, the free page cache does not manage its own memory, so the work-around
used by the page directory is unnecessary for the free page cache.

#### Solution 2: Heap-Allocated Linked List

The second solution discussed in the page directory section is impractical for the free page cache. However, a different
solution that would not have worked for the page directory is possible for this case: removing the free page cache from
the metadata entirely.

The free page cache is actually a derivative of the page directory. It's an optimization to make allocating more
efficient and to reduce external fragmentation. Instead of tracking this information on disk, we can treat it as
operational state for the PMA. On startup, the free page cache can be computed while scanning the page directory to load
data from disk. During allocations, pages are pulled off of the free page cache as necessary. Newly free pages are
appended to the free page cache either directly before or directly after commit, depending on which page directory
solution is used. In the event of a crash, the state of the free page cache can always be correctly computed from the
page directory.

On heap, the free page cache can be a copy of the `phk_malloc` Linked List implementation. In the worst case, search and
access operations have cost `O(n)`. However, `n` is very small, since pages are only appended to the free page cache
after deallocation of an allocation greater than 1/4 of a page in size, and page reuse by new allocations is frequent.

#### Possible Extension

The `phk_malloc` code would attempt to merge runs of free pages whenever possible: this makes sense as `malloc` manages
memory. However, the New Mars PMA manages not just memory but also disk. For the purposes of maintaining disk locality,
it might be better if the PMA:
1. Does not merge page runs (also nice because merging page runs in a B+ Tree would be annoying)
2. Separates free pages into two caches: one for single pages, and one for multi-page runs (single pages can be used by
   either new shared page or new full page allocations, but multi-page runs can only be used by multi-page allocations)

### Free DPage Cache

Unlike either LMDB or `phk_malloc`, the New Mars PMA also needs to manage a cache of dpages that are freed (LMDB can
copy-on-write to heap and then `pwrite` dirty pages to disk because users never access data directly; `phk_malloc`
doesn't use disk at all). At first glance, it appears that this cache can be managed just like the free page cache. On
second glance, it appears that it's actually more cumbersome than that. Technically, the free dpage cache is *also* a
derivative of the page directory, but indirectly: a free dpage is one not referenced by any page in the directory. It's
possible to build the dpage cache by subtracting the set of used dpages from the set of all dpages, but the process
would be cumbersome: `O(n)` space and time complexity, where `n` is the size of the pier in pages, to find each of the
free dpages, the total number of which is unrelated to size of the pier.

#### Solution 1: B+ Trees and Copy-on-Write semantics

See "Solution 1" for the free page cache.

Note that this solution is rather inefficient, as copy-on-write semantics will trigger changes to the free dpage cache,
which will in turn trigger more copy-on-write semantics. This situation eventually reaches a point of equilibrium, but
it's possible that a single allocation can result in many pages needing to be copied.

#### Solution 2: Dedicated "Free DPage Cache" Array Page

The only time dpages are freed is when copy-on-write semantics occur. As mentioned above, if we use an Array for the
page directory and remove the free page cache from the metadata, then copy-on-write is only triggered when writing to a
shared allocation page. This forms a natural upper bound on the number of dpages which could be free at one time: the
total number of all shared allocation pages.

However, there is actually another upper bound: the number of dirty pages. Every dirty dpage implies a dirty page,
though a dirty page does not imply a dirty dpage (due to multipage allocations). In addition, dirty dpages are reused
between syncs (each copy-on-write prefers to pull off the free dpage stack rather than use new disk space). These two
properties together form a second upper bound on the number of dpages free at any time.

So the upper bound on the number of free dpages is `min(# shared allocation pages, # dirty pages)`. As seen above, the
number of dirty pages is limited to roughly 160. How big is a free dpage cache entry? Can we easily store up to 160 of
them? A free dpage is just an offset (in pages), ergo an 8-byte number. Therefore, a dedicated "free dpage cache" page
could store up to 512 entries (assuming no cache metadata): more than three times as many. This page would need
copy-on-write semantics as well, reducing the upper bound by one (since the free dpage cache page by definition takes
priority over pages being added to the free dpage cache).

### Memory to Disk Mapping

The New Mars PMA goes to great lengths to leave the mapping from memory address to disk offset unchanged whenever
possible. Doing so greatly reduces the complexity of the code, at the cost of an ever-growing pier which, under normal
operation, will forever be as big as the high-water mark of its usage. This is an excellent trade-off to make for the
following reasons:
- Pier size is probably more-or-less stable (doubtful that users are allocating and then deleting massive files)
- Disk is cheap
- Piers rarely move
- Thanks to the page directory, defragmentation is trivial
- If we really need to, we could zero pages on deallocation, which would make pier compression incredibly efficient

### Metadata Counter

The original LMDB design uses a counter in the metadata which is incremented by the number of transactions. The New Mars
PMA replaces this with two counters: epoch number and event number. This makes sense, considering that we would only
want to sync 0 or 1 times per event: there is no case in which we would want to sync in the middle of processing an
event.
