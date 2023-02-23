/** @file mdb.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "lmdb.h"
#include "midl.h"

#define MDB_PID_T  pid_t
#define MDB_THR_T  pthread_t
#define MDB_OFF_T  off_t

#ifdef __GNUC__
#define  ESECT  __attribute__ ((section("text_env")))
#else
#define ESECT
#endif

#define CALL_CONV

#define mdb_func_  __func__

/* Internal error codes, not exposed outside liblmdb */
#define MDB_NO_ROOT    (MDB_LAST_ERRCODE + 10)
#define MDB_OWNERDEAD  EOWNERDEAD  /**< #LOCK_MUTEX0() result if dead owner */

#define THREAD_RET  void *
#define THREAD_CREATE(thr,start,arg)  pthread_create(&thr,NULL,start,arg)
#define THREAD_FINISH(thr)  pthread_join(thr,NULL)

  /** Shared mutex/semaphore as the original is stored.
   *
   *  Not for copies.  Instead it can be assigned to an #mdb_mutexref_t.
   *  When mdb_mutexref_t is a pointer and mdb_mutex_t is not, then it
   *  is array[size 1] so it can be assigned to the pointer.
   */
typedef pthread_mutex_t mdb_mutex_t[1];
  /** Reference to an #mdb_mutex_t */
typedef pthread_mutex_t *mdb_mutexref_t;
  /** Lock the reader or writer mutex.
   *  Returns 0 or a code to give #mdb_mutex_failed(), as in #LOCK_MUTEX().
   */
#define LOCK_MUTEX0(mutex)  pthread_mutex_lock(mutex)
  /** Unlock the reader or writer mutex.
   */
#define UNLOCK_MUTEX(mutex)  pthread_mutex_unlock(mutex)
  /** Mark mutex-protected data as repaired, after death of previous owner.
   */
#define mdb_mutex_consistent(mutex)  pthread_mutex_consistent(mutex)

  /** Get the error code for the last failed system function.
   */
#define  ErrCode()  errno

  /** An abstraction for a file handle.
   *  On POSIX systems file handles are small integers. On Windows
   *  they're opaque pointers.
   */
#define  HANDLE  int

  /**  A value for an invalid file handle.
   *  Mainly used to initialize file variables and signify that they are
   *  unused.
   */
#define INVALID_HANDLE_VALUE  (-1)

  /** Get the size of a memory page for the system.
   *  This is the basic size that the platform's memory manager uses, and is
   *  fundamental to the use of memory-mapped files.
   */
#define  GET_PAGESIZE(x)  ((x) = sysconf(_SC_PAGE_SIZE))

#define  Z  MDB_FMT_Z  /**< printf/scanf format modifier for size_t */
#define  Yu  MDB_PRIy(u)  /**< printf format for #mdb_size_t */
#define  Yd  MDB_PRIy(d)  /**< printf format for 'signed #mdb_size_t' */

#define MNAME_LEN  (sizeof(pthread_mutex_t))

  /** Lock mutex, handle any error, set rc = result.
   *  Return 0 on success, nonzero (not rc) on error.
   */
#define LOCK_MUTEX(rc, env, mutex) \
  (((rc) = LOCK_MUTEX0(mutex)) && \
   ((rc) = mdb_mutex_failed(env, mutex, rc)))
static int mdb_mutex_failed(MDB_env *env, mdb_mutexref_t mutex, int rc);

/**  A flag for opening a file and requesting synchronous data writes.
 *  This is only used when writing a meta page. It's not strictly needed;
 *  we could just do a normal write and then immediately perform a flush.
 *  But if this flag is available it saves us an extra system call.
 *
 *  @note If O_DSYNC is undefined but exists in /usr/include,
 * preferably set some compiler flag to get the definition.
 */
#ifndef MDB_DSYNC
# ifdef O_DSYNC
# define MDB_DSYNC  O_DSYNC
# else
# define MDB_DSYNC  O_SYNC
# endif
#endif

/** Function for flushing the data of a file. Define this to fsync
 *  if fdatasync() is not supported.
 */
#ifndef MDB_FDATASYNC
# define MDB_FDATASYNC  fdatasync
#endif

#ifndef MDB_MSYNC
# define MDB_MSYNC(addr,len,flags)  msync(addr,len,flags)
#endif

#ifndef MS_SYNC
#define  MS_SYNC  1
#endif

#ifndef MS_ASYNC
#define  MS_ASYNC  0
#endif

  /** A page number in the database.
   *  Note that 64 bit page numbers are overkill, since pages themselves
   *  already represent 12-13 bits of addressable memory, and the OS will
   *  always limit applications to a maximum of 63 bits of address space.
   *
   *  @note In the #MDB_node structure, we only store 48 bits of this value,
   *  which thus limits us to only 60 bits of addressable data.
   */
typedef MDB_ID  pgno_t;

  /** A transaction ID.
   *  See struct MDB_txn.mt_txnid for details.
   */
typedef MDB_ID  txnid_t;

#define DPRINTF(args)  ((void) 0)
#define DPUTS(arg)  DPRINTF(("%s", arg))
#define DDBI(mc) \
  (((mc)->mc_flags & C_SUB) ? -(int)(mc)->mc_dbi : (int)(mc)->mc_dbi)

  /**  @brief The maximum size of a database page.
   *
   *  It is 32k or 64k, since value-PAGEBASE must fit in
   *  #MDB_page.%mp_upper.
   *
   *  LMDB will use database pages < OS pages if needed.
   *  That causes more I/O in write transactions: The OS must
   *  know (read) the whole page before writing a partial page.
   *
   *  Note that we don't currently support Huge pages. On Linux,
   *  regular data files cannot use Huge pages, and in general
   *  Huge pages aren't actually pageable. We rely on the OS
   *  demand-pager to read our data and page it out when memory
   *  pressure from other processes is high. So until OSs have
   *  actual paging support for Huge pages, they're not viable.
   */
#define MAX_PAGESIZE   0x10000

  /**  A stamp that identifies a file as an LMDB file.
   *  There's nothing special about this value other than that it is easily
   *  recognizable, and it will reflect any byte order mismatches.
   */
#define MDB_MAGIC   0xBEEFC0DE

  /**  The version number for a database's datafile format. */
#define MDB_DATA_VERSION  1
  /**  The version number for a database's lockfile format. */
#define MDB_LOCK_VERSION  2
  /** Number of bits representing #MDB_LOCK_VERSION in #MDB_LOCK_FORMAT.
   *  The remaining bits must leave room for #MDB_lock_desc.
   */
#define MDB_LOCK_VERSION_BITS 12

  /**  The maximum size of a key we can write to the environment. */
#if MDB_MAXKEYSIZE
#define ENV_MAXKEY(env)  (MDB_MAXKEYSIZE)
#else
#define ENV_MAXKEY(env)  ((env)->me_maxkey)
#endif

  /**  @brief The maximum size of a data item.
   *
   *  We only store a 32 bit value for node sizes.
   */
#define MAXDATASIZE  0xffffffffUL

#define  DKBUF
#define DKEY(x)  0

  /** An invalid page number.
   *  Mainly used to denote an empty tree.
   */
#define P_INVALID   (~(pgno_t)0)

  /** Test if the flags \b f are set in a flag word \b w. */
#define F_ISSET(w, f)   (((w) & (f)) == (f))

  /** Round \b n up to an even number. */
#define EVEN(n)    (((n) + 1U) & -2) /* sign-extending -2 to match n+1U */

  /** Least significant 1-bit of \b n.  n must be of an unsigned type. */
#define LOW_BIT(n)    ((n) & (-(n)))

  /** (log2(\b p2) % \b n), for p2 = power of 2 and 0 < n < 8. */
#define LOG2_MOD(p2, n)  (7 - 86 / ((p2) % ((1U<<(n))-1) + 11))
  /* Explanation: Let p2 = 2**(n*y + x), x<n and M = (1U<<n)-1. Now p2 =
   * (M+1)**y * 2**x = 2**x (mod M). Finally "/" "happens" to return 7-x.
   */

  /** Should be alignment of \b type. Ensure it is a power of 2. */
#define ALIGNOF2(type) \
  LOW_BIT(offsetof(struct { char ch_; type align_; }, align_))

  /**  Used for offsets within a single page.
   *  Since memory pages are typically 4 or 8KB in size, 12-13 bits,
   *  this is plenty.
   */
typedef uint16_t   indx_t;

typedef unsigned long long  mdb_hash_t;

  /**  Default size of memory map.
   *  This is certainly too small for any actual applications. Apps should always set
   *  the size explicitly using #mdb_env_set_mapsize().
   */
#define DEFAULT_MAPSIZE  1048576

  /**  The size of a CPU cache line in bytes. We want our lock structures
   *  aligned to this size to avoid false cache line sharing in the
   *  lock table.
   *  This value works for most CPUs. For Itanium this should be 128.
   */
#ifndef CACHELINE
#define CACHELINE  64
#endif

  /**  The information we store in a single slot of the reader table.
   *  In addition to a transaction ID, we also record the process and
   *  thread ID that owns a slot, so that we can detect stale information,
   *  e.g. threads or processes that went away without cleaning up.
   *  @note We currently don't check for stale records. We simply re-init
   *  the table when we know that we're the only process opening the
   *  lock file.
   */
typedef struct MDB_rxbody {
  /**  Current Transaction ID when this transaction began, or (txnid_t)-1.
   *  Multiple readers that start at the same time will probably have the
   *  same ID here. Again, it's not important to exclude them from
   *  anything; all we need to know is which version of the DB they
   *  started from so we can avoid overwriting any data used in that
   *  particular version.
   */
  volatile txnid_t    mrb_txnid;
  /** The process ID of the process owning this reader txn. */
  volatile MDB_PID_T  mrb_pid;
  /** The thread ID of the thread owning this txn. */
  volatile MDB_THR_T  mrb_tid;
} MDB_rxbody;

  /** The actual reader record, with cacheline padding. */
typedef struct MDB_reader {
  union {
    MDB_rxbody mrx;
    /** shorthand for mrb_txnid */
#define  mr_txnid  mru.mrx.mrb_txnid
#define  mr_pid  mru.mrx.mrb_pid
#define  mr_tid  mru.mrx.mrb_tid
    /** cache line alignment */
    char pad[(sizeof(MDB_rxbody)+CACHELINE-1) & ~(CACHELINE-1)];
  } mru;
} MDB_reader;

  /** The header for the reader table.
   *  The table resides in a memory-mapped file. (This is a different file
   *  than is used for the main database.)
   *
   *  For POSIX the actual mutexes reside in the shared memory of this
   *  mapped file. On Windows, mutexes are named objects allocated by the
   *  kernel; we store the mutex names in this mapped file so that other
   *  processes can grab them. This same approach is also used on
   *  MacOSX/Darwin (using named semaphores) since MacOSX doesn't support
   *  process-shared POSIX mutexes. For these cases where a named object
   *  is used, the object name is derived from a 64 bit FNV hash of the
   *  environment pathname. As such, naming collisions are extremely
   *  unlikely. If a collision occurs, the results are unpredictable.
   */
typedef struct MDB_txbody {
    /** Stamp identifying this as an LMDB file. It must be set
     *  to #MDB_MAGIC. */
  uint32_t  mtb_magic;
    /** Format of this lock file. Must be set to #MDB_LOCK_FORMAT. */
  uint32_t  mtb_format;
    /**  The ID of the last transaction committed to the database.
     *  This is recorded here only for convenience; the value can always
     *  be determined by reading the main database meta pages.
     */
  volatile txnid_t    mtb_txnid;
    /** The number of slots that have been used in the reader table.
     *  This always records the maximum count, it is not decremented
     *  when readers release their slots.
     */
  volatile unsigned  mtb_numreaders;
    /** Mutex protecting access to this table.
     *  This is the reader table lock used with LOCK_MUTEX().
     */
  mdb_mutex_t  mtb_rmutex;
} MDB_txbody;

  /** The actual reader table definition. */
typedef struct MDB_txninfo {
  union {
    MDB_txbody mtb;
#define mti_magic  mt1.mtb.mtb_magic
#define mti_format  mt1.mtb.mtb_format
#define mti_rmutex  mt1.mtb.mtb_rmutex
#define mti_txnid  mt1.mtb.mtb_txnid
#define mti_numreaders  mt1.mtb.mtb_numreaders
#define mti_mutexid  mt1.mtb.mtb_mutexid
    char pad[(sizeof(MDB_txbody)+CACHELINE-1) & ~(CACHELINE-1)];
  } mt1;
  union {
    mdb_mutex_t  mt2_wmutex;
#define mti_wmutex  mt2.mt2_wmutex
    char pad[(MNAME_LEN+CACHELINE-1) & ~(CACHELINE-1)];
  } mt2;
  MDB_reader  mti_readers[1];
} MDB_txninfo;

  /** Lockfile format signature: version, features and field layout */
#define MDB_LOCK_FORMAT \
  ((uint32_t)         \
   (((MDB_LOCK_VERSION) % (1U << MDB_LOCK_VERSION_BITS)) \
    + MDB_lock_desc     * (1U << MDB_LOCK_VERSION_BITS)))

/* We do not know the inside of a POSIX mutex and how to check if mutexes
 * used by two executables are compatible. Just check alignment and size.
 */
# define MDB_LOCK_TYPE  (10 + \
    LOG2_MOD(ALIGNOF2(pthread_mutex_t), 5) + \
    sizeof(pthread_mutex_t) / 4U % 22 * 5)

enum {
  /** Magic number for lockfile layout and features.
   *
   *  This *attempts* to stop liblmdb variants compiled with conflicting
   *  options from using the lockfile at the same time and thus breaking
   *  it.  It describes locking types, and sizes and sometimes alignment
   *  of the various lockfile items.
   *
   *  The detected ranges are mostly guesswork, or based simply on how
   *  big they could be without using more bits.  So we can tweak them
   *  in good conscience when updating #MDB_LOCK_VERSION.
   */
  MDB_lock_desc =
  /* Default CACHELINE=64 vs. other values (have seen mention of 32-256) */
  (CACHELINE==64 ? 0 : 1 + LOG2_MOD(CACHELINE >> (CACHELINE>64), 5))
  + 6  * (sizeof(MDB_PID_T)/4 % 3)    /* legacy(2) to word(4/8)? */
  + 18 * (sizeof(pthread_t)/4 % 5)    /* can be struct{id, active data} */
  + 90 * (sizeof(MDB_txbody) / CACHELINE % 3)
  + 270 * (MDB_LOCK_TYPE % 120)
  /* The above is < 270*120 < 2**15 */
  + ((sizeof(txnid_t) == 8) << 15)    /* 32bit/64bit */
  + ((sizeof(MDB_reader) > CACHELINE) << 16)
  + (1 << 17)
  /* 18 bits total: Must be <= (32 - MDB_LOCK_VERSION_BITS). */
};
/** @} */

/** Common header for all page types. The page type depends on #mp_flags.
 *
 * #P_BRANCH and #P_LEAF pages have unsorted '#MDB_node's at the end, with
 * sorted #mp_ptrs[] entries referring to them. Exception: #P_LEAF2 pages
 * omit mp_ptrs and pack sorted #MDB_DUPFIXED values after the page header.
 *
 * #P_OVERFLOW records occupy one or more contiguous pages where only the
 * first has a page header. They hold the real data of #F_BIGDATA nodes.
 *
 * #P_SUBP sub-pages are small leaf "pages" with duplicate data.
 * A node with flag #F_DUPDATA but not #F_SUBDATA contains a sub-page.
 * (Duplicate data can also go in sub-databases, which use normal pages.)
 *
 * #P_META pages contain #MDB_meta, the start point of an LMDB snapshot.
 *
 * Each non-metapage up to #MDB_meta.%mm_last_pg is reachable exactly once
 * in the snapshot: Either used by a database or listed in a freeDB record.
 */
typedef struct MDB_page {
#define  mp_pgno  mp_p.p_pgno
#define  mp_next  mp_p.p_next
  union {
    pgno_t    p_pgno;  /**< page number */
    struct MDB_page *p_next; /**< for in-memory list of freed pages */
  } mp_p;
  uint16_t  mp_pad;      /**< key size if this is a LEAF2 page */
/**  @defgroup mdb_page  Page Flags
 *  @ingroup internal
 *  Flags for the page headers.
 *  @{
 */
#define  P_BRANCH   0x01    /**< branch page */
#define  P_LEAF     0x02    /**< leaf page */
#define  P_OVERFLOW   0x04    /**< overflow page */
#define  P_META     0x08    /**< meta page */
#define  P_DIRTY     0x10    /**< dirty page, also set for #P_SUBP pages */
#define  P_LEAF2     0x20    /**< for #MDB_DUPFIXED records */
#define  P_SUBP     0x40    /**< for #MDB_DUPSORT sub-pages */
#define  P_LOOSE     0x4000    /**< page was dirtied then freed, can be reused */
#define  P_KEEP     0x8000    /**< leave this page alone during spill */
/** @} */
  uint16_t  mp_flags;    /**< @ref mdb_page */
#define mp_lower  mp_pb.pb.pb_lower
#define mp_upper  mp_pb.pb.pb_upper
#define mp_pages  mp_pb.pb_pages
  union {
    struct {
      indx_t    pb_lower;    /**< lower bound of free space */
      indx_t    pb_upper;    /**< upper bound of free space */
    } pb;
    uint32_t  pb_pages;  /**< number of overflow pages */
  } mp_pb;
  indx_t    mp_ptrs[1];    /**< dynamic size */
} MDB_page;

  /** Size of the page header, excluding dynamic data at the end */
#define PAGEHDRSZ   ((unsigned) offsetof(MDB_page, mp_ptrs))

  /** Address of first usable data byte in a page, after the header */
#define METADATA(p)   ((void *)((char *)(p) + PAGEHDRSZ))

  /** ITS#7713, change PAGEBASE to handle 65536 byte pages */
#define  PAGEBASE  0

  /** Number of nodes on a page */
#define NUMKEYS(p)   (((p)->mp_lower - (PAGEHDRSZ-PAGEBASE)) >> 1)

  /** The amount of space remaining in the page */
#define SIZELEFT(p)   (indx_t)((p)->mp_upper - (p)->mp_lower)

  /** The percentage of space used in the page, in tenths of a percent. */
#define PAGEFILL(env, p) (1000L * ((env)->me_psize - PAGEHDRSZ - SIZELEFT(p)) / \
        ((env)->me_psize - PAGEHDRSZ))
  /** The minimum page fill factor, in tenths of a percent.
   *  Pages emptier than this are candidates for merging.
   */
#define FILL_THRESHOLD   250

  /** Test if a page is a leaf page */
#define IS_LEAF(p)   F_ISSET((p)->mp_flags, P_LEAF)
  /** Test if a page is a LEAF2 page */
#define IS_LEAF2(p)   F_ISSET((p)->mp_flags, P_LEAF2)
  /** Test if a page is a branch page */
#define IS_BRANCH(p)   F_ISSET((p)->mp_flags, P_BRANCH)
  /** Test if a page is an overflow page */
#define IS_OVERFLOW(p)   F_ISSET((p)->mp_flags, P_OVERFLOW)
  /** Test if a page is a sub page */
#define IS_SUBP(p)   F_ISSET((p)->mp_flags, P_SUBP)

  /** The number of overflow pages needed to store the given size. */
#define OVPAGES(size, psize)  ((PAGEHDRSZ-1 + (size)) / (psize) + 1)

  /** Link in #MDB_txn.%mt_loose_pgs list.
   *  Kept outside the page header, which is needed when reusing the page.
   */
#define NEXT_LOOSE_PAGE(p)    (*(MDB_page **)((p) + 2))

  /** Header for a single key/data pair within a page.
   * Used in pages of type #P_BRANCH and #P_LEAF without #P_LEAF2.
   * We guarantee 2-byte alignment for 'MDB_node's.
   *
   * #mn_lo and #mn_hi are used for data size on leaf nodes, and for child
   * pgno on branch nodes.  On 64 bit platforms, #mn_flags is also used
   * for pgno.  (Branch nodes have no flags).  Lo and hi are in host byte
   * order in case some accesses can be optimized to 32-bit word access.
   *
   * Leaf node flags describe node contents.  #F_BIGDATA says the node's
   * data part is the page number of an overflow page with actual data.
   * #F_DUPDATA and #F_SUBDATA can be combined giving duplicate data in
   * a sub-page/sub-database, and named databases (just #F_SUBDATA).
   */
typedef struct MDB_node {
  /** part of data size or pgno
   *  @{ */
  unsigned short  mn_lo, mn_hi;
  /** @} */
/** @defgroup mdb_node Node Flags
 *  @ingroup internal
 *  Flags for node headers.
 *  @{
 */
#define F_BIGDATA   0x01      /**< data put on overflow page */
#define F_SUBDATA   0x02      /**< data is a sub-database */
#define F_DUPDATA   0x04      /**< data has duplicates */

/** valid flags for #mdb_node_add() */
#define  NODE_ADD_FLAGS  (F_DUPDATA|F_SUBDATA|MDB_RESERVE|MDB_APPEND)

/** @} */
  unsigned short  mn_flags;    /**< @ref mdb_node */
  unsigned short  mn_ksize;    /**< key size */
  char    mn_data[1];      /**< key and data are appended here */
} MDB_node;

  /** Size of the node header, excluding dynamic data at the end */
#define NODESIZE   offsetof(MDB_node, mn_data)

  /** Bit position of top word in page number, for shifting mn_flags */
#define PGNO_TOPWORD ((pgno_t)-1 > 0xffffffffu ? 32 : 0)

  /** Size of a node in a branch page with a given key.
   *  This is just the node header plus the key, there is no data.
   */
#define INDXSIZE(k)   (NODESIZE + ((k) == NULL ? 0 : (k)->mv_size))

  /** Size of a node in a leaf page with a given key and data.
   *  This is node header plus key plus data size.
   */
#define LEAFSIZE(k, d)   (NODESIZE + (k)->mv_size + (d)->mv_size)

  /** Address of node \b i in page \b p */
#define NODEPTR(p, i)   ((MDB_node *)((char *)(p) + (p)->mp_ptrs[i] + PAGEBASE))

  /** Address of the key for the node */
#define NODEKEY(node)   (void *)((node)->mn_data)

  /** Address of the data for a node */
#define NODEDATA(node)   (void *)((char *)(node)->mn_data + (node)->mn_ksize)

  /** Get the page number pointed to by a branch node */
#define NODEPGNO(node) \
  ((node)->mn_lo | ((pgno_t) (node)->mn_hi << 16) | \
   (PGNO_TOPWORD ? ((pgno_t) (node)->mn_flags << PGNO_TOPWORD) : 0))
  /** Set the page number in a branch node */
#define SETPGNO(node,pgno)  do { \
  (node)->mn_lo = (pgno) & 0xffff; (node)->mn_hi = (pgno) >> 16; \
  if (PGNO_TOPWORD) (node)->mn_flags = (pgno) >> PGNO_TOPWORD; } while(0)

  /** Get the size of the data in a leaf node */
#define NODEDSZ(node)   ((node)->mn_lo | ((unsigned)(node)->mn_hi << 16))
  /** Set the size of the data for a leaf node */
#define SETDSZ(node,size)  do { \
  (node)->mn_lo = (size) & 0xffff; (node)->mn_hi = (size) >> 16;} while(0)
  /** The size of a key in a node */
#define NODEKSZ(node)   ((node)->mn_ksize)

  /** Copy a page number from src to dst */
#define COPY_PGNO(dst,src)  dst = src
  /** The address of a key in a LEAF2 page.
   *  LEAF2 pages are used for #MDB_DUPFIXED sorted-duplicate sub-DBs.
   *  There are no node headers, keys are stored contiguously.
   */
#define LEAF2KEY(p, i, ks)  ((char *)(p) + PAGEHDRSZ + ((i)*(ks)))

  /** Set the \b node's key into \b keyptr, if requested. */
#define MDB_GET_KEY(node, keyptr)  { if ((keyptr) != NULL) { \
  (keyptr)->mv_size = NODEKSZ(node); (keyptr)->mv_data = NODEKEY(node); } }

  /** Set the \b node's key into \b key. */
#define MDB_GET_KEY2(node, key)  { key.mv_size = NODEKSZ(node); key.mv_data = NODEKEY(node); }

  /** Information about a single database in the environment. */
typedef struct MDB_db {
  uint32_t  md_pad;    /**< also ksize for LEAF2 pages */
  uint16_t  md_flags;  /**< @ref mdb_dbi_open */
  uint16_t  md_depth;  /**< depth of this tree */
  pgno_t    md_branch_pages;  /**< number of internal pages */
  pgno_t    md_leaf_pages;    /**< number of leaf pages */
  pgno_t    md_overflow_pages;  /**< number of overflow pages */
  mdb_size_t  md_entries;    /**< number of data items */
  pgno_t    md_root;    /**< the root page of this tree */
} MDB_db;

#define MDB_VALID  0x8000    /**< DB handle is valid, for me_dbflags */
#define PERSISTENT_FLAGS  (0xffff & ~(MDB_VALID))
  /** #mdb_dbi_open() flags */
#define VALID_FLAGS  (MDB_REVERSEKEY|MDB_DUPSORT|MDB_INTEGERKEY|MDB_DUPFIXED|\
  MDB_INTEGERDUP|MDB_REVERSEDUP|MDB_CREATE)

  /** Handle for the DB used to track free pages. */
#define  FREE_DBI  0
  /** Handle for the default DB. */
#define  MAIN_DBI  1
  /** Number of DBs in metapage (free and main) - also hardcoded elsewhere */
#define CORE_DBS  2

  /** Number of meta pages - also hardcoded elsewhere */
#define NUM_METAS  2

  /** Meta page content.
   *  A meta page is the start point for accessing a database snapshot.
   *  Pages 0-1 are meta pages. Transaction N writes meta page #(N % 2).
   */
typedef struct MDB_meta {
    /** Stamp identifying this as an LMDB file. It must be set
     *  to #MDB_MAGIC. */
  uint32_t  mm_magic;
    /** Version number of this file. Must be set to #MDB_DATA_VERSION. */
  uint32_t  mm_version;
  void    *mm_address;    /**< address for fixed mapping */
  mdb_size_t  mm_mapsize;      /**< size of mmap region */
  MDB_db    mm_dbs[CORE_DBS];  /**< first is free space, 2nd is main db */
  /** The size of pages used in this DB */
#define  mm_psize  mm_dbs[FREE_DBI].md_pad
  /** Any persistent environment flags. @ref mdb_env */
#define  mm_flags  mm_dbs[FREE_DBI].md_flags
  /** Last used page in the datafile.
   *  Actually the file may be shorter if the freeDB lists the final pages.
   */
  pgno_t    mm_last_pg;
  volatile txnid_t  mm_txnid;  /**< txnid that committed this page */
} MDB_meta;

  /** Buffer for a stack-allocated meta page.
   *  The members define size and alignment, and silence type
   *  aliasing warnings.  They are not used directly; that could
   *  mean incorrectly using several union members in parallel.
   */
typedef union MDB_metabuf {
  MDB_page  mb_page;
  struct {
    char    mm_pad[PAGEHDRSZ];
    MDB_meta  mm_meta;
  } mb_metabuf;
} MDB_metabuf;

  /** Auxiliary DB info.
   *  The information here is mostly static/read-only. There is
   *  only a single copy of this record in the environment.
   */
typedef struct MDB_dbx {
  MDB_val    md_name;    /**< name of the database */
  MDB_cmp_func  *md_cmp;  /**< function for comparing keys */
  MDB_cmp_func  *md_dcmp;  /**< function for comparing data items */
  MDB_rel_func  *md_rel;  /**< user relocate function */
  void    *md_relctx;    /**< user-provided context for md_rel */
} MDB_dbx;

  /** A database transaction.
   *  Every operation requires a transaction handle.
   */
struct MDB_txn {
  MDB_txn    *mt_parent;    /**< parent of a nested txn */
  /** Nested txn under this txn, set together with flag #MDB_TXN_HAS_CHILD */
  MDB_txn    *mt_child;
  pgno_t    mt_next_pgno;  /**< next unallocated page */
  /** The ID of this transaction. IDs are integers incrementing from 1.
   *  Only committed write transactions increment the ID. If a transaction
   *  aborts, the ID may be re-used by the next writer.
   */
  txnid_t    mt_txnid;
  MDB_env    *mt_env;    /**< the DB environment */
  /** The list of pages that became unused during this transaction.
   */
  MDB_IDL    mt_free_pgs;
  /** The list of loose pages that became unused and may be reused
   *  in this transaction, linked through #NEXT_LOOSE_PAGE(page).
   */
  MDB_page  *mt_loose_pgs;
  /** Number of loose pages (#mt_loose_pgs) */
  int      mt_loose_count;
  /** The sorted list of dirty pages we temporarily wrote to disk
   *  because the dirty list was full. page numbers in here are
   *  shifted left by 1, deleted slots have the LSB set.
   */
  MDB_IDL    mt_spill_pgs;
  union {
    /** For write txns: Modified pages. Sorted when not MDB_WRITEMAP. */
    MDB_ID2L  dirty_list;
    /** For read txns: This thread/txn's reader table slot, or NULL. */
    MDB_reader  *reader;
  } mt_u;
  /** Array of records for each DB known in the environment. */
  MDB_dbx    *mt_dbxs;
  /** Array of MDB_db records for each known DB */
  MDB_db    *mt_dbs;
  /** Array of sequence numbers for each DB handle */
  unsigned int  *mt_dbiseqs;
/** @defgroup mt_dbflag  Transaction DB Flags
 *  @ingroup internal
 * @{
 */
#define DB_DIRTY  0x01    /**< DB was written in this txn */
#define DB_STALE  0x02    /**< Named-DB record is older than txnID */
#define DB_NEW    0x04    /**< Named-DB handle opened in this txn */
#define DB_VALID  0x08    /**< DB handle is valid, see also #MDB_VALID */
#define DB_USRVALID  0x10    /**< As #DB_VALID, but not set for #FREE_DBI */
#define DB_DUPDATA  0x20    /**< DB is #MDB_DUPSORT data */
/** @} */
  /** In write txns, array of cursors for each DB */
  MDB_cursor  **mt_cursors;
  /** Array of flags for each DB */
  unsigned char  *mt_dbflags;
  /**  Number of DB records in use, or 0 when the txn is finished.
   *  This number only ever increments until the txn finishes; we
   *  don't decrement it when individual DB handles are closed.
   */
  MDB_dbi    mt_numdbs;

/** @defgroup mdb_txn  Transaction Flags
 *  @ingroup internal
 *  @{
 */
  /** #mdb_txn_begin() flags */
#define MDB_TXN_BEGIN_FLAGS  (MDB_NOMETASYNC|MDB_NOSYNC|MDB_RDONLY)
#define MDB_TXN_NOMETASYNC  MDB_NOMETASYNC  /**< don't sync meta for this txn on commit */
#define MDB_TXN_NOSYNC    MDB_NOSYNC  /**< don't sync this txn on commit */
#define MDB_TXN_RDONLY    MDB_RDONLY  /**< read-only transaction */
  /* internal txn flags */
#define MDB_TXN_WRITEMAP  MDB_WRITEMAP  /**< copy of #MDB_env flag in writers */
#define MDB_TXN_FINISHED  0x01    /**< txn is finished or never began */
#define MDB_TXN_ERROR    0x02    /**< txn is unusable after an error */
#define MDB_TXN_DIRTY    0x04    /**< must write, even if dirty list is empty */
#define MDB_TXN_SPILLS    0x08    /**< txn or a parent has spilled pages */
#define MDB_TXN_HAS_CHILD  0x10    /**< txn has an #MDB_txn.%mt_child */
  /** most operations on the txn are currently illegal */
#define MDB_TXN_BLOCKED    (MDB_TXN_FINISHED|MDB_TXN_ERROR|MDB_TXN_HAS_CHILD)
/** @} */
  unsigned int  mt_flags;    /**< @ref mdb_txn */
  /** #dirty_list room: Array size - \#dirty pages visible to this txn.
   *  Includes ancestor txns' dirty pages not hidden by other txns'
   *  dirty/spilled pages. Thus commit(nested txn) has room to merge
   *  dirty_list into mt_parent after freeing hidden mt_parent pages.
   */
  unsigned int  mt_dirty_room;
};

/** Enough space for 2^32 nodes with minimum of 2 keys per node. I.e., plenty.
 * At 4 keys per node, enough for 2^64 nodes, so there's probably no need to
 * raise this on a 64 bit machine.
 */
#define CURSOR_STACK     32

struct MDB_xcursor;

  /** Cursors are used for all DB operations.
   *  A cursor holds a path of (page pointer, key index) from the DB
   *  root to a position in the DB, plus other state. #MDB_DUPSORT
   *  cursors include an xcursor to the current data item. Write txns
   *  track their cursors and keep them up to date when data moves.
   *  Exception: An xcursor's pointer to a #P_SUBP page can be stale.
   *  (A node with #F_DUPDATA but no #F_SUBDATA contains a subpage).
   */
struct MDB_cursor {
  /** Next cursor on this DB in this txn */
  MDB_cursor  *mc_next;
  /** Backup of the original cursor if this cursor is a shadow */
  MDB_cursor  *mc_backup;
  /** Context used for databases with #MDB_DUPSORT, otherwise NULL */
  struct MDB_xcursor  *mc_xcursor;
  /** The transaction that owns this cursor */
  MDB_txn    *mc_txn;
  /** The database handle this cursor operates on */
  MDB_dbi    mc_dbi;
  /** The database record for this cursor */
  MDB_db    *mc_db;
  /** The database auxiliary record for this cursor */
  MDB_dbx    *mc_dbx;
  /** The @ref mt_dbflag for this database */
  unsigned char  *mc_dbflag;
  unsigned short   mc_snum;  /**< number of pushed pages */
  unsigned short  mc_top;    /**< index of top page, normally mc_snum-1 */
/** @defgroup mdb_cursor  Cursor Flags
 *  @ingroup internal
 *  Cursor state flags.
 *  @{
 */
#define C_INITIALIZED  0x01  /**< cursor has been initialized and is valid */
#define C_EOF  0x02      /**< No more data */
#define C_SUB  0x04      /**< Cursor is a sub-cursor */
#define C_DEL  0x08      /**< last op was a cursor_del */
#define C_UNTRACK  0x40    /**< Un-track cursor when closing */
#define C_WRITEMAP  MDB_TXN_WRITEMAP /**< Copy of txn flag */
#define C_ORIG_RDONLY  MDB_TXN_RDONLY
/** @} */
  unsigned int  mc_flags;  /**< @ref mdb_cursor */
  MDB_page  *mc_pg[CURSOR_STACK];  /**< stack of pushed pages */
  indx_t    mc_ki[CURSOR_STACK];  /**< stack of page indices */
#define MC_OVPG(mc)      ((MDB_page *)0)
#define MC_SET_OVPG(mc, pg)  ((void)0)
};

  /** Context for sorted-dup records.
   *  We could have gone to a fully recursive design, with arbitrarily
   *  deep nesting of sub-databases. But for now we only handle these
   *  levels - main DB, optional sub-DB, sorted-duplicate DB.
   */
typedef struct MDB_xcursor {
  /** A sub-cursor for traversing the Dup DB */
  MDB_cursor mx_cursor;
  /** The database record for this Dup DB */
  MDB_db  mx_db;
  /**  The auxiliary DB record for this Dup DB */
  MDB_dbx  mx_dbx;
  /** The @ref mt_dbflag for this Dup DB */
  unsigned char mx_dbflag;
} MDB_xcursor;

  /** Check if there is an inited xcursor */
#define XCURSOR_INITED(mc) \
  ((mc)->mc_xcursor && ((mc)->mc_xcursor->mx_cursor.mc_flags & C_INITIALIZED))

  /** Update the xcursor's sub-page pointer, if any, in \b mc.  Needed
   *  when the node which contains the sub-page may have moved.  Called
   *  with leaf page \b mp = mc->mc_pg[\b top].
   */
#define XCURSOR_REFRESH(mc, top, mp) do { \
  MDB_page *xr_pg = (mp); \
  MDB_node *xr_node; \
  if (!XCURSOR_INITED(mc) || (mc)->mc_ki[top] >= NUMKEYS(xr_pg)) break; \
  xr_node = NODEPTR(xr_pg, (mc)->mc_ki[top]); \
  if ((xr_node->mn_flags & (F_DUPDATA|F_SUBDATA)) == F_DUPDATA) \
    (mc)->mc_xcursor->mx_cursor.mc_pg[0] = NODEDATA(xr_node); \
} while (0)

  /** State of FreeDB old pages, stored in the MDB_env */
typedef struct MDB_pgstate {
  pgno_t    *mf_pghead;  /**< Reclaimed freeDB pages, or NULL before use */
  txnid_t    mf_pglast;  /**< ID of last used record, or 0 if !mf_pghead */
} MDB_pgstate;

  /** The database environment. */
struct MDB_env {
  HANDLE    me_fd;    /**< The main data file */
  HANDLE    me_lfd;    /**< The lock file */
  HANDLE    me_mfd;    /**< For writing and syncing the meta pages */
  /** Failed to update the meta page. Probably an I/O error. */
#define  MDB_FATAL_ERROR  0x80000000U
  /** Some fields are initialized. */
#define  MDB_ENV_ACTIVE  0x20000000U
  /** me_txkey is set */
#define  MDB_ENV_TXKEY  0x10000000U
  /** fdatasync is unreliable */
#define  MDB_FSYNCONLY  0x08000000U
  uint32_t   me_flags;    /**< @ref mdb_env */
  unsigned int  me_psize;  /**< DB page size, inited from me_os_psize */
  unsigned int  me_os_psize;  /**< OS page size, from #GET_PAGESIZE */
  /** Max #MDB_txninfo.%mti_numreaders of interest to #mdb_env_close() */
  volatile int  me_close_readers;
  MDB_dbi    me_maxdbs;    /**< size of the DB table */
  MDB_PID_T  me_pid;    /**< process ID of this env */
  char    *me_path;    /**< path to the DB files */
  char    *me_map;    /**< the memory map of the data file */
  MDB_txninfo  *me_txns;    /**< the memory map of the lock file or NULL */
  MDB_meta  *me_metas[NUM_METAS];  /**< pointers to the two meta pages */
  mdb_size_t  me_mapsize;    /**< size of the data memory map */
  MDB_OFF_T  me_size;    /**< current file size */
  pgno_t    me_maxpg;    /**< me_mapsize / me_psize */
  MDB_dbx    *me_dbxs;    /**< array of static DB info */
  uint16_t  *me_dbflags;  /**< array of flags from MDB_db.md_flags */
  unsigned int  *me_dbiseqs;  /**< array of dbi sequence numbers */
  MDB_pgstate  me_pgstate;    /**< state of old pages from freeDB */
# define    me_pglast  me_pgstate.mf_pglast
# define    me_pghead  me_pgstate.mf_pghead
  MDB_page  *me_dpages;    /**< list of malloc'd blocks for re-use */
  /** IDL of pages that became unused in a write txn */
  MDB_IDL    me_free_pgs;
  /** ID2L of pages written during a write txn. Length MDB_IDL_UM_SIZE. */
  MDB_ID2L  me_dirty_list;
  /** Max number of freelist items that can fit in a single overflow page */
  int      me_maxfree_1pg;
  void    *me_userctx;   /**< User-settable context */
  MDB_assert_func *me_assert_func; /**< Callback for assertion failures */
};

  /** max bytes to write in one call */
#define MAX_WRITE    (0x40000000U >> (sizeof(ssize_t) == 4))

static int  mdb_page_alloc(MDB_cursor *mc, int num, MDB_page **mp);
static int  mdb_page_new(MDB_cursor *mc, uint32_t flags, int num, MDB_page **mp);
static int  mdb_page_touch(MDB_cursor *mc);

#define MDB_END_NAMES {"committed", "empty-commit", "abort", "reset", \
  "reset-tmp", "fail-begin", "fail-beginchild"}
enum {
  /* mdb_txn_end operation number, for logging */
  MDB_END_COMMITTED, MDB_END_EMPTY_COMMIT, MDB_END_ABORT, MDB_END_RESET,
  MDB_END_RESET_TMP, MDB_END_FAIL_BEGIN, MDB_END_FAIL_BEGINCHILD
};
#define MDB_END_OPMASK  0x0F  /**< mask for #mdb_txn_end() operation number */
#define MDB_END_UPDATE  0x10  /**< update env state (DBIs) */
#define MDB_END_FREE  0x20  /**< free txn unless it is #MDB_env.%me_txn0 */
#define MDB_END_SLOT MDB_NOTLS  /**< release any reader slot if #MDB_NOTLS */
static void mdb_txn_end(MDB_txn *txn, unsigned mode);

static int  mdb_page_get(MDB_cursor *mc, pgno_t pgno, MDB_page **mp, int *lvl);
static int  mdb_page_search_root(MDB_cursor *mc,
          MDB_val *key, int modify);
#define MDB_PS_MODIFY  1
#define MDB_PS_ROOTONLY  2
#define MDB_PS_FIRST  4
#define MDB_PS_LAST    8
static int  mdb_page_search(MDB_cursor *mc,
          MDB_val *key, int flags);
static int  mdb_page_merge(MDB_cursor *csrc, MDB_cursor *cdst);

#define MDB_SPLIT_REPLACE  MDB_APPENDDUP  /**< newkey is not new */
static int  mdb_page_split(MDB_cursor *mc, MDB_val *newkey, MDB_val *newdata,
        pgno_t newpgno, unsigned int nflags);

static int  mdb_env_read_header(MDB_env *env, int prev, MDB_meta *meta);
static MDB_meta *mdb_env_pick_meta(const MDB_env *env);
static int  mdb_env_write_meta(MDB_txn *txn);
static void mdb_env_close0(MDB_env *env, int excl);

static MDB_node *mdb_node_search(MDB_cursor *mc, MDB_val *key, int *exactp);
static int  mdb_node_add(MDB_cursor *mc, indx_t indx,
          MDB_val *key, MDB_val *data, pgno_t pgno, unsigned int flags);
static void mdb_node_del(MDB_cursor *mc, int ksize);
static void mdb_node_shrink(MDB_page *mp, indx_t indx);
static int  mdb_node_move(MDB_cursor *csrc, MDB_cursor *cdst, int fromleft);
static int  mdb_node_read(MDB_cursor *mc, MDB_node *leaf, MDB_val *data);
static size_t  mdb_leaf_size(MDB_env *env, MDB_val *key, MDB_val *data);
static size_t  mdb_branch_size(MDB_env *env, MDB_val *key);

static int  mdb_rebalance(MDB_cursor *mc);
static int  mdb_update_key(MDB_cursor *mc, MDB_val *key);

static int  mdb_del0(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data, unsigned flags);

static int  mdb_drop0(MDB_cursor *mc, int subs);
static void mdb_default_cmp(MDB_txn *txn, MDB_dbi dbi);

/** @cond */
static MDB_cmp_func  mdb_cmp_memn, mdb_cmp_memnr, mdb_cmp_int, mdb_cmp_cint, mdb_cmp_long;
/** @endcond */

#define mdb_cmp_clong mdb_cmp_long

/** True if we need #mdb_cmp_clong() instead of \b cmp for #MDB_INTEGERDUP */
#define NEED_CMP_CLONG(cmp, ksize) \
  (UINT_MAX < MDB_SIZE_MAX && \
   (cmp) == mdb_cmp_int && (ksize) == sizeof(mdb_size_t))

/** Return the library version info. */
char * ESECT
mdb_version(int *major, int *minor, int *patch)
{
  if (major) *major = MDB_VERSION_MAJOR;
  if (minor) *minor = MDB_VERSION_MINOR;
  if (patch) *patch = MDB_VERSION_PATCH;
  return MDB_VERSION_STRING;
}

/** Table of descriptions for LMDB @ref errors */
static char *const mdb_errstr[] = {
  "MDB_KEYEXIST: Key/data pair already exists",
  "MDB_NOTFOUND: No matching key/data pair found",
  "MDB_PAGE_NOTFOUND: Requested page not found",
  "MDB_CORRUPTED: Located page was wrong type",
  "MDB_PANIC: Update of meta page failed or environment had fatal error",
  "MDB_VERSION_MISMATCH: Database environment version mismatch",
  "MDB_INVALID: File is not an LMDB file",
  "MDB_MAP_FULL: Environment mapsize limit reached",
  "MDB_DBS_FULL: Environment maxdbs limit reached",
  "MDB_READERS_FULL: Environment maxreaders limit reached",
  "MDB_TLS_FULL: Thread-local storage keys full - too many environments open",
  "MDB_TXN_FULL: Transaction has too many dirty pages - transaction too big",
  "MDB_CURSOR_FULL: Internal error - cursor stack limit reached",
  "MDB_PAGE_FULL: Internal error - page has no more space",
  "MDB_MAP_RESIZED: Database contents grew beyond environment mapsize",
  "MDB_INCOMPATIBLE: Operation and DB incompatible, or DB flags changed",
  "MDB_BAD_RSLOT: Invalid reuse of reader locktable slot",
  "MDB_BAD_TXN: Transaction must abort, has a child, or is invalid",
  "MDB_BAD_VALSIZE: Unsupported size of key/DB name/data, or wrong DUPFIXED size",
  "MDB_BAD_DBI: The specified DBI handle was closed/changed unexpectedly",
  "MDB_PROBLEM: Unexpected problem - txn should abort",
};

char *
mdb_strerror(int err)
{
  int i;
  if (!err)
    return ("Successful return: 0");

  if (err >= MDB_KEYEXIST && err <= MDB_LAST_ERRCODE) {
    i = err - MDB_KEYEXIST;
    return mdb_errstr[i];
  }

  if (err < 0)
    return "Invalid error code";
  return strerror(err);
}

/** assert(3) variant in cursor context */
#define mdb_cassert(mc, expr)  mdb_assert0((mc)->mc_txn->mt_env, expr, #expr)
/** assert(3) variant in transaction context */
#define mdb_tassert(txn, expr)  mdb_assert0((txn)->mt_env, expr, #expr)
/** assert(3) variant in environment context */
#define mdb_eassert(env, expr)  mdb_assert0(env, expr, #expr)

#ifndef NDEBUG
# define mdb_assert0(env, expr, expr_txt) ((expr) ? (void)0 : \
    mdb_assert_fail(env, expr_txt, mdb_func_, __FILE__, __LINE__))

static void ESECT
mdb_assert_fail(MDB_env *env, const char *expr_txt,
  const char *func, const char *file, int line)
{
  char buf[400];
  sprintf(buf, "%.100s:%d: Assertion '%.200s' failed in %.40s()",
    file, line, expr_txt, func);
  if (env->me_assert_func)
    env->me_assert_func(env, buf);
  fprintf(stderr, "%s\n", buf);
  abort();
}
#else
# define mdb_assert0(env, expr, expr_txt) ((void) 0)
#endif /* NDEBUG */

int
mdb_cmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b)
{
  return txn->mt_dbxs[dbi].md_cmp(a, b);
}

int
mdb_dcmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b)
{
  MDB_cmp_func *dcmp = txn->mt_dbxs[dbi].md_dcmp;
  if (NEED_CMP_CLONG(dcmp, a->mv_size))
    dcmp = mdb_cmp_clong;
  return dcmp(a, b);
}

/** Allocate memory for a page.
 * Re-use old malloc'd pages first for singletons, otherwise just malloc.
 * Set #MDB_TXN_ERROR on failure.
 */
static MDB_page *
mdb_page_malloc(MDB_txn *txn, unsigned num)
{
  MDB_env *env = txn->mt_env;
  MDB_page *ret = env->me_dpages;
  size_t psize = env->me_psize, sz = psize, off;
  /* For ! #MDB_NOMEMINIT, psize counts how much to init.
   * For a single page alloc, we init everything after the page header.
   * For multi-page, we init the final page; if the caller needed that
   * many pages they will be filling in at least up to the last page.
   */
  if (num == 1) {
    if (ret) {
      env->me_dpages = ret->mp_next;
      return ret;
    }
    psize -= off = PAGEHDRSZ;
  } else {
    sz *= num;
    off = sz - psize;
  }
  if ((ret = malloc(sz)) != NULL) {
    if (!(env->me_flags & MDB_NOMEMINIT)) {
      memset((char *)ret + off, 0, psize);
      ret->mp_pad = 0;
    }
  } else {
    txn->mt_flags |= MDB_TXN_ERROR;
  }
  return ret;
}
/** Free a single page.
 * Saves single pages to a list, for future reuse.
 * (This is not used for multi-page overflow pages.)
 */
static void
mdb_page_free(MDB_env *env, MDB_page *mp)
{
  mp->mp_next = env->me_dpages;
  env->me_dpages = mp;
}

/** Free a dirty page */
static void
mdb_dpage_free(MDB_env *env, MDB_page *dp)
{
  if (!IS_OVERFLOW(dp) || dp->mp_pages == 1) {
    mdb_page_free(env, dp);
  } else {
    /* large pages just get freed directly */
    free(dp);
  }
}

/**  Return all dirty pages to dpage list */
static void
mdb_dlist_free(MDB_txn *txn)
{
  MDB_env *env = txn->mt_env;
  MDB_ID2L dl = txn->mt_u.dirty_list;
  unsigned i, n = dl[0].mid;

  for (i = 1; i <= n; i++) {
    mdb_dpage_free(env, dl[i].mptr);
  }
  dl[0].mid = 0;
}

#define MDB_PAGE_UNREF(txn, mp)
#define MDB_CURSOR_UNREF(mc, force) ((void)0)

/** Loosen or free a single page.
 * Saves single pages to a list for future reuse
 * in this same txn. It has been pulled from the freeDB
 * and already resides on the dirty list, but has been
 * deleted. Use these pages first before pulling again
 * from the freeDB.
 *
 * If the page wasn't dirtied in this txn, just add it
 * to this txn's free list.
 */
static int
mdb_page_loose(MDB_cursor *mc, MDB_page *mp)
{
  int loose = 0;
  pgno_t pgno = mp->mp_pgno;
  MDB_txn *txn = mc->mc_txn;

  if ((mp->mp_flags & P_DIRTY) && mc->mc_dbi != FREE_DBI) {
    if (txn->mt_parent) {
      MDB_ID2 *dl = txn->mt_u.dirty_list;
      /* If txn has a parent, make sure the page is in our
       * dirty list.
       */
      if (dl[0].mid) {
        unsigned x = mdb_mid2l_search(dl, pgno);
        if (x <= dl[0].mid && dl[x].mid == pgno) {
          if (mp != dl[x].mptr) { /* bad cursor? */
            mc->mc_flags &= ~(C_INITIALIZED|C_EOF);
            txn->mt_flags |= MDB_TXN_ERROR;
            return MDB_PROBLEM;
          }
          /* ok, it's ours */
          loose = 1;
        }
      }
    } else {
      /* no parent txn, so it's just ours */
      loose = 1;
    }
  }
  if (loose) {
    DPRINTF(("loosen db %d page %"Yu, DDBI(mc), mp->mp_pgno));
    NEXT_LOOSE_PAGE(mp) = txn->mt_loose_pgs;
    txn->mt_loose_pgs = mp;
    txn->mt_loose_count++;
    mp->mp_flags |= P_LOOSE;
  } else {
    int rc = mdb_midl_append(&txn->mt_free_pgs, pgno);
    if (rc)
      return rc;
  }

  return MDB_SUCCESS;
}

/** Set or clear P_KEEP in dirty, non-overflow, non-sub pages watched by txn.
 * @param[in] mc A cursor handle for the current operation.
 * @param[in] pflags Flags of the pages to update:
 * P_DIRTY to set P_KEEP, P_DIRTY|P_KEEP to clear it.
 * @param[in] all No shortcuts. Needed except after a full #mdb_page_flush().
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_pages_xkeep(MDB_cursor *mc, unsigned pflags, int all)
{
  enum { Mask = P_SUBP|P_DIRTY|P_LOOSE|P_KEEP };
  MDB_txn *txn = mc->mc_txn;
  MDB_cursor *m3, *m0 = mc;
  MDB_xcursor *mx;
  MDB_page *dp, *mp;
  MDB_node *leaf;
  unsigned i, j;
  int rc = MDB_SUCCESS, level;

  /* Mark pages seen by cursors: First m0, then tracked cursors */
  for (i = txn->mt_numdbs;; ) {
    if (mc->mc_flags & C_INITIALIZED) {
      for (m3 = mc;; m3 = &mx->mx_cursor) {
        mp = NULL;
        for (j=0; j<m3->mc_snum; j++) {
          mp = m3->mc_pg[j];
          if ((mp->mp_flags & Mask) == pflags)
            mp->mp_flags ^= P_KEEP;
        }
        mx = m3->mc_xcursor;
        /* Proceed to mx if it is at a sub-database */
        if (! (mx && (mx->mx_cursor.mc_flags & C_INITIALIZED)))
          break;
        if (! (mp && (mp->mp_flags & P_LEAF)))
          break;
        leaf = NODEPTR(mp, m3->mc_ki[j-1]);
        if (!(leaf->mn_flags & F_SUBDATA))
          break;
      }
    }
    mc = mc->mc_next;
    for (; !mc || mc == m0; mc = txn->mt_cursors[--i])
      if (i == 0)
        goto mark_done;
  }

mark_done:
  if (all) {
    /* Mark dirty root pages */
    for (i=0; i<txn->mt_numdbs; i++) {
      if (txn->mt_dbflags[i] & DB_DIRTY) {
        pgno_t pgno = txn->mt_dbs[i].md_root;
        if (pgno == P_INVALID)
          continue;
        if ((rc = mdb_page_get(m0, pgno, &dp, &level)) != MDB_SUCCESS)
          break;
        if ((dp->mp_flags & Mask) == pflags && level <= 1)
          dp->mp_flags ^= P_KEEP;
      }
    }
  }

  return rc;
}

static int mdb_page_flush(MDB_txn *txn, int keep);

/**  Spill pages from the dirty list back to disk.
 * This is intended to prevent running into #MDB_TXN_FULL situations,
 * but note that they may still occur in a few cases:
 *  1) our estimate of the txn size could be too small. Currently this
 *   seems unlikely, except with a large number of #MDB_MULTIPLE items.
 *  2) child txns may run out of space if their parents dirtied a
 *   lot of pages and never spilled them. TODO: we probably should do
 *   a preemptive spill during #mdb_txn_begin() of a child txn, if
 *   the parent's dirty_room is below a given threshold.
 *
 * Otherwise, if not using nested txns, it is expected that apps will
 * not run into #MDB_TXN_FULL any more. The pages are flushed to disk
 * the same way as for a txn commit, e.g. their P_DIRTY flag is cleared.
 * If the txn never references them again, they can be left alone.
 * If the txn only reads them, they can be used without any fuss.
 * If the txn writes them again, they can be dirtied immediately without
 * going thru all of the work of #mdb_page_touch(). Such references are
 * handled by #mdb_page_unspill().
 *
 * Also note, we never spill DB root pages, nor pages of active cursors,
 * because we'll need these back again soon anyway. And in nested txns,
 * we can't spill a page in a child txn if it was already spilled in a
 * parent txn. That would alter the parent txns' data even though
 * the child hasn't committed yet, and we'd have no way to undo it if
 * the child aborted.
 *
 * @param[in] m0 cursor A cursor handle identifying the transaction and
 *  database for which we are checking space.
 * @param[in] key For a put operation, the key being stored.
 * @param[in] data For a put operation, the data being stored.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_spill(MDB_cursor *m0, MDB_val *key, MDB_val *data)
{
  MDB_txn *txn = m0->mc_txn;
  MDB_page *dp;
  MDB_ID2L dl = txn->mt_u.dirty_list;
  unsigned int i, j, need;
  int rc;

  if (m0->mc_flags & C_SUB)
    return MDB_SUCCESS;

  /* Estimate how much space this op will take */
  i = m0->mc_db->md_depth;
  /* Named DBs also dirty the main DB */
  if (m0->mc_dbi >= CORE_DBS)
    i += txn->mt_dbs[MAIN_DBI].md_depth;
  /* For puts, roughly factor in the key+data size */
  if (key)
    i += (LEAFSIZE(key, data) + txn->mt_env->me_psize) / txn->mt_env->me_psize;
  i += i;  /* double it for good measure */
  need = i;

  if (txn->mt_dirty_room > i)
    return MDB_SUCCESS;

  if (!txn->mt_spill_pgs) {
    txn->mt_spill_pgs = mdb_midl_alloc(MDB_IDL_UM_MAX);
    if (!txn->mt_spill_pgs)
      return ENOMEM;
  } else {
    /* purge deleted slots */
    MDB_IDL sl = txn->mt_spill_pgs;
    unsigned int num = sl[0];
    j=0;
    for (i=1; i<=num; i++) {
      if (!(sl[i] & 1))
        sl[++j] = sl[i];
    }
    sl[0] = j;
  }

  /* Preserve pages which may soon be dirtied again */
  if ((rc = mdb_pages_xkeep(m0, P_DIRTY, 1)) != MDB_SUCCESS)
    goto done;

  /* Less aggressive spill - we originally spilled the entire dirty list,
   * with a few exceptions for cursor pages and DB root pages. But this
   * turns out to be a lot of wasted effort because in a large txn many
   * of those pages will need to be used again. So now we spill only 1/8th
   * of the dirty pages. Testing revealed this to be a good tradeoff,
   * better than 1/2, 1/4, or 1/10.
   */
  if (need < MDB_IDL_UM_MAX / 8)
    need = MDB_IDL_UM_MAX / 8;

  /* Save the page IDs of all the pages we're flushing */
  /* flush from the tail forward, this saves a lot of shifting later on. */
  for (i=dl[0].mid; i && need; i--) {
    MDB_ID pn = dl[i].mid << 1;
    dp = dl[i].mptr;
    if (dp->mp_flags & (P_LOOSE|P_KEEP))
      continue;
    /* Can't spill twice, make sure it's not already in a parent's
     * spill list.
     */
    if (txn->mt_parent) {
      MDB_txn *tx2;
      for (tx2 = txn->mt_parent; tx2; tx2 = tx2->mt_parent) {
        if (tx2->mt_spill_pgs) {
          j = mdb_midl_search(tx2->mt_spill_pgs, pn);
          if (j <= tx2->mt_spill_pgs[0] && tx2->mt_spill_pgs[j] == pn) {
            dp->mp_flags |= P_KEEP;
            break;
          }
        }
      }
      if (tx2)
        continue;
    }
    if ((rc = mdb_midl_append(&txn->mt_spill_pgs, pn)))
      goto done;
    need--;
  }
  mdb_midl_sort(txn->mt_spill_pgs);

  /* Flush the spilled part of dirty list */
  if ((rc = mdb_page_flush(txn, i)) != MDB_SUCCESS)
    goto done;

  /* Reset any dirty pages we kept that page_flush didn't see */
  rc = mdb_pages_xkeep(m0, P_DIRTY|P_KEEP, i);

done:
  txn->mt_flags |= rc ? MDB_TXN_ERROR : MDB_TXN_SPILLS;
  return rc;
}

/** Find oldest txnid still referenced. Expects txn->mt_txnid > 0. */
static txnid_t
mdb_find_oldest(MDB_txn *txn)
{
  int i;
  txnid_t mr, oldest = txn->mt_txnid - 1;
  if (txn->mt_env->me_txns) {
    MDB_reader *r = txn->mt_env->me_txns->mti_readers;
    for (i = txn->mt_env->me_txns->mti_numreaders; --i >= 0; ) {
      if (r[i].mr_pid) {
        mr = r[i].mr_txnid;
        if (oldest > mr)
          oldest = mr;
      }
    }
  }
  return oldest;
}

/** Add a page to the txn's dirty list */
static void
mdb_page_dirty(MDB_txn *txn, MDB_page *mp)
{
  MDB_ID2 mid;
  int rc, (*insert)(MDB_ID2L, MDB_ID2 *);
  if (txn->mt_flags & MDB_TXN_WRITEMAP)
    insert = mdb_mid2l_append;
  else
    insert = mdb_mid2l_insert;
  mid.mid = mp->mp_pgno;
  mid.mptr = mp;
  rc = insert(txn->mt_u.dirty_list, &mid);
  mdb_tassert(txn, rc == 0);
  txn->mt_dirty_room--;
}

/** Allocate page numbers and memory for writing.  Maintain me_pglast,
 * me_pghead and mt_next_pgno.  Set #MDB_TXN_ERROR on failure.
 *
 * If there are free pages available from older transactions, they
 * are re-used first. Otherwise allocate a new page at mt_next_pgno.
 * Do not modify the freedB, just merge freeDB records into me_pghead[]
 * and move me_pglast to say which records were consumed.  Only this
 * function can create me_pghead and move me_pglast/mt_next_pgno.
 * When #MDB_DEVEL & 2, it is not affected by #mdb_freelist_save(): it
 * then uses the transaction's original snapshot of the freeDB.
 * @param[in] mc cursor A cursor handle identifying the transaction and
 *  database for which we are allocating.
 * @param[in] num the number of pages to allocate.
 * @param[out] mp Address of the allocated page(s). Requests for multiple pages
 *  will always be satisfied by a single contiguous chunk of memory.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_alloc(MDB_cursor *mc, int num, MDB_page **mp)
{
#ifdef MDB_PARANOID  /* Seems like we can ignore this now */
  /* Get at most <Max_retries> more freeDB records once me_pghead
   * has enough pages.  If not enough, use new pages from the map.
   * If <Paranoid> and mc is updating the freeDB, only get new
   * records if me_pghead is empty. Then the freelist cannot play
   * catch-up with itself by growing while trying to save it.
   */
  enum { Paranoid = 1, Max_retries = 500 };
#else
  enum { Paranoid = 0, Max_retries = INT_MAX /*infinite*/ };
#endif
  int rc, retry = num * 60;
  MDB_txn *txn = mc->mc_txn;
  MDB_env *env = txn->mt_env;
  pgno_t pgno, *mop = env->me_pghead;
  unsigned i, j, mop_len = mop ? mop[0] : 0, n2 = num-1;
  MDB_page *np;
  txnid_t oldest = 0, last;
  MDB_cursor_op op;
  MDB_cursor m2;
  int found_old = 0;

  /* If there are any loose pages, just use them */
  if (num == 1 && txn->mt_loose_pgs) {
    np = txn->mt_loose_pgs;
    txn->mt_loose_pgs = NEXT_LOOSE_PAGE(np);
    txn->mt_loose_count--;
    DPRINTF(("db %d use loose page %"Yu, DDBI(mc), np->mp_pgno));
    *mp = np;
    return MDB_SUCCESS;
  }

  *mp = NULL;

  /* If our dirty list is already full, we can't do anything */
  if (txn->mt_dirty_room == 0) {
    rc = MDB_TXN_FULL;
    goto fail;
  }

  for (op = MDB_FIRST;; op = MDB_NEXT) {
    MDB_val key, data;
    MDB_node *leaf;
    pgno_t *idl;

    /* Seek a big enough contiguous page range. Prefer
     * pages at the tail, just truncating the list.
     */
    if (mop_len > n2) {
      i = mop_len;
      do {
        pgno = mop[i];
        if (mop[i-n2] == pgno+n2)
          goto search_done;
      } while (--i > n2);
      if (--retry < 0)
        break;
    }

    if (op == MDB_FIRST) {  /* 1st iteration */
      /* Prepare to fetch more and coalesce */
      last = env->me_pglast;
      oldest = env->me_pgoldest;
      mdb_cursor_init(&m2, txn, FREE_DBI, NULL);
      if (last) {
        op = MDB_SET_RANGE;
        key.mv_data = &last; /* will look up last+1 */
        key.mv_size = sizeof(last);
      }
      if (Paranoid && mc->mc_dbi == FREE_DBI)
        retry = -1;
    }
    if (Paranoid && retry < 0 && mop_len)
      break;

    last++;
    /* Do not fetch more if the record will be too recent */
    if (oldest <= last) {
      if (!found_old) {
        oldest = mdb_find_oldest(txn);
        env->me_pgoldest = oldest;
        found_old = 1;
      }
      if (oldest <= last)
        break;
    }
    rc = mdb_cursor_get(&m2, &key, NULL, op);
    if (rc) {
      if (rc == MDB_NOTFOUND)
        break;
      goto fail;
    }
    last = *(txnid_t*)key.mv_data;
    if (oldest <= last) {
      if (!found_old) {
        oldest = mdb_find_oldest(txn);
        env->me_pgoldest = oldest;
        found_old = 1;
      }
      if (oldest <= last)
        break;
    }
    np = m2.mc_pg[m2.mc_top];
    leaf = NODEPTR(np, m2.mc_ki[m2.mc_top]);
    if ((rc = mdb_node_read(&m2, leaf, &data)) != MDB_SUCCESS)
      goto fail;

    idl = (MDB_ID *) data.mv_data;
    i = idl[0];
    if (!mop) {
      if (!(env->me_pghead = mop = mdb_midl_alloc(i))) {
        rc = ENOMEM;
        goto fail;
      }
    } else {
      if ((rc = mdb_midl_need(&env->me_pghead, i)) != 0)
        goto fail;
      mop = env->me_pghead;
    }
    env->me_pglast = last;
    /* Merge in descending sorted order */
    mdb_midl_xmerge(mop, idl);
    mop_len = mop[0];
  }

  /* Use new pages from the map when nothing suitable in the freeDB */
  i = 0;
  pgno = txn->mt_next_pgno;
  if (pgno + num >= env->me_maxpg) {
      DPUTS("DB size maxed out");
      rc = MDB_MAP_FULL;
      goto fail;
  }

search_done:
  if (env->me_flags & MDB_WRITEMAP) {
    np = (MDB_page *)(env->me_map + env->me_psize * pgno);
  } else {
    if (!(np = mdb_page_malloc(txn, num))) {
      rc = ENOMEM;
      goto fail;
    }
  }
  if (i) {
    mop[0] = mop_len -= num;
    /* Move any stragglers down */
    for (j = i-num; j < mop_len; )
      mop[++j] = mop[++i];
  } else {
    txn->mt_next_pgno = pgno + num;
  }
  np->mp_pgno = pgno;
  mdb_page_dirty(txn, np);
  *mp = np;

  return MDB_SUCCESS;

fail:
  txn->mt_flags |= MDB_TXN_ERROR;
  return rc;
}

/** Copy the used portions of a non-overflow page.
 * @param[in] dst page to copy into
 * @param[in] src page to copy from
 * @param[in] psize size of a page
 */
static void
mdb_page_copy(MDB_page *dst, MDB_page *src, unsigned int psize)
{
  enum { Align = sizeof(pgno_t) };
  indx_t upper = src->mp_upper, lower = src->mp_lower, unused = upper-lower;

  /* If page isn't full, just copy the used portion. Adjust
   * alignment so memcpy may copy words instead of bytes.
   */
  if ((unused &= -Align) && !IS_LEAF2(src)) {
    upper = (upper + PAGEBASE) & -Align;
    memcpy(dst, src, (lower + PAGEBASE + (Align-1)) & -Align);
    memcpy((pgno_t *)((char *)dst+upper), (pgno_t *)((char *)src+upper),
      psize - upper);
  } else {
    memcpy(dst, src, psize - unused);
  }
}

/** Pull a page off the txn's spill list, if present.
 * If a page being referenced was spilled to disk in this txn, bring
 * it back and make it dirty/writable again.
 * @param[in] txn the transaction handle.
 * @param[in] mp the page being referenced. It must not be dirty.
 * @param[out] ret the writable page, if any. ret is unchanged if
 * mp wasn't spilled.
 */
static int
mdb_page_unspill(MDB_txn *txn, MDB_page *mp, MDB_page **ret)
{
  MDB_env *env = txn->mt_env;
  const MDB_txn *tx2;
  unsigned x;
  pgno_t pgno = mp->mp_pgno, pn = pgno << 1;

  for (tx2 = txn; tx2; tx2=tx2->mt_parent) {
    if (!tx2->mt_spill_pgs)
      continue;
    x = mdb_midl_search(tx2->mt_spill_pgs, pn);
    if (x <= tx2->mt_spill_pgs[0] && tx2->mt_spill_pgs[x] == pn) {
      MDB_page *np;
      int num;
      if (txn->mt_dirty_room == 0)
        return MDB_TXN_FULL;
      if (IS_OVERFLOW(mp))
        num = mp->mp_pages;
      else
        num = 1;
      if (env->me_flags & MDB_WRITEMAP) {
        np = mp;
      } else {
        np = mdb_page_malloc(txn, num);
        if (!np)
          return ENOMEM;
        if (num > 1)
          memcpy(np, mp, num * env->me_psize);
        else
          mdb_page_copy(np, mp, env->me_psize);
      }
      if (tx2 == txn) {
        /* If in current txn, this page is no longer spilled.
         * If it happens to be the last page, truncate the spill list.
         * Otherwise mark it as deleted by setting the LSB.
         */
        if (x == txn->mt_spill_pgs[0])
          txn->mt_spill_pgs[0]--;
        else
          txn->mt_spill_pgs[x] |= 1;
      }  /* otherwise, if belonging to a parent txn, the
         * page remains spilled until child commits
         */

      mdb_page_dirty(txn, np);
      np->mp_flags |= P_DIRTY;
      *ret = np;
      break;
    }
  }
  return MDB_SUCCESS;
}

/** Touch a page: make it dirty and re-insert into tree with updated pgno.
 * Set #MDB_TXN_ERROR on failure.
 * @param[in] mc cursor pointing to the page to be touched
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_touch(MDB_cursor *mc)
{
  MDB_page *mp = mc->mc_pg[mc->mc_top], *np;
  MDB_txn *txn = mc->mc_txn;
  MDB_cursor *m2, *m3;
  pgno_t  pgno;
  int rc;

  if (!F_ISSET(mp->mp_flags, P_DIRTY)) {
    if (txn->mt_flags & MDB_TXN_SPILLS) {
      np = NULL;
      rc = mdb_page_unspill(txn, mp, &np);
      if (rc)
        goto fail;
      if (np)
        goto done;
    }
    if ((rc = mdb_midl_need(&txn->mt_free_pgs, 1)) ||
      (rc = mdb_page_alloc(mc, 1, &np)))
      goto fail;
    pgno = np->mp_pgno;
    DPRINTF(("touched db %d page %"Yu" -> %"Yu, DDBI(mc),
      mp->mp_pgno, pgno));
    mdb_cassert(mc, mp->mp_pgno != pgno);
    mdb_midl_xappend(txn->mt_free_pgs, mp->mp_pgno);
    /* Update the parent page, if any, to point to the new page */
    if (mc->mc_top) {
      MDB_page *parent = mc->mc_pg[mc->mc_top-1];
      MDB_node *node = NODEPTR(parent, mc->mc_ki[mc->mc_top-1]);
      SETPGNO(node, pgno);
    } else {
      mc->mc_db->md_root = pgno;
    }
  } else if (txn->mt_parent && !IS_SUBP(mp)) {
    MDB_ID2 mid, *dl = txn->mt_u.dirty_list;
    pgno = mp->mp_pgno;
    /* If txn has a parent, make sure the page is in our
     * dirty list.
     */
    if (dl[0].mid) {
      unsigned x = mdb_mid2l_search(dl, pgno);
      if (x <= dl[0].mid && dl[x].mid == pgno) {
        if (mp != dl[x].mptr) { /* bad cursor? */
          mc->mc_flags &= ~(C_INITIALIZED|C_EOF);
          txn->mt_flags |= MDB_TXN_ERROR;
          return MDB_PROBLEM;
        }
        return 0;
      }
    }
    mdb_cassert(mc, dl[0].mid < MDB_IDL_UM_MAX);
    /* No - copy it */
    np = mdb_page_malloc(txn, 1);
    if (!np)
      return ENOMEM;
    mid.mid = pgno;
    mid.mptr = np;
    rc = mdb_mid2l_insert(dl, &mid);
    mdb_cassert(mc, rc == 0);
  } else {
    return 0;
  }

  mdb_page_copy(np, mp, txn->mt_env->me_psize);
  np->mp_pgno = pgno;
  np->mp_flags |= P_DIRTY;

done:
  /* Adjust cursors pointing to mp */
  mc->mc_pg[mc->mc_top] = np;
  m2 = txn->mt_cursors[mc->mc_dbi];
  if (mc->mc_flags & C_SUB) {
    for (; m2; m2=m2->mc_next) {
      m3 = &m2->mc_xcursor->mx_cursor;
      if (m3->mc_snum < mc->mc_snum) continue;
      if (m3->mc_pg[mc->mc_top] == mp)
        m3->mc_pg[mc->mc_top] = np;
    }
  } else {
    for (; m2; m2=m2->mc_next) {
      if (m2->mc_snum < mc->mc_snum) continue;
      if (m2 == mc) continue;
      if (m2->mc_pg[mc->mc_top] == mp) {
        m2->mc_pg[mc->mc_top] = np;
        if (IS_LEAF(np))
          XCURSOR_REFRESH(m2, mc->mc_top, np);
      }
    }
  }
  MDB_PAGE_UNREF(mc->mc_txn, mp);
  return 0;

fail:
  txn->mt_flags |= MDB_TXN_ERROR;
  return rc;
}

int
mdb_env_sync0(MDB_env *env, int force, pgno_t numpgs)
{
  int rc = 0;
  if (env->me_flags & MDB_RDONLY)
    return EACCES;
  if (force
    || !(env->me_flags & MDB_NOSYNC)
    ) {
    if (env->me_flags & MDB_WRITEMAP) {
      int flags = ((env->me_flags & MDB_MAPASYNC) && !force)
        ? MS_ASYNC : MS_SYNC;
      if (MDB_MSYNC(env->me_map, env->me_psize * numpgs, flags))
        rc = ErrCode();
    } else {
      if (MDB_FDATASYNC(env->me_fd))
        rc = ErrCode();
    }
  }
  return rc;
}

int
mdb_env_sync(MDB_env *env, int force)
{
  MDB_meta *m = mdb_env_pick_meta(env);
  return mdb_env_sync0(env, force, m->mm_last_pg+1);
}

enum Pidlock_op {
  Pidset = F_SETLK, Pidcheck = F_GETLK
};

/** Common code for #mdb_txn_begin() and #mdb_txn_renew().
 * @param[in] txn the transaction handle to initialize
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_txn_renew0(MDB_txn *txn)
{
  MDB_env *env = txn->mt_env;
  MDB_txninfo *ti = env->me_txns;
  MDB_meta *meta;
  unsigned int i, nr, flags = txn->mt_flags;
  uint16_t x;
  int rc, new_notls = 0;

  if ((flags &= MDB_TXN_RDONLY) != 0) {
    if (!ti) {
      meta = mdb_env_pick_meta(env);
      txn->mt_txnid = meta->mm_txnid;
      txn->mt_u.reader = NULL;
    } else {
      MDB_reader *r = (env->me_flags & MDB_NOTLS) ? txn->mt_u.reader :
        pthread_getspecific(env->me_txkey);
      if (r) {
        if (r->mr_pid != env->me_pid || r->mr_txnid != (txnid_t)-1)
          return MDB_BAD_RSLOT;
      } else {
        MDB_PID_T pid = env->me_pid;
        MDB_THR_T tid = pthread_self();
        mdb_mutexref_t rmutex = env->me_rmutex;

        if (!env->me_live_reader) {
          rc = mdb_reader_pid(env, Pidset, pid);
          if (rc)
            return rc;
          env->me_live_reader = 1;
        }

        if (LOCK_MUTEX(rc, env, rmutex))
          return rc;
        nr = ti->mti_numreaders;
        for (i=0; i<nr; i++)
          if (ti->mti_readers[i].mr_pid == 0)
            break;
        r = &ti->mti_readers[i];
        /* Claim the reader slot, carefully since other code
         * uses the reader table un-mutexed: First reset the
         * slot, next publish it in mti_numreaders.  After
         * that, it is safe for mdb_env_close() to touch it.
         * When it will be closed, we can finally claim it.
         */
        r->mr_pid = 0;
        r->mr_txnid = (txnid_t)-1;
        r->mr_tid = tid;
        if (i == nr)
          ti->mti_numreaders = ++nr;
        env->me_close_readers = nr;
        r->mr_pid = pid;
        UNLOCK_MUTEX(rmutex);

        new_notls = (env->me_flags & MDB_NOTLS);
        if (!new_notls && (rc=pthread_setspecific(env->me_txkey, r))) {
          r->mr_pid = 0;
          return rc;
        }
      }
      do /* LY: Retry on a race, ITS#7970. */
        r->mr_txnid = ti->mti_txnid;
      while(r->mr_txnid != ti->mti_txnid);
      txn->mt_txnid = r->mr_txnid;
      txn->mt_u.reader = r;
      meta = env->me_metas[txn->mt_txnid & 1];
    }

  } else {
    /* Not yet touching txn == env->me_txn0, it may be active */
    if (ti) {
      if (LOCK_MUTEX(rc, env, env->me_wmutex))
        return rc;
      txn->mt_txnid = ti->mti_txnid;
      meta = env->me_metas[txn->mt_txnid & 1];
    } else {
      meta = mdb_env_pick_meta(env);
      txn->mt_txnid = meta->mm_txnid;
    }
    txn->mt_txnid++;
    txn->mt_child = NULL;
    txn->mt_loose_pgs = NULL;
    txn->mt_loose_count = 0;
    txn->mt_dirty_room = MDB_IDL_UM_MAX;
    txn->mt_u.dirty_list = env->me_dirty_list;
    txn->mt_u.dirty_list[0].mid = 0;
    txn->mt_free_pgs = env->me_free_pgs;
    txn->mt_free_pgs[0] = 0;
    txn->mt_spill_pgs = NULL;
    env->me_txn = txn;
    memcpy(txn->mt_dbiseqs, env->me_dbiseqs, env->me_maxdbs * sizeof(unsigned int));
  }

  /* Copy the DB info and flags */
  memcpy(txn->mt_dbs, meta->mm_dbs, CORE_DBS * sizeof(MDB_db));

  /* Moved to here to avoid a data race in read TXNs */
  txn->mt_next_pgno = meta->mm_last_pg+1;
  txn->mt_flags = flags;

  /* Setup db info */
  txn->mt_numdbs = env->me_numdbs;
  for (i=CORE_DBS; i<txn->mt_numdbs; i++) {
    x = env->me_dbflags[i];
    txn->mt_dbs[i].md_flags = x & PERSISTENT_FLAGS;
    txn->mt_dbflags[i] = (x & MDB_VALID) ? DB_VALID|DB_USRVALID|DB_STALE : 0;
  }
  txn->mt_dbflags[MAIN_DBI] = DB_VALID|DB_USRVALID;
  txn->mt_dbflags[FREE_DBI] = DB_VALID;

  if (env->me_flags & MDB_FATAL_ERROR) {
    DPUTS("environment had fatal error, must shutdown!");
    rc = MDB_PANIC;
  } else if (env->me_maxpg < txn->mt_next_pgno) {
    rc = MDB_MAP_RESIZED;
  } else {
    return MDB_SUCCESS;
  }
  mdb_txn_end(txn, new_notls /*0 or MDB_END_SLOT*/ | MDB_END_FAIL_BEGIN);
  return rc;
}

/** Export or close DBI handles opened in this txn. */
static void
mdb_dbis_update(MDB_txn *txn, int keep)
{
  int i;
  MDB_dbi n = txn->mt_numdbs;
  MDB_env *env = txn->mt_env;
  unsigned char *tdbflags = txn->mt_dbflags;

  for (i = n; --i >= CORE_DBS;) {
    if (tdbflags[i] & DB_NEW) {
      if (keep) {
        env->me_dbflags[i] = txn->mt_dbs[i].md_flags | MDB_VALID;
      } else {
        char *ptr = env->me_dbxs[i].md_name.mv_data;
        if (ptr) {
          env->me_dbxs[i].md_name.mv_data = NULL;
          env->me_dbxs[i].md_name.mv_size = 0;
          env->me_dbflags[i] = 0;
          env->me_dbiseqs[i]++;
          free(ptr);
        }
      }
    }
  }
  if (keep && env->me_numdbs < n)
    env->me_numdbs = n;
}

/** End a transaction, except successful commit of a nested transaction.
 * May be called twice for readonly txns: First reset it, then abort.
 * @param[in] txn the transaction handle to end
 * @param[in] mode why and how to end the transaction
 */
static void
mdb_txn_end(MDB_txn *txn, unsigned mode)
{
  MDB_env  *env = txn->mt_env;

  /* Export or close DBI handles opened in this txn */
  mdb_dbis_update(txn, mode & MDB_END_UPDATE);

  DPRINTF(("%s txn %"Yu"%c %p on mdbenv %p, root page %"Yu,
    names[mode & MDB_END_OPMASK],
    txn->mt_txnid, (txn->mt_flags & MDB_TXN_RDONLY) ? 'r' : 'w',
    (void *) txn, (void *)env, txn->mt_dbs[MAIN_DBI].md_root));

  if (F_ISSET(txn->mt_flags, MDB_TXN_RDONLY)) {
    if (txn->mt_u.reader) {
      txn->mt_u.reader->mr_txnid = (txnid_t)-1;
      if (!(env->me_flags & MDB_NOTLS)) {
        txn->mt_u.reader = NULL; /* txn does not own reader */
      } else if (mode & MDB_END_SLOT) {
        txn->mt_u.reader->mr_pid = 0;
        txn->mt_u.reader = NULL;
      } /* else txn owns the slot until it does MDB_END_SLOT */
    }
    txn->mt_numdbs = 0;    /* prevent further DBI activity */
    txn->mt_flags |= MDB_TXN_FINISHED;

  } else if (!F_ISSET(txn->mt_flags, MDB_TXN_FINISHED)) {
    pgno_t *pghead = env->me_pghead;

    if (!(mode & MDB_END_UPDATE)) /* !(already closed cursors) */
      mdb_cursors_close(txn, 0);
    if (!(env->me_flags & MDB_WRITEMAP)) {
      mdb_dlist_free(txn);
    }

    txn->mt_numdbs = 0;
    txn->mt_flags = MDB_TXN_FINISHED;

    if (!txn->mt_parent) {
      mdb_midl_shrink(&txn->mt_free_pgs);
      env->me_free_pgs = txn->mt_free_pgs;
      /* me_pgstate: */
      env->me_pghead = NULL;
      env->me_pglast = 0;

      env->me_txn = NULL;
      mode = 0;  /* txn == env->me_txn0, do not free() it */

      /* The writer mutex was locked in mdb_txn_begin. */
      if (env->me_txns)
        UNLOCK_MUTEX(env->me_wmutex);
    } else {
      txn->mt_parent->mt_child = NULL;
      txn->mt_parent->mt_flags &= ~MDB_TXN_HAS_CHILD;
      env->me_pgstate = ((MDB_ntxn *)txn)->mnt_pgstate;
      mdb_midl_free(txn->mt_free_pgs);
      free(txn->mt_u.dirty_list);
    }
    mdb_midl_free(txn->mt_spill_pgs);

    mdb_midl_free(pghead);
  }
  if (mode & MDB_END_FREE)
    free(txn);
}

void
mdb_txn_reset(MDB_txn *txn)
{
  if (txn == NULL)
    return;

  /* This call is only valid for read-only txns */
  if (!(txn->mt_flags & MDB_TXN_RDONLY))
    return;

  mdb_txn_end(txn, MDB_END_RESET);
}

/** Save the freelist as of this transaction to the freeDB.
 * This changes the freelist. Keep trying until it stabilizes.
 *
 * When (MDB_DEVEL) & 2, the changes do not affect #mdb_page_alloc(),
 * it then uses the transaction's original snapshot of the freeDB.
 */
static int
mdb_freelist_save(MDB_txn *txn)
{
  /* env->me_pghead[] can grow and shrink during this call.
   * env->me_pglast and txn->mt_free_pgs[] can only grow.
   * Page numbers cannot disappear from txn->mt_free_pgs[].
   */
  MDB_cursor mc;
  MDB_env  *env = txn->mt_env;
  int rc, maxfree_1pg = env->me_maxfree_1pg, more = 1;
  txnid_t  pglast = 0, head_id = 0;
  pgno_t  freecnt = 0, *free_pgs, *mop;
  ssize_t  head_room = 0, total_room = 0, mop_len, clean_limit;

  mdb_cursor_init(&mc, txn, FREE_DBI, NULL);

  if (env->me_pghead) {
    /* Make sure first page of freeDB is touched and on freelist */
    rc = mdb_page_search(&mc, NULL, MDB_PS_FIRST|MDB_PS_MODIFY);
    if (rc && rc != MDB_NOTFOUND)
      return rc;
  }

  if (!env->me_pghead && txn->mt_loose_pgs) {
    /* Put loose page numbers in mt_free_pgs, since
     * we may be unable to return them to me_pghead.
     */
    MDB_page *mp = txn->mt_loose_pgs;
    MDB_ID2 *dl = txn->mt_u.dirty_list;
    unsigned x;
    if ((rc = mdb_midl_need(&txn->mt_free_pgs, txn->mt_loose_count)) != 0)
      return rc;
    for (; mp; mp = NEXT_LOOSE_PAGE(mp)) {
      mdb_midl_xappend(txn->mt_free_pgs, mp->mp_pgno);
      /* must also remove from dirty list */
      if (txn->mt_flags & MDB_TXN_WRITEMAP) {
        for (x=1; x<=dl[0].mid; x++)
          if (dl[x].mid == mp->mp_pgno)
            break;
        mdb_tassert(txn, x <= dl[0].mid);
      } else {
        x = mdb_mid2l_search(dl, mp->mp_pgno);
        mdb_tassert(txn, dl[x].mid == mp->mp_pgno);
        mdb_dpage_free(env, mp);
      }
      dl[x].mptr = NULL;
    }
    {
      /* squash freed slots out of the dirty list */
      unsigned y;
      for (y=1; dl[y].mptr && y <= dl[0].mid; y++);
      if (y <= dl[0].mid) {
        for(x=y, y++;;) {
          while (!dl[y].mptr && y <= dl[0].mid) y++;
          if (y > dl[0].mid) break;
          dl[x++] = dl[y++];
        }
        dl[0].mid = x-1;
      } else {
        /* all slots freed */
        dl[0].mid = 0;
      }
    }
    txn->mt_loose_pgs = NULL;
    txn->mt_loose_count = 0;
  }

  /* MDB_RESERVE cancels meminit in ovpage malloc (when no WRITEMAP) */
  clean_limit = (env->me_flags & (MDB_NOMEMINIT|MDB_WRITEMAP))
    ? SSIZE_MAX : maxfree_1pg;

  for (;;) {
    /* Come back here after each Put() in case freelist changed */
    MDB_val key, data;
    pgno_t *pgs;
    ssize_t j;

    /* If using records from freeDB which we have not yet
     * deleted, delete them and any we reserved for me_pghead.
     */
    while (pglast < env->me_pglast) {
      rc = mdb_cursor_first(&mc, &key, NULL);
      if (rc)
        return rc;
      pglast = head_id = *(txnid_t *)key.mv_data;
      total_room = head_room = 0;
      mdb_tassert(txn, pglast <= env->me_pglast);
      rc = mdb_cursor_del(&mc, 0);
      if (rc)
        return rc;
    }

    /* Save the IDL of pages freed by this txn, to a single record */
    if (freecnt < txn->mt_free_pgs[0]) {
      if (!freecnt) {
        /* Make sure last page of freeDB is touched and on freelist */
        rc = mdb_page_search(&mc, NULL, MDB_PS_LAST|MDB_PS_MODIFY);
        if (rc && rc != MDB_NOTFOUND)
          return rc;
      }
      free_pgs = txn->mt_free_pgs;
      /* Write to last page of freeDB */
      key.mv_size = sizeof(txn->mt_txnid);
      key.mv_data = &txn->mt_txnid;
      do {
        freecnt = free_pgs[0];
        data.mv_size = MDB_IDL_SIZEOF(free_pgs);
        rc = mdb_cursor_put(&mc, &key, &data, MDB_RESERVE);
        if (rc)
          return rc;
        /* Retry if mt_free_pgs[] grew during the Put() */
        free_pgs = txn->mt_free_pgs;
      } while (freecnt < free_pgs[0]);
      mdb_midl_sort(free_pgs);
      memcpy(data.mv_data, free_pgs, data.mv_size);
      continue;
    }

    mop = env->me_pghead;
    mop_len = (mop ? mop[0] : 0) + txn->mt_loose_count;

    /* Reserve records for me_pghead[]. Split it if multi-page,
     * to avoid searching freeDB for a page range. Use keys in
     * range [1,me_pglast]: Smaller than txnid of oldest reader.
     */
    if (total_room >= mop_len) {
      if (total_room == mop_len || --more < 0)
        break;
    } else if (head_room >= maxfree_1pg && head_id > 1) {
      /* Keep current record (overflow page), add a new one */
      head_id--;
      head_room = 0;
    }
    /* (Re)write {key = head_id, IDL length = head_room} */
    total_room -= head_room;
    head_room = mop_len - total_room;
    if (head_room > maxfree_1pg && head_id > 1) {
      /* Overflow multi-page for part of me_pghead */
      head_room /= head_id; /* amortize page sizes */
      head_room += maxfree_1pg - head_room % (maxfree_1pg + 1);
    } else if (head_room < 0) {
      /* Rare case, not bothering to delete this record */
      head_room = 0;
    }
    key.mv_size = sizeof(head_id);
    key.mv_data = &head_id;
    data.mv_size = (head_room + 1) * sizeof(pgno_t);
    rc = mdb_cursor_put(&mc, &key, &data, MDB_RESERVE);
    if (rc)
      return rc;
    /* IDL is initially empty, zero out at least the length */
    pgs = (pgno_t *)data.mv_data;
    j = head_room > clean_limit ? head_room : 0;
    do {
      pgs[j] = 0;
    } while (--j >= 0);
    total_room += head_room;
  }

  /* Return loose page numbers to me_pghead, though usually none are
   * left at this point.  The pages themselves remain in dirty_list.
   */
  if (txn->mt_loose_pgs) {
    MDB_page *mp = txn->mt_loose_pgs;
    unsigned count = txn->mt_loose_count;
    MDB_IDL loose;
    /* Room for loose pages + temp IDL with same */
    if ((rc = mdb_midl_need(&env->me_pghead, 2*count+1)) != 0)
      return rc;
    mop = env->me_pghead;
    loose = mop + MDB_IDL_ALLOCLEN(mop) - count;
    for (count = 0; mp; mp = NEXT_LOOSE_PAGE(mp))
      loose[ ++count ] = mp->mp_pgno;
    loose[0] = count;
    mdb_midl_sort(loose);
    mdb_midl_xmerge(mop, loose);
    txn->mt_loose_pgs = NULL;
    txn->mt_loose_count = 0;
    mop_len = mop[0];
  }

  /* Fill in the reserved me_pghead records */
  rc = MDB_SUCCESS;
  if (mop_len) {
    MDB_val key, data;

    mop += mop_len;
    rc = mdb_cursor_first(&mc, &key, &data);
    for (; !rc; rc = mdb_cursor_next(&mc, &key, &data, MDB_NEXT)) {
      txnid_t id = *(txnid_t *)key.mv_data;
      ssize_t  len = (ssize_t)(data.mv_size / sizeof(MDB_ID)) - 1;
      MDB_ID save;

      mdb_tassert(txn, len >= 0 && id <= env->me_pglast);
      key.mv_data = &id;
      if (len > mop_len) {
        len = mop_len;
        data.mv_size = (len + 1) * sizeof(MDB_ID);
      }
      data.mv_data = mop -= len;
      save = mop[0];
      mop[0] = len;
      rc = mdb_cursor_put(&mc, &key, &data, MDB_CURRENT);
      mop[0] = save;
      if (rc || !(mop_len -= len))
        break;
    }
  }
  return rc;
}

/** Flush (some) dirty pages to the map, after clearing their dirty flag.
 * @param[in] txn the transaction that's being committed
 * @param[in] keep number of initial pages in dirty_list to keep dirty.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_flush(MDB_txn *txn, int keep)
{
  MDB_env    *env = txn->mt_env;
  MDB_ID2L  dl = txn->mt_u.dirty_list;
  unsigned  psize = env->me_psize, j;
  int      i, pagecount = dl[0].mid, rc;
  size_t    size = 0;
  MDB_OFF_T  pos = 0;
  pgno_t    pgno = 0;
  MDB_page  *dp = NULL;
  struct iovec iov[MDB_COMMIT_PAGES];
  HANDLE fd = env->me_fd;
  ssize_t    wsize = 0, wres;
  MDB_OFF_T  wpos = 0, next_pos = 1; /* impossible pos, so pos != next_pos */
  int      n = 0;

  j = i = keep;
  if (env->me_flags & MDB_WRITEMAP
    ) {
    /* Clear dirty flags */
    while (++i <= pagecount) {
      dp = dl[i].mptr;
      /* Don't flush this page yet */
      if (dp->mp_flags & (P_LOOSE|P_KEEP)) {
        dp->mp_flags &= ~P_KEEP;
        dl[++j] = dl[i];
        continue;
      }
      dp->mp_flags &= ~P_DIRTY;
    }
    goto done;
  }

  /* Write the pages */
  for (;;) {
    if (++i <= pagecount) {
      dp = dl[i].mptr;
      /* Don't flush this page yet */
      if (dp->mp_flags & (P_LOOSE|P_KEEP)) {
        dp->mp_flags &= ~P_KEEP;
        dl[i].mid = 0;
        continue;
      }
      pgno = dl[i].mid;
      /* clear dirty flag */
      dp->mp_flags &= ~P_DIRTY;
      pos = pgno * psize;
      size = psize;
      if (IS_OVERFLOW(dp)) size *= dp->mp_pages;
    }
    /* Write up to MDB_COMMIT_PAGES dirty pages at a time. */
    if (pos!=next_pos || n==MDB_COMMIT_PAGES || wsize+size>MAX_WRITE
      ) {
      if (n) {
retry_write:
        /* Write previous page(s) */
        DPRINTF(("committing page %"Z"u", pgno));
#ifdef MDB_USE_PWRITEV
        wres = pwritev(fd, iov, n, wpos);
#else
        if (n == 1) {
          wres = pwrite(fd, iov[0].iov_base, wsize, wpos);
        } else {
retry_seek:
          if (lseek(fd, wpos, SEEK_SET) == -1) {
            rc = ErrCode();
            if (rc == EINTR)
              goto retry_seek;
            DPRINTF(("lseek: %s", strerror(rc)));
            return rc;
          }
          wres = writev(fd, iov, n);
        }
#endif
        if (wres != wsize) {
          if (wres < 0) {
            rc = ErrCode();
            if (rc == EINTR)
              goto retry_write;
            DPRINTF(("Write error: %s", strerror(rc)));
          } else {
            rc = EIO; /* TODO: Use which error code? */
            DPUTS("short write, filesystem full?");
          }
          return rc;
        }
        n = 0;
      }
      if (i > pagecount)
        break;
      wpos = pos;
      wsize = 0;
    }
    iov[n].iov_len = size;
    iov[n].iov_base = (char *)dp;
    DPRINTF(("committing page %"Yu, pgno));
    next_pos = pos + size;
    wsize += size;
    n++;
  }

  if (!(env->me_flags & MDB_WRITEMAP)) {
    for (i = keep; ++i <= pagecount; ) {
      dp = dl[i].mptr;
      /* This is a page we skipped above */
      if (!dl[i].mid) {
        dl[++j] = dl[i];
        dl[j].mid = dp->mp_pgno;
        continue;
      }
      mdb_dpage_free(env, dp);
    }
  }

done:
  i--;
  txn->mt_dirty_room += i - j;
  dl[0].mid = j;
  return MDB_SUCCESS;
}

static int ESECT mdb_env_share_locks(MDB_env *env, int *excl);

int
mdb_txn_commit(MDB_txn *txn)
{
  int    rc;
  unsigned int i, end_mode;
  MDB_env  *env;

  if (txn == NULL)
    return EINVAL;

  /* mdb_txn_end() mode for a commit which writes nothing */
  end_mode = MDB_END_EMPTY_COMMIT|MDB_END_UPDATE|MDB_END_SLOT|MDB_END_FREE;

  if (txn->mt_child) {
    rc = mdb_txn_commit(txn->mt_child);
    if (rc)
      goto fail;
  }

  env = txn->mt_env;

  if (F_ISSET(txn->mt_flags, MDB_TXN_RDONLY)) {
    goto done;
  }

  if (txn->mt_flags & (MDB_TXN_FINISHED|MDB_TXN_ERROR)) {
    DPUTS("txn has failed/finished, can't commit");
    if (txn->mt_parent)
      txn->mt_parent->mt_flags |= MDB_TXN_ERROR;
    rc = MDB_BAD_TXN;
    goto fail;
  }

  if (txn->mt_parent) {
    MDB_txn *parent = txn->mt_parent;
    MDB_page **lp;
    MDB_ID2L dst, src;
    MDB_IDL pspill;
    unsigned x, y, len, ps_len;

    /* Append our free list to parent's */
    rc = mdb_midl_append_list(&parent->mt_free_pgs, txn->mt_free_pgs);
    if (rc)
      goto fail;
    mdb_midl_free(txn->mt_free_pgs);
    /* Failures after this must either undo the changes
     * to the parent or set MDB_TXN_ERROR in the parent.
     */

    parent->mt_next_pgno = txn->mt_next_pgno;
    parent->mt_flags = txn->mt_flags;

    /* Merge our cursors into parent's and close them */
    mdb_cursors_close(txn, 1);

    /* Update parent's DB table. */
    memcpy(parent->mt_dbs, txn->mt_dbs, txn->mt_numdbs * sizeof(MDB_db));
    parent->mt_numdbs = txn->mt_numdbs;
    parent->mt_dbflags[FREE_DBI] = txn->mt_dbflags[FREE_DBI];
    parent->mt_dbflags[MAIN_DBI] = txn->mt_dbflags[MAIN_DBI];
    for (i=CORE_DBS; i<txn->mt_numdbs; i++) {
      /* preserve parent's DB_NEW status */
      x = parent->mt_dbflags[i] & DB_NEW;
      parent->mt_dbflags[i] = txn->mt_dbflags[i] | x;
    }

    dst = parent->mt_u.dirty_list;
    src = txn->mt_u.dirty_list;
    /* Remove anything in our dirty list from parent's spill list */
    if ((pspill = parent->mt_spill_pgs) && (ps_len = pspill[0])) {
      x = y = ps_len;
      pspill[0] = (pgno_t)-1;
      /* Mark our dirty pages as deleted in parent spill list */
      for (i=0, len=src[0].mid; ++i <= len; ) {
        MDB_ID pn = src[i].mid << 1;
        while (pn > pspill[x])
          x--;
        if (pn == pspill[x]) {
          pspill[x] = 1;
          y = --x;
        }
      }
      /* Squash deleted pagenums if we deleted any */
      for (x=y; ++x <= ps_len; )
        if (!(pspill[x] & 1))
          pspill[++y] = pspill[x];
      pspill[0] = y;
    }

    /* Remove anything in our spill list from parent's dirty list */
    if (txn->mt_spill_pgs && txn->mt_spill_pgs[0]) {
      for (i=1; i<=txn->mt_spill_pgs[0]; i++) {
        MDB_ID pn = txn->mt_spill_pgs[i];
        if (pn & 1)
          continue;  /* deleted spillpg */
        pn >>= 1;
        y = mdb_mid2l_search(dst, pn);
        if (y <= dst[0].mid && dst[y].mid == pn) {
          free(dst[y].mptr);
          while (y < dst[0].mid) {
            dst[y] = dst[y+1];
            y++;
          }
          dst[0].mid--;
        }
      }
    }

    /* Find len = length of merging our dirty list with parent's */
    x = dst[0].mid;
    dst[0].mid = 0;    /* simplify loops */
    if (parent->mt_parent) {
      len = x + src[0].mid;
      y = mdb_mid2l_search(src, dst[x].mid + 1) - 1;
      for (i = x; y && i; y--) {
        pgno_t yp = src[y].mid;
        while (yp < dst[i].mid)
          i--;
        if (yp == dst[i].mid) {
          i--;
          len--;
        }
      }
    } else { /* Simplify the above for single-ancestor case */
      len = MDB_IDL_UM_MAX - txn->mt_dirty_room;
    }
    /* Merge our dirty list with parent's */
    y = src[0].mid;
    for (i = len; y; dst[i--] = src[y--]) {
      pgno_t yp = src[y].mid;
      while (yp < dst[x].mid)
        dst[i--] = dst[x--];
      if (yp == dst[x].mid)
        free(dst[x--].mptr);
    }
    mdb_tassert(txn, i == x);
    dst[0].mid = len;
    free(txn->mt_u.dirty_list);
    parent->mt_dirty_room = txn->mt_dirty_room;
    if (txn->mt_spill_pgs) {
      if (parent->mt_spill_pgs) {
        /* TODO: Prevent failure here, so parent does not fail */
        rc = mdb_midl_append_list(&parent->mt_spill_pgs, txn->mt_spill_pgs);
        if (rc)
          parent->mt_flags |= MDB_TXN_ERROR;
        mdb_midl_free(txn->mt_spill_pgs);
        mdb_midl_sort(parent->mt_spill_pgs);
      } else {
        parent->mt_spill_pgs = txn->mt_spill_pgs;
      }
    }

    /* Append our loose page list to parent's */
    for (lp = &parent->mt_loose_pgs; *lp; lp = &NEXT_LOOSE_PAGE(*lp))
      ;
    *lp = txn->mt_loose_pgs;
    parent->mt_loose_count += txn->mt_loose_count;

    parent->mt_child = NULL;
    mdb_midl_free(((MDB_ntxn *)txn)->mnt_pgstate.mf_pghead);
    free(txn);
    return rc;
  }

  if (txn != env->me_txn) {
    DPUTS("attempt to commit unknown transaction");
    rc = EINVAL;
    goto fail;
  }

  mdb_cursors_close(txn, 0);

  if (!txn->mt_u.dirty_list[0].mid &&
    !(txn->mt_flags & (MDB_TXN_DIRTY|MDB_TXN_SPILLS)))
    goto done;

  DPRINTF(("committing txn %"Yu" %p on mdbenv %p, root page %"Yu,
      txn->mt_txnid, (void*)txn, (void*)env, txn->mt_dbs[MAIN_DBI].md_root));

  /* Update DB root pointers */
  if (txn->mt_numdbs > CORE_DBS) {
    MDB_cursor mc;
    MDB_dbi i;
    MDB_val data;
    data.mv_size = sizeof(MDB_db);

    mdb_cursor_init(&mc, txn, MAIN_DBI, NULL);
    for (i = CORE_DBS; i < txn->mt_numdbs; i++) {
      if (txn->mt_dbflags[i] & DB_DIRTY) {
        if (TXN_DBI_CHANGED(txn, i)) {
          rc = MDB_BAD_DBI;
          goto fail;
        }
        data.mv_data = &txn->mt_dbs[i];
        rc = mdb_cursor_put(&mc, &txn->mt_dbxs[i].md_name, &data,
          F_SUBDATA);
        if (rc)
          goto fail;
      }
    }
  }

  rc = mdb_freelist_save(txn);
  if (rc)
    goto fail;

  mdb_midl_free(env->me_pghead);
  env->me_pghead = NULL;
  mdb_midl_shrink(&txn->mt_free_pgs);

  if ((rc = mdb_page_flush(txn, 0)))
    goto fail;
  if (!F_ISSET(txn->mt_flags, MDB_TXN_NOSYNC) &&
    (rc = mdb_env_sync0(env, 0, txn->mt_next_pgno)))
    goto fail;
  if ((rc = mdb_env_write_meta(txn)))
    goto fail;
  end_mode = MDB_END_COMMITTED|MDB_END_UPDATE;
  if (env->me_flags & MDB_PREVSNAPSHOT) {
    if (!(env->me_flags & MDB_NOLOCK)) {
      int excl;
      rc = mdb_env_share_locks(env, &excl);
      if (rc)
        goto fail;
    }
    env->me_flags ^= MDB_PREVSNAPSHOT;
  }

done:
  mdb_txn_end(txn, end_mode);
  return MDB_SUCCESS;

fail:
  mdb_txn_abort(txn);
  return rc;
}

/** Read the environment parameters of a DB environment before
 * mapping it into memory.
 * @param[in] env the environment handle
 * @param[in] prev whether to read the backup meta page
 * @param[out] meta address of where to store the meta information
 * @return 0 on success, non-zero on failure.
 */
static int ESECT
mdb_env_read_header(MDB_env *env, int prev, MDB_meta *meta)
{
  MDB_metabuf  pbuf;
  MDB_page  *p;
  MDB_meta  *m;
  int      i, rc, off;
  enum { Size = sizeof(pbuf) };

  /* We don't know the page size yet, so use a minimum value.
   * Read both meta pages so we can use the latest one.
   */

  for (i=off=0; i<NUM_METAS; i++, off += meta->mm_psize) {
    rc = pread(env->me_fd, &pbuf, Size, off);
    if (rc != Size) {
      if (rc == 0 && off == 0)
        return ENOENT;
      rc = rc < 0 ? (int) ErrCode() : MDB_INVALID;
      DPRINTF(("read: %s", mdb_strerror(rc)));
      return rc;
    }

    p = (MDB_page *)&pbuf;

    if (!F_ISSET(p->mp_flags, P_META)) {
      DPRINTF(("page %"Yu" not a meta page", p->mp_pgno));
      return MDB_INVALID;
    }

    m = METADATA(p);
    if (m->mm_magic != MDB_MAGIC) {
      DPUTS("meta has invalid magic");
      return MDB_INVALID;
    }

    if (m->mm_version != MDB_DATA_VERSION) {
      DPRINTF(("database is version %u, expected version %u",
        m->mm_version, MDB_DATA_VERSION));
      return MDB_VERSION_MISMATCH;
    }

    if (off == 0 || (prev ? m->mm_txnid < meta->mm_txnid : m->mm_txnid > meta->mm_txnid))
      *meta = *m;
  }
  return 0;
}

/** Fill in most of the zeroed #MDB_meta for an empty database environment */
static void ESECT
mdb_env_init_meta0(MDB_env *env, MDB_meta *meta)
{
  meta->mm_magic = MDB_MAGIC;
  meta->mm_version = MDB_DATA_VERSION;
  meta->mm_mapsize = env->me_mapsize;
  meta->mm_psize = env->me_psize;
  meta->mm_last_pg = NUM_METAS-1;
  meta->mm_flags = env->me_flags & 0xffff;
  meta->mm_flags |= MDB_INTEGERKEY; /* this is mm_dbs[FREE_DBI].md_flags */
  meta->mm_dbs[FREE_DBI].md_root = P_INVALID;
  meta->mm_dbs[MAIN_DBI].md_root = P_INVALID;
}

/** Write the environment parameters of a freshly created DB environment.
 * @param[in] env the environment handle
 * @param[in] meta the #MDB_meta to write
 * @return 0 on success, non-zero on failure.
 */
static int ESECT
mdb_env_init_meta(MDB_env *env, MDB_meta *meta)
{
  MDB_page *p, *q;
  int rc;
  unsigned int   psize;
  int len;
#define DO_PWRITE(rc, fd, ptr, size, len, pos)  do { \
  len = pwrite(fd, ptr, size, pos);  \
  if (len == -1 && ErrCode() == EINTR) continue; \
  rc = (len >= 0); break; } while(1)
  DPUTS("writing new meta page");

  psize = env->me_psize;

  p = calloc(NUM_METAS, psize);
  if (!p)
    return ENOMEM;
  p->mp_pgno = 0;
  p->mp_flags = P_META;
  *(MDB_meta *)METADATA(p) = *meta;

  q = (MDB_page *)((char *)p + psize);
  q->mp_pgno = 1;
  q->mp_flags = P_META;
  *(MDB_meta *)METADATA(q) = *meta;

  DO_PWRITE(rc, env->me_fd, p, psize * NUM_METAS, len, 0);
  if (!rc)
    rc = ErrCode();
  else if ((unsigned) len == psize * NUM_METAS)
    rc = MDB_SUCCESS;
  else
    rc = ENOSPC;
  free(p);
  return rc;
}

/** Update the environment info to commit a transaction.
 * @param[in] txn the transaction that's being committed
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_env_write_meta(MDB_txn *txn)
{
  MDB_env *env;
  MDB_meta  meta, metab, *mp;
  unsigned flags;
  mdb_size_t mapsize;
  MDB_OFF_T off;
  int rc, len, toggle;
  char *ptr;
  HANDLE mfd;
  int r2;

  toggle = txn->mt_txnid & 1;
  DPRINTF(("writing meta page %d for root page %"Yu,
    toggle, txn->mt_dbs[MAIN_DBI].md_root));

  env = txn->mt_env;
  flags = txn->mt_flags | env->me_flags;
  mp = env->me_metas[toggle];
  mapsize = env->me_metas[toggle ^ 1]->mm_mapsize;
  /* Persist any increases of mapsize config */
  if (mapsize < env->me_mapsize)
    mapsize = env->me_mapsize;

  if (flags & MDB_WRITEMAP) {
    mp->mm_mapsize = mapsize;
    mp->mm_dbs[FREE_DBI] = txn->mt_dbs[FREE_DBI];
    mp->mm_dbs[MAIN_DBI] = txn->mt_dbs[MAIN_DBI];
    mp->mm_last_pg = txn->mt_next_pgno - 1;
#if (__GNUC__ * 100 + __GNUC_MINOR__ >= 404) && /* TODO: portability */  \
  !(defined(__i386__) || defined(__x86_64__))
    /* LY: issue a memory barrier, if not x86. ITS#7969 */
    __sync_synchronize();
#endif
    mp->mm_txnid = txn->mt_txnid;
    if (!(flags & (MDB_NOMETASYNC|MDB_NOSYNC))) {
      unsigned meta_size = env->me_psize;
      rc = (env->me_flags & MDB_MAPASYNC) ? MS_ASYNC : MS_SYNC;
      ptr = (char *)mp - PAGEHDRSZ;
      /* POSIX msync() requires ptr = start of OS page */
      r2 = (ptr - env->me_map) & (env->me_os_psize - 1);
      ptr -= r2;
      meta_size += r2;
      if (MDB_MSYNC(ptr, meta_size, rc)) {
        rc = ErrCode();
        goto fail;
      }
    }
    goto done;
  }
  metab.mm_txnid = mp->mm_txnid;
  metab.mm_last_pg = mp->mm_last_pg;

  meta.mm_mapsize = mapsize;
  meta.mm_dbs[FREE_DBI] = txn->mt_dbs[FREE_DBI];
  meta.mm_dbs[MAIN_DBI] = txn->mt_dbs[MAIN_DBI];
  meta.mm_last_pg = txn->mt_next_pgno - 1;
  meta.mm_txnid = txn->mt_txnid;

  off = offsetof(MDB_meta, mm_mapsize);
  ptr = (char *)&meta + off;
  len = sizeof(MDB_meta) - off;
  off += (char *)mp - env->me_map;

  /* Write to the SYNC fd unless MDB_NOSYNC/MDB_NOMETASYNC.
   * (me_mfd goes to the same file as me_fd, but writing to it
   * also syncs to disk.  Avoids a separate fdatasync() call.)
   */
  mfd = (flags & (MDB_NOSYNC|MDB_NOMETASYNC)) ? env->me_fd : env->me_mfd;
retry_write:
  rc = pwrite(mfd, ptr, len, off);
  if (rc != len) {
    rc = rc < 0 ? ErrCode() : EIO;
    if (rc == EINTR)
      goto retry_write;
    DPUTS("write failed, disk error?");
    /* On a failure, the pagecache still contains the new data.
     * Write some old data back, to prevent it from being used.
     * Use the non-SYNC fd; we know it will fail anyway.
     */
    meta.mm_last_pg = metab.mm_last_pg;
    meta.mm_txnid = metab.mm_txnid;
    r2 = pwrite(env->me_fd, ptr, len, off);
    (void)r2;  /* Silence warnings. We don't care about pwrite's return value */
fail:
    env->me_flags |= MDB_FATAL_ERROR;
    return rc;
  }
done:
  /* Memory ordering issues are irrelevant; since the entire writer
   * is wrapped by wmutex, all of these changes will become visible
   * after the wmutex is unlocked. Since the DB is multi-version,
   * readers will get consistent data regardless of how fresh or
   * how stale their view of these values is.
   */
  if (env->me_txns)
    env->me_txns->mti_txnid = txn->mt_txnid;

  return MDB_SUCCESS;
}

/** Check both meta pages to see which one is newer.
 * @param[in] env the environment handle
 * @return newest #MDB_meta.
 */
static MDB_meta *
mdb_env_pick_meta(const MDB_env *env)
{
  MDB_meta *const *metas = env->me_metas;
  return metas[ (metas[0]->mm_txnid < metas[1]->mm_txnid) ^
    ((env->me_flags & MDB_PREVSNAPSHOT) != 0) ];
}

int ESECT
mdb_env_create(MDB_env **env)
{
  MDB_env *e;

  e = calloc(1, sizeof(MDB_env));
  if (!e)
    return ENOMEM;

  e->me_maxdbs = e->me_numdbs = CORE_DBS;
  e->me_fd = INVALID_HANDLE_VALUE;
  e->me_lfd = INVALID_HANDLE_VALUE;
  e->me_mfd = INVALID_HANDLE_VALUE;
  e->me_pid = getpid();
  GET_PAGESIZE(e->me_os_psize);
  *env = e;
  return MDB_SUCCESS;
}

static int ESECT
mdb_env_map(MDB_env *env, void *addr)
{
  MDB_page *p;
  unsigned int flags = env->me_flags;
  int mmap_flags = MAP_SHARED;
  int prot = PROT_READ;
  if (flags & MDB_WRITEMAP) {
    prot |= PROT_WRITE;
    if (ftruncate(env->me_fd, env->me_mapsize) < 0)
      return ErrCode();
  }
  env->me_map = mmap(addr, env->me_mapsize, prot, mmap_flags,
    env->me_fd, 0);
  if (env->me_map == MAP_FAILED) {
    env->me_map = NULL;
    return ErrCode();
  }

  if (flags & MDB_NORDAHEAD) {
    /* Turn off readahead. It's harmful when the DB is larger than RAM. */
#ifdef MADV_RANDOM
    madvise(env->me_map, env->me_mapsize, MADV_RANDOM);
#else
# ifdef POSIX_MADV_RANDOM
    posix_madvise(env->me_map, env->me_mapsize, POSIX_MADV_RANDOM);
# endif /* POSIX_MADV_RANDOM */
#endif /* MADV_RANDOM */
  }

  /* Can happen because the address argument to mmap() is just a
   * hint.  mmap() can pick another, e.g. if the range is in use.
   * The MAP_FIXED flag would prevent that, but then mmap could
   * instead unmap existing pages to make room for the new map.
   */
  if (addr && env->me_map != addr)
    return EBUSY;  /* TODO: Make a new MDB_* error code? */

  p = (MDB_page *)env->me_map;
  env->me_metas[0] = METADATA(p);
  env->me_metas[1] = (MDB_meta *)((char *)env->me_metas[0] + env->me_psize);

  return MDB_SUCCESS;
}

int ESECT
mdb_env_set_mapsize(MDB_env *env, mdb_size_t size)
{
  /* If env is already open, caller is responsible for making
   * sure there are no active txns.
   */
  if (env->me_map) {
    MDB_meta *meta;
    void *old;
    int rc;
    if (env->me_txn)
      return EINVAL;
    meta = mdb_env_pick_meta(env);
    if (!size)
      size = meta->mm_mapsize;
    {
      /* Silently round up to minimum if the size is too small */
      mdb_size_t minsize = (meta->mm_last_pg + 1) * env->me_psize;
      if (size < minsize)
        size = minsize;
    }
    /* For MDB_VL32 this bit is a noop since we dynamically remap
     * chunks of the DB anyway.
     */
    munmap(env->me_map, env->me_mapsize);
    env->me_mapsize = size;
    old = (env->me_flags & MDB_FIXEDMAP) ? env->me_map : NULL;
    rc = mdb_env_map(env, old);
    if (rc)
      return rc;
  }
  env->me_mapsize = size;
  if (env->me_psize)
    env->me_maxpg = env->me_mapsize / env->me_psize;
  return MDB_SUCCESS;
}

static int ESECT
mdb_fsize(HANDLE fd, mdb_size_t *size)
{
  struct stat st;

  if (fstat(fd, &st))
    return ErrCode();

  *size = st.st_size;
  return MDB_SUCCESS;
}

/** Character type for file names: char on Unix, wchar_t on Windows */
typedef char  mdb_nchar_t;
#define MDB_NAME(str)  str    /**< #mdb_nchar_t[] string literal */
#define mdb_name_cpy  strcpy  /**< Copy name (#mdb_nchar_t string) */

/** Filename - string of #mdb_nchar_t[] */
typedef struct MDB_name {
  int mn_len;          /**< Length  */
  int mn_alloced;        /**< True if #mn_val was malloced */
  mdb_nchar_t  *mn_val;    /**< Contents */
} MDB_name;

/** Filename suffixes [datafile,lockfile][without,with MDB_NOSUBDIR] */
static const mdb_nchar_t *const mdb_suffixes[2][2] = {
  { MDB_NAME("/data.mdb"), MDB_NAME("")      },
  { MDB_NAME("/lock.mdb"), MDB_NAME("-lock") }
};

#define MDB_SUFFLEN 9  /**< Max string length in #mdb_suffixes[] */

/** Set up filename + scratch area for filename suffix, for opening files.
 * It should be freed with #mdb_fname_destroy().
 * On Windows, paths are converted from char *UTF-8 to wchar_t *UTF-16.
 *
 * @param[in] path Pathname for #mdb_env_open().
 * @param[in] envflags Whether a subdir and/or lockfile will be used.
 * @param[out] fname Resulting filename, with room for a suffix if necessary.
 */
static int ESECT
mdb_fname_init(const char *path, unsigned envflags, MDB_name *fname)
{
  int no_suffix = F_ISSET(envflags, MDB_NOSUBDIR|MDB_NOLOCK);
  fname->mn_alloced = 0;
  fname->mn_len = strlen(path);
  if (no_suffix)
    fname->mn_val = (char *) path;
  else if ((fname->mn_val = malloc(fname->mn_len + MDB_SUFFLEN+1)) != NULL) {
    fname->mn_alloced = 1;
    strcpy(fname->mn_val, path);
  }
  else
    return ENOMEM;
  return MDB_SUCCESS;
}

/** Destroy \b fname from #mdb_fname_init() */
#define mdb_fname_destroy(fname) \
  do { if ((fname).mn_alloced) free((fname).mn_val); } while (0)

#ifdef O_CLOEXEC /* POSIX.1-2008: Set FD_CLOEXEC atomically at open() */
# define MDB_CLOEXEC    O_CLOEXEC
#else
# define MDB_CLOEXEC    0
#endif

/** File type, access mode etc. for #mdb_fopen() */
enum mdb_fopen_type {
  /* A comment in mdb_fopen() explains some O_* flag choices. */
  MDB_O_RDONLY= O_RDONLY,                            /**< for RDONLY me_fd */
  MDB_O_RDWR  = O_RDWR  |O_CREAT,                    /**< for me_fd */
  MDB_O_META  = O_WRONLY|MDB_DSYNC     |MDB_CLOEXEC, /**< for me_mfd */
  MDB_O_COPY  = O_WRONLY|O_CREAT|O_EXCL|MDB_CLOEXEC, /**< for #mdb_env_copy() */
  /** Bitmask for open() flags in enum #mdb_fopen_type.  The other bits
   * distinguish otherwise-equal MDB_O_* constants from each other.
   */
  MDB_O_MASK  = MDB_O_RDWR|MDB_CLOEXEC | MDB_O_RDONLY|MDB_O_META|MDB_O_COPY,
  MDB_O_LOCKS = MDB_O_RDWR|MDB_CLOEXEC | ((MDB_O_MASK+1) & ~MDB_O_MASK) /**< for me_lfd */
};

/** Open an LMDB file.
 * @param[in] env  The LMDB environment.
 * @param[in,out] fname  Path from from #mdb_fname_init().  A suffix is
 * appended if necessary to create the filename, without changing mn_len.
 * @param[in] which  Determines file type, access mode, etc.
 * @param[in] mode  The Unix permissions for the file, if we create it.
 * @param[out] res  Resulting file handle.
 * @return 0 on success, non-zero on failure.
 */
static int ESECT
mdb_fopen(const MDB_env *env, MDB_name *fname,
  enum mdb_fopen_type which, mdb_mode_t mode,
  HANDLE *res)
{
  int rc = MDB_SUCCESS;
  HANDLE fd;
  int flags;

  if (fname->mn_alloced)    /* modifiable copy */
    mdb_name_cpy(fname->mn_val + fname->mn_len,
      mdb_suffixes[which==MDB_O_LOCKS][F_ISSET(env->me_flags, MDB_NOSUBDIR)]);

  /* The directory must already exist.  Usually the file need not.
   * MDB_O_META requires the file because we already created it using
   * MDB_O_RDWR.  MDB_O_COPY must not overwrite an existing file.
   *
   * With MDB_O_COPY we do not want the OS to cache the writes, since
   * the source data is already in the OS cache.
   *
   * The lockfile needs FD_CLOEXEC (close file descriptor on exec*())
   * to avoid the flock() issues noted under Caveats in lmdb.h.
   * Also set it for other filehandles which the user cannot get at
   * and close himself, which he may need after fork().  I.e. all but
   * me_fd, which programs do use via mdb_env_get_fd().
   */
  fd = open(fname->mn_val, which & MDB_O_MASK, mode);

  if (fd == INVALID_HANDLE_VALUE)
    rc = ErrCode();
  else {
    if (which != MDB_O_RDONLY && which != MDB_O_RDWR) {
      /* Set CLOEXEC if we could not pass it to open() */
      if (!MDB_CLOEXEC && (flags = fcntl(fd, F_GETFD)) != -1)
        (void) fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
    }
    if (which == MDB_O_COPY && env->me_psize >= env->me_os_psize) {
      /* This may require buffer alignment.  There is no portable
       * way to ask how much, so we require OS pagesize alignment.
       */
# ifdef F_NOCACHE  /* __APPLE__ */
      (void) fcntl(fd, F_NOCACHE, 1);
# elif defined O_DIRECT
      /* open(...O_DIRECT...) would break on filesystems without
       * O_DIRECT support (ITS#7682). Try to set it here instead.
       */
      if ((flags = fcntl(fd, F_GETFL)) != -1)
        (void) fcntl(fd, F_SETFL, flags | O_DIRECT);
# endif
    }
  }

  *res = fd;
  return rc;
}

/** Further setup required for opening an LMDB environment
 */
static int ESECT
mdb_env_open2(MDB_env *env, int prev)
{
  unsigned int flags = env->me_flags;
  int i, newenv = 0, rc;
  MDB_meta meta;

  if ((i = mdb_env_read_header(env, prev, &meta)) != 0) {
    if (i != ENOENT)
      return i;
    DPUTS("new mdbenv");
    newenv = 1;
    env->me_psize = env->me_os_psize;
    if (env->me_psize > MAX_PAGESIZE)
      env->me_psize = MAX_PAGESIZE;
    memset(&meta, 0, sizeof(meta));
    mdb_env_init_meta0(env, &meta);
    meta.mm_mapsize = DEFAULT_MAPSIZE;
  } else {
    env->me_psize = meta.mm_psize;
  }

  /* Was a mapsize configured? */
  if (!env->me_mapsize) {
    env->me_mapsize = meta.mm_mapsize;
  }
  {
    /* Make sure mapsize >= committed data size.  Even when using
     * mm_mapsize, which could be broken in old files (ITS#7789).
     */
    mdb_size_t minsize = (meta.mm_last_pg + 1) * meta.mm_psize;
    if (env->me_mapsize < minsize)
      env->me_mapsize = minsize;
  }
  meta.mm_mapsize = env->me_mapsize;

  if (newenv && !(flags & MDB_FIXEDMAP)) {
    /* mdb_env_map() may grow the datafile.  Write the metapages
     * first, so the file will be valid if initialization fails.
     * Except with FIXEDMAP, since we do not yet know mm_address.
     * We could fill in mm_address later, but then a different
     * program might end up doing that - one with a memory layout
     * and map address which does not suit the main program.
     */
    rc = mdb_env_init_meta(env, &meta);
    if (rc)
      return rc;
    newenv = 0;
  }

  rc = mdb_env_map(env, (flags & MDB_FIXEDMAP) ? meta.mm_address : NULL);
  if (rc)
    return rc;

  if (newenv) {
    if (flags & MDB_FIXEDMAP)
      meta.mm_address = env->me_map;
    i = mdb_env_init_meta(env, &meta);
    if (i != MDB_SUCCESS) {
      return i;
    }
  }

  env->me_maxfree_1pg = (env->me_psize - PAGEHDRSZ) / sizeof(pgno_t) - 1;
  env->me_nodemax = ((env->me_psize - PAGEHDRSZ) & -2)
    - sizeof(indx_t);
#if !(MDB_MAXKEYSIZE)
  env->me_maxkey = env->me_nodemax - (NODESIZE + sizeof(MDB_db));
#endif
  env->me_maxpg = env->me_mapsize / env->me_psize;

  return MDB_SUCCESS;
}


/** Release a reader thread's slot in the reader lock table.
 *  This function is called automatically when a thread exits.
 * @param[in] ptr This points to the slot in the reader lock table.
 */
static void
mdb_env_reader_dest(void *ptr)
{
  MDB_reader *reader = ptr;
  if (reader->mr_pid == getpid()) /* catch pthread_exit() in child process */
    /* We omit the mutex, so do this atomically (i.e. skip mr_txnid) */
    reader->mr_pid = 0;
}

/** Downgrade the exclusive lock on the region back to shared */
static int ESECT
mdb_env_share_locks(MDB_env *env, int *excl)
{
  int rc = 0;
  MDB_meta *meta = mdb_env_pick_meta(env);

  env->me_txns->mti_txnid = meta->mm_txnid;

  {
    struct flock lock_info;
    /* The shared lock replaces the existing lock */
    memset((void *)&lock_info, 0, sizeof(lock_info));
    lock_info.l_type = F_RDLCK;
    lock_info.l_whence = SEEK_SET;
    lock_info.l_start = 0;
    lock_info.l_len = 1;
    while ((rc = fcntl(env->me_lfd, F_SETLK, &lock_info)) &&
        (rc = ErrCode()) == EINTR) ;
    *excl = rc ? -1 : 0;  /* error may mean we lost the lock */
  }

  return rc;
}

/** Try to get exclusive lock, otherwise shared.
 *  Maintain *excl = -1: no/unknown lock, 0: shared, 1: exclusive.
 */
static int ESECT
mdb_env_excl_lock(MDB_env *env, int *excl)
{
  int rc = 0;
  struct flock lock_info;
  memset((void *)&lock_info, 0, sizeof(lock_info));
  lock_info.l_type = F_WRLCK;
  lock_info.l_whence = SEEK_SET;
  lock_info.l_start = 0;
  lock_info.l_len = 1;
  while ((rc = fcntl(env->me_lfd, F_SETLK, &lock_info)) &&
      (rc = ErrCode()) == EINTR) ;
  if (!rc) {
    *excl = 1;
  } else
  {
    lock_info.l_type = F_RDLCK;
    while ((rc = fcntl(env->me_lfd, F_SETLKW, &lock_info)) &&
        (rc = ErrCode()) == EINTR) ;
    if (rc == 0)
      *excl = 0;
  }
  return rc;
}

#ifdef MDB_USE_HASH
/*
 * hash_64 - 64 bit Fowler/Noll/Vo-0 FNV-1a hash code
 *
 * @(#) $Revision: 5.1 $
 * @(#) $Id: hash_64a.c,v 5.1 2009/06/30 09:01:38 chongo Exp $
 * @(#) $Source: /usr/local/src/cmd/fnv/RCS/hash_64a.c,v $
 *
 *    http://www.isthe.com/chongo/tech/comp/fnv/index.html
 *
 ***
 *
 * Please do not copyright this code.  This code is in the public domain.
 *
 * LANDON CURT NOLL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
 * EVENT SHALL LANDON CURT NOLL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * By:
 *  chongo <Landon Curt Noll> /\oo/\
 *    http://www.isthe.com/chongo/
 *
 * Share and Enjoy!  :-)
 */

/** perform a 64 bit Fowler/Noll/Vo FNV-1a hash on a buffer
 * @param[in] val  value to hash
 * @param[in] len  length of value
 * @return 64 bit hash
 */
static mdb_hash_t
mdb_hash(const void *val, size_t len)
{
  const unsigned char *s = (const unsigned char *) val, *end = s + len;
  mdb_hash_t hval = 0xcbf29ce484222325ULL;
  /*
   * FNV-1a hash each octet of the buffer
   */
  while (s < end) {
    hval = (hval ^ *s++) * 0x100000001b3ULL;
  }
  /* return our new hash value */
  return hval;
}

/** Hash the string and output the encoded hash.
 * This uses modified RFC1924 Ascii85 encoding to accommodate systems with
 * very short name limits. We don't care about the encoding being reversible,
 * we just want to preserve as many bits of the input as possible in a
 * small printable string.
 * @param[in] str string to hash
 * @param[out] encbuf an array of 11 chars to hold the hash
 */
static const char mdb_a85[]= "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~";

static void ESECT
mdb_pack85(unsigned long long l, char *out)
{
  int i;

  for (i=0; i<10 && l; i++) {
    *out++ = mdb_a85[l % 85];
    l /= 85;
  }
  *out = '\0';
}

/** Init #MDB_env.me_mutexname[] except the char which #MUTEXNAME() will set.
 *  Changes to this code must be reflected in #MDB_LOCK_FORMAT.
 */
static void ESECT
mdb_env_mname_init(MDB_env *env)
{
  char *nm = env->me_mutexname;
  strcpy(nm, MUTEXNAME_PREFIX);
  mdb_pack85(env->me_txns->mti_mutexid, nm + sizeof(MUTEXNAME_PREFIX));
}

/** Return env->me_mutexname after filling in ch ('r'/'w') for convenience */
#define MUTEXNAME(env, ch) ( \
    (void) ((env)->me_mutexname[sizeof(MUTEXNAME_PREFIX)-1] = (ch)), \
    (env)->me_mutexname)

#endif

  /** Only a subset of the @ref mdb_env flags can be changed
   *  at runtime. Changing other flags requires closing the
   *  environment and re-opening it with the new flags.
   */
#define  CHANGEABLE  (MDB_NOSYNC|MDB_NOMETASYNC|MDB_MAPASYNC|MDB_NOMEMINIT)
#define  CHANGELESS  (MDB_FIXEDMAP|MDB_NOSUBDIR|MDB_RDONLY| \
  MDB_WRITEMAP|MDB_NOTLS|MDB_NOLOCK|MDB_NORDAHEAD|MDB_PREVSNAPSHOT)

#if VALID_FLAGS & PERSISTENT_FLAGS & (CHANGEABLE|CHANGELESS)
# error "Persistent DB flags & env flags overlap, but both go in mm_flags"
#endif

int ESECT
mdb_env_open(MDB_env *env, const char *path, unsigned int flags, mdb_mode_t mode)
{
  int rc, excl = -1;
  MDB_name fname;

  if (env->me_fd!=INVALID_HANDLE_VALUE || (flags & ~(CHANGEABLE|CHANGELESS)))
    return EINVAL;

  flags |= env->me_flags;

  rc = mdb_fname_init(path, flags, &fname);
  if (rc)
    return rc;

  flags |= MDB_ENV_ACTIVE;  /* tell mdb_env_close0() to clean up */

  if (flags & MDB_RDONLY) {
    /* silently ignore WRITEMAP when we're only getting read access */
    flags &= ~MDB_WRITEMAP;
  } else {
    if (!((env->me_free_pgs = mdb_midl_alloc(MDB_IDL_UM_MAX)) &&
        (env->me_dirty_list = calloc(MDB_IDL_UM_SIZE, sizeof(MDB_ID2)))))
      rc = ENOMEM;
  }

  env->me_flags = flags;
  if (rc)
    goto leave;

  env->me_path = strdup(path);
  env->me_dbxs = calloc(env->me_maxdbs, sizeof(MDB_dbx));
  env->me_dbflags = calloc(env->me_maxdbs, sizeof(uint16_t));
  env->me_dbiseqs = calloc(env->me_maxdbs, sizeof(unsigned int));
  if (!(env->me_dbxs && env->me_path && env->me_dbflags && env->me_dbiseqs)) {
    rc = ENOMEM;
    goto leave;
  }
  env->me_dbxs[FREE_DBI].md_cmp = mdb_cmp_long; /* aligned MDB_INTEGERKEY */

  rc = mdb_fopen(env, &fname,
    (flags & MDB_RDONLY) ? MDB_O_RDONLY : MDB_O_RDWR,
    mode, &env->me_fd);
  if (rc)
    goto leave;

  if ((rc = mdb_env_open2(env, flags & MDB_PREVSNAPSHOT)) == MDB_SUCCESS) {
    /* Synchronous fd for meta writes. Needed even with
     * MDB_NOSYNC/MDB_NOMETASYNC, in case these get reset.
     */
    if (!(flags & (MDB_RDONLY|MDB_WRITEMAP))) {
      rc = mdb_fopen(env, &fname, MDB_O_META, mode, &env->me_mfd);
      if (rc)
        goto leave;
    }
    DPRINTF(("opened dbenv %p", (void *) env));
    if (excl > 0 && !(flags & MDB_PREVSNAPSHOT)) {
      rc = mdb_env_share_locks(env, &excl);
      if (rc)
        goto leave;
    }
    if (!(flags & MDB_RDONLY)) {
      MDB_txn *txn;
      int tsize = sizeof(MDB_txn), size = tsize + env->me_maxdbs *
        (sizeof(MDB_db)+sizeof(MDB_cursor *)+sizeof(unsigned int)+1);
      if ((env->me_pbuf = calloc(1, env->me_psize)) &&
        (txn = calloc(1, size)))
      {
        txn->mt_dbs = (MDB_db *)((char *)txn + tsize);
        txn->mt_cursors = (MDB_cursor **)(txn->mt_dbs + env->me_maxdbs);
        txn->mt_dbiseqs = (unsigned int *)(txn->mt_cursors + env->me_maxdbs);
        txn->mt_dbflags = (unsigned char *)(txn->mt_dbiseqs + env->me_maxdbs);
        txn->mt_env = env;
        txn->mt_dbxs = env->me_dbxs;
        txn->mt_flags = MDB_TXN_FINISHED;
        env->me_txn0 = txn;
      } else {
        rc = ENOMEM;
      }
    }
  }

leave:
  if (rc) {
    mdb_env_close0(env, excl);
  }
  mdb_fname_destroy(fname);
  return rc;
}

/** Destroy resources from mdb_env_open(), clear our readers & DBIs */
static void ESECT
mdb_env_close0(MDB_env *env, int excl)
{
  int i;

  if (!(env->me_flags & MDB_ENV_ACTIVE))
    return;

  /* Doing this here since me_dbxs may not exist during mdb_env_close */
  if (env->me_dbxs) {
    for (i = env->me_maxdbs; --i >= CORE_DBS; )
      free(env->me_dbxs[i].md_name.mv_data);
    free(env->me_dbxs);
  }

  free(env->me_pbuf);
  free(env->me_dbiseqs);
  free(env->me_dbflags);
  free(env->me_path);
  free(env->me_dirty_list);
  free(env->me_txn0);
  mdb_midl_free(env->me_free_pgs);

  if (env->me_flags & MDB_ENV_TXKEY) {
    pthread_key_delete(env->me_txkey);
  }

  if (env->me_map) {
    munmap(env->me_map, env->me_mapsize);
  }
  if (env->me_mfd != INVALID_HANDLE_VALUE)
    (void) close(env->me_mfd);
  if (env->me_fd != INVALID_HANDLE_VALUE)
    (void) close(env->me_fd);
  if (env->me_txns) {
    MDB_PID_T pid = getpid();
    /* Clearing readers is done in this function because
     * me_txkey with its destructor must be disabled first.
     *
     * We skip the the reader mutex, so we touch only
     * data owned by this process (me_close_readers and
     * our readers), and clear each reader atomically.
     */
    for (i = env->me_close_readers; --i >= 0; )
      if (env->me_txns->mti_readers[i].mr_pid == pid)
        env->me_txns->mti_readers[i].mr_pid = 0;
    /* If we have the filelock:  If we are the
     * only remaining user, clean up robust
     * mutexes.
     */
    if (excl == 0)
      mdb_env_excl_lock(env, &excl);
    if (excl > 0) {
      pthread_mutex_destroy(env->me_txns->mti_rmutex);
      pthread_mutex_destroy(env->me_txns->mti_wmutex);
    }

    munmap((void *)env->me_txns, (env->me_maxreaders-1)*sizeof(MDB_reader)+sizeof(MDB_txninfo));
  }
  if (env->me_lfd != INVALID_HANDLE_VALUE) {
    (void) close(env->me_lfd);
  }

  env->me_flags &= ~(MDB_ENV_ACTIVE|MDB_ENV_TXKEY);
}

void ESECT
mdb_env_close(MDB_env *env)
{
  MDB_page *dp;

  if (env == NULL)
    return;

  while ((dp = env->me_dpages) != NULL) {
    env->me_dpages = dp->mp_next;
    free(dp);
  }

  mdb_env_close0(env, 0);
  free(env);
}

/** Compare two items pointing at aligned #mdb_size_t's */
static int
mdb_cmp_long(const MDB_val *a, const MDB_val *b)
{
  return (*(mdb_size_t *)a->mv_data < *(mdb_size_t *)b->mv_data) ? -1 :
    *(mdb_size_t *)a->mv_data > *(mdb_size_t *)b->mv_data;
}

/** Compare two items pointing at aligned unsigned int's.
 *
 *  This is also set as #MDB_INTEGERDUP|#MDB_DUPFIXED's #MDB_dbx.%md_dcmp,
 *  but #mdb_cmp_clong() is called instead if the data type is #mdb_size_t.
 */
static int
mdb_cmp_int(const MDB_val *a, const MDB_val *b)
{
  return (*(unsigned int *)a->mv_data < *(unsigned int *)b->mv_data) ? -1 :
    *(unsigned int *)a->mv_data > *(unsigned int *)b->mv_data;
}

/** Compare two items pointing at unsigned ints of unknown alignment.
 *  Nodes and keys are guaranteed to be 2-byte aligned.
 */
static int
mdb_cmp_cint(const MDB_val *a, const MDB_val *b)
{
  unsigned short *u, *c;
  int x;

  u = (unsigned short *) ((char *) a->mv_data + a->mv_size);
  c = (unsigned short *) ((char *) b->mv_data + a->mv_size);
  do {
    x = *--u - *--c;
  } while(!x && u > (unsigned short *)a->mv_data);
  return x;
}

/** Compare two items lexically */
static int
mdb_cmp_memn(const MDB_val *a, const MDB_val *b)
{
  int diff;
  ssize_t len_diff;
  unsigned int len;

  len = a->mv_size;
  len_diff = (ssize_t) a->mv_size - (ssize_t) b->mv_size;
  if (len_diff > 0) {
    len = b->mv_size;
    len_diff = 1;
  }

  diff = memcmp(a->mv_data, b->mv_data, len);
  return diff ? diff : len_diff<0 ? -1 : len_diff;
}

/** Compare two items in reverse byte order */
static int
mdb_cmp_memnr(const MDB_val *a, const MDB_val *b)
{
  const unsigned char  *p1, *p2, *p1_lim;
  ssize_t len_diff;
  int diff;

  p1_lim = (const unsigned char *)a->mv_data;
  p1 = (const unsigned char *)a->mv_data + a->mv_size;
  p2 = (const unsigned char *)b->mv_data + b->mv_size;

  len_diff = (ssize_t) a->mv_size - (ssize_t) b->mv_size;
  if (len_diff > 0) {
    p1_lim += len_diff;
    len_diff = 1;
  }

  while (p1 > p1_lim) {
    diff = *--p1 - *--p2;
    if (diff)
      return diff;
  }
  return len_diff<0 ? -1 : len_diff;
}

/** Search for key within a page, using binary search.
 * Returns the smallest entry larger or equal to the key.
 * If exactp is non-null, stores whether the found entry was an exact match
 * in *exactp (1 or 0).
 * Updates the cursor index with the index of the found entry.
 * If no entry larger or equal to the key is found, returns NULL.
 */
static MDB_node *
mdb_node_search(MDB_cursor *mc, MDB_val *key, int *exactp)
{
  unsigned int   i = 0, nkeys;
  int     low, high;
  int     rc = 0;
  MDB_page *mp = mc->mc_pg[mc->mc_top];
  MDB_node  *node = NULL;
  MDB_val   nodekey;
  MDB_cmp_func *cmp;
  DKBUF;

  nkeys = NUMKEYS(mp);

  DPRINTF(("searching %u keys in %s %spage %"Yu,
      nkeys, IS_LEAF(mp) ? "leaf" : "branch", IS_SUBP(mp) ? "sub-" : "",
      mdb_dbg_pgno(mp)));

  low = IS_LEAF(mp) ? 0 : 1;
  high = nkeys - 1;
  cmp = mc->mc_dbx->md_cmp;

  /* Branch pages have no data, so if using integer keys,
   * alignment is guaranteed. Use faster mdb_cmp_int.
   */
  if (cmp == mdb_cmp_cint && IS_BRANCH(mp)) {
    if (NODEPTR(mp, 1)->mn_ksize == sizeof(mdb_size_t))
      cmp = mdb_cmp_long;
    else
      cmp = mdb_cmp_int;
  }

  if (IS_LEAF2(mp)) {
    nodekey.mv_size = mc->mc_db->md_pad;
    node = NODEPTR(mp, 0);  /* fake */
    while (low <= high) {
      i = (low + high) >> 1;
      nodekey.mv_data = LEAF2KEY(mp, i, nodekey.mv_size);
      rc = cmp(key, &nodekey);
      DPRINTF(("found leaf index %u [%s], rc = %i",
          i, DKEY(&nodekey), rc));
      if (rc == 0)
        break;
      if (rc > 0)
        low = i + 1;
      else
        high = i - 1;
    }
  } else {
    while (low <= high) {
      i = (low + high) >> 1;

      node = NODEPTR(mp, i);
      nodekey.mv_size = NODEKSZ(node);
      nodekey.mv_data = NODEKEY(node);

      rc = cmp(key, &nodekey);
      if (rc == 0)
        break;
      if (rc > 0)
        low = i + 1;
      else
        high = i - 1;
    }
  }

  if (rc > 0) {  /* Found entry is less than the key. */
    i++;  /* Skip to get the smallest entry larger than key. */
    if (!IS_LEAF2(mp))
      node = NODEPTR(mp, i);
  }
  if (exactp)
    *exactp = (rc == 0 && nkeys > 0);
  /* store the key index */
  mc->mc_ki[mc->mc_top] = i;
  if (i >= nkeys)
    /* There is no entry larger or equal to the key. */
    return NULL;

  /* nodeptr is fake for LEAF2 */
  return node;
}

/** Find the address of the page corresponding to a given page number.
 * Set #MDB_TXN_ERROR on failure.
 * @param[in] mc the cursor accessing the page.
 * @param[in] pgno the page number for the page to retrieve.
 * @param[out] ret address of a pointer where the page's address will be stored.
 * @param[out] lvl dirty_list inheritance level of found page. 1=current txn, 0=mapped page.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_get(MDB_cursor *mc, pgno_t pgno, MDB_page **ret, int *lvl)
{
  MDB_txn *txn = mc->mc_txn;
  MDB_page *p = NULL;
  int level;

  if (! (mc->mc_flags & (C_ORIG_RDONLY|C_WRITEMAP))) {
    MDB_txn *tx2 = txn;
    level = 1;
    do {
      MDB_ID2L dl = tx2->mt_u.dirty_list;
      unsigned x;
      /* Spilled pages were dirtied in this txn and flushed
       * because the dirty list got full. Bring this page
       * back in from the map (but don't unspill it here,
       * leave that unless page_touch happens again).
       */
      if (tx2->mt_spill_pgs) {
        MDB_ID pn = pgno << 1;
        x = mdb_midl_search(tx2->mt_spill_pgs, pn);
        if (x <= tx2->mt_spill_pgs[0] && tx2->mt_spill_pgs[x] == pn) {
          goto mapped;
        }
      }
      if (dl[0].mid) {
        unsigned x = mdb_mid2l_search(dl, pgno);
        if (x <= dl[0].mid && dl[x].mid == pgno) {
          p = dl[x].mptr;
          goto done;
        }
      }
      level++;
    } while ((tx2 = tx2->mt_parent) != NULL);
  }

  if (pgno >= txn->mt_next_pgno) {
    DPRINTF(("page %"Yu" not found", pgno));
    txn->mt_flags |= MDB_TXN_ERROR;
    return MDB_PAGE_NOTFOUND;
  }

  level = 0;

mapped:
  {
    MDB_env *env = txn->mt_env;
    p = (MDB_page *)(env->me_map + env->me_psize * pgno);
  }

done:
  *ret = p;
  if (lvl)
    *lvl = level;
  return MDB_SUCCESS;
}

/** Finish #mdb_page_search() / #mdb_page_search_lowest().
 *  The cursor is at the root page, set up the rest of it.
 */
static int
mdb_page_search_root(MDB_cursor *mc, MDB_val *key, int flags)
{
  MDB_page  *mp = mc->mc_pg[mc->mc_top];
  int rc;
  DKBUF;

  while (IS_BRANCH(mp)) {
    MDB_node  *node;
    indx_t    i;

    DPRINTF(("branch page %"Yu" has %u keys", mp->mp_pgno, NUMKEYS(mp)));
    /* Don't assert on branch pages in the FreeDB. We can get here
     * while in the process of rebalancing a FreeDB branch page; we must
     * let that proceed. ITS#8336
     */
    mdb_cassert(mc, !mc->mc_dbi || NUMKEYS(mp) > 1);
    DPRINTF(("found index 0 to page %"Yu, NODEPGNO(NODEPTR(mp, 0))));

    if (flags & (MDB_PS_FIRST|MDB_PS_LAST)) {
      i = 0;
      if (flags & MDB_PS_LAST) {
        i = NUMKEYS(mp) - 1;
        /* if already init'd, see if we're already in right place */
        if (mc->mc_flags & C_INITIALIZED) {
          if (mc->mc_ki[mc->mc_top] == i) {
            mc->mc_top = mc->mc_snum++;
            mp = mc->mc_pg[mc->mc_top];
            goto ready;
          }
        }
      }
    } else {
      int   exact;
      node = mdb_node_search(mc, key, &exact);
      if (node == NULL)
        i = NUMKEYS(mp) - 1;
      else {
        i = mc->mc_ki[mc->mc_top];
        if (!exact) {
          mdb_cassert(mc, i > 0);
          i--;
        }
      }
      DPRINTF(("following index %u for key [%s]", i, DKEY(key)));
    }

    mdb_cassert(mc, i < NUMKEYS(mp));
    node = NODEPTR(mp, i);

    if ((rc = mdb_page_get(mc, NODEPGNO(node), &mp, NULL)) != 0)
      return rc;

    mc->mc_ki[mc->mc_top] = i;
    if ((rc = mdb_cursor_push(mc, mp)))
      return rc;

ready:
    if (flags & MDB_PS_MODIFY) {
      if ((rc = mdb_page_touch(mc)) != 0)
        return rc;
      mp = mc->mc_pg[mc->mc_top];
    }
  }

  if (!IS_LEAF(mp)) {
    DPRINTF(("internal error, index points to a %02X page!?",
        mp->mp_flags));
    mc->mc_txn->mt_flags |= MDB_TXN_ERROR;
    return MDB_CORRUPTED;
  }

  DPRINTF(("found leaf page %"Yu" for key [%s]", mp->mp_pgno,
      key ? DKEY(key) : "null"));
  mc->mc_flags |= C_INITIALIZED;
  mc->mc_flags &= ~C_EOF;

  return MDB_SUCCESS;
}

/** Search for the lowest key under the current branch page.
 * This just bypasses a NUMKEYS check in the current page
 * before calling mdb_page_search_root(), because the callers
 * are all in situations where the current page is known to
 * be underfilled.
 */
static int
mdb_page_search_lowest(MDB_cursor *mc)
{
  MDB_page  *mp = mc->mc_pg[mc->mc_top];
  MDB_node  *node = NODEPTR(mp, 0);
  int rc;

  if ((rc = mdb_page_get(mc, NODEPGNO(node), &mp, NULL)) != 0)
    return rc;

  mc->mc_ki[mc->mc_top] = 0;
  if ((rc = mdb_cursor_push(mc, mp)))
    return rc;
  return mdb_page_search_root(mc, NULL, MDB_PS_FIRST);
}

static int
mdb_ovpage_free(MDB_cursor *mc, MDB_page *mp)
{
  MDB_txn *txn = mc->mc_txn;
  pgno_t pg = mp->mp_pgno;
  unsigned x = 0, ovpages = mp->mp_pages;
  MDB_env *env = txn->mt_env;
  MDB_IDL sl = txn->mt_spill_pgs;
  MDB_ID pn = pg << 1;
  int rc;

  DPRINTF(("free ov page %"Yu" (%d)", pg, ovpages));
  /* If the page is dirty or on the spill list we just acquired it,
   * so we should give it back to our current free list, if any.
   * Otherwise put it onto the list of pages we freed in this txn.
   *
   * Won't create me_pghead: me_pglast must be inited along with it.
   * Unsupported in nested txns: They would need to hide the page
   * range in ancestor txns' dirty and spilled lists.
   */
  if (env->me_pghead &&
    !txn->mt_parent &&
    ((mp->mp_flags & P_DIRTY) ||
     (sl && (x = mdb_midl_search(sl, pn)) <= sl[0] && sl[x] == pn)))
  {
    unsigned i, j;
    pgno_t *mop;
    MDB_ID2 *dl, ix, iy;
    rc = mdb_midl_need(&env->me_pghead, ovpages);
    if (rc)
      return rc;
    if (!(mp->mp_flags & P_DIRTY)) {
      /* This page is no longer spilled */
      if (x == sl[0])
        sl[0]--;
      else
        sl[x] |= 1;
      goto release;
    }
    /* Remove from dirty list */
    dl = txn->mt_u.dirty_list;
    x = dl[0].mid--;
    for (ix = dl[x]; ix.mptr != mp; ix = iy) {
      if (x > 1) {
        x--;
        iy = dl[x];
        dl[x] = ix;
      } else {
        mdb_cassert(mc, x > 1);
        j = ++(dl[0].mid);
        dl[j] = ix;    /* Unsorted. OK when MDB_TXN_ERROR. */
        txn->mt_flags |= MDB_TXN_ERROR;
        return MDB_PROBLEM;
      }
    }
    txn->mt_dirty_room++;
    if (!(env->me_flags & MDB_WRITEMAP))
      mdb_dpage_free(env, mp);
release:
    /* Insert in me_pghead */
    mop = env->me_pghead;
    j = mop[0] + ovpages;
    for (i = mop[0]; i && mop[i] < pg; i--)
      mop[j--] = mop[i];
    while (j>i)
      mop[j--] = pg++;
    mop[0] += ovpages;
  } else {
    rc = mdb_midl_append_range(&txn->mt_free_pgs, pg, ovpages);
    if (rc)
      return rc;
  }
  mc->mc_db->md_overflow_pages -= ovpages;
  return 0;
}

/** Return the data associated with a given node.
 * @param[in] mc The cursor for this operation.
 * @param[in] leaf The node being read.
 * @param[out] data Updated to point to the node's data.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_node_read(MDB_cursor *mc, MDB_node *leaf, MDB_val *data)
{
  MDB_page  *omp;    /* overflow page */
  pgno_t     pgno;
  int rc;

  if (MC_OVPG(mc)) {
    MDB_PAGE_UNREF(mc->mc_txn, MC_OVPG(mc));
    MC_SET_OVPG(mc, NULL);
  }
  if (!F_ISSET(leaf->mn_flags, F_BIGDATA)) {
    data->mv_size = NODEDSZ(leaf);
    data->mv_data = NODEDATA(leaf);
    return MDB_SUCCESS;
  }

  /* Read overflow data.
   */
  data->mv_size = NODEDSZ(leaf);
  memcpy(&pgno, NODEDATA(leaf), sizeof(pgno));
  if ((rc = mdb_page_get(mc, pgno, &omp, NULL)) != 0) {
    DPRINTF(("read overflow page %"Yu" failed", pgno));
    return rc;
  }
  data->mv_data = METADATA(omp);
  MC_SET_OVPG(mc, omp);

  return MDB_SUCCESS;
}

int
mdb_get(MDB_txn *txn, MDB_dbi dbi,
    MDB_val *key, MDB_val *data)
{
  MDB_cursor  mc;
  MDB_xcursor  mx;
  int exact = 0, rc;
  DKBUF;

  DPRINTF(("===> get db %u key [%s]", dbi, DKEY(key)));

  if (!key || !data || !TXN_DBI_EXIST(txn, dbi, DB_USRVALID))
    return EINVAL;

  if (txn->mt_flags & MDB_TXN_BLOCKED)
    return MDB_BAD_TXN;

  mdb_cursor_init(&mc, txn, dbi, &mx);
  rc = mdb_cursor_set(&mc, key, data, MDB_SET, &exact);
  /* unref all the pages when MDB_VL32 - caller must copy the data
   * before doing anything else
   */
  MDB_CURSOR_UNREF(&mc, 1);
  return rc;
}

/** Allocate and initialize new pages for a database.
 * Set #MDB_TXN_ERROR on failure.
 * @param[in] mc a cursor on the database being added to.
 * @param[in] flags flags defining what type of page is being allocated.
 * @param[in] num the number of pages to allocate. This is usually 1,
 * unless allocating overflow pages for a large record.
 * @param[out] mp Address of a page, or NULL on failure.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_new(MDB_cursor *mc, uint32_t flags, int num, MDB_page **mp)
{
  MDB_page  *np;
  int rc;

  if ((rc = mdb_page_alloc(mc, num, &np)))
    return rc;
  DPRINTF(("allocated new mpage %"Yu", page size %u",
      np->mp_pgno, mc->mc_txn->mt_env->me_psize));
  np->mp_flags = flags | P_DIRTY;
  np->mp_lower = (PAGEHDRSZ-PAGEBASE);
  np->mp_upper = mc->mc_txn->mt_env->me_psize - PAGEBASE;

  if (IS_BRANCH(np))
    mc->mc_db->md_branch_pages++;
  else if (IS_LEAF(np))
    mc->mc_db->md_leaf_pages++;
  else if (IS_OVERFLOW(np)) {
    mc->mc_db->md_overflow_pages += num;
    np->mp_pages = num;
  }
  *mp = np;

  return 0;
}

/** Calculate the size of a leaf node.
 * The size depends on the environment's page size; if a data item
 * is too large it will be put onto an overflow page and the node
 * size will only include the key and not the data. Sizes are always
 * rounded up to an even number of bytes, to guarantee 2-byte alignment
 * of the #MDB_node headers.
 * @param[in] env The environment handle.
 * @param[in] key The key for the node.
 * @param[in] data The data for the node.
 * @return The number of bytes needed to store the node.
 */
static size_t
mdb_leaf_size(MDB_env *env, MDB_val *key, MDB_val *data)
{
  size_t     sz;

  sz = LEAFSIZE(key, data);
  if (sz > env->me_nodemax) {
    /* put on overflow page */
    sz -= data->mv_size - sizeof(pgno_t);
  }

  return EVEN(sz + sizeof(indx_t));
}

/** Calculate the size of a branch node.
 * The size should depend on the environment's page size but since
 * we currently don't support spilling large keys onto overflow
 * pages, it's simply the size of the #MDB_node header plus the
 * size of the key. Sizes are always rounded up to an even number
 * of bytes, to guarantee 2-byte alignment of the #MDB_node headers.
 * @param[in] env The environment handle.
 * @param[in] key The key for the node.
 * @return The number of bytes needed to store the node.
 */
static size_t
mdb_branch_size(MDB_env *env, MDB_val *key)
{
  size_t     sz;

  sz = INDXSIZE(key);
  if (sz > env->me_nodemax) {
    /* put on overflow page */
    /* not implemented */
    /* sz -= key->size - sizeof(pgno_t); */
  }

  return sz + sizeof(indx_t);
}

/** Add a node to the page pointed to by the cursor.
 * Set #MDB_TXN_ERROR on failure.
 * @param[in] mc The cursor for this operation.
 * @param[in] indx The index on the page where the new node should be added.
 * @param[in] key The key for the new node.
 * @param[in] data The data for the new node, if any.
 * @param[in] pgno The page number, if adding a branch node.
 * @param[in] flags Flags for the node.
 * @return 0 on success, non-zero on failure. Possible errors are:
 * <ul>
 *  <li>ENOMEM - failed to allocate overflow pages for the node.
 *  <li>MDB_PAGE_FULL - there is insufficient room in the page. This error
 *  should never happen since all callers already calculate the
 *  page's free space before calling this function.
 * </ul>
 */
static int
mdb_node_add(MDB_cursor *mc, indx_t indx,
    MDB_val *key, MDB_val *data, pgno_t pgno, unsigned int flags)
{
  unsigned int   i;
  size_t     node_size = NODESIZE;
  ssize_t     room;
  indx_t     ofs;
  MDB_node  *node;
  MDB_page  *mp = mc->mc_pg[mc->mc_top];
  MDB_page  *ofp = NULL;    /* overflow page */
  void    *ndata;
  DKBUF;

  mdb_cassert(mc, mp->mp_upper >= mp->mp_lower);

  DPRINTF(("add to %s %spage %"Yu" index %i, data size %"Z"u key size %"Z"u [%s]",
      IS_LEAF(mp) ? "leaf" : "branch",
    IS_SUBP(mp) ? "sub-" : "",
    mdb_dbg_pgno(mp), indx, data ? data->mv_size : 0,
    key ? key->mv_size : 0, key ? DKEY(key) : "null"));

  if (IS_LEAF2(mp)) {
    /* Move higher keys up one slot. */
    int ksize = mc->mc_db->md_pad, dif;
    char *ptr = LEAF2KEY(mp, indx, ksize);
    dif = NUMKEYS(mp) - indx;
    if (dif > 0)
      memmove(ptr+ksize, ptr, dif*ksize);
    /* insert new key */
    memcpy(ptr, key->mv_data, ksize);

    /* Just using these for counting */
    mp->mp_lower += sizeof(indx_t);
    mp->mp_upper -= ksize - sizeof(indx_t);
    return MDB_SUCCESS;
  }

  room = (ssize_t)SIZELEFT(mp) - (ssize_t)sizeof(indx_t);
  if (key != NULL)
    node_size += key->mv_size;
  if (IS_LEAF(mp)) {
    mdb_cassert(mc, key && data);
    if (F_ISSET(flags, F_BIGDATA)) {
      /* Data already on overflow page. */
      node_size += sizeof(pgno_t);
    } else if (node_size + data->mv_size > mc->mc_txn->mt_env->me_nodemax) {
      int ovpages = OVPAGES(data->mv_size, mc->mc_txn->mt_env->me_psize);
      int rc;
      /* Put data on overflow page. */
      DPRINTF(("data size is %"Z"u, node would be %"Z"u, put data on overflow page",
          data->mv_size, node_size+data->mv_size));
      node_size = EVEN(node_size + sizeof(pgno_t));
      if ((ssize_t)node_size > room)
        goto full;
      if ((rc = mdb_page_new(mc, P_OVERFLOW, ovpages, &ofp)))
        return rc;
      DPRINTF(("allocated overflow page %"Yu, ofp->mp_pgno));
      flags |= F_BIGDATA;
      goto update;
    } else {
      node_size += data->mv_size;
    }
  }
  node_size = EVEN(node_size);
  if ((ssize_t)node_size > room)
    goto full;

update:
  /* Move higher pointers up one slot. */
  for (i = NUMKEYS(mp); i > indx; i--)
    mp->mp_ptrs[i] = mp->mp_ptrs[i - 1];

  /* Adjust free space offsets. */
  ofs = mp->mp_upper - node_size;
  mdb_cassert(mc, ofs >= mp->mp_lower + sizeof(indx_t));
  mp->mp_ptrs[indx] = ofs;
  mp->mp_upper = ofs;
  mp->mp_lower += sizeof(indx_t);

  /* Write the node data. */
  node = NODEPTR(mp, indx);
  node->mn_ksize = (key == NULL) ? 0 : key->mv_size;
  node->mn_flags = flags;
  if (IS_LEAF(mp))
    SETDSZ(node,data->mv_size);
  else
    SETPGNO(node,pgno);

  if (key)
    memcpy(NODEKEY(node), key->mv_data, key->mv_size);

  if (IS_LEAF(mp)) {
    ndata = NODEDATA(node);
    if (ofp == NULL) {
      if (F_ISSET(flags, F_BIGDATA))
        memcpy(ndata, data->mv_data, sizeof(pgno_t));
      else if (F_ISSET(flags, MDB_RESERVE))
        data->mv_data = ndata;
      else
        memcpy(ndata, data->mv_data, data->mv_size);
    } else {
      memcpy(ndata, &ofp->mp_pgno, sizeof(pgno_t));
      ndata = METADATA(ofp);
      if (F_ISSET(flags, MDB_RESERVE))
        data->mv_data = ndata;
      else
        memcpy(ndata, data->mv_data, data->mv_size);
    }
  }

  return MDB_SUCCESS;

full:
  DPRINTF(("not enough room in page %"Yu", got %u ptrs",
    mdb_dbg_pgno(mp), NUMKEYS(mp)));
  DPRINTF(("upper-lower = %u - %u = %"Z"d", mp->mp_upper,mp->mp_lower,room));
  DPRINTF(("node size = %"Z"u", node_size));
  mc->mc_txn->mt_flags |= MDB_TXN_ERROR;
  return MDB_PAGE_FULL;
}

/** Delete the specified node from a page.
 * @param[in] mc Cursor pointing to the node to delete.
 * @param[in] ksize The size of a node. Only used if the page is
 * part of a #MDB_DUPFIXED database.
 */
static void
mdb_node_del(MDB_cursor *mc, int ksize)
{
  MDB_page *mp = mc->mc_pg[mc->mc_top];
  indx_t  indx = mc->mc_ki[mc->mc_top];
  unsigned int   sz;
  indx_t     i, j, numkeys, ptr;
  MDB_node  *node;
  char    *base;

  DPRINTF(("delete node %u on %s page %"Yu, indx,
      IS_LEAF(mp) ? "leaf" : "branch", mdb_dbg_pgno(mp)));
  numkeys = NUMKEYS(mp);
  mdb_cassert(mc, indx < numkeys);

  if (IS_LEAF2(mp)) {
    int x = numkeys - 1 - indx;
    base = LEAF2KEY(mp, indx, ksize);
    if (x)
      memmove(base, base + ksize, x * ksize);
    mp->mp_lower -= sizeof(indx_t);
    mp->mp_upper += ksize - sizeof(indx_t);
    return;
  }

  node = NODEPTR(mp, indx);
  sz = NODESIZE + node->mn_ksize;
  if (IS_LEAF(mp)) {
    if (F_ISSET(node->mn_flags, F_BIGDATA))
      sz += sizeof(pgno_t);
    else
      sz += NODEDSZ(node);
  }
  sz = EVEN(sz);

  ptr = mp->mp_ptrs[indx];
  for (i = j = 0; i < numkeys; i++) {
    if (i != indx) {
      mp->mp_ptrs[j] = mp->mp_ptrs[i];
      if (mp->mp_ptrs[i] < ptr)
        mp->mp_ptrs[j] += sz;
      j++;
    }
  }

  base = (char *)mp + mp->mp_upper + PAGEBASE;
  memmove(base + sz, base, ptr - mp->mp_upper);

  mp->mp_lower -= sizeof(indx_t);
  mp->mp_upper += sz;
}

/** Compact the main page after deleting a node on a subpage.
 * @param[in] mp The main page to operate on.
 * @param[in] indx The index of the subpage on the main page.
 */
static void
mdb_node_shrink(MDB_page *mp, indx_t indx)
{
  MDB_node *node;
  MDB_page *sp, *xp;
  char *base;
  indx_t delta, nsize, len, ptr;
  int i;

  node = NODEPTR(mp, indx);
  sp = (MDB_page *)NODEDATA(node);
  delta = SIZELEFT(sp);
  nsize = NODEDSZ(node) - delta;

  /* Prepare to shift upward, set len = length(subpage part to shift) */
  if (IS_LEAF2(sp)) {
    len = nsize;
    if (nsize & 1)
      return;    /* do not make the node uneven-sized */
  } else {
    xp = (MDB_page *)((char *)sp + delta); /* destination subpage */
    for (i = NUMKEYS(sp); --i >= 0; )
      xp->mp_ptrs[i] = sp->mp_ptrs[i] - delta;
    len = PAGEHDRSZ;
  }
  sp->mp_upper = sp->mp_lower;
  COPY_PGNO(sp->mp_pgno, mp->mp_pgno);
  SETDSZ(node, nsize);

  /* Shift <lower nodes...initial part of subpage> upward */
  base = (char *)mp + mp->mp_upper + PAGEBASE;
  memmove(base + delta, base, (char *)sp + len - base);

  ptr = mp->mp_ptrs[indx];
  for (i = NUMKEYS(mp); --i >= 0; ) {
    if (mp->mp_ptrs[i] <= ptr)
      mp->mp_ptrs[i] += delta;
  }
  mp->mp_upper += delta;
}

/** Initial setup of a sorted-dups cursor.
 * Sorted duplicates are implemented as a sub-database for the given key.
 * The duplicate data items are actually keys of the sub-database.
 * Operations on the duplicate data items are performed using a sub-cursor
 * initialized when the sub-database is first accessed. This function does
 * the preliminary setup of the sub-cursor, filling in the fields that
 * depend only on the parent DB.
 * @param[in] mc The main cursor whose sorted-dups cursor is to be initialized.
 */
static void
mdb_xcursor_init0(MDB_cursor *mc)
{
  MDB_xcursor *mx = mc->mc_xcursor;

  mx->mx_cursor.mc_xcursor = NULL;
  mx->mx_cursor.mc_txn = mc->mc_txn;
  mx->mx_cursor.mc_db = &mx->mx_db;
  mx->mx_cursor.mc_dbx = &mx->mx_dbx;
  mx->mx_cursor.mc_dbi = mc->mc_dbi;
  mx->mx_cursor.mc_dbflag = &mx->mx_dbflag;
  mx->mx_cursor.mc_snum = 0;
  mx->mx_cursor.mc_top = 0;
  MC_SET_OVPG(&mx->mx_cursor, NULL);
  mx->mx_cursor.mc_flags = C_SUB | (mc->mc_flags & (C_ORIG_RDONLY|C_WRITEMAP));
  mx->mx_dbx.md_name.mv_size = 0;
  mx->mx_dbx.md_name.mv_data = NULL;
  mx->mx_dbx.md_cmp = mc->mc_dbx->md_dcmp;
  mx->mx_dbx.md_dcmp = NULL;
  mx->mx_dbx.md_rel = mc->mc_dbx->md_rel;
}

/** Final setup of a sorted-dups cursor.
 *  Sets up the fields that depend on the data from the main cursor.
 * @param[in] mc The main cursor whose sorted-dups cursor is to be initialized.
 * @param[in] node The data containing the #MDB_db record for the
 * sorted-dup database.
 */
static void
mdb_xcursor_init1(MDB_cursor *mc, MDB_node *node)
{
  MDB_xcursor *mx = mc->mc_xcursor;

  mx->mx_cursor.mc_flags &= C_SUB|C_ORIG_RDONLY|C_WRITEMAP;
  if (node->mn_flags & F_SUBDATA) {
    memcpy(&mx->mx_db, NODEDATA(node), sizeof(MDB_db));
    mx->mx_cursor.mc_pg[0] = 0;
    mx->mx_cursor.mc_snum = 0;
    mx->mx_cursor.mc_top = 0;
  } else {
    MDB_page *fp = NODEDATA(node);
    mx->mx_db.md_pad = 0;
    mx->mx_db.md_flags = 0;
    mx->mx_db.md_depth = 1;
    mx->mx_db.md_branch_pages = 0;
    mx->mx_db.md_leaf_pages = 1;
    mx->mx_db.md_overflow_pages = 0;
    mx->mx_db.md_entries = NUMKEYS(fp);
    COPY_PGNO(mx->mx_db.md_root, fp->mp_pgno);
    mx->mx_cursor.mc_snum = 1;
    mx->mx_cursor.mc_top = 0;
    mx->mx_cursor.mc_flags |= C_INITIALIZED;
    mx->mx_cursor.mc_pg[0] = fp;
    mx->mx_cursor.mc_ki[0] = 0;
    if (mc->mc_db->md_flags & MDB_DUPFIXED) {
      mx->mx_db.md_flags = MDB_DUPFIXED;
      mx->mx_db.md_pad = fp->mp_pad;
      if (mc->mc_db->md_flags & MDB_INTEGERDUP)
        mx->mx_db.md_flags |= MDB_INTEGERKEY;
    }
  }
  DPRINTF(("Sub-db -%u root page %"Yu, mx->mx_cursor.mc_dbi,
    mx->mx_db.md_root));
  mx->mx_dbflag = DB_VALID|DB_USRVALID|DB_DUPDATA;
  if (NEED_CMP_CLONG(mx->mx_dbx.md_cmp, mx->mx_db.md_pad))
    mx->mx_dbx.md_cmp = mdb_cmp_clong;
}


/** Fixup a sorted-dups cursor due to underlying update.
 *  Sets up some fields that depend on the data from the main cursor.
 *  Almost the same as init1, but skips initialization steps if the
 *  xcursor had already been used.
 * @param[in] mc The main cursor whose sorted-dups cursor is to be fixed up.
 * @param[in] src_mx The xcursor of an up-to-date cursor.
 * @param[in] new_dupdata True if converting from a non-#F_DUPDATA item.
 */
static void
mdb_xcursor_init2(MDB_cursor *mc, MDB_xcursor *src_mx, int new_dupdata)
{
  MDB_xcursor *mx = mc->mc_xcursor;

  if (new_dupdata) {
    mx->mx_cursor.mc_snum = 1;
    mx->mx_cursor.mc_top = 0;
    mx->mx_cursor.mc_flags |= C_INITIALIZED;
    mx->mx_cursor.mc_ki[0] = 0;
    mx->mx_dbflag = DB_VALID|DB_USRVALID|DB_DUPDATA;
#if UINT_MAX < MDB_SIZE_MAX  /* matches mdb_xcursor_init1:NEED_CMP_CLONG() */
    mx->mx_dbx.md_cmp = src_mx->mx_dbx.md_cmp;
#endif
  } else if (!(mx->mx_cursor.mc_flags & C_INITIALIZED)) {
    return;
  }
  mx->mx_db = src_mx->mx_db;
  mx->mx_cursor.mc_pg[0] = src_mx->mx_cursor.mc_pg[0];
  DPRINTF(("Sub-db -%u root page %"Yu, mx->mx_cursor.mc_dbi,
    mx->mx_db.md_root));
}

/** Replace the key for a branch node with a new key.
 * Set #MDB_TXN_ERROR on failure.
 * @param[in] mc Cursor pointing to the node to operate on.
 * @param[in] key The new key to use.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_update_key(MDB_cursor *mc, MDB_val *key)
{
  MDB_page    *mp;
  MDB_node    *node;
  char      *base;
  size_t       len;
  int         delta, ksize, oksize;
  indx_t       ptr, i, numkeys, indx;
  DKBUF;

  indx = mc->mc_ki[mc->mc_top];
  mp = mc->mc_pg[mc->mc_top];
  node = NODEPTR(mp, indx);
  ptr = mp->mp_ptrs[indx];

  /* Sizes must be 2-byte aligned. */
  ksize = EVEN(key->mv_size);
  oksize = EVEN(node->mn_ksize);
  delta = ksize - oksize;

  /* Shift node contents if EVEN(key length) changed. */
  if (delta) {
    if (delta > 0 && SIZELEFT(mp) < delta) {
      pgno_t pgno;
      /* not enough space left, do a delete and split */
      DPRINTF(("Not enough room, delta = %d, splitting...", delta));
      pgno = NODEPGNO(node);
      mdb_node_del(mc, 0);
      return mdb_page_split(mc, key, NULL, pgno, MDB_SPLIT_REPLACE);
    }

    numkeys = NUMKEYS(mp);
    for (i = 0; i < numkeys; i++) {
      if (mp->mp_ptrs[i] <= ptr)
        mp->mp_ptrs[i] -= delta;
    }

    base = (char *)mp + mp->mp_upper + PAGEBASE;
    len = ptr - mp->mp_upper + NODESIZE;
    memmove(base - delta, base, len);
    mp->mp_upper -= delta;

    node = NODEPTR(mp, indx);
  }

  /* But even if no shift was needed, update ksize */
  if (node->mn_ksize != key->mv_size)
    node->mn_ksize = key->mv_size;

  if (key->mv_size)
    memcpy(NODEKEY(node), key->mv_data, key->mv_size);

  return MDB_SUCCESS;
}

/** Perform \b act while tracking temporary cursor \b mn */
#define WITH_CURSOR_TRACKING(mn, act) do { \
  MDB_cursor dummy, *tracked, **tp = &(mn).mc_txn->mt_cursors[mn.mc_dbi]; \
  if ((mn).mc_flags & C_SUB) { \
    dummy.mc_flags =  C_INITIALIZED; \
    dummy.mc_xcursor = (MDB_xcursor *)&(mn);  \
    tracked = &dummy; \
  } else { \
    tracked = &(mn); \
  } \
  tracked->mc_next = *tp; \
  *tp = tracked; \
  { act; } \
  *tp = tracked->mc_next; \
} while (0)

/** Move a node from csrc to cdst.
 */
static int
mdb_node_move(MDB_cursor *csrc, MDB_cursor *cdst, int fromleft)
{
  MDB_node    *srcnode;
  MDB_val     key, data;
  pgno_t  srcpg;
  MDB_cursor mn;
  int       rc;
  unsigned short flags;

  DKBUF;

  /* Mark src and dst as dirty. */
  if ((rc = mdb_page_touch(csrc)) ||
      (rc = mdb_page_touch(cdst)))
    return rc;

  if (IS_LEAF2(csrc->mc_pg[csrc->mc_top])) {
    key.mv_size = csrc->mc_db->md_pad;
    key.mv_data = LEAF2KEY(csrc->mc_pg[csrc->mc_top], csrc->mc_ki[csrc->mc_top], key.mv_size);
    data.mv_size = 0;
    data.mv_data = NULL;
    srcpg = 0;
    flags = 0;
  } else {
    srcnode = NODEPTR(csrc->mc_pg[csrc->mc_top], csrc->mc_ki[csrc->mc_top]);
    mdb_cassert(csrc, !((size_t)srcnode & 1));
    srcpg = NODEPGNO(srcnode);
    flags = srcnode->mn_flags;
    if (csrc->mc_ki[csrc->mc_top] == 0 && IS_BRANCH(csrc->mc_pg[csrc->mc_top])) {
      unsigned int snum = csrc->mc_snum;
      MDB_node *s2;
      /* must find the lowest key below src */
      rc = mdb_page_search_lowest(csrc);
      if (rc)
        return rc;
      if (IS_LEAF2(csrc->mc_pg[csrc->mc_top])) {
        key.mv_size = csrc->mc_db->md_pad;
        key.mv_data = LEAF2KEY(csrc->mc_pg[csrc->mc_top], 0, key.mv_size);
      } else {
        s2 = NODEPTR(csrc->mc_pg[csrc->mc_top], 0);
        key.mv_size = NODEKSZ(s2);
        key.mv_data = NODEKEY(s2);
      }
      csrc->mc_snum = snum--;
      csrc->mc_top = snum;
    } else {
      key.mv_size = NODEKSZ(srcnode);
      key.mv_data = NODEKEY(srcnode);
    }
    data.mv_size = NODEDSZ(srcnode);
    data.mv_data = NODEDATA(srcnode);
  }
  mn.mc_xcursor = NULL;
  if (IS_BRANCH(cdst->mc_pg[cdst->mc_top]) && cdst->mc_ki[cdst->mc_top] == 0) {
    unsigned int snum = cdst->mc_snum;
    MDB_node *s2;
    MDB_val bkey;
    /* must find the lowest key below dst */
    mdb_cursor_copy(cdst, &mn);
    rc = mdb_page_search_lowest(&mn);
    if (rc)
      return rc;
    if (IS_LEAF2(mn.mc_pg[mn.mc_top])) {
      bkey.mv_size = mn.mc_db->md_pad;
      bkey.mv_data = LEAF2KEY(mn.mc_pg[mn.mc_top], 0, bkey.mv_size);
    } else {
      s2 = NODEPTR(mn.mc_pg[mn.mc_top], 0);
      bkey.mv_size = NODEKSZ(s2);
      bkey.mv_data = NODEKEY(s2);
    }
    mn.mc_snum = snum--;
    mn.mc_top = snum;
    mn.mc_ki[snum] = 0;
    rc = mdb_update_key(&mn, &bkey);
    if (rc)
      return rc;
  }

  DPRINTF(("moving %s node %u [%s] on page %"Yu" to node %u on page %"Yu,
      IS_LEAF(csrc->mc_pg[csrc->mc_top]) ? "leaf" : "branch",
      csrc->mc_ki[csrc->mc_top],
    DKEY(&key),
      csrc->mc_pg[csrc->mc_top]->mp_pgno,
      cdst->mc_ki[cdst->mc_top], cdst->mc_pg[cdst->mc_top]->mp_pgno));

  /* Add the node to the destination page.
   */
  rc = mdb_node_add(cdst, cdst->mc_ki[cdst->mc_top], &key, &data, srcpg, flags);
  if (rc != MDB_SUCCESS)
    return rc;

  /* Delete the node from the source page.
   */
  mdb_node_del(csrc, key.mv_size);

  {
    /* Adjust other cursors pointing to mp */
    MDB_cursor *m2, *m3;
    MDB_dbi dbi = csrc->mc_dbi;
    MDB_page *mpd, *mps;

    mps = csrc->mc_pg[csrc->mc_top];
    /* If we're adding on the left, bump others up */
    if (fromleft) {
      mpd = cdst->mc_pg[csrc->mc_top];
      for (m2 = csrc->mc_txn->mt_cursors[dbi]; m2; m2=m2->mc_next) {
        if (csrc->mc_flags & C_SUB)
          m3 = &m2->mc_xcursor->mx_cursor;
        else
          m3 = m2;
        if (!(m3->mc_flags & C_INITIALIZED) || m3->mc_top < csrc->mc_top)
          continue;
        if (m3 != cdst &&
          m3->mc_pg[csrc->mc_top] == mpd &&
          m3->mc_ki[csrc->mc_top] >= cdst->mc_ki[csrc->mc_top]) {
          m3->mc_ki[csrc->mc_top]++;
        }
        if (m3 !=csrc &&
          m3->mc_pg[csrc->mc_top] == mps &&
          m3->mc_ki[csrc->mc_top] == csrc->mc_ki[csrc->mc_top]) {
          m3->mc_pg[csrc->mc_top] = cdst->mc_pg[cdst->mc_top];
          m3->mc_ki[csrc->mc_top] = cdst->mc_ki[cdst->mc_top];
          m3->mc_ki[csrc->mc_top-1]++;
        }
        if (IS_LEAF(mps))
          XCURSOR_REFRESH(m3, csrc->mc_top, m3->mc_pg[csrc->mc_top]);
      }
    } else
    /* Adding on the right, bump others down */
    {
      for (m2 = csrc->mc_txn->mt_cursors[dbi]; m2; m2=m2->mc_next) {
        if (csrc->mc_flags & C_SUB)
          m3 = &m2->mc_xcursor->mx_cursor;
        else
          m3 = m2;
        if (m3 == csrc) continue;
        if (!(m3->mc_flags & C_INITIALIZED) || m3->mc_top < csrc->mc_top)
          continue;
        if (m3->mc_pg[csrc->mc_top] == mps) {
          if (!m3->mc_ki[csrc->mc_top]) {
            m3->mc_pg[csrc->mc_top] = cdst->mc_pg[cdst->mc_top];
            m3->mc_ki[csrc->mc_top] = cdst->mc_ki[cdst->mc_top];
            m3->mc_ki[csrc->mc_top-1]--;
          } else {
            m3->mc_ki[csrc->mc_top]--;
          }
          if (IS_LEAF(mps))
            XCURSOR_REFRESH(m3, csrc->mc_top, m3->mc_pg[csrc->mc_top]);
        }
      }
    }
  }

  /* Update the parent separators.
   */
  if (csrc->mc_ki[csrc->mc_top] == 0) {
    if (csrc->mc_ki[csrc->mc_top-1] != 0) {
      if (IS_LEAF2(csrc->mc_pg[csrc->mc_top])) {
        key.mv_data = LEAF2KEY(csrc->mc_pg[csrc->mc_top], 0, key.mv_size);
      } else {
        srcnode = NODEPTR(csrc->mc_pg[csrc->mc_top], 0);
        key.mv_size = NODEKSZ(srcnode);
        key.mv_data = NODEKEY(srcnode);
      }
      DPRINTF(("update separator for source page %"Yu" to [%s]",
        csrc->mc_pg[csrc->mc_top]->mp_pgno, DKEY(&key)));
      mdb_cursor_copy(csrc, &mn);
      mn.mc_snum--;
      mn.mc_top--;
      /* We want mdb_rebalance to find mn when doing fixups */
      WITH_CURSOR_TRACKING(mn,
        rc = mdb_update_key(&mn, &key));
      if (rc)
        return rc;
    }
    if (IS_BRANCH(csrc->mc_pg[csrc->mc_top])) {
      MDB_val   nullkey;
      indx_t  ix = csrc->mc_ki[csrc->mc_top];
      nullkey.mv_size = 0;
      csrc->mc_ki[csrc->mc_top] = 0;
      rc = mdb_update_key(csrc, &nullkey);
      csrc->mc_ki[csrc->mc_top] = ix;
      mdb_cassert(csrc, rc == MDB_SUCCESS);
    }
  }

  if (cdst->mc_ki[cdst->mc_top] == 0) {
    if (cdst->mc_ki[cdst->mc_top-1] != 0) {
      if (IS_LEAF2(csrc->mc_pg[csrc->mc_top])) {
        key.mv_data = LEAF2KEY(cdst->mc_pg[cdst->mc_top], 0, key.mv_size);
      } else {
        srcnode = NODEPTR(cdst->mc_pg[cdst->mc_top], 0);
        key.mv_size = NODEKSZ(srcnode);
        key.mv_data = NODEKEY(srcnode);
      }
      DPRINTF(("update separator for destination page %"Yu" to [%s]",
        cdst->mc_pg[cdst->mc_top]->mp_pgno, DKEY(&key)));
      mdb_cursor_copy(cdst, &mn);
      mn.mc_snum--;
      mn.mc_top--;
      /* We want mdb_rebalance to find mn when doing fixups */
      WITH_CURSOR_TRACKING(mn,
        rc = mdb_update_key(&mn, &key));
      if (rc)
        return rc;
    }
    if (IS_BRANCH(cdst->mc_pg[cdst->mc_top])) {
      MDB_val   nullkey;
      indx_t  ix = cdst->mc_ki[cdst->mc_top];
      nullkey.mv_size = 0;
      cdst->mc_ki[cdst->mc_top] = 0;
      rc = mdb_update_key(cdst, &nullkey);
      cdst->mc_ki[cdst->mc_top] = ix;
      mdb_cassert(cdst, rc == MDB_SUCCESS);
    }
  }

  return MDB_SUCCESS;
}

/** Merge one page into another.
 *  The nodes from the page pointed to by \b csrc will
 *  be copied to the page pointed to by \b cdst and then
 *  the \b csrc page will be freed.
 * @param[in] csrc Cursor pointing to the source page.
 * @param[in] cdst Cursor pointing to the destination page.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_merge(MDB_cursor *csrc, MDB_cursor *cdst)
{
  MDB_page  *psrc, *pdst;
  MDB_node  *srcnode;
  MDB_val     key, data;
  unsigned   nkeys;
  int       rc;
  indx_t     i, j;

  psrc = csrc->mc_pg[csrc->mc_top];
  pdst = cdst->mc_pg[cdst->mc_top];

  DPRINTF(("merging page %"Yu" into %"Yu, psrc->mp_pgno, pdst->mp_pgno));

  mdb_cassert(csrc, csrc->mc_snum > 1);  /* can't merge root page */
  mdb_cassert(csrc, cdst->mc_snum > 1);

  /* Mark dst as dirty. */
  if ((rc = mdb_page_touch(cdst)))
    return rc;

  /* get dst page again now that we've touched it. */
  pdst = cdst->mc_pg[cdst->mc_top];

  /* Move all nodes from src to dst.
   */
  j = nkeys = NUMKEYS(pdst);
  if (IS_LEAF2(psrc)) {
    key.mv_size = csrc->mc_db->md_pad;
    key.mv_data = METADATA(psrc);
    for (i = 0; i < NUMKEYS(psrc); i++, j++) {
      rc = mdb_node_add(cdst, j, &key, NULL, 0, 0);
      if (rc != MDB_SUCCESS)
        return rc;
      key.mv_data = (char *)key.mv_data + key.mv_size;
    }
  } else {
    for (i = 0; i < NUMKEYS(psrc); i++, j++) {
      srcnode = NODEPTR(psrc, i);
      if (i == 0 && IS_BRANCH(psrc)) {
        MDB_cursor mn;
        MDB_node *s2;
        mdb_cursor_copy(csrc, &mn);
        mn.mc_xcursor = NULL;
        /* must find the lowest key below src */
        rc = mdb_page_search_lowest(&mn);
        if (rc)
          return rc;
        if (IS_LEAF2(mn.mc_pg[mn.mc_top])) {
          key.mv_size = mn.mc_db->md_pad;
          key.mv_data = LEAF2KEY(mn.mc_pg[mn.mc_top], 0, key.mv_size);
        } else {
          s2 = NODEPTR(mn.mc_pg[mn.mc_top], 0);
          key.mv_size = NODEKSZ(s2);
          key.mv_data = NODEKEY(s2);
        }
      } else {
        key.mv_size = srcnode->mn_ksize;
        key.mv_data = NODEKEY(srcnode);
      }

      data.mv_size = NODEDSZ(srcnode);
      data.mv_data = NODEDATA(srcnode);
      rc = mdb_node_add(cdst, j, &key, &data, NODEPGNO(srcnode), srcnode->mn_flags);
      if (rc != MDB_SUCCESS)
        return rc;
    }
  }

  DPRINTF(("dst page %"Yu" now has %u keys (%.1f%% filled)",
      pdst->mp_pgno, NUMKEYS(pdst),
    (float)PAGEFILL(cdst->mc_txn->mt_env, pdst) / 10));

  /* Unlink the src page from parent and add to free list.
   */
  csrc->mc_top--;
  mdb_node_del(csrc, 0);
  if (csrc->mc_ki[csrc->mc_top] == 0) {
    key.mv_size = 0;
    rc = mdb_update_key(csrc, &key);
    if (rc) {
      csrc->mc_top++;
      return rc;
    }
  }
  csrc->mc_top++;

  psrc = csrc->mc_pg[csrc->mc_top];
  /* If not operating on FreeDB, allow this page to be reused
   * in this txn. Otherwise just add to free list.
   */
  rc = mdb_page_loose(csrc, psrc);
  if (rc)
    return rc;
  if (IS_LEAF(psrc))
    csrc->mc_db->md_leaf_pages--;
  else
    csrc->mc_db->md_branch_pages--;
  {
    /* Adjust other cursors pointing to mp */
    MDB_cursor *m2, *m3;
    MDB_dbi dbi = csrc->mc_dbi;
    unsigned int top = csrc->mc_top;

    for (m2 = csrc->mc_txn->mt_cursors[dbi]; m2; m2=m2->mc_next) {
      if (csrc->mc_flags & C_SUB)
        m3 = &m2->mc_xcursor->mx_cursor;
      else
        m3 = m2;
      if (m3 == csrc) continue;
      if (m3->mc_snum < csrc->mc_snum) continue;
      if (m3->mc_pg[top] == psrc) {
        m3->mc_pg[top] = pdst;
        m3->mc_ki[top] += nkeys;
        m3->mc_ki[top-1] = cdst->mc_ki[top-1];
      } else if (m3->mc_pg[top-1] == csrc->mc_pg[top-1] &&
        m3->mc_ki[top-1] > csrc->mc_ki[top-1]) {
        m3->mc_ki[top-1]--;
      }
      if (IS_LEAF(psrc))
        XCURSOR_REFRESH(m3, top, m3->mc_pg[top]);
    }
  }
  {
    unsigned int snum = cdst->mc_snum;
    uint16_t depth = cdst->mc_db->md_depth;
    mdb_cursor_pop(cdst);
    rc = mdb_rebalance(cdst);
    /* Did the tree height change? */
    if (depth != cdst->mc_db->md_depth)
      snum += cdst->mc_db->md_depth - depth;
    cdst->mc_snum = snum;
    cdst->mc_top = snum-1;
  }
  return rc;
}

/** Rebalance the tree after a delete operation.
 * @param[in] mc Cursor pointing to the page where rebalancing
 * should begin.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_rebalance(MDB_cursor *mc)
{
  MDB_node  *node;
  int rc, fromleft;
  unsigned int ptop, minkeys, thresh;
  MDB_cursor  mn;
  indx_t oldki;

  if (IS_BRANCH(mc->mc_pg[mc->mc_top])) {
    minkeys = 2;
    thresh = 1;
  } else {
    minkeys = 1;
    thresh = FILL_THRESHOLD;
  }
  DPRINTF(("rebalancing %s page %"Yu" (has %u keys, %.1f%% full)",
      IS_LEAF(mc->mc_pg[mc->mc_top]) ? "leaf" : "branch",
      mdb_dbg_pgno(mc->mc_pg[mc->mc_top]), NUMKEYS(mc->mc_pg[mc->mc_top]),
    (float)PAGEFILL(mc->mc_txn->mt_env, mc->mc_pg[mc->mc_top]) / 10));

  if (PAGEFILL(mc->mc_txn->mt_env, mc->mc_pg[mc->mc_top]) >= thresh &&
    NUMKEYS(mc->mc_pg[mc->mc_top]) >= minkeys) {
    DPRINTF(("no need to rebalance page %"Yu", above fill threshold",
        mdb_dbg_pgno(mc->mc_pg[mc->mc_top])));
    return MDB_SUCCESS;
  }

  if (mc->mc_snum < 2) {
    MDB_page *mp = mc->mc_pg[0];
    if (IS_SUBP(mp)) {
      DPUTS("Can't rebalance a subpage, ignoring");
      return MDB_SUCCESS;
    }
    if (NUMKEYS(mp) == 0) {
      DPUTS("tree is completely empty");
      mc->mc_db->md_root = P_INVALID;
      mc->mc_db->md_depth = 0;
      mc->mc_db->md_leaf_pages = 0;
      rc = mdb_midl_append(&mc->mc_txn->mt_free_pgs, mp->mp_pgno);
      if (rc)
        return rc;
      /* Adjust cursors pointing to mp */
      mc->mc_snum = 0;
      mc->mc_top = 0;
      mc->mc_flags &= ~C_INITIALIZED;
      {
        MDB_cursor *m2, *m3;
        MDB_dbi dbi = mc->mc_dbi;

        for (m2 = mc->mc_txn->mt_cursors[dbi]; m2; m2=m2->mc_next) {
          if (mc->mc_flags & C_SUB)
            m3 = &m2->mc_xcursor->mx_cursor;
          else
            m3 = m2;
          if (!(m3->mc_flags & C_INITIALIZED) || (m3->mc_snum < mc->mc_snum))
            continue;
          if (m3->mc_pg[0] == mp) {
            m3->mc_snum = 0;
            m3->mc_top = 0;
            m3->mc_flags &= ~C_INITIALIZED;
          }
        }
      }
    } else if (IS_BRANCH(mp) && NUMKEYS(mp) == 1) {
      int i;
      DPUTS("collapsing root page!");
      rc = mdb_midl_append(&mc->mc_txn->mt_free_pgs, mp->mp_pgno);
      if (rc)
        return rc;
      mc->mc_db->md_root = NODEPGNO(NODEPTR(mp, 0));
      rc = mdb_page_get(mc, mc->mc_db->md_root, &mc->mc_pg[0], NULL);
      if (rc)
        return rc;
      mc->mc_db->md_depth--;
      mc->mc_db->md_branch_pages--;
      mc->mc_ki[0] = mc->mc_ki[1];
      for (i = 1; i<mc->mc_db->md_depth; i++) {
        mc->mc_pg[i] = mc->mc_pg[i+1];
        mc->mc_ki[i] = mc->mc_ki[i+1];
      }
      {
        /* Adjust other cursors pointing to mp */
        MDB_cursor *m2, *m3;
        MDB_dbi dbi = mc->mc_dbi;

        for (m2 = mc->mc_txn->mt_cursors[dbi]; m2; m2=m2->mc_next) {
          if (mc->mc_flags & C_SUB)
            m3 = &m2->mc_xcursor->mx_cursor;
          else
            m3 = m2;
          if (m3 == mc) continue;
          if (!(m3->mc_flags & C_INITIALIZED))
            continue;
          if (m3->mc_pg[0] == mp) {
            for (i=0; i<mc->mc_db->md_depth; i++) {
              m3->mc_pg[i] = m3->mc_pg[i+1];
              m3->mc_ki[i] = m3->mc_ki[i+1];
            }
            m3->mc_snum--;
            m3->mc_top--;
          }
        }
      }
    } else
      DPUTS("root page doesn't need rebalancing");
    return MDB_SUCCESS;
  }

  /* The parent (branch page) must have at least 2 pointers,
   * otherwise the tree is invalid.
   */
  ptop = mc->mc_top-1;
  mdb_cassert(mc, NUMKEYS(mc->mc_pg[ptop]) > 1);

  /* Leaf page fill factor is below the threshold.
   * Try to move keys from left or right neighbor, or
   * merge with a neighbor page.
   */

  /* Find neighbors.
   */
  mn.mc_xcursor = NULL;

  oldki = mc->mc_ki[mc->mc_top];
  if (mc->mc_ki[ptop] == 0) {
    /* We're the leftmost leaf in our parent.
     */
    DPUTS("reading right neighbor");
    mn.mc_ki[ptop]++;
    node = NODEPTR(mc->mc_pg[ptop], mn.mc_ki[ptop]);
    rc = mdb_page_get(mc, NODEPGNO(node), &mn.mc_pg[mn.mc_top], NULL);
    if (rc)
      return rc;
    mn.mc_ki[mn.mc_top] = 0;
    mc->mc_ki[mc->mc_top] = NUMKEYS(mc->mc_pg[mc->mc_top]);
    fromleft = 0;
  } else {
    /* There is at least one neighbor to the left.
     */
    DPUTS("reading left neighbor");
    mn.mc_ki[ptop]--;
    node = NODEPTR(mc->mc_pg[ptop], mn.mc_ki[ptop]);
    rc = mdb_page_get(mc, NODEPGNO(node), &mn.mc_pg[mn.mc_top], NULL);
    if (rc)
      return rc;
    mn.mc_ki[mn.mc_top] = NUMKEYS(mn.mc_pg[mn.mc_top]) - 1;
    mc->mc_ki[mc->mc_top] = 0;
    fromleft = 1;
  }

  DPRINTF(("found neighbor page %"Yu" (%u keys, %.1f%% full)",
      mn.mc_pg[mn.mc_top]->mp_pgno, NUMKEYS(mn.mc_pg[mn.mc_top]),
    (float)PAGEFILL(mc->mc_txn->mt_env, mn.mc_pg[mn.mc_top]) / 10));

  /* If the neighbor page is above threshold and has enough keys,
   * move one key from it. Otherwise we should try to merge them.
   * (A branch page must never have less than 2 keys.)
   */
  if (PAGEFILL(mc->mc_txn->mt_env, mn.mc_pg[mn.mc_top]) >= thresh && NUMKEYS(mn.mc_pg[mn.mc_top]) > minkeys) {
    rc = mdb_node_move(&mn, mc, fromleft);
    if (fromleft) {
      /* if we inserted on left, bump position up */
      oldki++;
    }
  } else {
    if (!fromleft) {
      rc = mdb_page_merge(&mn, mc);
    } else {
      oldki += NUMKEYS(mn.mc_pg[mn.mc_top]);
      mn.mc_ki[mn.mc_top] += mc->mc_ki[mn.mc_top] + 1;
      /* We want mdb_rebalance to find mn when doing fixups */
      WITH_CURSOR_TRACKING(mn,
        rc = mdb_page_merge(mc, &mn));
    }
    mc->mc_flags &= ~C_EOF;
  }
  mc->mc_ki[mc->mc_top] = oldki;
  return rc;
}

int
mdb_del(MDB_txn *txn, MDB_dbi dbi,
    MDB_val *key, MDB_val *data)
{
  if (!key || !TXN_DBI_EXIST(txn, dbi, DB_USRVALID))
    return EINVAL;

  if (txn->mt_flags & (MDB_TXN_RDONLY|MDB_TXN_BLOCKED))
    return (txn->mt_flags & MDB_TXN_RDONLY) ? EACCES : MDB_BAD_TXN;

  if (!F_ISSET(txn->mt_dbs[dbi].md_flags, MDB_DUPSORT)) {
    /* must ignore any data */
    data = NULL;
  }

  return mdb_del0(txn, dbi, key, data, 0);
}

static int
mdb_del0(MDB_txn *txn, MDB_dbi dbi,
  MDB_val *key, MDB_val *data, unsigned flags)
{
  MDB_cursor mc;
  MDB_xcursor mx;
  MDB_cursor_op op;
  MDB_val rdata, *xdata;
  int     rc, exact = 0;
  DKBUF;

  DPRINTF(("====> delete db %u key [%s]", dbi, DKEY(key)));

  mdb_cursor_init(&mc, txn, dbi, &mx);

  if (data) {
    op = MDB_GET_BOTH;
    rdata = *data;
    xdata = &rdata;
  } else {
    op = MDB_SET;
    xdata = NULL;
    flags |= MDB_NODUPDATA;
  }
  rc = mdb_cursor_set(&mc, key, xdata, op, &exact);
  if (rc == 0) {
    /* let mdb_page_split know about this cursor if needed:
     * delete will trigger a rebalance; if it needs to move
     * a node from one page to another, it will have to
     * update the parent's separator key(s). If the new sepkey
     * is larger than the current one, the parent page may
     * run out of space, triggering a split. We need this
     * cursor to be consistent until the end of the rebalance.
     */
    mc.mc_next = txn->mt_cursors[dbi];
    txn->mt_cursors[dbi] = &mc;
    rc = mdb_cursor_del(&mc, flags);
    txn->mt_cursors[dbi] = mc.mc_next;
  }
  return rc;
}

/** Split a page and insert a new node.
 * Set #MDB_TXN_ERROR on failure.
 * @param[in,out] mc Cursor pointing to the page and desired insertion index.
 * The cursor will be updated to point to the actual page and index where
 * the node got inserted after the split.
 * @param[in] newkey The key for the newly inserted node.
 * @param[in] newdata The data for the newly inserted node.
 * @param[in] newpgno The page number, if the new node is a branch node.
 * @param[in] nflags The #NODE_ADD_FLAGS for the new node.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_page_split(MDB_cursor *mc, MDB_val *newkey, MDB_val *newdata, pgno_t newpgno,
  unsigned int nflags)
{
  unsigned int flags;
  int     rc = MDB_SUCCESS, new_root = 0, did_split = 0;
  indx_t     newindx;
  pgno_t     pgno = 0;
  int   i, j, split_indx, nkeys, pmax;
  MDB_env   *env = mc->mc_txn->mt_env;
  MDB_node  *node;
  MDB_val   sepkey, rkey, xdata, *rdata = &xdata;
  MDB_page  *copy = NULL;
  MDB_page  *mp, *rp, *pp;
  int ptop;
  MDB_cursor  mn;
  DKBUF;

  mp = mc->mc_pg[mc->mc_top];
  newindx = mc->mc_ki[mc->mc_top];
  nkeys = NUMKEYS(mp);

  DPRINTF(("-----> splitting %s page %"Yu" and adding [%s] at index %i/%i",
      IS_LEAF(mp) ? "leaf" : "branch", mp->mp_pgno,
      DKEY(newkey), mc->mc_ki[mc->mc_top], nkeys));

  /* Create a right sibling. */
  if ((rc = mdb_page_new(mc, mp->mp_flags, 1, &rp)))
    return rc;
  rp->mp_pad = mp->mp_pad;
  DPRINTF(("new right sibling: page %"Yu, rp->mp_pgno));

  /* Usually when splitting the root page, the cursor
   * height is 1. But when called from mdb_update_key,
   * the cursor height may be greater because it walks
   * up the stack while finding the branch slot to update.
   */
  if (mc->mc_top < 1) {
    if ((rc = mdb_page_new(mc, P_BRANCH, 1, &pp)))
      goto done;
    /* shift current top to make room for new parent */
    for (i=mc->mc_snum; i>0; i--) {
      mc->mc_pg[i] = mc->mc_pg[i-1];
      mc->mc_ki[i] = mc->mc_ki[i-1];
    }
    mc->mc_pg[0] = pp;
    mc->mc_ki[0] = 0;
    mc->mc_db->md_root = pp->mp_pgno;
    DPRINTF(("root split! new root = %"Yu, pp->mp_pgno));
    new_root = mc->mc_db->md_depth++;

    /* Add left (implicit) pointer. */
    if ((rc = mdb_node_add(mc, 0, NULL, NULL, mp->mp_pgno, 0)) != MDB_SUCCESS) {
      /* undo the pre-push */
      mc->mc_pg[0] = mc->mc_pg[1];
      mc->mc_ki[0] = mc->mc_ki[1];
      mc->mc_db->md_root = mp->mp_pgno;
      mc->mc_db->md_depth--;
      goto done;
    }
    mc->mc_snum++;
    mc->mc_top++;
    ptop = 0;
  } else {
    ptop = mc->mc_top-1;
    DPRINTF(("parent branch page is %"Yu, mc->mc_pg[ptop]->mp_pgno));
  }

  mdb_cursor_copy(mc, &mn);
  mn.mc_xcursor = NULL;
  mn.mc_pg[mn.mc_top] = rp;
  mn.mc_ki[ptop] = mc->mc_ki[ptop]+1;

  if (nflags & MDB_APPEND) {
    mn.mc_ki[mn.mc_top] = 0;
    sepkey = *newkey;
    split_indx = newindx;
    nkeys = 0;
  } else {

    split_indx = (nkeys+1) / 2;

    if (IS_LEAF2(rp)) {
      char *split, *ins;
      int x;
      unsigned int lsize, rsize, ksize;
      /* Move half of the keys to the right sibling */
      x = mc->mc_ki[mc->mc_top] - split_indx;
      ksize = mc->mc_db->md_pad;
      split = LEAF2KEY(mp, split_indx, ksize);
      rsize = (nkeys - split_indx) * ksize;
      lsize = (nkeys - split_indx) * sizeof(indx_t);
      mp->mp_lower -= lsize;
      rp->mp_lower += lsize;
      mp->mp_upper += rsize - lsize;
      rp->mp_upper -= rsize - lsize;
      sepkey.mv_size = ksize;
      if (newindx == split_indx) {
        sepkey.mv_data = newkey->mv_data;
      } else {
        sepkey.mv_data = split;
      }
      if (x<0) {
        ins = LEAF2KEY(mp, mc->mc_ki[mc->mc_top], ksize);
        memcpy(rp->mp_ptrs, split, rsize);
        sepkey.mv_data = rp->mp_ptrs;
        memmove(ins+ksize, ins, (split_indx - mc->mc_ki[mc->mc_top]) * ksize);
        memcpy(ins, newkey->mv_data, ksize);
        mp->mp_lower += sizeof(indx_t);
        mp->mp_upper -= ksize - sizeof(indx_t);
      } else {
        if (x)
          memcpy(rp->mp_ptrs, split, x * ksize);
        ins = LEAF2KEY(rp, x, ksize);
        memcpy(ins, newkey->mv_data, ksize);
        memcpy(ins+ksize, split + x * ksize, rsize - x * ksize);
        rp->mp_lower += sizeof(indx_t);
        rp->mp_upper -= ksize - sizeof(indx_t);
        mc->mc_ki[mc->mc_top] = x;
      }
    } else {
      int psize, nsize, k;
      /* Maximum free space in an empty page */
      pmax = env->me_psize - PAGEHDRSZ;
      if (IS_LEAF(mp))
        nsize = mdb_leaf_size(env, newkey, newdata);
      else
        nsize = mdb_branch_size(env, newkey);
      nsize = EVEN(nsize);

      /* grab a page to hold a temporary copy */
      copy = mdb_page_malloc(mc->mc_txn, 1);
      if (copy == NULL) {
        rc = ENOMEM;
        goto done;
      }
      copy->mp_pgno  = mp->mp_pgno;
      copy->mp_flags = mp->mp_flags;
      copy->mp_lower = (PAGEHDRSZ-PAGEBASE);
      copy->mp_upper = env->me_psize - PAGEBASE;

      /* prepare to insert */
      for (i=0, j=0; i<nkeys; i++) {
        if (i == newindx) {
          copy->mp_ptrs[j++] = 0;
        }
        copy->mp_ptrs[j++] = mp->mp_ptrs[i];
      }

      /* When items are relatively large the split point needs
       * to be checked, because being off-by-one will make the
       * difference between success or failure in mdb_node_add.
       *
       * It's also relevant if a page happens to be laid out
       * such that one half of its nodes are all "small" and
       * the other half of its nodes are "large." If the new
       * item is also "large" and falls on the half with
       * "large" nodes, it also may not fit.
       *
       * As a final tweak, if the new item goes on the last
       * spot on the page (and thus, onto the new page), bias
       * the split so the new page is emptier than the old page.
       * This yields better packing during sequential inserts.
       */
      if (nkeys < 32 || nsize > pmax/16 || newindx >= nkeys) {
        /* Find split point */
        psize = 0;
        if (newindx <= split_indx || newindx >= nkeys) {
          i = 0; j = 1;
          k = newindx >= nkeys ? nkeys : split_indx+1+IS_LEAF(mp);
        } else {
          i = nkeys; j = -1;
          k = split_indx-1;
        }
        for (; i!=k; i+=j) {
          if (i == newindx) {
            psize += nsize;
            node = NULL;
          } else {
            node = (MDB_node *)((char *)mp + copy->mp_ptrs[i] + PAGEBASE);
            psize += NODESIZE + NODEKSZ(node) + sizeof(indx_t);
            if (IS_LEAF(mp)) {
              if (F_ISSET(node->mn_flags, F_BIGDATA))
                psize += sizeof(pgno_t);
              else
                psize += NODEDSZ(node);
            }
            psize = EVEN(psize);
          }
          if (psize > pmax || i == k-j) {
            split_indx = i + (j<0);
            break;
          }
        }
      }
      if (split_indx == newindx) {
        sepkey.mv_size = newkey->mv_size;
        sepkey.mv_data = newkey->mv_data;
      } else {
        node = (MDB_node *)((char *)mp + copy->mp_ptrs[split_indx] + PAGEBASE);
        sepkey.mv_size = node->mn_ksize;
        sepkey.mv_data = NODEKEY(node);
      }
    }
  }

  DPRINTF(("separator is %d [%s]", split_indx, DKEY(&sepkey)));

  /* Copy separator key to the parent.
   */
  if (SIZELEFT(mn.mc_pg[ptop]) < mdb_branch_size(env, &sepkey)) {
    int snum = mc->mc_snum;
    mn.mc_snum--;
    mn.mc_top--;
    did_split = 1;
    /* We want other splits to find mn when doing fixups */
    WITH_CURSOR_TRACKING(mn,
      rc = mdb_page_split(&mn, &sepkey, NULL, rp->mp_pgno, 0));
    if (rc)
      goto done;

    /* root split? */
    if (mc->mc_snum > snum) {
      ptop++;
    }
    /* Right page might now have changed parent.
     * Check if left page also changed parent.
     */
    if (mn.mc_pg[ptop] != mc->mc_pg[ptop] &&
        mc->mc_ki[ptop] >= NUMKEYS(mc->mc_pg[ptop])) {
      for (i=0; i<ptop; i++) {
        mc->mc_pg[i] = mn.mc_pg[i];
        mc->mc_ki[i] = mn.mc_ki[i];
      }
      mc->mc_pg[ptop] = mn.mc_pg[ptop];
      if (mn.mc_ki[ptop]) {
        mc->mc_ki[ptop] = mn.mc_ki[ptop] - 1;
      } else {
        /* find right page's left sibling */
        mc->mc_ki[ptop] = mn.mc_ki[ptop];
        rc = mdb_cursor_sibling(mc, 0);
      }
    }
  } else {
    mn.mc_top--;
    rc = mdb_node_add(&mn, mn.mc_ki[ptop], &sepkey, NULL, rp->mp_pgno, 0);
    mn.mc_top++;
  }
  if (rc != MDB_SUCCESS) {
    if (rc == MDB_NOTFOUND) /* improper mdb_cursor_sibling() result */
      rc = MDB_PROBLEM;
    goto done;
  }
  if (nflags & MDB_APPEND) {
    mc->mc_pg[mc->mc_top] = rp;
    mc->mc_ki[mc->mc_top] = 0;
    rc = mdb_node_add(mc, 0, newkey, newdata, newpgno, nflags);
    if (rc)
      goto done;
    for (i=0; i<mc->mc_top; i++)
      mc->mc_ki[i] = mn.mc_ki[i];
  } else if (!IS_LEAF2(mp)) {
    /* Move nodes */
    mc->mc_pg[mc->mc_top] = rp;
    i = split_indx;
    j = 0;
    do {
      if (i == newindx) {
        rkey.mv_data = newkey->mv_data;
        rkey.mv_size = newkey->mv_size;
        if (IS_LEAF(mp)) {
          rdata = newdata;
        } else
          pgno = newpgno;
        flags = nflags;
        /* Update index for the new key. */
        mc->mc_ki[mc->mc_top] = j;
      } else {
        node = (MDB_node *)((char *)mp + copy->mp_ptrs[i] + PAGEBASE);
        rkey.mv_data = NODEKEY(node);
        rkey.mv_size = node->mn_ksize;
        if (IS_LEAF(mp)) {
          xdata.mv_data = NODEDATA(node);
          xdata.mv_size = NODEDSZ(node);
          rdata = &xdata;
        } else
          pgno = NODEPGNO(node);
        flags = node->mn_flags;
      }

      if (!IS_LEAF(mp) && j == 0) {
        /* First branch index doesn't need key data. */
        rkey.mv_size = 0;
      }

      rc = mdb_node_add(mc, j, &rkey, rdata, pgno, flags);
      if (rc)
        goto done;
      if (i == nkeys) {
        i = 0;
        j = 0;
        mc->mc_pg[mc->mc_top] = copy;
      } else {
        i++;
        j++;
      }
    } while (i != split_indx);

    nkeys = NUMKEYS(copy);
    for (i=0; i<nkeys; i++)
      mp->mp_ptrs[i] = copy->mp_ptrs[i];
    mp->mp_lower = copy->mp_lower;
    mp->mp_upper = copy->mp_upper;
    memcpy(NODEPTR(mp, nkeys-1), NODEPTR(copy, nkeys-1),
      env->me_psize - copy->mp_upper - PAGEBASE);

    /* reset back to original page */
    if (newindx < split_indx) {
      mc->mc_pg[mc->mc_top] = mp;
    } else {
      mc->mc_pg[mc->mc_top] = rp;
      mc->mc_ki[ptop]++;
      /* Make sure mc_ki is still valid.
       */
      if (mn.mc_pg[ptop] != mc->mc_pg[ptop] &&
        mc->mc_ki[ptop] >= NUMKEYS(mc->mc_pg[ptop])) {
        for (i=0; i<=ptop; i++) {
          mc->mc_pg[i] = mn.mc_pg[i];
          mc->mc_ki[i] = mn.mc_ki[i];
        }
      }
    }
    if (nflags & MDB_RESERVE) {
      node = NODEPTR(mc->mc_pg[mc->mc_top], mc->mc_ki[mc->mc_top]);
      if (!(node->mn_flags & F_BIGDATA))
        newdata->mv_data = NODEDATA(node);
    }
  } else {
    if (newindx >= split_indx) {
      mc->mc_pg[mc->mc_top] = rp;
      mc->mc_ki[ptop]++;
      /* Make sure mc_ki is still valid.
       */
      if (mn.mc_pg[ptop] != mc->mc_pg[ptop] &&
        mc->mc_ki[ptop] >= NUMKEYS(mc->mc_pg[ptop])) {
        for (i=0; i<=ptop; i++) {
          mc->mc_pg[i] = mn.mc_pg[i];
          mc->mc_ki[i] = mn.mc_ki[i];
        }
      }
    }
  }

  {
    /* Adjust other cursors pointing to mp */
    MDB_cursor *m2, *m3;
    MDB_dbi dbi = mc->mc_dbi;
    nkeys = NUMKEYS(mp);

    for (m2 = mc->mc_txn->mt_cursors[dbi]; m2; m2=m2->mc_next) {
      if (mc->mc_flags & C_SUB)
        m3 = &m2->mc_xcursor->mx_cursor;
      else
        m3 = m2;
      if (m3 == mc)
        continue;
      if (!(m2->mc_flags & m3->mc_flags & C_INITIALIZED))
        continue;
      if (new_root) {
        int k;
        /* sub cursors may be on different DB */
        if (m3->mc_pg[0] != mp)
          continue;
        /* root split */
        for (k=new_root; k>=0; k--) {
          m3->mc_ki[k+1] = m3->mc_ki[k];
          m3->mc_pg[k+1] = m3->mc_pg[k];
        }
        if (m3->mc_ki[0] >= nkeys) {
          m3->mc_ki[0] = 1;
        } else {
          m3->mc_ki[0] = 0;
        }
        m3->mc_pg[0] = mc->mc_pg[0];
        m3->mc_snum++;
        m3->mc_top++;
      }
      if (m3->mc_top >= mc->mc_top && m3->mc_pg[mc->mc_top] == mp) {
        if (m3->mc_ki[mc->mc_top] >= newindx && !(nflags & MDB_SPLIT_REPLACE))
          m3->mc_ki[mc->mc_top]++;
        if (m3->mc_ki[mc->mc_top] >= nkeys) {
          m3->mc_pg[mc->mc_top] = rp;
          m3->mc_ki[mc->mc_top] -= nkeys;
          for (i=0; i<mc->mc_top; i++) {
            m3->mc_ki[i] = mn.mc_ki[i];
            m3->mc_pg[i] = mn.mc_pg[i];
          }
        }
      } else if (!did_split && m3->mc_top >= ptop && m3->mc_pg[ptop] == mc->mc_pg[ptop] &&
        m3->mc_ki[ptop] >= mc->mc_ki[ptop]) {
        m3->mc_ki[ptop]++;
      }
      if (IS_LEAF(mp))
        XCURSOR_REFRESH(m3, mc->mc_top, m3->mc_pg[mc->mc_top]);
    }
  }
  DPRINTF(("mp left: %d, rp left: %d", SIZELEFT(mp), SIZELEFT(rp)));

done:
  if (copy)          /* tmp page */
    mdb_page_free(env, copy);
  if (rc)
    mc->mc_txn->mt_flags |= MDB_TXN_ERROR;
  return rc;
}

int
mdb_put(MDB_txn *txn, MDB_dbi dbi,
    MDB_val *key, MDB_val *data, unsigned int flags)
{
  MDB_cursor mc;
  MDB_xcursor mx;
  int rc;

  if (!key || !data || !TXN_DBI_EXIST(txn, dbi, DB_USRVALID))
    return EINVAL;

  if (flags & ~(MDB_NOOVERWRITE|MDB_NODUPDATA|MDB_RESERVE|MDB_APPEND|MDB_APPENDDUP))
    return EINVAL;

  if (txn->mt_flags & (MDB_TXN_RDONLY|MDB_TXN_BLOCKED))
    return (txn->mt_flags & MDB_TXN_RDONLY) ? EACCES : MDB_BAD_TXN;

  mdb_cursor_init(&mc, txn, dbi, &mx);
  mc.mc_next = txn->mt_cursors[dbi];
  txn->mt_cursors[dbi] = &mc;
  rc = mdb_cursor_put(&mc, key, data, flags);
  txn->mt_cursors[dbi] = mc.mc_next;
  return rc;
}

#ifndef MDB_WBUF
#define MDB_WBUF  (1024*1024)
#endif
#define MDB_EOF    0x10  /**< #mdb_env_copyfd1() is done reading */

  /** State needed for a double-buffering compacting copy. */
typedef struct mdb_copy {
  MDB_env *mc_env;
  MDB_txn *mc_txn;
  pthread_mutex_t mc_mutex;
  pthread_cond_t mc_cond;  /**< Condition variable for #mc_new */
  char *mc_wbuf[2];
  char *mc_over[2];
  int mc_wlen[2];
  int mc_olen[2];
  pgno_t mc_next_pgno;
  HANDLE mc_fd;
  int mc_toggle;      /**< Buffer number in provider */
  int mc_new;        /**< (0-2 buffers to write) | (#MDB_EOF at end) */
  /** Error code.  Never cleared if set.  Both threads can set nonzero
   *  to fail the copy.  Not mutex-protected, LMDB expects atomic int.
   */
  volatile int mc_error;
} mdb_copy;

  /** Dedicated writer thread for compacting copy. */
static THREAD_RET ESECT CALL_CONV
mdb_env_copythr(void *arg)
{
  mdb_copy *my = arg;
  char *ptr;
  int toggle = 0, wsize, rc;
  int len;
#define DO_WRITE(rc, fd, ptr, w2, len)  len = write(fd, ptr, w2); rc = (len >= 0)
#ifdef SIGPIPE
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGPIPE);
  if ((rc = pthread_sigmask(SIG_BLOCK, &set, NULL)) != 0)
    my->mc_error = rc;
#endif

  pthread_mutex_lock(&my->mc_mutex);
  for(;;) {
    while (!my->mc_new)
      pthread_cond_wait(&my->mc_cond, &my->mc_mutex);
    if (my->mc_new == 0 + MDB_EOF) /* 0 buffers, just EOF */
      break;
    wsize = my->mc_wlen[toggle];
    ptr = my->mc_wbuf[toggle];
again:
    rc = MDB_SUCCESS;
    while (wsize > 0 && !my->mc_error) {
      DO_WRITE(rc, my->mc_fd, ptr, wsize, len);
      if (!rc) {
        rc = ErrCode();
#if defined(SIGPIPE)
        if (rc == EPIPE) {
          /* Collect the pending SIGPIPE, otherwise at least OS X
           * gives it to the process on thread-exit (ITS#8504).
           */
          int tmp;
          sigwait(&set, &tmp);
        }
#endif
        break;
      } else if (len > 0) {
        rc = MDB_SUCCESS;
        ptr += len;
        wsize -= len;
        continue;
      } else {
        rc = EIO;
        break;
      }
    }
    if (rc) {
      my->mc_error = rc;
    }
    /* If there's an overflow page tail, write it too */
    if (my->mc_olen[toggle]) {
      wsize = my->mc_olen[toggle];
      ptr = my->mc_over[toggle];
      my->mc_olen[toggle] = 0;
      goto again;
    }
    my->mc_wlen[toggle] = 0;
    toggle ^= 1;
    /* Return the empty buffer to provider */
    my->mc_new--;
    pthread_cond_signal(&my->mc_cond);
  }
  pthread_mutex_unlock(&my->mc_mutex);
  return (THREAD_RET)0;
#undef DO_WRITE
}

  /** Give buffer and/or #MDB_EOF to writer thread, await unused buffer.
   *
   * @param[in] my control structure.
   * @param[in] adjust (1 to hand off 1 buffer) | (MDB_EOF when ending).
   */
static int ESECT
mdb_env_cthr_toggle(mdb_copy *my, int adjust)
{
  pthread_mutex_lock(&my->mc_mutex);
  my->mc_new += adjust;
  pthread_cond_signal(&my->mc_cond);
  while (my->mc_new & 2)    /* both buffers in use */
    pthread_cond_wait(&my->mc_cond, &my->mc_mutex);
  pthread_mutex_unlock(&my->mc_mutex);

  my->mc_toggle ^= (adjust & 1);
  /* Both threads reset mc_wlen, to be safe from threading errors */
  my->mc_wlen[my->mc_toggle] = 0;
  return my->mc_error;
}

  /** Depth-first tree traversal for compacting copy.
   * @param[in] my control structure.
   * @param[in,out] pg database root.
   * @param[in] flags includes #F_DUPDATA if it is a sorted-duplicate sub-DB.
   */
static int ESECT
mdb_env_cwalk(mdb_copy *my, pgno_t *pg, int flags)
{
  MDB_cursor mc = {0};
  MDB_node *ni;
  MDB_page *mo, *mp, *leaf;
  char *buf, *ptr;
  int rc, toggle;
  unsigned int i;

  /* Empty DB, nothing to do */
  if (*pg == P_INVALID)
    return MDB_SUCCESS;

  mc.mc_snum = 1;
  mc.mc_txn = my->mc_txn;
  mc.mc_flags = my->mc_txn->mt_flags & (C_ORIG_RDONLY|C_WRITEMAP);

  rc = mdb_page_get(&mc, *pg, &mc.mc_pg[0], NULL);
  if (rc)
    return rc;
  rc = mdb_page_search_root(&mc, NULL, MDB_PS_FIRST);
  if (rc)
    return rc;

  /* Make cursor pages writable */
  buf = ptr = malloc(my->mc_env->me_psize * mc.mc_snum);
  if (buf == NULL)
    return ENOMEM;

  for (i=0; i<mc.mc_top; i++) {
    mdb_page_copy((MDB_page *)ptr, mc.mc_pg[i], my->mc_env->me_psize);
    mc.mc_pg[i] = (MDB_page *)ptr;
    ptr += my->mc_env->me_psize;
  }

  /* This is writable space for a leaf page. Usually not needed. */
  leaf = (MDB_page *)ptr;

  toggle = my->mc_toggle;
  while (mc.mc_snum > 0) {
    unsigned n;
    mp = mc.mc_pg[mc.mc_top];
    n = NUMKEYS(mp);

    if (IS_LEAF(mp)) {
      if (!IS_LEAF2(mp) && !(flags & F_DUPDATA)) {
        for (i=0; i<n; i++) {
          ni = NODEPTR(mp, i);
          if (ni->mn_flags & F_BIGDATA) {
            MDB_page *omp;
            pgno_t pg;

            /* Need writable leaf */
            if (mp != leaf) {
              mc.mc_pg[mc.mc_top] = leaf;
              mdb_page_copy(leaf, mp, my->mc_env->me_psize);
              mp = leaf;
              ni = NODEPTR(mp, i);
            }

            memcpy(&pg, NODEDATA(ni), sizeof(pg));
            memcpy(NODEDATA(ni), &my->mc_next_pgno, sizeof(pgno_t));
            rc = mdb_page_get(&mc, pg, &omp, NULL);
            if (rc)
              goto done;
            if (my->mc_wlen[toggle] >= MDB_WBUF) {
              rc = mdb_env_cthr_toggle(my, 1);
              if (rc)
                goto done;
              toggle = my->mc_toggle;
            }
            mo = (MDB_page *)(my->mc_wbuf[toggle] + my->mc_wlen[toggle]);
            memcpy(mo, omp, my->mc_env->me_psize);
            mo->mp_pgno = my->mc_next_pgno;
            my->mc_next_pgno += omp->mp_pages;
            my->mc_wlen[toggle] += my->mc_env->me_psize;
            if (omp->mp_pages > 1) {
              my->mc_olen[toggle] = my->mc_env->me_psize * (omp->mp_pages - 1);
              my->mc_over[toggle] = (char *)omp + my->mc_env->me_psize;
              rc = mdb_env_cthr_toggle(my, 1);
              if (rc)
                goto done;
              toggle = my->mc_toggle;
            }
          } else if (ni->mn_flags & F_SUBDATA) {
            MDB_db db;

            /* Need writable leaf */
            if (mp != leaf) {
              mc.mc_pg[mc.mc_top] = leaf;
              mdb_page_copy(leaf, mp, my->mc_env->me_psize);
              mp = leaf;
              ni = NODEPTR(mp, i);
            }

            memcpy(&db, NODEDATA(ni), sizeof(db));
            my->mc_toggle = toggle;
            rc = mdb_env_cwalk(my, &db.md_root, ni->mn_flags & F_DUPDATA);
            if (rc)
              goto done;
            toggle = my->mc_toggle;
            memcpy(NODEDATA(ni), &db, sizeof(db));
          }
        }
      }
    } else {
      mc.mc_ki[mc.mc_top]++;
      if (mc.mc_ki[mc.mc_top] < n) {
        pgno_t pg;
again:
        ni = NODEPTR(mp, mc.mc_ki[mc.mc_top]);
        pg = NODEPGNO(ni);
        rc = mdb_page_get(&mc, pg, &mp, NULL);
        if (rc)
          goto done;
        mc.mc_top++;
        mc.mc_snum++;
        mc.mc_ki[mc.mc_top] = 0;
        if (IS_BRANCH(mp)) {
          /* Whenever we advance to a sibling branch page,
           * we must proceed all the way down to its first leaf.
           */
          mdb_page_copy(mc.mc_pg[mc.mc_top], mp, my->mc_env->me_psize);
          goto again;
        } else
          mc.mc_pg[mc.mc_top] = mp;
        continue;
      }
    }
    if (my->mc_wlen[toggle] >= MDB_WBUF) {
      rc = mdb_env_cthr_toggle(my, 1);
      if (rc)
        goto done;
      toggle = my->mc_toggle;
    }
    mo = (MDB_page *)(my->mc_wbuf[toggle] + my->mc_wlen[toggle]);
    mdb_page_copy(mo, mp, my->mc_env->me_psize);
    mo->mp_pgno = my->mc_next_pgno++;
    my->mc_wlen[toggle] += my->mc_env->me_psize;
    if (mc.mc_top) {
      /* Update parent if there is one */
      ni = NODEPTR(mc.mc_pg[mc.mc_top-1], mc.mc_ki[mc.mc_top-1]);
      SETPGNO(ni, mo->mp_pgno);
      mdb_cursor_pop(&mc);
    } else {
      /* Otherwise we're done */
      *pg = mo->mp_pgno;
      break;
    }
  }
done:
  free(buf);
  return rc;
}

  /** Copy environment with compaction. */
static int ESECT
mdb_env_copyfd1(MDB_env *env, HANDLE fd)
{
  MDB_meta *mm;
  MDB_page *mp;
  mdb_copy my = {0};
  MDB_txn *txn = NULL;
  pthread_t thr;
  pgno_t root, new_root;
  int rc = MDB_SUCCESS;

  if ((rc = pthread_mutex_init(&my.mc_mutex, NULL)) != 0)
    return rc;
  if ((rc = pthread_cond_init(&my.mc_cond, NULL)) != 0)
    goto done2;
# ifdef HAVE_MEMALIGN
  my.mc_wbuf[0] = memalign(env->me_os_psize, MDB_WBUF*2);
  if (my.mc_wbuf[0] == NULL) {
    rc = errno;
    goto done;
  }
# else
  {
    void *p;
    if ((rc = posix_memalign(&p, env->me_os_psize, MDB_WBUF*2)) != 0)
      goto done;
    my.mc_wbuf[0] = p;
  }
# endif
  memset(my.mc_wbuf[0], 0, MDB_WBUF*2);
  my.mc_wbuf[1] = my.mc_wbuf[0] + MDB_WBUF;
  my.mc_next_pgno = NUM_METAS;
  my.mc_env = env;
  my.mc_fd = fd;
  rc = THREAD_CREATE(thr, mdb_env_copythr, &my);
  if (rc)
    goto done;

  rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
  if (rc)
    goto finish;

  mp = (MDB_page *)my.mc_wbuf[0];
  memset(mp, 0, NUM_METAS * env->me_psize);
  mp->mp_pgno = 0;
  mp->mp_flags = P_META;
  mm = (MDB_meta *)METADATA(mp);
  mdb_env_init_meta0(env, mm);
  mm->mm_address = env->me_metas[0]->mm_address;

  mp = (MDB_page *)(my.mc_wbuf[0] + env->me_psize);
  mp->mp_pgno = 1;
  mp->mp_flags = P_META;
  *(MDB_meta *)METADATA(mp) = *mm;
  mm = (MDB_meta *)METADATA(mp);

  /* Set metapage 1 with current main DB */
  root = new_root = txn->mt_dbs[MAIN_DBI].md_root;
  if (root != P_INVALID) {
    /* Count free pages + freeDB pages.  Subtract from last_pg
     * to find the new last_pg, which also becomes the new root.
     */
    MDB_ID freecount = 0;
    MDB_cursor mc;
    MDB_val key, data;
    mdb_cursor_init(&mc, txn, FREE_DBI, NULL);
    while ((rc = mdb_cursor_get(&mc, &key, &data, MDB_NEXT)) == 0)
      freecount += *(MDB_ID *)data.mv_data;
    if (rc != MDB_NOTFOUND)
      goto finish;
    freecount += txn->mt_dbs[FREE_DBI].md_branch_pages +
      txn->mt_dbs[FREE_DBI].md_leaf_pages +
      txn->mt_dbs[FREE_DBI].md_overflow_pages;

    new_root = txn->mt_next_pgno - 1 - freecount;
    mm->mm_last_pg = new_root;
    mm->mm_dbs[MAIN_DBI] = txn->mt_dbs[MAIN_DBI];
    mm->mm_dbs[MAIN_DBI].md_root = new_root;
  } else {
    /* When the DB is empty, handle it specially to
     * fix any breakage like page leaks from ITS#8174.
     */
    mm->mm_dbs[MAIN_DBI].md_flags = txn->mt_dbs[MAIN_DBI].md_flags;
  }
  if (root != P_INVALID || mm->mm_dbs[MAIN_DBI].md_flags) {
    mm->mm_txnid = 1;    /* use metapage 1 */
  }

  my.mc_wlen[0] = env->me_psize * NUM_METAS;
  my.mc_txn = txn;
  rc = mdb_env_cwalk(&my, &root, 0);
  if (rc == MDB_SUCCESS && root != new_root) {
    rc = MDB_INCOMPATIBLE;  /* page leak or corrupt DB */
  }

finish:
  if (rc)
    my.mc_error = rc;
  mdb_env_cthr_toggle(&my, 1 | MDB_EOF);
  rc = THREAD_FINISH(thr);
  mdb_txn_abort(txn);

done:
  free(my.mc_wbuf[0]);
  pthread_cond_destroy(&my.mc_cond);
done2:
  pthread_mutex_destroy(&my.mc_mutex);
  return rc ? rc : my.mc_error;
}

  /** Copy environment as-is. */
static int ESECT
mdb_env_copyfd0(MDB_env *env, HANDLE fd)
{
  MDB_txn *txn = NULL;
  mdb_mutexref_t wmutex = NULL;
  int rc;
  mdb_size_t wsize, w3;
  char *ptr;
  ssize_t len;
  size_t w2;
#define DO_WRITE(rc, fd, ptr, w2, len)  len = write(fd, ptr, w2); rc = (len >= 0)

  /* Do the lock/unlock of the reader mutex before starting the
   * write txn.  Otherwise other read txns could block writers.
   */
  rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
  if (rc)
    return rc;

  if (env->me_txns) {
    /* We must start the actual read txn after blocking writers */
    mdb_txn_end(txn, MDB_END_RESET_TMP);

    /* Temporarily block writers until we snapshot the meta pages */
    wmutex = env->me_wmutex;
    if (LOCK_MUTEX(rc, env, wmutex))
      goto leave;

    rc = mdb_txn_renew0(txn);
    if (rc) {
      UNLOCK_MUTEX(wmutex);
      goto leave;
    }
  }

  wsize = env->me_psize * NUM_METAS;
  ptr = env->me_map;
  w2 = wsize;
  while (w2 > 0) {
    DO_WRITE(rc, fd, ptr, w2, len);
    if (!rc) {
      rc = ErrCode();
      break;
    } else if (len > 0) {
      rc = MDB_SUCCESS;
      ptr += len;
      w2 -= len;
      continue;
    } else {
      /* Non-blocking or async handles are not supported */
      rc = EIO;
      break;
    }
  }
  if (wmutex)
    UNLOCK_MUTEX(wmutex);

  if (rc)
    goto leave;

  w3 = txn->mt_next_pgno * env->me_psize;
  {
    mdb_size_t fsize = 0;
    if ((rc = mdb_fsize(env->me_fd, &fsize)))
      goto leave;
    if (w3 > fsize)
      w3 = fsize;
  }
  wsize = w3 - wsize;
  while (wsize > 0) {
    if (wsize > MAX_WRITE)
      w2 = MAX_WRITE;
    else
      w2 = wsize;
    DO_WRITE(rc, fd, ptr, w2, len);
    if (!rc) {
      rc = ErrCode();
      break;
    } else if (len > 0) {
      rc = MDB_SUCCESS;
      ptr += len;
      wsize -= len;
      continue;
    } else {
      rc = EIO;
      break;
    }
  }

leave:
  mdb_txn_abort(txn);
  return rc;
}

int ESECT
mdb_env_copyfd2(MDB_env *env, HANDLE fd, unsigned int flags)
{
  if (flags & MDB_CP_COMPACT)
    return mdb_env_copyfd1(env, fd);
  else
    return mdb_env_copyfd0(env, fd);
}

int ESECT
mdb_env_copyfd(MDB_env *env, HANDLE fd)
{
  return mdb_env_copyfd2(env, fd, 0);
}

int ESECT
mdb_env_copy2(MDB_env *env, const char *path, unsigned int flags)
{
  int rc;
  MDB_name fname;
  HANDLE newfd = INVALID_HANDLE_VALUE;

  rc = mdb_fname_init(path, env->me_flags | MDB_NOLOCK, &fname);
  if (rc == MDB_SUCCESS) {
    rc = mdb_fopen(env, &fname, MDB_O_COPY, 0666, &newfd);
    mdb_fname_destroy(fname);
  }
  if (rc == MDB_SUCCESS) {
    rc = mdb_env_copyfd2(env, newfd, flags);
    if (close(newfd) < 0 && rc == MDB_SUCCESS)
      rc = ErrCode();
  }
  return rc;
}

int ESECT
mdb_env_copy(MDB_env *env, const char *path)
{
  return mdb_env_copy2(env, path, 0);
}

int ESECT
mdb_env_set_flags(MDB_env *env, unsigned int flag, int onoff)
{
  if (flag & ~CHANGEABLE)
    return EINVAL;
  if (onoff)
    env->me_flags |= flag;
  else
    env->me_flags &= ~flag;
  return MDB_SUCCESS;
}

int ESECT
mdb_env_get_flags(MDB_env *env, unsigned int *arg)
{
  if (!env || !arg)
    return EINVAL;

  *arg = env->me_flags & (CHANGEABLE|CHANGELESS);
  return MDB_SUCCESS;
}

int ESECT
mdb_env_set_userctx(MDB_env *env, void *ctx)
{
  if (!env)
    return EINVAL;
  env->me_userctx = ctx;
  return MDB_SUCCESS;
}

void * ESECT
mdb_env_get_userctx(MDB_env *env)
{
  return env ? env->me_userctx : NULL;
}

int ESECT
mdb_env_set_assert(MDB_env *env, MDB_assert_func *func)
{
  if (!env)
    return EINVAL;
#ifndef NDEBUG
  env->me_assert_func = func;
#endif
  return MDB_SUCCESS;
}

int ESECT
mdb_env_get_path(MDB_env *env, const char **arg)
{
  if (!env || !arg)
    return EINVAL;

  *arg = env->me_path;
  return MDB_SUCCESS;
}

int ESECT
mdb_env_get_fd(MDB_env *env, mdb_filehandle_t *arg)
{
  if (!env || !arg)
    return EINVAL;

  *arg = env->me_fd;
  return MDB_SUCCESS;
}

/** Common code for #mdb_stat() and #mdb_env_stat().
 * @param[in] env the environment to operate in.
 * @param[in] db the #MDB_db record containing the stats to return.
 * @param[out] arg the address of an #MDB_stat structure to receive the stats.
 * @return 0, this function always succeeds.
 */
static int ESECT
mdb_stat0(MDB_env *env, MDB_db *db, MDB_stat *arg)
{
  arg->ms_psize = env->me_psize;
  arg->ms_depth = db->md_depth;
  arg->ms_branch_pages = db->md_branch_pages;
  arg->ms_leaf_pages = db->md_leaf_pages;
  arg->ms_overflow_pages = db->md_overflow_pages;
  arg->ms_entries = db->md_entries;

  return MDB_SUCCESS;
}

int ESECT
mdb_env_stat(MDB_env *env, MDB_stat *arg)
{
  MDB_meta *meta;

  if (env == NULL || arg == NULL)
    return EINVAL;

  meta = mdb_env_pick_meta(env);

  return mdb_stat0(env, &meta->mm_dbs[MAIN_DBI], arg);
}

int ESECT
mdb_env_info(MDB_env *env, MDB_envinfo *arg)
{
  MDB_meta *meta;

  if (env == NULL || arg == NULL)
    return EINVAL;

  meta = mdb_env_pick_meta(env);
  arg->me_mapaddr = meta->mm_address;
  arg->me_last_pgno = meta->mm_last_pg;
  arg->me_last_txnid = meta->mm_txnid;

  arg->me_mapsize = env->me_mapsize;
  arg->me_numreaders = env->me_txns ? env->me_txns->mti_numreaders : 0;
  return MDB_SUCCESS;
}

/** Set the default comparison functions for a database.
 * Called immediately after a database is opened to set the defaults.
 * The user can then override them with #mdb_set_compare() or
 * #mdb_set_dupsort().
 * @param[in] txn A transaction handle returned by #mdb_txn_begin()
 * @param[in] dbi A database handle returned by #mdb_dbi_open()
 */
static void
mdb_default_cmp(MDB_txn *txn, MDB_dbi dbi)
{
  uint16_t f = txn->mt_dbs[dbi].md_flags;

  txn->mt_dbxs[dbi].md_cmp =
    (f & MDB_REVERSEKEY) ? mdb_cmp_memnr :
    (f & MDB_INTEGERKEY) ? mdb_cmp_cint  : mdb_cmp_memn;

  txn->mt_dbxs[dbi].md_dcmp =
    !(f & MDB_DUPSORT) ? 0 :
    ((f & MDB_INTEGERDUP)
     ? ((f & MDB_DUPFIXED)   ? mdb_cmp_int   : mdb_cmp_cint)
     : ((f & MDB_REVERSEDUP) ? mdb_cmp_memnr : mdb_cmp_memn));
}

/** Add all the DB's pages to the free list.
 * @param[in] mc Cursor on the DB to free.
 * @param[in] subs non-Zero to check for sub-DBs in this DB.
 * @return 0 on success, non-zero on failure.
 */
static int
mdb_drop0(MDB_cursor *mc, int subs)
{
  int rc;

  rc = mdb_page_search(mc, NULL, MDB_PS_FIRST);
  if (rc == MDB_SUCCESS) {
    MDB_txn *txn = mc->mc_txn;
    MDB_node *ni;
    MDB_cursor mx;
    unsigned int i;

    /* DUPSORT sub-DBs have no ovpages/DBs. Omit scanning leaves.
     * This also avoids any P_LEAF2 pages, which have no nodes.
     * Also if the DB doesn't have sub-DBs and has no overflow
     * pages, omit scanning leaves.
     */
    if ((mc->mc_flags & C_SUB) ||
      (!subs && !mc->mc_db->md_overflow_pages))
      mdb_cursor_pop(mc);

    mdb_cursor_copy(mc, &mx);
    while (mc->mc_snum > 0) {
      MDB_page *mp = mc->mc_pg[mc->mc_top];
      unsigned n = NUMKEYS(mp);
      if (IS_LEAF(mp)) {
        for (i=0; i<n; i++) {
          ni = NODEPTR(mp, i);
          if (ni->mn_flags & F_BIGDATA) {
            MDB_page *omp;
            pgno_t pg;
            memcpy(&pg, NODEDATA(ni), sizeof(pg));
            rc = mdb_page_get(mc, pg, &omp, NULL);
            if (rc != 0)
              goto done;
            mdb_cassert(mc, IS_OVERFLOW(omp));
            rc = mdb_midl_append_range(&txn->mt_free_pgs,
              pg, omp->mp_pages);
            if (rc)
              goto done;
            mc->mc_db->md_overflow_pages -= omp->mp_pages;
            if (!mc->mc_db->md_overflow_pages && !subs)
              break;
          } else if (subs && (ni->mn_flags & F_SUBDATA)) {
            mdb_xcursor_init1(mc, ni);
            rc = mdb_drop0(&mc->mc_xcursor->mx_cursor, 0);
            if (rc)
              goto done;
          }
        }
        if (!subs && !mc->mc_db->md_overflow_pages)
          goto pop;
      } else {
        if ((rc = mdb_midl_need(&txn->mt_free_pgs, n)) != 0)
          goto done;
        for (i=0; i<n; i++) {
          pgno_t pg;
          ni = NODEPTR(mp, i);
          pg = NODEPGNO(ni);
          /* free it */
          mdb_midl_xappend(txn->mt_free_pgs, pg);
        }
      }
      if (!mc->mc_top)
        break;
      mc->mc_ki[mc->mc_top] = i;
      rc = mdb_cursor_sibling(mc, 1);
      if (rc) {
        if (rc != MDB_NOTFOUND)
          goto done;
        /* no more siblings, go back to beginning
         * of previous level.
         */
pop:
        mdb_cursor_pop(mc);
        mc->mc_ki[0] = 0;
        for (i=1; i<mc->mc_snum; i++) {
          mc->mc_ki[i] = 0;
          mc->mc_pg[i] = mx.mc_pg[i];
        }
      }
    }
    /* free it */
    rc = mdb_midl_append(&txn->mt_free_pgs, mc->mc_db->md_root);
done:
    if (rc)
      txn->mt_flags |= MDB_TXN_ERROR;
    /* drop refcount for mx's pages */
    MDB_CURSOR_UNREF(&mx, 0);
  } else if (rc == MDB_NOTFOUND) {
    rc = MDB_SUCCESS;
  }
  mc->mc_flags &= ~C_INITIALIZED;
  return rc;
}

int mdb_drop(MDB_txn *txn, MDB_dbi dbi, int del)
{
  MDB_cursor *mc, *m2;
  int rc;

  if ((unsigned)del > 1 || !TXN_DBI_EXIST(txn, dbi, DB_USRVALID))
    return EINVAL;

  if (F_ISSET(txn->mt_flags, MDB_TXN_RDONLY))
    return EACCES;

  if (TXN_DBI_CHANGED(txn, dbi))
    return MDB_BAD_DBI;

  rc = mdb_cursor_open(txn, dbi, &mc);
  if (rc)
    return rc;

  rc = mdb_drop0(mc, mc->mc_db->md_flags & MDB_DUPSORT);
  /* Invalidate the dropped DB's cursors */
  for (m2 = txn->mt_cursors[dbi]; m2; m2 = m2->mc_next)
    m2->mc_flags &= ~(C_INITIALIZED|C_EOF);
  if (rc)
    goto leave;

  /* Can't delete the main DB */
  if (del && dbi >= CORE_DBS) {
    rc = mdb_del0(txn, MAIN_DBI, &mc->mc_dbx->md_name, NULL, F_SUBDATA);
    if (!rc) {
      txn->mt_dbflags[dbi] = DB_STALE;
      mdb_dbi_close(txn->mt_env, dbi);
    } else {
      txn->mt_flags |= MDB_TXN_ERROR;
    }
  } else {
    /* reset the DB record, mark it dirty */
    txn->mt_dbflags[dbi] |= DB_DIRTY;
    txn->mt_dbs[dbi].md_depth = 0;
    txn->mt_dbs[dbi].md_branch_pages = 0;
    txn->mt_dbs[dbi].md_leaf_pages = 0;
    txn->mt_dbs[dbi].md_overflow_pages = 0;
    txn->mt_dbs[dbi].md_entries = 0;
    txn->mt_dbs[dbi].md_root = P_INVALID;

    txn->mt_flags |= MDB_TXN_DIRTY;
  }
leave:
  mdb_cursor_close(mc);
  return rc;
}

/** Handle #LOCK_MUTEX0() failure.
 * Try to repair the lock file if the mutex owner died.
 * @param[in] env  the environment handle
 * @param[in] mutex  LOCK_MUTEX0() mutex
 * @param[in] rc  LOCK_MUTEX0() error (nonzero)
 * @return 0 on success with the mutex locked, or an error code on failure.
 */
static int ESECT
mdb_mutex_failed(MDB_env *env, mdb_mutexref_t mutex, int rc)
{
  int rlocked, rc2;
  MDB_meta *meta;

  if (rc == MDB_OWNERDEAD) {
    /* We own the mutex. Clean up after dead previous owner. */
    rc = MDB_SUCCESS;
    rlocked = (mutex == env->me_rmutex);
    if (!rlocked) {
      /* Keep mti_txnid updated, otherwise next writer can
       * overwrite data which latest meta page refers to.
       */
      meta = mdb_env_pick_meta(env);
      env->me_txns->mti_txnid = meta->mm_txnid;
      /* env is hosed if the dead thread was ours */
      if (env->me_txn) {
        env->me_flags |= MDB_FATAL_ERROR;
        env->me_txn = NULL;
        rc = MDB_PANIC;
      }
    }
    DPRINTF(("%cmutex owner died, %s", (rlocked ? 'r' : 'w'),
      (rc ? "this process' env is hosed" : "recovering")));
    rc2 = mdb_reader_check0(env, rlocked, NULL);
    if (rc2 == 0)
      rc2 = mdb_mutex_consistent(mutex);
    if (rc || (rc = rc2)) {
      DPRINTF(("LOCK_MUTEX recovery failed, %s", mdb_strerror(rc)));
      UNLOCK_MUTEX(mutex);
    }
  } else {
    DPRINTF(("LOCK_MUTEX failed, %s", mdb_strerror(rc)));
  }

  return rc;
}
/** @} */
