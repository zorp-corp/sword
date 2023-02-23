/** @file lmdb.h */
#ifndef _LMDB_H_
#define _LMDB_H_

#include <sys/types.h>
#include <inttypes.h>
#include <limits.h>

/** Unix permissions for creating files, or dummy definition for Windows */
typedef	int	mdb_mode_t;

#define MDB_FMT_Z	"z"			/**< printf/scanf format modifier for size_t */

/** Unsigned type used for mapsize, entry counts and page/transaction IDs.
 *
 *	It is normally size_t, hence the name. Defining MDB_VL32 makes it
 *	uint64_t, but do not try this unless you know what you are doing.
 */
typedef size_t	mdb_size_t;
#define MDB_SIZE_MAX	SIZE_MAX	/**< max #mdb_size_t */
/** #mdb_size_t printf formats, \b t = one of [diouxX] without quotes */
#define MDB_PRIy(t)	MDB_FMT_Z #t
/** #mdb_size_t scanf formats, \b t = one of [dioux] without quotes */
#define MDB_SCNy(t)	MDB_FMT_Z #t

typedef int mdb_filehandle_t;

/** @brief Opaque structure for a database environment.
 *
 * A DB environment supports multiple databases, all residing in the same
 * shared-memory map.
 */
typedef struct MDB_env MDB_env;

/** @brief Opaque structure for a transaction handle.
 *
 * All database operations require a transaction handle. Transactions may be
 * read-only or read-write.
 */
typedef struct MDB_txn MDB_txn;

/** @brief A handle for an individual database in the DB environment. */
typedef unsigned int	MDB_dbi;

/** @brief Generic structure used for passing keys and data in and out
 * of the database.
 *
 * Values returned from the database are valid only until a subsequent
 * update operation, or the end of the transaction. Do not modify or
 * free them, they commonly point into the database itself.
 *
 * Key sizes must be between 1 and #mdb_env_get_maxkeysize() inclusive.
 * The same applies to data sizes in databases with the #MDB_DUPSORT flag.
 * Other data items can in theory be from 0 to 0xffffffff bytes long.
 */
typedef struct MDB_val {
	size_t		 mv_size;	/**< size of the data item */
	void		*mv_data;	/**< address of the data item */
} MDB_val;

/** @brief A callback function used to compare two keys in a database */
typedef int  (MDB_cmp_func)(const MDB_val *a, const MDB_val *b);

/** @brief A callback function used to relocate a position-dependent data item
 * in a fixed-address database.
 *
 * The \b newptr gives the item's desired address in
 * the memory map, and \b oldptr gives its previous address. The item's actual
 * data resides at the address in \b item.  This callback is expected to walk
 * through the fields of the record in \b item and modify any
 * values based at the \b oldptr address to be relative to the \b newptr address.
 * @param[in,out] item The item that is to be relocated.
 * @param[in] oldptr The previous address.
 * @param[in] newptr The new address to relocate to.
 * @param[in] relctx An application-provided context, set by #mdb_set_relctx().
 * @todo This feature is currently unimplemented.
 */
typedef void (MDB_rel_func)(MDB_val *item, void *oldptr, void *newptr, void *relctx);

/** @defgroup	mdb_env	Environment Flags
 *	@{
 */
	/** mmap at a fixed address (experimental) */
#define MDB_FIXEDMAP	0x01
	/** no environment directory */
#define MDB_NOSUBDIR	0x4000
	/** don't fsync after commit */
#define MDB_NOSYNC		0x10000
	/** read only */
#define MDB_RDONLY		0x20000
	/** don't fsync metapage after commit */
#define MDB_NOMETASYNC		0x40000
	/** use writable mmap */
#define MDB_WRITEMAP		0x80000
	/** use asynchronous msync when #MDB_WRITEMAP is used */
#define MDB_MAPASYNC		0x100000
	/** tie reader locktable slots to #MDB_txn objects instead of to threads */
#define MDB_NOTLS		0x200000
	/** don't do any locking, caller must manage their own locks */
#define MDB_NOLOCK		0x400000
	/** don't do readahead (no effect on Windows) */
#define MDB_NORDAHEAD	0x800000
	/** don't initialize malloc'd memory before writing to datafile */
#define MDB_NOMEMINIT	0x1000000
	/** use the previous snapshot rather than the latest one */
#define MDB_PREVSNAPSHOT	0x2000000
/** @} */

/**	@defgroup	mdb_dbi_open	Database Flags
 *	@{
 */
	/** use reverse string keys */
#define MDB_REVERSEKEY	0x02
	/** use sorted duplicates */
#define MDB_DUPSORT		0x04
	/** numeric keys in native byte order, either unsigned int or #mdb_size_t.
	 *	(lmdb expects 32-bit int <= size_t <= 32/64-bit mdb_size_t.)
	 *  The keys must all be of the same size. */
#define MDB_INTEGERKEY	0x08
	/** with #MDB_DUPSORT, sorted dup items have fixed size */
#define MDB_DUPFIXED	0x10
	/** with #MDB_DUPSORT, dups are #MDB_INTEGERKEY-style integers */
#define MDB_INTEGERDUP	0x20
	/** with #MDB_DUPSORT, use reverse string dups */
#define MDB_REVERSEDUP	0x40
	/** create DB if not already existing */
#define MDB_CREATE		0x40000
/** @} */

/**	@defgroup mdb_put	Write Flags
 *	@{
 */
/** For put: Don't write if the key already exists. */
#define MDB_NOOVERWRITE	0x10
/** Only for #MDB_DUPSORT<br>
 * For put: don't write if the key and data pair already exist.<br>
 * For mdb_cursor_del: remove all duplicate data items.
 */
#define MDB_NODUPDATA	0x20
/** For mdb_cursor_put: overwrite the current key/data pair */
#define MDB_CURRENT	0x40
/** For put: Just reserve space for data, don't copy it. Return a
 * pointer to the reserved space.
 */
#define MDB_RESERVE	0x10000
/** Data is being appended, don't split full pages. */
#define MDB_APPEND	0x20000
/** Duplicate data is being appended, don't split full pages. */
#define MDB_APPENDDUP	0x40000
/** Store multiple data items in one call. Only for #MDB_DUPFIXED. */
#define MDB_MULTIPLE	0x80000
/*	@} */

/**	@defgroup mdb_copy	Copy Flags
 *	@{
 */
/** Compacting copy: Omit free space from copy, and renumber all
 * pages sequentially.
 */
#define MDB_CP_COMPACT	0x01
/*	@} */

/** @defgroup  errors	Return Codes
 *
 *	BerkeleyDB uses -30800 to -30999, we'll go under them
 *	@{
 */
	/**	Successful result */
#define MDB_SUCCESS	 0
	/** key/data pair already exists */
#define MDB_KEYEXIST	(-30799)
	/** key/data pair not found (EOF) */
#define MDB_NOTFOUND	(-30798)
	/** Requested page not found - this usually indicates corruption */
#define MDB_PAGE_NOTFOUND	(-30797)
	/** Located page was wrong type */
#define MDB_CORRUPTED	(-30796)
	/** Update of meta page failed or environment had fatal error */
#define MDB_PANIC		(-30795)
	/** Environment version mismatch */
#define MDB_VERSION_MISMATCH	(-30794)
	/** File is not a valid LMDB file */
#define MDB_INVALID	(-30793)
	/** Environment mapsize reached */
#define MDB_MAP_FULL	(-30792)
	/** Environment maxdbs reached */
#define MDB_DBS_FULL	(-30791)
	/** Environment maxreaders reached */
#define MDB_READERS_FULL	(-30790)
	/** Too many TLS keys in use - Windows only */
#define MDB_TLS_FULL	(-30789)
	/** Txn has too many dirty pages */
#define MDB_TXN_FULL	(-30788)
	/** Cursor stack too deep - internal error */
#define MDB_CURSOR_FULL	(-30787)
	/** Page has not enough space - internal error */
#define MDB_PAGE_FULL	(-30786)
	/** Database contents grew beyond environment mapsize */
#define MDB_MAP_RESIZED	(-30785)
	/** Operation and DB incompatible, or DB type changed. This can mean:
	 *	<ul>
	 *	<li>The operation expects an #MDB_DUPSORT / #MDB_DUPFIXED database.
	 *	<li>Opening a named DB when the unnamed DB has #MDB_DUPSORT / #MDB_INTEGERKEY.
	 *	<li>Accessing a data record as a database, or vice versa.
	 *	<li>The database was dropped and recreated with different flags.
	 *	</ul>
	 */
#define MDB_INCOMPATIBLE	(-30784)
	/** Invalid reuse of reader locktable slot */
#define MDB_BAD_RSLOT		(-30783)
	/** Transaction must abort, has a child, or is invalid */
#define MDB_BAD_TXN			(-30782)
	/** Unsupported size of key/DB name/data, or wrong DUPFIXED size */
#define MDB_BAD_VALSIZE		(-30781)
	/** The specified DBI was changed unexpectedly */
#define MDB_BAD_DBI		(-30780)
	/** Unexpected problem - txn should abort */
#define MDB_PROBLEM		(-30779)
	/** The last defined error code */
#define MDB_LAST_ERRCODE	MDB_PROBLEM
/** @} */

/** @brief Statistics for a database in the environment */
typedef struct MDB_stat {
	unsigned int	ms_psize;			/**< Size of a database page.
											This is currently the same for all databases. */
	unsigned int	ms_depth;			/**< Depth (height) of the B-tree */
	mdb_size_t		ms_branch_pages;	/**< Number of internal (non-leaf) pages */
	mdb_size_t		ms_leaf_pages;		/**< Number of leaf pages */
	mdb_size_t		ms_overflow_pages;	/**< Number of overflow pages */
	mdb_size_t		ms_entries;			/**< Number of data items */
} MDB_stat;

/** @brief Information about the environment */
typedef struct MDB_envinfo {
	void	*me_mapaddr;			/**< Address of map, if fixed */
	mdb_size_t	me_mapsize;				/**< Size of the data memory map */
	mdb_size_t	me_last_pgno;			/**< ID of the last used page */
	mdb_size_t	me_last_txnid;			/**< ID of the last committed transaction */
	unsigned int me_maxreaders;		/**< max reader slots in the environment */
	unsigned int me_numreaders;		/**< max reader slots used in the environment */
} MDB_envinfo;

	/** @brief Return the LMDB library version information.
	 *
	 * @param[out] major if non-NULL, the library major version number is copied here
	 * @param[out] minor if non-NULL, the library minor version number is copied here
	 * @param[out] patch if non-NULL, the library patch version number is copied here
	 * @retval "version string" The library version as a string
	 */
char *mdb_version(int *major, int *minor, int *patch);

	/** @brief Return a string describing a given error code.
	 *
	 * This function is a superset of the ANSI C X3.159-1989 (ANSI C) strerror(3)
	 * function. If the error code is greater than or equal to 0, then the string
	 * returned by the system function strerror(3) is returned. If the error code
	 * is less than 0, an error string corresponding to the LMDB library error is
	 * returned. See @ref errors for a list of LMDB-specific error codes.
	 * @param[in] err The error code
	 * @retval "error message" The description of the error
	 */
char *mdb_strerror(int err);

	/** @brief Create an LMDB environment handle.
	 *
	 * This function allocates memory for a #MDB_env structure. To release
	 * the allocated memory and discard the handle, call #mdb_env_close().
	 * Before the handle may be used, it must be opened using #mdb_env_open().
	 * Various other options may also need to be set before opening the handle,
	 * e.g. #mdb_env_set_mapsize(), #mdb_env_set_maxreaders(), #mdb_env_set_maxdbs(),
	 * depending on usage requirements.
	 * @param[out] env The address where the new handle will be stored
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_env_create(MDB_env **env);

	/** @brief Open an environment handle.
	 *
	 * If this function fails, #mdb_env_close() must be called to discard the #MDB_env handle.
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[in] path The directory in which the database files reside. This
	 * directory must already exist and be writable.
	 * @param[in] flags Special options for this environment. This parameter
	 * must be set to 0 or by bitwise OR'ing together one or more of the
	 * values described here.
	 * Flags set by mdb_env_set_flags() are also used.
	 * <ul>
	 *	<li>#MDB_FIXEDMAP
	 *      use a fixed address for the mmap region. This flag must be specified
	 *      when creating the environment, and is stored persistently in the environment.
	 *		If successful, the memory map will always reside at the same virtual address
	 *		and pointers used to reference data items in the database will be constant
	 *		across multiple invocations. This option may not always work, depending on
	 *		how the operating system has allocated memory to shared libraries and other uses.
	 *		The feature is highly experimental.
	 *	<li>#MDB_NOSUBDIR
	 *		By default, LMDB creates its environment in a directory whose
	 *		pathname is given in \b path, and creates its data and lock files
	 *		under that directory. With this option, \b path is used as-is for
	 *		the database main data file. The database lock file is the \b path
	 *		with "-lock" appended.
	 *	<li>#MDB_RDONLY
	 *		Open the environment in read-only mode. No write operations will be
	 *		allowed. LMDB will still modify the lock file - except on read-only
	 *		filesystems, where LMDB does not use locks.
	 *	<li>#MDB_WRITEMAP
	 *		Use a writeable memory map unless MDB_RDONLY is set. This uses
	 *		fewer mallocs but loses protection from application bugs
	 *		like wild pointer writes and other bad updates into the database.
	 *		This may be slightly faster for DBs that fit entirely in RAM, but
	 *		is slower for DBs larger than RAM.
	 *		Incompatible with nested transactions.
	 *		Do not mix processes with and without MDB_WRITEMAP on the same
	 *		environment.  This can defeat durability (#mdb_env_sync etc).
	 *	<li>#MDB_NOMETASYNC
	 *		Flush system buffers to disk only once per transaction, omit the
	 *		metadata flush. Defer that until the system flushes files to disk,
	 *		or next non-MDB_RDONLY commit or #mdb_env_sync(). This optimization
	 *		maintains database integrity, but a system crash may undo the last
	 *		committed transaction. I.e. it preserves the ACI (atomicity,
	 *		consistency, isolation) but not D (durability) database property.
	 *		This flag may be changed at any time using #mdb_env_set_flags().
	 *	<li>#MDB_NOSYNC
	 *		Don't flush system buffers to disk when committing a transaction.
	 *		This optimization means a system crash can corrupt the database or
	 *		lose the last transactions if buffers are not yet flushed to disk.
	 *		The risk is governed by how often the system flushes dirty buffers
	 *		to disk and how often #mdb_env_sync() is called.  However, if the
	 *		filesystem preserves write order and the #MDB_WRITEMAP flag is not
	 *		used, transactions exhibit ACI (atomicity, consistency, isolation)
	 *		properties and only lose D (durability).  I.e. database integrity
	 *		is maintained, but a system crash may undo the final transactions.
	 *		Note that (#MDB_NOSYNC | #MDB_WRITEMAP) leaves the system with no
	 *		hint for when to write transactions to disk, unless #mdb_env_sync()
	 *		is called. (#MDB_MAPASYNC | #MDB_WRITEMAP) may be preferable.
	 *		This flag may be changed at any time using #mdb_env_set_flags().
	 *	<li>#MDB_MAPASYNC
	 *		When using #MDB_WRITEMAP, use asynchronous flushes to disk.
	 *		As with #MDB_NOSYNC, a system crash can then corrupt the
	 *		database or lose the last transactions. Calling #mdb_env_sync()
	 *		ensures on-disk database integrity until next commit.
	 *		This flag may be changed at any time using #mdb_env_set_flags().
	 *	<li>#MDB_NOTLS
	 *		Don't use Thread-Local Storage. Tie reader locktable slots to
	 *		#MDB_txn objects instead of to threads. I.e. #mdb_txn_reset() keeps
	 *		the slot reserved for the #MDB_txn object. A thread may use parallel
	 *		read-only transactions. A read-only transaction may span threads if
	 *		the user synchronizes its use. Applications that multiplex many
	 *		user threads over individual OS threads need this option. Such an
	 *		application must also serialize the write transactions in an OS
	 *		thread, since LMDB's write locking is unaware of the user threads.
	 *	<li>#MDB_NOLOCK
	 *		Don't do any locking. If concurrent access is anticipated, the
	 *		caller must manage all concurrency itself. For proper operation
	 *		the caller must enforce single-writer semantics, and must ensure
	 *		that no readers are using old transactions while a writer is
	 *		active. The simplest approach is to use an exclusive lock so that
	 *		no readers may be active at all when a writer begins.
	 *	<li>#MDB_NORDAHEAD
	 *		Turn off readahead. Most operating systems perform readahead on
	 *		read requests by default. This option turns it off if the OS
	 *		supports it. Turning it off may help random read performance
	 *		when the DB is larger than RAM and system RAM is full.
	 *		The option is not implemented on Windows.
	 *	<li>#MDB_NOMEMINIT
	 *		Don't initialize malloc'd memory before writing to unused spaces
	 *		in the data file. By default, memory for pages written to the data
	 *		file is obtained using malloc. While these pages may be reused in
	 *		subsequent transactions, freshly malloc'd pages will be initialized
	 *		to zeroes before use. This avoids persisting leftover data from other
	 *		code (that used the heap and subsequently freed the memory) into the
	 *		data file. Note that many other system libraries may allocate
	 *		and free memory from the heap for arbitrary uses. E.g., stdio may
	 *		use the heap for file I/O buffers. This initialization step has a
	 *		modest performance cost so some applications may want to disable
	 *		it using this flag. This option can be a problem for applications
	 *		which handle sensitive data like passwords, and it makes memory
	 *		checkers like Valgrind noisy. This flag is not needed with #MDB_WRITEMAP,
	 *		which writes directly to the mmap instead of using malloc for pages. The
	 *		initialization is also skipped if #MDB_RESERVE is used; the
	 *		caller is expected to overwrite all of the memory that was
	 *		reserved in that case.
	 *		This flag may be changed at any time using #mdb_env_set_flags().
	 *	<li>#MDB_PREVSNAPSHOT
	 *		Open the environment with the previous snapshot rather than the latest
	 *		one. This loses the latest transaction, but may help work around some
	 *		types of corruption. If opened with write access, this must be the
	 *		only process using the environment. This flag is automatically reset
	 *		after a write transaction is successfully committed.
	 * </ul>
	 * @param[in] mode The UNIX permissions to set on created files and semaphores.
	 * This parameter is ignored on Windows.
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>#MDB_VERSION_MISMATCH - the version of the LMDB library doesn't match the
	 *	version that created the database environment.
	 *	<li>#MDB_INVALID - the environment file headers are corrupted.
	 *	<li>ENOENT - the directory specified by the path parameter doesn't exist.
	 *	<li>EACCES - the user didn't have permission to access the environment files.
	 *	<li>EAGAIN - the environment was locked by another process.
	 * </ul>
	 */
int  mdb_env_open(MDB_env *env, const char *path, unsigned int flags, mdb_mode_t mode);

	/** @brief Copy an LMDB environment to the specified path.
	 *
	 * This function may be used to make a backup of an existing environment.
	 * No lockfile is created, since it gets recreated at need.
	 * @note This call can trigger significant file size growth if run in
	 * parallel with write transactions, because it employs a read-only
	 * transaction. See long-lived transactions under @ref caveats_sec.
	 * @param[in] env An environment handle returned by #mdb_env_create(). It
	 * must have already been opened successfully.
	 * @param[in] path The directory in which the copy will reside. This
	 * directory must already exist and be writable but must otherwise be
	 * empty.
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_env_copy(MDB_env *env, const char *path);

	/** @brief Copy an LMDB environment to the specified file descriptor.
	 *
	 * This function may be used to make a backup of an existing environment.
	 * No lockfile is created, since it gets recreated at need.
	 * @note This call can trigger significant file size growth if run in
	 * parallel with write transactions, because it employs a read-only
	 * transaction. See long-lived transactions under @ref caveats_sec.
	 * @param[in] env An environment handle returned by #mdb_env_create(). It
	 * must have already been opened successfully.
	 * @param[in] fd The filedescriptor to write the copy to. It must
	 * have already been opened for Write access.
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_env_copyfd(MDB_env *env, mdb_filehandle_t fd);

	/** @brief Copy an LMDB environment to the specified path, with options.
	 *
	 * This function may be used to make a backup of an existing environment.
	 * No lockfile is created, since it gets recreated at need.
	 * @note This call can trigger significant file size growth if run in
	 * parallel with write transactions, because it employs a read-only
	 * transaction. See long-lived transactions under @ref caveats_sec.
	 * @param[in] env An environment handle returned by #mdb_env_create(). It
	 * must have already been opened successfully.
	 * @param[in] path The directory in which the copy will reside. This
	 * directory must already exist and be writable but must otherwise be
	 * empty.
	 * @param[in] flags Special options for this operation. This parameter
	 * must be set to 0 or by bitwise OR'ing together one or more of the
	 * values described here.
	 * <ul>
	 *	<li>#MDB_CP_COMPACT - Perform compaction while copying: omit free
	 *		pages and sequentially renumber all pages in output. This option
	 *		consumes more CPU and runs more slowly than the default.
	 *		Currently it fails if the environment has suffered a page leak.
	 * </ul>
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_env_copy2(MDB_env *env, const char *path, unsigned int flags);

	/** @brief Copy an LMDB environment to the specified file descriptor,
	 *	with options.
	 *
	 * This function may be used to make a backup of an existing environment.
	 * No lockfile is created, since it gets recreated at need. See
	 * #mdb_env_copy2() for further details.
	 * @note This call can trigger significant file size growth if run in
	 * parallel with write transactions, because it employs a read-only
	 * transaction. See long-lived transactions under @ref caveats_sec.
	 * @param[in] env An environment handle returned by #mdb_env_create(). It
	 * must have already been opened successfully.
	 * @param[in] fd The filedescriptor to write the copy to. It must
	 * have already been opened for Write access.
	 * @param[in] flags Special options for this operation.
	 * See #mdb_env_copy2() for options.
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_env_copyfd2(MDB_env *env, mdb_filehandle_t fd, unsigned int flags);

	/** @brief Return statistics about the LMDB environment.
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[out] stat The address of an #MDB_stat structure
	 * 	where the statistics will be copied
	 */
int  mdb_env_stat(MDB_env *env, MDB_stat *stat);

	/** @brief Return information about the LMDB environment.
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[out] stat The address of an #MDB_envinfo structure
	 * 	where the information will be copied
	 */
int  mdb_env_info(MDB_env *env, MDB_envinfo *stat);

	/** @brief Flush the data buffers to disk.
	 *
	 * Data is always written to disk when #mdb_txn_commit() is called,
	 * but the operating system may keep it buffered. LMDB always flushes
	 * the OS buffers upon commit as well, unless the environment was
	 * opened with #MDB_NOSYNC or in part #MDB_NOMETASYNC. This call is
	 * not valid if the environment was opened with #MDB_RDONLY.
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[in] force If non-zero, force a synchronous flush.  Otherwise
	 *  if the environment has the #MDB_NOSYNC flag set the flushes
	 *	will be omitted, and with #MDB_MAPASYNC they will be asynchronous.
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>EACCES - the environment is read-only.
	 *	<li>EINVAL - an invalid parameter was specified.
	 *	<li>EIO - an error occurred during synchronization.
	 * </ul>
	 */
int  mdb_env_sync(MDB_env *env, int force);

	/** @brief Close the environment and release the memory map.
	 *
	 * Only a single thread may call this function. All transactions, databases,
	 * and cursors must already be closed before calling this function. Attempts to
	 * use any such handles after calling this function will cause a SIGSEGV.
	 * The environment handle will be freed and must not be used again after this call.
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 */
void mdb_env_close(MDB_env *env);

	/** @brief Set environment flags.
	 *
	 * This may be used to set some flags in addition to those from
	 * #mdb_env_open(), or to unset these flags.  If several threads
	 * change the flags at the same time, the result is undefined.
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[in] flags The flags to change, bitwise OR'ed together
	 * @param[in] onoff A non-zero value sets the flags, zero clears them.
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>EINVAL - an invalid parameter was specified.
	 * </ul>
	 */
int  mdb_env_set_flags(MDB_env *env, unsigned int flags, int onoff);

	/** @brief Get environment flags.
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[out] flags The address of an integer to store the flags
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>EINVAL - an invalid parameter was specified.
	 * </ul>
	 */
int  mdb_env_get_flags(MDB_env *env, unsigned int *flags);

	/** @brief Return the path that was used in #mdb_env_open().
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[out] path Address of a string pointer to contain the path. This
	 * is the actual string in the environment, not a copy. It should not be
	 * altered in any way.
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>EINVAL - an invalid parameter was specified.
	 * </ul>
	 */
int  mdb_env_get_path(MDB_env *env, const char **path);

	/** @brief Return the filedescriptor for the given environment.
	 *
	 * This function may be called after fork(), so the descriptor can be
	 * closed before exec*().  Other LMDB file descriptors have FD_CLOEXEC.
	 * (Until LMDB 0.9.18, only the lockfile had that.)
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[out] fd Address of a mdb_filehandle_t to contain the descriptor.
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>EINVAL - an invalid parameter was specified.
	 * </ul>
	 */
int  mdb_env_get_fd(MDB_env *env, mdb_filehandle_t *fd);

	/** @brief Set the size of the memory map to use for this environment.
	 *
	 * The size should be a multiple of the OS page size. The default is
	 * 10485760 bytes. The size of the memory map is also the maximum size
	 * of the database. The value should be chosen as large as possible,
	 * to accommodate future growth of the database.
	 * This function should be called after #mdb_env_create() and before #mdb_env_open().
	 * It may be called at later times if no transactions are active in
	 * this process. Note that the library does not check for this condition,
	 * the caller must ensure it explicitly.
	 *
	 * The new size takes effect immediately for the current process but
	 * will not be persisted to any others until a write transaction has been
	 * committed by the current process. Also, only mapsize increases are
	 * persisted into the environment.
	 *
	 * If the mapsize is increased by another process, and data has grown
	 * beyond the range of the current mapsize, #mdb_txn_begin() will
	 * return #MDB_MAP_RESIZED. This function may be called with a size
	 * of zero to adopt the new size.
	 *
	 * Any attempt to set a size smaller than the space already consumed
	 * by the environment will be silently changed to the current size of the used space.
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[in] size The size in bytes
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>EINVAL - an invalid parameter was specified, or the environment has
	 *   	an active write transaction.
	 * </ul>
	 */
int  mdb_env_set_mapsize(MDB_env *env, mdb_size_t size);

	/** @brief Set application information associated with the #MDB_env.
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @param[in] ctx An arbitrary pointer for whatever the application needs.
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_env_set_userctx(MDB_env *env, void *ctx);                                      // MAYBE?

	/** @brief Get the application information associated with the #MDB_env.
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create()
	 * @return The pointer set by #mdb_env_set_userctx().
	 */
void *mdb_env_get_userctx(MDB_env *env);                                                // MAYBE?

	/** @brief A callback function for most LMDB assert() failures,
	 * called before printing the message and aborting.
	 *
	 * @param[in] env An environment handle returned by #mdb_env_create().
	 * @param[in] msg The assertion message, not including newline.
	 */
typedef void MDB_assert_func(MDB_env *env, const char *msg);

	/** Set or reset the assert() callback of the environment.
	 * Disabled if liblmdb is built with NDEBUG.
	 * @note This hack should become obsolete as lmdb's error handling matures.
	 * @param[in] env An environment handle returned by #mdb_env_create().
	 * @param[in] func An #MDB_assert_func function, or 0.
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_env_set_assert(MDB_env *env, MDB_assert_func *func);

	/** @brief Empty or delete+close a database.
	 *
	 * See #mdb_dbi_close() for restrictions about closing the DB handle.
	 * @param[in] txn A transaction handle returned by #mdb_txn_begin()
	 * @param[in] dbi A database handle returned by #mdb_dbi_open()
	 * @param[in] del 0 to empty the DB, 1 to delete it from the
	 * environment and close the DB handle.
	 * @return A non-zero error value on failure and 0 on success.
	 */
int  mdb_drop(MDB_txn *txn, MDB_dbi dbi, int del);

	/** @brief Get items from a database.
	 *
	 * This function retrieves key/data pairs from the database. The address
	 * and length of the data associated with the specified \b key are returned
	 * in the structure to which \b data refers.
	 * If the database supports duplicate keys (#MDB_DUPSORT) then the
	 * first data item for the key will be returned. Retrieval of other
	 * items requires the use of #mdb_cursor_get().
	 *
	 * @note The memory pointed to by the returned values is owned by the
	 * database. The caller need not dispose of the memory, and may not
	 * modify it in any way. For values returned in a read-only transaction
	 * any modification attempts will cause a SIGSEGV.
	 * @note Values returned from the database are valid only until a
	 * subsequent update operation, or the end of the transaction.
	 * @param[in] txn A transaction handle returned by #mdb_txn_begin()
	 * @param[in] dbi A database handle returned by #mdb_dbi_open()
	 * @param[in] key The key to search for in the database
	 * @param[out] data The data corresponding to the key
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>#MDB_NOTFOUND - the key was not in the database.
	 *	<li>EINVAL - an invalid parameter was specified.
	 * </ul>
	 */
int  mdb_get(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data);

	/** @brief Store items into a database.
	 *
	 * This function stores key/data pairs in the database. The default behavior
	 * is to enter the new key/data pair, replacing any previously existing key
	 * if duplicates are disallowed, or adding a duplicate data item if
	 * duplicates are allowed (#MDB_DUPSORT).
	 * @param[in] txn A transaction handle returned by #mdb_txn_begin()
	 * @param[in] dbi A database handle returned by #mdb_dbi_open()
	 * @param[in] key The key to store in the database
	 * @param[in,out] data The data to store
	 * @param[in] flags Special options for this operation. This parameter
	 * must be set to 0 or by bitwise OR'ing together one or more of the
	 * values described here.
	 * <ul>
	 *	<li>#MDB_NODUPDATA - enter the new key/data pair only if it does not
	 *		already appear in the database. This flag may only be specified
	 *		if the database was opened with #MDB_DUPSORT. The function will
	 *		return #MDB_KEYEXIST if the key/data pair already appears in the
	 *		database.
	 *	<li>#MDB_NOOVERWRITE - enter the new key/data pair only if the key
	 *		does not already appear in the database. The function will return
	 *		#MDB_KEYEXIST if the key already appears in the database, even if
	 *		the database supports duplicates (#MDB_DUPSORT). The \b data
	 *		parameter will be set to point to the existing item.
	 *	<li>#MDB_RESERVE - reserve space for data of the given size, but
	 *		don't copy the given data. Instead, return a pointer to the
	 *		reserved space, which the caller can fill in later - before
	 *		the next update operation or the transaction ends. This saves
	 *		an extra memcpy if the data is being generated later.
	 *		LMDB does nothing else with this memory, the caller is expected
	 *		to modify all of the space requested. This flag must not be
	 *		specified if the database was opened with #MDB_DUPSORT.
	 *	<li>#MDB_APPEND - append the given key/data pair to the end of the
	 *		database. This option allows fast bulk loading when keys are
	 *		already known to be in the correct order. Loading unsorted keys
	 *		with this flag will cause a #MDB_KEYEXIST error.
	 *	<li>#MDB_APPENDDUP - as above, but for sorted dup data.
	 * </ul>
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>#MDB_MAP_FULL - the database is full, see #mdb_env_set_mapsize().
	 *	<li>#MDB_TXN_FULL - the transaction has too many dirty pages.
	 *	<li>EACCES - an attempt was made to write in a read-only transaction.
	 *	<li>EINVAL - an invalid parameter was specified.
	 * </ul>
	 */
int  mdb_put(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data,
			    unsigned int flags);

	/** @brief Delete items from a database.
	 *
	 * This function removes key/data pairs from the database.
	 * If the database does not support sorted duplicate data items
	 * (#MDB_DUPSORT) the data parameter is ignored.
	 * If the database supports sorted duplicates and the data parameter
	 * is NULL, all of the duplicate data items for the key will be
	 * deleted. Otherwise, if the data parameter is non-NULL
	 * only the matching data item will be deleted.
	 * This function will return #MDB_NOTFOUND if the specified key/data
	 * pair is not in the database.
	 * @param[in] txn A transaction handle returned by #mdb_txn_begin()
	 * @param[in] dbi A database handle returned by #mdb_dbi_open()
	 * @param[in] key The key to delete from the database
	 * @param[in] data The data to delete
	 * @return A non-zero error value on failure and 0 on success. Some possible
	 * errors are:
	 * <ul>
	 *	<li>EACCES - an attempt was made to write in a read-only transaction.
	 *	<li>EINVAL - an invalid parameter was specified.
	 * </ul>
	 */
int  mdb_del(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data);

	/** @brief A callback function used to print a message from the library.
	 *
	 * @param[in] msg The string to be printed.
	 * @param[in] ctx An arbitrary context pointer for the callback.
	 * @return < 0 on failure, >= 0 on success.
	 */
typedef int (MDB_msg_func)(const char *msg, void *ctx);

#endif /* _LMDB_H_ */
