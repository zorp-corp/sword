/*
 * ----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <phk@FreeBSD.ORG> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Poul-Henning Kamp
 * ----------------------------------------------------------------------------
 *
 */

#include <sys/cdefs.h>
#include <stdint.h>
#include "btree.h"

#define PHKPMA
#define PHKPMA_PATH "./phkpma"
/* ;;: tmp. for main test function */
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
/* ;;: end */

/* <eric>__FBSDID("$FreeBSD: /repoman/r/ncvs/src/lib/libc/stdlib/malloc.c,v 1.89 2004/07/04 16:11:01 stefanf Exp $"); */

/*
 * Defining MALLOC_EXTRA_SANITY will enable extra checks which are related
 * to internal conditions and consistency in malloc.c. This has a
 * noticeable runtime performance hit, and generally will not do you
 * any good unless you fiddle with the internals of malloc or want
 * to catch random pointer corruption as early as possible.
 */
#ifndef MALLOC_EXTRA_SANITY
#undef MALLOC_EXTRA_SANITY
#endif

/* <eric> we may want to remove all unnecessary checks for best performance */
#undef MALLOC_SANITY


/* <eric> end */

/*
 * What to use for Junk.  This is the byte value we use to fill with
 * when the 'J' option is enabled.
 */
#define SOME_JUNK	0xd0		/* as in "Duh" :-) */

/*
 * The basic parameters you can tweak.
 *
 * malloc_pageshift	pagesize = 1 << malloc_pageshift
 *			It's probably best if this is the native
 *			page size, but it doesn't have to be.
 *
 * malloc_minsize	minimum size of an allocation in bytes.
 *			If this is too small it's too much work
 *			to manage them.  This is also the smallest
 *			unit of alignment used for the storage
 *			returned by malloc/realloc.
 *
 */

/* <eric>#include "namespace.h" */
#if defined(__FreeBSD__)
#   if defined(__i386__)
#       define malloc_pageshift		12U
#       define malloc_minsize		16U
#   endif
#   if defined(__ia64__)
#	define malloc_pageshift		13U
#	define malloc_minsize		16U
#   endif
#   if defined(__alpha__)
#       define malloc_pageshift		13U
#       define malloc_minsize		16U
#   endif
#   if defined(__sparc64__)
#       define malloc_pageshift		13U
#       define malloc_minsize		16U
#   endif
#   if defined(__amd64__)
#       define malloc_pageshift		12U
#       define malloc_minsize		16U
#   endif
#   if defined(__arm__)
#       define malloc_pageshift         12U
#       define malloc_minsize           16U
#   endif
#   define HAS_UTRACE
    /*
     * Make malloc/free/realloc thread-safe in libc for use with
     * kernel threads.
     */
#   include "libc_private.h"
#   include "spinlock.h"
    static spinlock_t thread_lock	= _SPINLOCK_INITIALIZER;
    spinlock_t *__malloc_lock		= &thread_lock;
#   define _MALLOC_LOCK()		if (__isthreaded) _SPINLOCK(&thread_lock);
#   define _MALLOC_UNLOCK()		if (__isthreaded) _SPINUNLOCK(&thread_lock);
#endif /* __FreeBSD__ */

#if defined(__sparc__) && defined(sun)
#   define malloc_pageshift		12U
#   define malloc_minsize		16U
#   define MAP_ANON			(0)
    static int fdzero;
#   define MMAP_FD	fdzero
#   define INIT_MMAP() \
        { if ((fdzero = _open(_PATH_DEVZERO, O_RDWR, 0000)) == -1) \
            wrterror("open of /dev/zero"); }
#   define MADV_FREE			MADV_DONTNEED
#endif /* __sparc__ */

/* Insert your combination here... */
#if defined(__FOOCPU__) && defined(__BAROS__)
#   define malloc_pageshift		12U
#   define malloc_minsize		16U
#endif /* __FOOCPU__ && __BAROS__ */

/* #if (defined(__i386__) || defined(__powerpc__)) && defined(__linux__)		/\* <eric> *\/ */
#define	MADV_FREE			MADV_DONTNEED
#define malloc_pageshift	12U
#define malloc_minsize		8U
#define _getprogname()		"_getprogname"
#define issetugid()			0
#define EDOOFUS				88
/* #endif */

#ifndef ZEROSIZEPTR
#define ZEROSIZEPTR  NULL /*	((void *)(uintptr_t)(1 << (malloc_pageshift - 1))) */
#endif

/*
 * No user serviceable parts behind this point.
 */
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>
#include <fcntl.h>
#include <paths.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
/* <eric>#include "un-namespace.h" */

/*
 * This structure describes a page worth of chunks.
 */

struct pginfo {
    struct pginfo	*next;	/* next on the free list */
    void		*page;	/* Pointer to the page */
    u_short		size;	/* size of this page's chunks */
    u_short		shift;	/* How far to shift for this size chunks */
    u_short		free;	/* How many free chunks */
    u_short		total;	/* How many chunk */
    u_int		bits[1]; /* Which chunks are free */
};

/*
 * This structure describes a number of free pages.
 */

struct pgfree {
    struct pgfree	*next;	/* next run of free pages */
    struct pgfree	*prev;	/* prev run of free pages */
    void		*page;	/* pointer to free pages */
    void		*end;	/* pointer to end of free pages */
    size_t		size;	/* number of bytes free */
};

/*
 * How many bits per u_int in the bitmap.
 * Change only if not 8 bits/byte
 */
#define	MALLOC_BITS	(8*sizeof(u_int))

/*
 * Magic values to put in the page_directory
 */
#define MALLOC_NOT_MINE	((struct pginfo*) 0)
#define MALLOC_FREE     ((struct pginfo*) 1)
#define MALLOC_FIRST	((struct pginfo*) 2)
#define MALLOC_FOLLOW	((struct pginfo*) 3)
#define MALLOC_MAGIC	((struct pginfo*) 4)

#ifndef malloc_pageshift
#define malloc_pageshift		14U /* ;;: Consider: should we make this the pma pagesize (14) */
#endif

#ifndef malloc_minsize
#define malloc_minsize			16U
#endif

#if !defined(malloc_pagesize)
#define malloc_pagesize			(1UL<<malloc_pageshift)
#endif

#if ((1<<malloc_pageshift) != malloc_pagesize)
#error	"(1<<malloc_pageshift) != malloc_pagesize"
#endif

#ifndef malloc_maxsize
#define malloc_maxsize			((malloc_pagesize)>>1)
#endif

/* A mask for the offset inside a page.  */
#define malloc_pagemask	((malloc_pagesize)-1)

#define pageround(foo) (((foo) + (malloc_pagemask))&(~(malloc_pagemask)))
#define ptr2index(foo) (((u_long)(foo) >> malloc_pageshift)-malloc_origo)
#define index2ptr(foo) (((u_long)(foo) << malloc_pageshift)+malloc_origo)

#ifndef _MALLOC_LOCK
#define _MALLOC_LOCK()
#endif

#ifndef _MALLOC_UNLOCK
#define _MALLOC_UNLOCK()
#endif

#ifndef MMAP_FD
#define MMAP_FD (-1)
#endif

#ifndef INIT_MMAP
#define INIT_MMAP()
#endif

/* Number of free pages we cache */
static unsigned malloc_cache = 16;

/* The offset from pagenumber to index into the page directory */
static u_long malloc_origo;

/* The last index in the page directory we care about */
static u_long last_index;

/* Pointer to page directory. Allocated "as if with" malloc */
static struct	pginfo **page_dir;

static BT_state *bt_state;

/* How many slots in the page directory */
static unsigned	malloc_ninfo;

/* Free pages line up here */
static struct pgfree free_list;

/* Abort(), user doesn't handle problems.  */
static int malloc_abort = 1;

/* Are we trying to die ?  */
static int suicide;

/* always realloc ?  */
static int malloc_realloc;

#if defined(MADV_FREE)
/* pass the kernel a hint on free pages ?  */
static int malloc_hint = 0;
#endif

/* xmalloc behaviour ?  */
static int malloc_xmalloc;

/* sysv behaviour for malloc(0) ?  */
static int malloc_sysv;

/* zero fill ?  */
static int malloc_zero = 0;	/* <eric> don't waste instructions here */

/* junk fill ?  */
static int malloc_junk = 0;	/* <eric> don't waste instructions here */

#ifdef HAS_UTRACE

/* utrace ?  */
static int malloc_utrace;

struct ut { void *p; size_t s; void *r; };

void utrace(struct ut *, int);

#define UTRACE(a, b, c) \
        if (malloc_utrace) \
                {struct ut u; u.p=a; u.s = b; u.r=c; utrace(&u, sizeof u);}
#else /* !HAS_UTRACE */
#define UTRACE(a,b,c)
#endif /* HAS_UTRACE */

/* my last break. */
static void *malloc_brk;

/* one location cache for free-list holders */
static struct pgfree *px;

/* compile-time options */
const char *_malloc_options;

/* Name of the current public function */
static const char *malloc_func;

/* Macro for mmap */
#ifndef PHKPMA
#define MMAP(size) \
        mmap(NULL, (size), PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, \
            MMAP_FD, (off_t)0);
#else
#define MMAP(size) bt_malloc(bt_state, (size))
#endif

/*
 * Necessary function declarations
 */
static int extend_pgdir(u_long index);
static void *imalloc(size_t size);
static void ifree(void *ptr);
static void *irealloc(void *ptr, size_t size);

static void
wrtmessage(const char *p1, const char *p2, const char *p3, const char *p4)
{

    write(STDERR_FILENO, p1, strlen(p1));
    write(STDERR_FILENO, p2, strlen(p2));
    write(STDERR_FILENO, p3, strlen(p3));
    write(STDERR_FILENO, p4, strlen(p4));
}

void (*_malloc_message)(const char *p1, const char *p2, const char *p3,
            const char *p4) = wrtmessage;

static void
wrterror(char const *p)
{

    suicide = 1;
    _malloc_message(_getprogname(), malloc_func, " error: ", p);
    abort();
}

static void
wrtwarning(char *p)
{

    /*
     * Sensitive processes, somewhat arbitrarily defined here as setuid,
     * setgid, root and wheel cannot afford to have malloc mistakes.
     */
    if (malloc_abort || issetugid() || getuid() == 0 || getgid() == 0)
        wrterror(p);
    _malloc_message(_getprogname(), malloc_func, " warning: ", p);
}

/*
 * Allocate a number of pages from the OS
 */
static void *
map_pages(size_t pages)
#ifndef PHKPMA
{
    caddr_t result, tail;

    result = (caddr_t)pageround((u_long)sbrk(0));
    tail = result + (pages << malloc_pageshift);
    if (tail < result)
        return (NULL);

    if (brk(tail)) {
#ifdef MALLOC_EXTRA_SANITY
        wrterror("(ES): map_pages fails\n");
#endif /* MALLOC_EXTRA_SANITY */
        return (NULL);
    }

    last_index = ptr2index(tail) - 1;
    malloc_brk = tail;

    if ((last_index+1) >= malloc_ninfo && !extend_pgdir(last_index))
        return (NULL);

    return (result);
}
#else
{
  /* ;;: I don't quite follow the conditional extension above. Ensure this is
       correct */
  return bt_malloc(bt_state, pages);
}
#endif

/*
 * Extend page directory
 */
static int
extend_pgdir(u_long index)
{
    struct  pginfo **nEW, **old;
    u_long i, oldlen;

    /* Make it this many pages */
    i = index * sizeof *page_dir;
    i /= malloc_pagesize;
    i += 2;

    /* remember the old mapping size */
    oldlen = malloc_ninfo * sizeof *page_dir;

    /*
     * NOTE: we allocate new pages and copy the directory rather than tempt
     * fate by trying to "grow" the region.. There is nothing to prevent
     * us from accidently re-mapping space that's been allocated by our caller
     * via dlopen() or other mmap().
     *
     * The copy problem is not too bad, as there is 4K of page index per
     * 4MB of malloc arena.
     *
     * We can totally avoid the copy if we open a file descriptor to associate
     * the anon mappings with.  Then, when we remap the pages at the new
     * address, the old pages will be "magically" remapped..  But this means
     * keeping open a "secret" file descriptor.....
     */

    /* Get new pages */
    nEW = (struct pginfo**) MMAP(i * malloc_pagesize);
    if (nEW == MAP_FAILED)
        return (0);

    /* Copy the old stuff */
    memcpy(nEW, page_dir,
            malloc_ninfo * sizeof *page_dir);

    /* register the new size */
    malloc_ninfo = i * malloc_pagesize / sizeof *page_dir;

    /* swap the pointers */
    old = page_dir;
    page_dir = nEW;

    /* Now free the old stuff */
    munmap(old, oldlen);
    return (1);
}

/*
 * Initialize the world
 */
static void
malloc_init(void)
{                               /* ;;: call _pma_open() */
    const char *p;
    char b[64];
    int i, j;
    int save_errno = errno;

    INIT_MMAP();

#ifdef MALLOC_EXTRA_SANITY
    malloc_junk = 1;
#endif /* MALLOC_EXTRA_SANITY */

    for (i = 0; i < 3; i++) {
        if (i == 0) {
            j = readlink("/etc/malloc.conf", b, sizeof b - 1);
            if (j <= 0)
                continue;
            b[j] = '\0';
            p = b;
        } else if (i == 1 && issetugid() == 0) {
            p = getenv("MALLOC_OPTIONS");
        } else if (i == 1) {
            continue;
        } else {
            p = _malloc_options;
        }
        for (; p != NULL && *p != '\0'; p++) {
            switch (*p) {
                case '>': malloc_cache   <<= 1; break;
                case '<': malloc_cache   >>= 1; break;
                case 'a': malloc_abort   = 0; break;
                case 'A': malloc_abort   = 1; break;
#if defined(MADV_FREE)
                case 'h': malloc_hint    = 0; break;
                case 'H': malloc_hint    = 1; break;
#endif
                case 'r': malloc_realloc = 0; break;
                case 'R': malloc_realloc = 1; break;
                case 'j': malloc_junk    = 0; break;
                case 'J': malloc_junk    = 1; break;
#ifdef HAS_UTRACE
                case 'u': malloc_utrace  = 0; break;
                case 'U': malloc_utrace  = 1; break;
#endif
                case 'v': malloc_sysv    = 0; break;
                case 'V': malloc_sysv    = 1; break;
                case 'x': malloc_xmalloc = 0; break;
                case 'X': malloc_xmalloc = 1; break;
                case 'z': malloc_zero    = 0; break;
                case 'Z': malloc_zero    = 1; break;
                default:
                    _malloc_message(_getprogname(), malloc_func,
                         " warning: ", "unknown char in MALLOC_OPTIONS\n");
                    break;
            }
        }
    }


    UTRACE(0, 0, 0);

    /*
     * We want junk in the entire allocation, and zero only in the part
     * the user asked for.
     */
    if (malloc_zero)
        malloc_junk=1;

    bt_state_new(&bt_state);
    if (mkdir(PHKPMA_PATH, 0774) == -1)
      abort();
    if (bt_state_open(bt_state, PHKPMA_PATH, 0, 0644) != 0) {
      fprintf(stderr, "Error opening PMA: %s\n", strerror(errno));
      abort();
    }

    /* Allocate one page for the page directory */
    page_dir = (struct pginfo **) MMAP(malloc_pagesize); /* ;;: call _bt_malloc() */

    if (page_dir == MAP_FAILED)
        wrterror("mmap(2) failed, check limits\n");

    /*
     * We need a maximum of malloc_pageshift buckets, steal these from the
     * front of the page_directory;
     */
    malloc_origo = ((u_long)pageround((u_long)sbrk(0))) >> malloc_pageshift;
    malloc_origo -= malloc_pageshift;

    malloc_ninfo = malloc_pagesize / sizeof *page_dir;

    /* Recalculate the cache size in bytes, and make sure it's nonzero */

    if (!malloc_cache)
        malloc_cache++;

    malloc_cache <<= malloc_pageshift;

    /*
     * This is a nice hack from Kaleb Keithly (kaleb@x.org).
     * We can sbrk(2) further back when we keep this on a low address.
     */
    px = (struct pgfree *) imalloc (sizeof *px);
    errno = save_errno;
}

/*
 * Allocate a number of complete pages
 */
static void *
malloc_pages(size_t size)
{                               /* ;;: call bt_malloc() */
    void *p, *delay_free = NULL;
    size_t i;
    struct pgfree *pf;
    u_long index;

    size = pageround(size);

    p = NULL;

    /* Look for free pages before asking for more */
    for(pf = free_list.next; pf; pf = pf->next) {

#ifdef MALLOC_EXTRA_SANITY
        if (pf->size & malloc_pagemask)
            wrterror("(ES): junk length entry on free_list\n");
        if (!pf->size)
            wrterror("(ES): zero length entry on free_list\n");
        if (pf->page == pf->end)
            wrterror("(ES): zero entry on free_list\n");
        if (pf->page > pf->end)
            wrterror("(ES): sick entry on free_list\n");
        if ((void*)pf->page >= (void*)sbrk(0))
            wrterror("(ES): entry on free_list past brk\n");
        if (page_dir[ptr2index(pf->page)] != MALLOC_FREE)
            wrterror("(ES): non-free first page on free-list\n");
        if (page_dir[ptr2index(pf->end)-1] != MALLOC_FREE)
            wrterror("(ES): non-free last page on free-list\n");
#endif /* MALLOC_EXTRA_SANITY */

        if (pf->size < size)
            continue;

        if (pf->size == size) {
            p = pf->page;
            if (pf->next != NULL)
                    pf->next->prev = pf->prev;
            pf->prev->next = pf->next;
            delay_free = pf;
            break;
        }

        p = pf->page;
        pf->page = (char *)pf->page + size;
        pf->size -= size;
        break;
    }

#ifdef MALLOC_EXTRA_SANITY
    if (p != NULL && page_dir[ptr2index(p)] != MALLOC_FREE)
        wrterror("(ES): allocated non-free page on free-list\n");
#endif /* MALLOC_EXTRA_SANITY */

    size >>= malloc_pageshift;

    /* Map new pages */
    if (p == NULL)
        p = map_pages(size);

    if (p != NULL) {

        index = ptr2index(p);
        page_dir[index] = MALLOC_FIRST;
        for (i=1;i<size;i++)
            page_dir[index+i] = MALLOC_FOLLOW;

        if (malloc_junk)
            memset(p, SOME_JUNK, size << malloc_pageshift);
    }

    if (delay_free) {
        if (px == NULL)
            px =  (struct pgfree *) delay_free;
        else
            ifree(delay_free);
    }

    return (p);
}

/*
 * Allocate a page of fragments
 */

static __inline int
malloc_make_chunks(int bits)
{                 /* ;;: I'd guess we can avoid calling into bt routines here */
    struct  pginfo *bp;
    void *pp;
    int i, k, l;

    /* Allocate a new bucket */
    pp = malloc_pages(malloc_pagesize);
    if (pp == NULL)
        return (0);

    /* Find length of admin structure */
    l = offsetof(struct pginfo, bits[0]);
    l += sizeof bp->bits[0] *
        (((malloc_pagesize >> bits)+MALLOC_BITS-1) / MALLOC_BITS);

    /* Don't waste more than two chunks on this */
    if ((1<<(bits)) <= l+l) {
        bp = (struct  pginfo *)pp;
    } else {
        bp = (struct  pginfo *)imalloc(l);
        if (bp == NULL) {
            ifree(pp);
            return (0);
        }
    }

    bp->size = (1<<bits);
    bp->shift = bits;
    bp->total = bp->free = malloc_pagesize >> bits;
    bp->page = pp;

    /* set all valid bits in the bitmap */
    k = bp->total;
    i = 0;

    /* Do a bunch at a time */
    for(;k-i >= MALLOC_BITS; i += MALLOC_BITS)
        bp->bits[i / MALLOC_BITS] = ~0;

    for(; i < k; i++)
        bp->bits[i/MALLOC_BITS] |= 1<<(i%MALLOC_BITS);

    if (bp == bp->page) {
        /* Mark the ones we stole for ourselves */
        for(i=0;l > 0;i++) {
            bp->bits[i/MALLOC_BITS] &= ~(1<<(i%MALLOC_BITS));
            bp->free--;
            bp->total--;
            l -= (1 << bits);
        }
    }

    /* MALLOC_LOCK */

    page_dir[ptr2index(pp)] = bp;

    bp->next = page_dir[bits];
    page_dir[bits] = bp;

    /* MALLOC_UNLOCK */

    return (1);
}

/*
 * Allocate a fragment
 */
static void *
malloc_bytes(size_t size)
{                 /* ;;: I'd guess we can avoid calling into bt routines here */
    int i,j;
    u_int u;
    struct  pginfo *bp;
    int k;
    u_int *lp;

    /* Don't bother with anything less than this */
    if (size < malloc_minsize)
        size = malloc_minsize;

    /* Find the right bucket */
    j = 1;
    i = size-1;
    while (i >>= 1)
        j++;

    /* If it's empty, make a page more of that size chunks */
    if (page_dir[j] == NULL && !malloc_make_chunks(j))
        return (NULL);

    bp = page_dir[j];

    /* Find first word of bitmap which isn't empty */
    for (lp = bp->bits; !*lp; lp++)
        ;

    /* Find that bit, and tweak it */
    u = 1;
    k = 0;
    while (!(*lp & u)) {
        u += u;
        k++;
    }
    *lp ^= u;

    /* If there are no more free, remove from free-list */
    if (!--bp->free) {
        page_dir[j] = bp->next;
        bp->next = NULL;
    }

    /* Adjust to the real offset of that chunk */
    k += (lp-bp->bits)*MALLOC_BITS;
    k <<= bp->shift;

    if (malloc_junk)
        memset((u_char *)bp->page + k, SOME_JUNK, bp->size);

    return ((u_char *)bp->page + k);
}

/*
 * Allocate a piece of memory
 */
static void *
imalloc(size_t size)
{
    void *result;

    if (suicide)
        abort();

    if ((size + malloc_pagesize) < size)	/* Check for overflow */
        result = NULL;
    else if ((size + malloc_pagesize) >= (uintptr_t)page_dir)
        result = NULL;
    else if (size <= malloc_maxsize)
        result = malloc_bytes(size);
    else
        result = malloc_pages(size);

    if (malloc_zero && result != NULL)
        memset(result, 0, size);

    return (result);
}

/*
 * Change the size of an allocation.
 */
static void *
irealloc(void *ptr, size_t size)
{
    void *p;
    u_long osize, index;
    struct pginfo **mp;
    int i;

    if (suicide)
        abort();

    index = ptr2index(ptr);

#ifdef MALLOC_SANITY	/* <eric> */
    if (index < malloc_pageshift) {
        wrtwarning("junk pointer, too low to make sense\n");
        return (NULL);
    }

    if (index > last_index) {
        wrtwarning("junk pointer, too high to make sense\n");
        return (NULL);
    }
#endif	/* <eric> MALLOC_SANITY */

    mp = &page_dir[index];

    if (*mp == MALLOC_FIRST) {			/* Page allocation */

#ifdef MALLOC_SANITY	/* <eric> */
        /* Check the pointer */
        if ((u_long)ptr & malloc_pagemask) {
            wrtwarning("modified (page-) pointer\n");
            return (NULL);
        }
#endif	/* <eric> MALLOC_SANITY	*/

        /* Find the size in bytes */
        for (osize = malloc_pagesize; *(++mp) == MALLOC_FOLLOW;)
            osize += malloc_pagesize;

        if (!malloc_realloc &&                  /* Unless we have to, */
          size <= osize &&                      /* .. or are too small, */
          size > (osize - malloc_pagesize)) {	/* .. or can free a page, */
            if (malloc_junk)
                memset((u_char *)ptr + size, SOME_JUNK, osize-size);
            return (ptr);			/* ..don't do anything else. */
        }

    } else if (*mp >= MALLOC_MAGIC) {		/* Chunk allocation */

#ifdef MALLOC_SANITY	/* <eric> */
        /* Check the pointer for sane values */
        if (((u_long)ptr & ((*mp)->size-1))) {
            wrtwarning("modified (chunk-) pointer\n");
            return (NULL);
        }
#endif	/* <eric> MALLOC_SANITY */

        /* Find the chunk index in the page */
        i = ((u_long)ptr & malloc_pagemask) >> (*mp)->shift;

        /* Verify that it isn't a free chunk already */
        if ((*mp)->bits[i/MALLOC_BITS] & (1<<(i%MALLOC_BITS))) {
            wrtwarning("chunk is already free\n");
            return (NULL);
        }

        osize = (*mp)->size;

        if (!malloc_realloc &&		/* Unless we have to, */
          size <= osize &&              /* ..or are too small, */
          (size > osize/2 ||	        /* ..or could use a smaller size, */
          osize == malloc_minsize)) {	/* ..(if there is one) */
            if (malloc_junk)
                memset((u_char *)ptr + size, SOME_JUNK, osize-size);
            return (ptr);		/* ..don't do anything else. */
        }

    } else {
        wrtwarning("pointer to wrong page\n");
        return (NULL);
    }

    p = imalloc(size);

    if (p != NULL) {
        /* copy the lesser of the two sizes, and free the old one */
        if (!size || !osize)
            ;
        else if (osize < size)
            memcpy(p, ptr, osize);
        else
            memcpy(p, ptr, size);
        ifree(ptr);
    }
    return (p);
}

/*
 * Free a sequence of pages
 */

static __inline void
free_pages(void *ptr, u_long index, struct pginfo const *info)
{
    u_long i;
    struct pgfree *pf, *pt=NULL;
    u_long l;
    void *tail;

#ifdef MALLOC_SANITY	/* <eric> */
    if (info == MALLOC_FREE) {
        wrtwarning("page is already free\n");
        return;
    }

    if (info != MALLOC_FIRST) {
        wrtwarning("pointer to wrong page\n");
        return;
    }

    if ((u_long)ptr & malloc_pagemask) {
        wrtwarning("modified (page-) pointer\n");
        return;
    }
#endif	/* <eric> MALLOC_SANITY	*/

    /* Count how many pages and mark them free at the same time */
    page_dir[index] = MALLOC_FREE;
    for (i = 1; page_dir[index+i] == MALLOC_FOLLOW; i++)
        page_dir[index + i] = MALLOC_FREE;

    l = i << malloc_pageshift;

    if (malloc_junk)
        memset(ptr, SOME_JUNK, l);

#if defined(MADV_FREE)
        if (malloc_hint)
        madvise(ptr, l, MADV_FREE);

#endif

    tail = (char *)ptr+l;

    /* add to free-list */
    if (px == NULL)
        px =  (struct pgfree *) imalloc(sizeof *px);	/* This cannot fail... */
    px->page = ptr;
    px->end =  tail;
    px->size = l;
    if (free_list.next == NULL) {

        /* Nothing on free list, put this at head */
        px->next = free_list.next;
        px->prev = &free_list;
        free_list.next = px;
        pf = px;
        px = NULL;

    } else {

        /* Find the right spot, leave pf pointing to the modified entry. */
        tail = (char *)ptr+l;

        for(pf = free_list.next; pf->end < ptr && pf->next != NULL;
            pf = pf->next)
            ; /* Race ahead here */

        if (pf->page > tail) {
            /* Insert before entry */
            px->next = pf;
            px->prev = pf->prev;
            pf->prev = px;
            px->prev->next = px;
            pf = px;
            px = NULL;
        } else if (pf->end == ptr ) {
            /* Append to the previous entry */
            pf->end = (char *)pf->end + l;
            pf->size += l;
            if (pf->next != NULL && pf->end == pf->next->page ) {
                /* And collapse the next too. */
                pt = pf->next;
                pf->end = pt->end;
                pf->size += pt->size;
                pf->next = pt->next;
                if (pf->next != NULL)
                    pf->next->prev = pf;
            }
        } else if (pf->page == tail) {
            /* Prepend to entry */
            pf->size += l;
            pf->page = ptr;
        } else if (pf->next == NULL) {
            /* Append at tail of chain */
            px->next = NULL;
            px->prev = pf;
            pf->next = px;
            pf = px;
            px = NULL;
        } else {
            wrterror("freelist is destroyed\n");
        }
    }

    /* Return something to OS ? */
    if (pf->next == NULL &&			/* If we're the last one, */
      pf->size > malloc_cache &&		/* ..and the cache is full, */
      pf->end == malloc_brk &&			/* ..and none behind us, */
      malloc_brk == sbrk(0)) {			/* ..and it's OK to do... */

        /*
         * Keep the cache intact.  Notice that the '>' above guarantees that
         * the pf will always have at least one page afterwards.
         */
        pf->end = (char *)pf->page + malloc_cache;
        pf->size = malloc_cache;

        brk(pf->end);
        malloc_brk = pf->end;

        index = ptr2index(pf->end);

        for(i=index;i <= last_index;)
            page_dir[i++] = MALLOC_NOT_MINE;

        last_index = index - 1;

        /* XXX: We could realloc/shrink the pagedir here I guess. */
    }
    if (pt != NULL)
        ifree(pt);
}

/*
 * Free a chunk, and possibly the page it's on, if the page becomes empty.
 */

static __inline void
free_bytes(void *ptr, u_long index, struct pginfo *info)
{
    int i;
    struct pginfo **mp;
    void *vp;

    /* Find the chunk number on the page */
    i = ((u_long)ptr & malloc_pagemask) >> info->shift;

#ifdef MALLOC_SANITY	/* <eric> */
    if (((u_long)ptr & (info->size-1))) {
        wrtwarning("modified (chunk-) pointer\n");
        return;
    }
#endif	/* <eric> MALLOC_SANITY */

    if (info->bits[i/MALLOC_BITS] & (1<<(i%MALLOC_BITS))) {
        wrtwarning("chunk is already free\n");
        return;
    }

    if (malloc_junk)
        memset(ptr, SOME_JUNK, info->size);

    info->bits[i/MALLOC_BITS] |= 1<<(i%MALLOC_BITS);
    info->free++;

    mp = page_dir + info->shift;

    if (info->free == 1) {

        /* Page became non-full */

        mp = page_dir + info->shift;
        /* Insert in address order */
        while (*mp && (*mp)->next && (*mp)->next->page < info->page)
            mp = &(*mp)->next;
        info->next = *mp;
        *mp = info;
        return;
    }

    if (info->free != info->total)
        return;

    /* Find & remove this page in the queue */
    while (*mp != info) {
        mp = &((*mp)->next);
#ifdef MALLOC_EXTRA_SANITY
        if (!*mp)
                wrterror("(ES): Not on queue\n");
#endif /* MALLOC_EXTRA_SANITY */
    }
    *mp = info->next;

    /* Free the page & the info structure if need be */
    page_dir[ptr2index(info->page)] = MALLOC_FIRST;
    vp = info->page;		/* Order is important ! */
    if(vp != (void*)info)
        ifree(info);
    ifree(vp);
}

static void
ifree(void *ptr)
{
    struct pginfo *info;
    u_long index;

    /* This is legal */
    if (ptr == NULL)
        return;

    /* If we're already sinking, don't make matters any worse. */
    if (suicide)
        return;

    index = ptr2index(ptr);

#ifdef MALLOC_SANITY	/* <eric> */
    if (index < malloc_pageshift) {
        wrtwarning("junk pointer, too low to make sense\n");
        return;
    }

    if (index > last_index) {
        wrtwarning("junk pointer, too high to make sense\n");
        return;
    }
#endif	/* <eric> MALLOC_SANITY */

    info = page_dir[index];

    if (info < MALLOC_MAGIC)
        free_pages(ptr, index, info);
    else
        free_bytes(ptr, index, info);
    return;
}

static void *
pubrealloc(void *ptr, size_t size, const char *func)
{
    void *r;
    int err = 0;
    static int malloc_active; /* Recusion flag for public interface. */
    static unsigned malloc_started; /* Set when initialization has been done */

    _MALLOC_LOCK();
#ifdef MALLOC_SANITY	/* <eric> */
    /*
     * If a thread is inside our code with a functional lock held, and then
     * catches a signal which calls us again, we would get a deadlock if the
     * lock is not of a recursive type.
     */
    malloc_func = func;
    if (malloc_active > 0) {
        if (malloc_active == 1) {
            wrtwarning("recursive call\n");
            malloc_active = 2;
        }
        _MALLOC_UNLOCK();
        errno = EDOOFUS;
        return (NULL);
    }
    malloc_active = 1;
#endif	/* <eric> MALLOC_SANITY */

    if (!malloc_started) {
        if (ptr != NULL) {
            wrtwarning("malloc() has never been called\n");
            malloc_active = 0;
            _MALLOC_UNLOCK();
            errno = EDOOFUS;
            return (NULL);
        }
        malloc_init();
        malloc_started = 1;
    }

    if (ptr == ZEROSIZEPTR)
        ptr = NULL;
    if (malloc_sysv && !size) {
        if (ptr != NULL)
            ifree(ptr);
        r = NULL;
    } else if (!size) {
        if (ptr != NULL)
            ifree(ptr);
        r = ZEROSIZEPTR;
    } else if (ptr == NULL) {
        r = imalloc(size);
        err = (r == NULL);
    } else {
        r = irealloc(ptr, size);
        err = (r == NULL);
    }
#ifdef MALLOC_SANITY	/* <eric> */
    UTRACE(ptr, size, r);
    malloc_active = 0;
#endif

    _MALLOC_UNLOCK();

#ifdef MALLOC_SANITY
    if (malloc_xmalloc && err)
        wrterror("out of memory\n");
#endif	/* <eric> MALLOC_SANITY */
    if (err)
        errno = ENOMEM;
    return (r);
}

/*
 * These are the public exported interface routines.
 */

void *
phkmalloc(size_t size)	/* <eric> */
{

    return (pubrealloc(NULL, size, " in malloc():"));
}

/* ;;: export a sync routine. Or I guess client program can just call pma_sync() */

void
phkfree(void *ptr)		/* <eric> */
{

    pubrealloc(ptr, 0, " in free():");
}

void *
phkrealloc(void *ptr, size_t size)	/* <eric> */
{

    return (pubrealloc(ptr, size, " in realloc():"));
}


size_t
phkmalloc_usable_size(void * ptr) {			/* <eric> */

  size_t sz = -1;
  //	malloc_func = " in getsize():";
  u_long index = ptr2index(ptr);

  if ((index < malloc_pageshift) || (index > last_index)) {
    return sz; // Maximum possible size.
  }

  struct pginfo * info = page_dir[index];

  if (info < MALLOC_MAGIC) {
    if (info == MALLOC_FREE) {
      return 0;
    }
#ifdef MALLOC_SANITY	/* <eric> */
    if (info == MALLOC_FREE) {
      wrtwarning("page is already free\n");
      return 0;
    }

    if (info != MALLOC_FIRST) {
      wrtwarning("pointer to wrong page\n");
      return 0;
    }

    if ((u_long)ptr & malloc_pagemask) {
      wrtwarning("modified (page-) pointer\n");
      return 0;
    }
#endif	/* <eric> MALLOC_SANITY */

    /* EDB - this code ensures that the returned size is
       the amount of space from the (possibly interior) pointer
       to the end of the allocated space for this object. */

    u_long i;
    for (i = 1; page_dir[index+i] == MALLOC_FOLLOW; i++)
      ;

    sz = i << malloc_pageshift  - ((size_t) ptr & (malloc_pagesize - 1));
  }
  else {
    sz = info->size - ((size_t) ptr & (info->size - 1));
  }
  return sz;
}


void *
phkmalloc_normalize (void * ptr) {
  // First, check to see if this is a heap object.
  // If not, return NULL.
  u_long index = ptr2index(ptr);

  if ((index < malloc_pageshift) || (index > last_index)) {
    return NULL;
  }

  struct pginfo * info = page_dir[index];

  if (info < MALLOC_MAGIC) {
    if (info == MALLOC_FREE) {
      return 0;
    }
#ifdef MALLOC_SANITY	/* <eric> */
    if (info == MALLOC_FREE) {
      wrtwarning("page is already free\n");
      return 0;
    }

    if (info != MALLOC_FIRST) {
      wrtwarning("pointer to wrong page\n");
      return 0;
    }

    if ((u_long)ptr & malloc_pagemask) {
      wrtwarning("modified (page-) pointer\n");
      return 0;
    }
#endif	/* <eric> MALLOC_SANITY */

    /* March backwards until we find the first page. */

    u_long i;
    for (i = 0; page_dir[index-i] != MALLOC_FIRST; i++)
      ;

    return (void *) index2ptr(index-i);
  }
  else {
    return (void *) ((size_t) ptr & ~(info->size - 1));
  }

}

/* test code */
int main(int argc, char *argv[])
{
  int *a, *b, *c;
  a = phkmalloc(sizeof *a);
  b = phkmalloc(sizeof *b);
  c = phkmalloc(sizeof *c);
  *a = 10;
  *b = 20;
  *c = 30;
  fprintf(stderr, "a: %i, b: %i, c: %i", *a, *b, *c);
  bt_state_close(bt_state);
  if (bt_state_open(bt_state, PHKPMA_PATH, 0, 0644) != 0) {
    fprintf(stderr, "Error opening PMA: %s\n", strerror(errno));
    abort();
  }
  fprintf(stderr, "a: %i, b: %i, c: %i", *a, *b, *c);
  bt_state_close(bt_state);

  return 0;
}
