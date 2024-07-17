#include "btree.h"
#include "btree.c"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>


static inline size_t
_B2PAGES(size_t x)
/* return number of pages required to store x bytes */
{
  x += BT_PAGESIZE - 1;
  x &= ~(BT_PAGESIZE - 1);
  return x >> BT_PAGEBITS;
}
/* the opposite of b2pages */
#define _P2BYTES(x) ((size_t)(x) << BT_PAGEBITS)

#define DEMO_PRINTF(fmt, ...)                                              \
        fprintf(stderr, "%s:%d " fmt "\n", __func__, __LINE__, __VA_ARGS__)
#define DEMO_PUTS(arg)	DEMO_PRINTF("%s", arg)

#define EVENTNUM_IDX 0
static uint64_t eventnum = 0;

int main(int argc, char *argv[])
{
#define DEMO_PATH "./pma"

  BT_state      *state;
  struct stat    sb;
  char          *in_path;
  int            fd;
  size_t         size_b, size_p;
  unsigned char *in_dat;
  unsigned char *ret;

  DEMO_PUTS("== PMA WRITE Demo");

  /* create pma directory */
  if (mkdir(DEMO_PATH, 0774) == -1
      && errno != EEXIST) {
    DEMO_PRINTF("== failed to create PMA directory: %s", strerror(errno));
  }

  /* argparsing
   ./write FILEPATH
  */
  assert(argc == 2);
  in_path = argv[1];
  fd = open(in_path, O_RDONLY);

  /* fstat the input file */
  if (fstat(fd, &sb) == -1) {
    DEMO_PRINTF("== fstat of input file failed with: %s", strerror(errno));
    close(fd);
    return 1;
  }

  size_b = sb.st_size;
  size_p = _B2PAGES(size_b);

  /* mmap the input file so that it may be copied into the PMA */
  in_dat = mmap(NULL, size_b, PROT_READ, MAP_PRIVATE, fd, 0);
  if (in_dat == MAP_FAILED) {
    DEMO_PRINTF("== mmap of input file failed with: %s", strerror(errno));
    close(fd);
    return 1;
  }

  /* initialize state and open PMA */
  bt_state_new(&state);
  assert(SUCC(bt_state_open(state, DEMO_PATH, 0, 0644)));
  DEMO_PUTS("== PMA opened at " DEMO_PATH);

  eventnum = bt_meta_get(state, EVENTNUM_IDX);
  bt_meta_set(state, EVENTNUM_IDX, eventnum);

  /* malloc sufficient space to store shrek2 */
  ret = bt_malloc(state, size_p);

  /* memcpy the input file data into the pma alloced space */
  DEMO_PRINTF("== copying input file %s into PMA...", in_path);
  memcpy(ret, in_dat, size_b);
  DEMO_PRINTF("== done. address: %p size (bytes): 0x%zX", ret, size_b);

  /* shutdown the pma */
  DEMO_PUTS("== closing PMA");
  assert(SUCC(bt_state_close(state)));

  munmap(in_dat, size_b);
  return 0;
}
