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

static_assert(sizeof(unsigned long long) <= sizeof(uintptr_t));
static_assert(sizeof(unsigned long long) <= sizeof(size_t));

#define EVENTNUM_IDX 0
static uint64_t eventnum = 0;

int main(int argc, char *argv[])
{
#define DEMO_PATH "./pma"

  BT_state      *state;
  char          *addr_s, *size_s;
  uintptr_t      addr;
  size_t         size_b;


  DEMO_PUTS("== PMA READ Demo");

  /* create pma directory */
  if (mkdir(DEMO_PATH, 0774) == -1
      && errno != EEXIST) {
    DEMO_PRINTF("== failed to create PMA directory: %s", strerror(errno));
  }

  /* argparsing
   ./read ADDR SIZE
  */
  assert(argc == 3);
  addr_s = argv[1];
  size_s = argv[2];

  addr = strtoull(addr_s, NULL, 16);
  size_b = strtoull(size_s, NULL, 16);

  /* initialize state and open PMA */
  bt_state_new(&state);
  assert(SUCC(bt_state_open(state, DEMO_PATH, 0, 0644)));
  DEMO_PUTS("== PMA opened at " DEMO_PATH);

  eventnum = bt_meta_get(state, EVENTNUM_IDX);
  bt_meta_set(state, EVENTNUM_IDX, eventnum);

  /* the pma has restored the memory map of the process. just write to stdout
     what's present at addr */
  DEMO_PRINTF("== printing data at %p to stdout...", (void *)addr);
  fwrite((void *)addr, sizeof(char), size_b, stdout);

  /* shutdown the pma */
  DEMO_PUTS("== closing PMA");
  assert(SUCC(bt_state_close(state)));
}
