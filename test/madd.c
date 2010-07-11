/* Add a dirfile field */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  int r = 0;
  unsigned char val;
  gd_entry_t e;

  gd_entry_t E;
  memset(&E, 0, sizeof(E));
  E.field = "data";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.spf = 2;
  E.data_type = GD_UINT8;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add(D, &E);
  E.field_type = GD_CONST_ENTRY;
  E.const_type = GD_UINT8;
  gd_madd(D, &E, "data");
  int error = gd_error(D);

  /* check */
  int n = gd_nfields(D);
  gd_entry(D, "data/data", &e);
  int ge_error = gd_error(D);
  CHECKI(ge_error, 0);
  if (!r) {
    CHECKI(e.field_type, GD_CONST_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.const_type, GD_UINT8);
    gd_get_constant(D, "data/data", GD_UINT8, &val);
    CHECKU(val, 0);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKI(n, 2);

  return r;
}
