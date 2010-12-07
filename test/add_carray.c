#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  uint8_t val[] = {3, 4, 5, 6, 7};
  uint8_t data[5];
  int n, error;
  int r = 0;
  gd_entry_t e;
  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);

  gd_add_carray(D, "data", GD_UINT8, 5, GD_UINT8, &val, 0);
  error = gd_error(D);

  /* check */
  gd_entry(D, "data", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_CARRAY_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(scalar,const_type), GD_UINT8);
    gd_free_entry_strings(&e);
  }
  n = (int)gd_carray_len(D, "data");
  CHECKI(n, 5);
  gd_get_carray(D, "data", GD_UINT8, &data);
  for (n = 0; n < 5; ++n)
    CHECKIi(n, data[n], 3 + n);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
