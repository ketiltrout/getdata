#define GETDATA_C89_API
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
  int r = 0;
  gd_entry_t e;
  double dividend[2] = {33.3, 44.4}; 

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_crecip(D, "new", "in", dividend, 0);
  int error = gd_error(D);

  /* check */
  gd_get_entry(D, "new", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_RECIP_ENTRY);
    CHECKS(e.in_fields[0], "in");
    CHECKI(e.comp_scal, 1);
    CHECKF(e.cdividend[0], dividend[0]);
    CHECKF(e.cdividend[1], dividend[1]);
    CHECKI(e.fragment_index, 0);

    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
