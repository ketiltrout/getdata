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
  int error, r = 0;
#ifdef GD_NO_C99_API
  const double cdividend[2] = {33.3, 44.4};
#else
  const double complex cdividend = 33.3 + _Complex_I * 44.4;
#endif
  gd_entry_t e;
  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);

  gd_add_crecip(D, "new", "in", cdividend, 0);
  error = gd_error(D);

  /* check */
  gd_entry(D, "new", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_RECIP_ENTRY);
    CHECKS(e.in_fields[0], "in");
    CHECKI(e.comp_scal, 1);
    CHECKC(e.EN(recip,cdividend), cdividend);
    CHECKI(e.fragment_index, 0);

    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
