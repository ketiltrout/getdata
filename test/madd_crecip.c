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

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_phase(D, "new", "in", 3, 0);
#ifdef GD_NO_C99_API
  const double v[] = {3.2, 3.1};
#else
  const double complex v = 3.2 + _Complex_I * 3.1;
#endif
  gd_madd_crecip(D, "new", "meta", "in1", v);
  int error = gd_error(D);

  /* check */
  gd_entry(D, "new/meta", &e);
  int ge_error = gd_error(D);
  CHECKI(ge_error, 0);
  if (!r) {
    CHECKI(e.field_type, GD_RECIP_ENTRY);
    CHECKS(e.in_fields[0], "in1");
    CHECKC(e.u.recip.cdividend, v);
    CHECKI(e.comp_scal, 1);
    CHECKI(e.fragment_index, 0);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
