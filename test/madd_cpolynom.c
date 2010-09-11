/* Add a complex POLYNOM field */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  int r = 0;
  int j;
  gd_entry_t e;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_phase(D, "new", "in", 3, 0);
#ifdef GD_NO_C99_API
  const double a[4][2] = {{1, 29.03}, {0.3, 12.34}, {0.5, 99.55}, {1.8, 45.32}};
  gd_madd_cpolynom(D, "new", "meta", 3, "in", (double*)a);
#else
  const double complex a[4] = {1 + _Complex_I * 29.03, 0.3 + _Complex_I * 12.34,
    0.5 + _Complex_I * 99.55, 1.8 + _Complex_I * 45.32};
  gd_madd_cpolynom(D, "new", "meta", 3, "in", a);
#endif
  int error = gd_error(D);

  /* check */
  gd_entry(D, "new/meta", &e);
  int ge_error = gd_error(D);
  CHECKI(ge_error, 0);
  if (!r) {
    CHECKI(e.field_type, GD_POLYNOM_ENTRY);
    CHECKS(e.in_fields[0], "in");
    CHECKI(e.fragment_index, 0);
    CHECKI(e.u.polynom.poly_ord, 3);
    CHECKI(e.comp_scal, 1);
    for (j = 0; j < 4; ++j)
      CHECKCi(j,e.u.polynom.ca[j], a[j]);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
