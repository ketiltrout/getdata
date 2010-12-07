/* Add a LINCOM field */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  int error, ge_error, r = 0;
  gd_entry_t e;
  const char* in_fields[2] = {"in1", "in2"};
  const double m[2] = {1, 0.3};
  const double b[2] = {0, 0.9};

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_phase(D, "new", "in", 3, 0);
  gd_madd_lincom(D, "new", "meta", 2, in_fields, m, b);
  error = gd_error(D);

  /* check */
  gd_entry(D, "new/meta", &e);
  ge_error = gd_error(D);
  CHECKI(ge_error, 0);
  if (!r) {
    CHECKI(e.field_type, GD_LINCOM_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(lincom,n_fields), 2);
    CHECKS(e.in_fields[0], "in1");
    CHECKS(e.in_fields[1], "in2");
    CHECKF(e.EN(lincom,m)[0], m[0]);
    CHECKF(e.EN(lincom,m)[1], m[1]);
    CHECKF(e.EN(lincom,b)[0], b[0]);
    CHECKF(e.EN(lincom,b)[1], b[1]);
    CHECKI(e.comp_scal, 0);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  return r;
}
