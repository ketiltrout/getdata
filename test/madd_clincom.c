/* Add a LINCOM field */
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
  const char* in_fields[2] = {"in1", "in2"};
#ifdef GD_NO_C99_API
  const double m[2][2] = {{1, 3.3}, {0.3, 18.3}};
  const double b[2][2] = {{2, 3.8}, {2.1, 9.8}};
  gd_madd_clincom(D, "new", "meta", 2, in_fields, (double*)m, (double*)b);
#else
  const double complex m[2] = {1 + _Complex_I * 3.3, 0.3 + _Complex_I * 18.3};
  const double complex b[2] = {2 + _Complex_I * 3.8, 2.1 + _Complex_I * 9.8};
  gd_madd_clincom(D, "new", "meta", 2, in_fields, m, b);
#endif
  int error = gd_error(D);

  /* check */
  gd_entry(D, "new/meta", &e);
  int ge_error = gd_error(D);
  CHECKI(ge_error, 0);
  if (!r) {
    CHECKI(e.field_type, GD_LINCOM_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(lincom,n_fields), 2);
    CHECKS(e.in_fields[0], "in1");
    CHECKS(e.in_fields[1], "in2");
    CHECKC(e.EN(lincom,cm)[0], m[0]);
    CHECKC(e.EN(lincom,cm)[1], m[1]);
    CHECKC(e.EN(lincom,cb)[0], b[0]);
    CHECKC(e.EN(lincom,cb)[1], b[1]);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
