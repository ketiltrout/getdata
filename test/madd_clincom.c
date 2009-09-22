/* Add a LINCOM field */
#include "../src/getdata.h"

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

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add_phase(D, "new", "in", 3, 0);
  const char* in_fields[2] = {"in1", "in2"};
  const double complex m[2] = {1 + _Complex_I * 3.3, 0.3 + _Complex_I * 18.3};
  const double complex b[2] = {2 + _Complex_I * 3.8, 2.1 + _Complex_I * 9.8};
  dirfile_madd_clincom(D, "new", "meta", 2, in_fields, m, b);
  int error = get_error(D);

  /* check */
  get_entry(D, "new/meta", &e);
  if (get_error(D))
    r = 1;
  else {
    if (e.field_type != GD_LINCOM_ENTRY) {
      fprintf(stderr, "field_type = %i\n", e.field_type);
      r = 1;
    }
    if (e.fragment_index != 0) {
      fprintf(stderr, "fragment_index = %i\n", e.fragment_index);
      r = 1;
    }
    if (e.n_fields != 2) {
      fprintf(stderr, "n_fields = %i\n", e.n_fields);
      r = 1;
    }
    if (strcmp(e.in_fields[0], "in1")) {
      fprintf(stderr, "in_fields[0] = %s\n", e.in_fields[0]);
      r = 1;
    }
    if (strcmp(e.in_fields[1], "in2")) {
      fprintf(stderr, "in_fields[1] = %s\n", e.in_fields[1]);
      r = 1;
    }
    if (cabs(e.cm[0] - m[0]) > 1e-6) {
      fprintf(stderr, "m[0] = %g;%g\n", creal(e.m[0]), cimag(e.m[0]));
      r = 1;
    }
    if (cabs(e.cm[1] - m[1]) > 1e-6) {
      fprintf(stderr, "m[1] = %g;%g\n", creal(e.m[1]), cimag(e.m[1]));
      r = 1;
    }
    if (cabs(e.cb[0] - b[0]) > 1e-6) {
      fprintf(stderr, "b[0] = %g;%g\n", creal(e.b[0]), cimag(e.b[0]));
      r = 1;
    }
    if (cabs(e.cb[1] - b[1]) > 1e-6) {
      fprintf(stderr, "b[1] = %g;%g\n", creal(e.b[1]), cimag(e.b[1]));
      r = 1;
    }
    if (e.comp_scal != 1) {
      fprintf(stderr, "comp_scal = %i\n", e.comp_scal);
      r = 1;
    }
    dirfile_free_entry_strings(&e);
  }

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (r)
    return 1;

  return (error != GD_E_OK);
}
