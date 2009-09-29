/* Add a complex POLYNOM field */
#include "../src/getdata.h"

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

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add_phase(D, "new", "in", 3, 0);
  const double complex a[4] = {1 + _Complex_I * 29.03, 0.3 + _Complex_I * 12.34,
    0.5 + _Complex_I * 99.55, 1.8 + _Complex_I * 45.32};
  dirfile_madd_cpolynom(D, "new", "meta", 3, "in", a);
  int error = get_error(D);

  /* check */
  get_entry(D, "new/meta", &e);
  if (get_error(D))
    r = 1;
  else {
    if (e.field_type != GD_POLYNOM_ENTRY) {
      fprintf(stderr, "field_type = %i\n", e.field_type);
      r = 1;
    }
    if (strcmp(e.in_fields[0], "in")) {
      fprintf(stderr, "in_field = %s\n", e.in_fields[0]);
      r = 1;
    }
    if (e.fragment_index != 0) {
      fprintf(stderr, "fragment_index = %i\n", e.fragment_index);
      r = 1;
    }
    if (e.poly_ord != 3) {
      fprintf(stderr, "poly_ord = %i\n", e.poly_ord);
      r = 1;
    }
    if (e.comp_scal != 1) {
      fprintf(stderr, "poly_ord = %i\n", e.poly_ord);
      r = 1;
    }
    for (j = 0; j < 4; ++j)
      if (cabs(e.ca[j] - a[j]) > 1e-6) {
        fprintf(stderr, "a[%i] = %g\n", j, e.a[j]);
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
