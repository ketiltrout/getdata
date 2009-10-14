/* Add a dirfile field */
#include "../src/getdata.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  int r = 0;

  gd_entry_t E, e;
  E.field = "data";
  E.field_type = GD_LINCOM_ENTRY;
  E.fragment_index = 0;
  E.n_fields = 1;
  E.comp_scal = 0;
  E.in_fields[0] = "INDEX";
  E.m[0] = 1.;
  E.scalar[0] = NULL;
  E.scalar[0 + GD_MAX_LINCOM] = "c";

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add_spec(D, "c CONST INT64 4", 0);
  dirfile_add(D, &E);
  int error = get_error(D);

  /* check */
  get_entry(D, "data", &e);
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
    if (e.n_fields != 1) {
      fprintf(stderr, "n_fields = %i\n", e.n_fields);
      r = 1;
    }
    if (fabs(e.m[0] - 1) > 1e-6) {
      fprintf(stderr, "m[0] = %g\n", e.m[0]);
      r = 1;
    }
    if (fabs(e.b[0] - 4) > 1e-6) {
      fprintf(stderr, "b[0] = %g\n", e.b[0]);
      r = 1;
    }
    if (e.scalar[0] != NULL) {
      fprintf(stderr, "scalar[0] = %p\n", e.scalar[0]);
      r = 1;
    }
    if (strcmp(e.scalar[0 + GD_MAX_LINCOM], "c")) {
      fprintf(stderr, "scalar[0 + GD_MAX_LINCOM] = %s\n",
          e.scalar[0 + GD_MAX_LINCOM]);
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
