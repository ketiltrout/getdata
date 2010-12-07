/* add a barth-style meta field with add_spec */
#include "test.h"

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
  int error, n, r = 0;
  gd_entry_t e;
  unsigned char val;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_spec(D, "INDEX/meta CONST UINT8 2", 0);
  error = gd_error(D);

  /* check */
  n = gd_nfields(D);

  CHECKI(n, 1);

  gd_entry(D, "INDEX/meta", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_CONST_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(scalar,const_type), GD_UINT8);
    gd_get_constant(D, "INDEX/meta", GD_UINT8, &val);
    CHECKI(val, 2);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
