/* Add a BIT field */
#include "test.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  int r = 0;
  gd_entry_t e;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_bit(D, "new", "input", 1, 1, 0);
  int error = gd_error(D);

  /* check */
  gd_entry(D, "new", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_BIT_ENTRY);
    CHECKS(e.in_fields[0], "input");
    CHECKI(e.fragment_index, 0);
    CHECKI(e.u.bit.bitnum, 1);
    CHECKI(e.u.bit.numbits, 1);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);

  return r;
}
