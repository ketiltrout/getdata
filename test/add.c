/* Add a dirfile field */
#include "test.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  int error, r = 0;
  DIRFILE *D;
  gd_entry_t E, e;

  E.field = "data";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.EN(raw,spf) = 2;
  E.EN(raw,data_type) = GD_UINT8;
  E.scalar[0] = NULL;

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add(D, &E);
  error = gd_error(D);

  /* check */
  gd_entry(D, "data", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_RAW_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(raw,spf), 2);
    CHECKI(e.EN(raw,data_type), GD_UINT8);
    CHECKP(e.scalar[0]);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);

  return r;
}
