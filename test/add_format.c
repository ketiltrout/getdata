/* Add a dirfile field with to a bad fragment_index */
#include "test.h"

#include <stdlib.h>
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
  int error, n, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  E.field =  "new";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 21;
  E.EN(raw,spf) = 2;
  E.EN(raw,data_type) = GD_UINT8;

  D = gd_open(filedir, GD_RDWR | GD_CREAT);
  gd_add(D, &E);
  error = gd_error(D);

  /* check */
  n = gd_nfields(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(n,1);
  CHECKI(error, GD_E_BAD_INDEX);

  return r;
}
