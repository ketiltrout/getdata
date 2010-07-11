/* Add a LINCOM field */
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
  int r = 0;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT);
  const char* in_fields[2] = {"in1", "in2"};
  const double m[2] = {1, 0.3};
  const double b[2] = {0, 0.9};
  gd_add_lincom(D, "new", 0, in_fields, m, b, 0);
  int error = gd_error(D);

  /* check */
  int n = gd_nfields(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(n, 1);
  CHECKI(error, GD_E_BAD_ENTRY);

  return r;
}
