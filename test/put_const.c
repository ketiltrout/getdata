/* Add a RAW field */
#include "test.h"

#include <inttypes.h>
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
  uint8_t val = 0;
  int error, r = 0;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_const(D, "data", GD_UINT8, GD_UINT8, &val, 0);
  val = 23;
  gd_put_constant(D, "data", GD_UINT8, &val);
  error = gd_error(D);
  gd_close(D);

  /* check */
  val = 0;
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_get_constant(D, "data", GD_UINT8, &val);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKU(val, 23);
  CHECKI(error,GD_E_OK);
  return r;
}
