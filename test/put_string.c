/* Add a RAW field */
#include "test.h"

#include <inttypes.h>
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
  char string[1024] = "";
  int r = 0;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_string(D, "data", "some string", 0);
  gd_put_string(D, "data", "some other string");
  int error = gd_error(D);
  gd_close(D);

  /* check */
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_get_string(D, "data", 1023, string);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKS(string,"some other string");
  CHECKI(error,GD_E_OK);
  return r;
}
