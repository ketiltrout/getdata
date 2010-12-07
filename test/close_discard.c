/* Closing a dirfile should succeed cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  int ret, n, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0666));

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  gd_add_spec(D, "data RAW UINT8 1", 0);
  ret = gd_discard(D);
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_nfields(D);
  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(ret, 0);
  CHECKI(n, 1);

  return r;
}
