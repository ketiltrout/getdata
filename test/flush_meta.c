/* Attempt to flush meta data */
#include "test.h"

#include <inttypes.h>
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
  const char* data = __TEST__ "dirfile/new";
  struct stat buf;
  int error, stat_format, r = 0;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_TRUNC |
      GD_VERBOSE);
  gd_add_raw(D, "new", GD_UINT8, 2, 0);
  gd_metaflush(D);
  error = gd_error(D);

  gd_close(D);

  stat_format = stat(format, &buf);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(stat_format, 0);
  CHECKI(error, GD_E_OK);

  return r;
}
