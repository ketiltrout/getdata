/* Creating a read-only dirfile should fail cleanly */
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
  int unlink_ret, rmdir_ret, r = 0;

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_CREAT);
  int error = gd_error(D);
  gd_close(D);

  unlink_ret = unlink(format);
  rmdir_ret = rmdir(filedir);

  CHECKI(unlink_ret, -1);
  CHECKI(rmdir_ret, -1);
  CHECKI(error, GD_E_ACCMODE);

  return r;
}
