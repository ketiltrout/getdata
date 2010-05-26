/* Creating a dirfile should succeed cleanly */
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

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  int error = gd_error(D);
  gd_close(D);

  int unlink_ret = unlink(format);
  int rmdir_ret = rmdir(filedir);

  CHECKI(unlink_ret, 0);
  CHECKI(rmdir_ret, 0);
  CHECKI(error, GD_E_OK);
  
  return r;
}
