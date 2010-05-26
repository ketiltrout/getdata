/* Test include */
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
  const char* format1 = __TEST__ "dirfile/format1";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_include(D, "format1", 0, GD_CREAT);
  int error = gd_error(D);
  gd_close(D);

  int unlink_format1 = unlink(format1);
  unlink(format);
  rmdir(filedir);
  
  CHECKI(error,0);
  CHECKI(unlink_format1, 0);

  return r;
}
