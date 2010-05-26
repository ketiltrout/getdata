/* Truncating a dirfile should succeed cleanly */
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
  const char* data = __TEST__ "dirfile/data";
  int fd, r = 0;
  struct stat buf;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format, strlen(format));
  close(fd);

  close(open(data, O_CREAT | O_EXCL | O_WRONLY, 0666));

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_TRUNC | GD_VERBOSE);
  int error = gd_error(D);
  gd_close(D);

  int unlink_data = unlink(data);
  CHECKI(unlink_data, -1);

  int stat_format = stat(format, &buf);
  CHECKI(stat_format, 0);
  CHECK((buf.st_size > 0),buf.st_size,"%lli","%s",(long long)buf.st_size,"> 0");

  unlink(format);
  rmdir(filedir);

  CHECKI(error,GD_E_OK);
  return r;
}
