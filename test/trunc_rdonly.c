/* Truncating a read-only dirfile should fail cleanly */
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
  int fd, error, unlink_data, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format, strlen(format));
  close(fd);

  close(open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666));

  D = gd_open(filedir, GD_RDONLY | GD_TRUNC);
  error = gd_error(D);
  gd_close(D);

  unlink_data = unlink(data);
  CHECKI(unlink_data, 0);

  unlink(format);
  rmdir(filedir);

  CHECKI(error,GD_E_ACCMODE);
  return r;
}
