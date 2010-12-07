/* Attempt to read POLYNOM */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "polynom POLYNOM data 2 3\n";
  unsigned char c = 0;
  int fd, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY);
  n = gd_getdata(D, "lincom", 5, 0, 1, 0, GD_UINT8, &c);

  error = gd_error(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(n, 0);
  CHECKI(error, GD_E_BAD_CODE);

  return r;
}
