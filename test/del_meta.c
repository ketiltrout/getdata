/* Attempt to delete a field */
#include "test.h"

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
  const char* format_data = "data RAW UINT8 8\n"
    "META data e CONST UINT8 1\n"
    "META data q CONST UINT8 2\n"
    "META data a CONST UINT8 3\n"
    "META data b CONST UINT8 4\n"
    "META data z CONST UINT8 5\n"
    "META data l CONST UINT8 6\n";
  int fd, ret, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR);
  ret = gd_delete(D, "data", 0);
  error = gd_error(D);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_DELETE);
  CHECKI(ret, -1);

  return r;
}
