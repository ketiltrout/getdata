/* Attempt to write UINT8 */
#include "test.h"

#include <inttypes.h>
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
  const char* format_data = "data CONST UINT8 8\nPROTECT all\n";
  uint8_t d = 3;
  int fd, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  n = gd_put_constant(D, "data", GD_UINT8, &d);
  error = gd_error(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_PROTECTED);
  CHECKI(n, -1);
  return r;
}
