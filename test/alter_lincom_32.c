/* Test field modifying */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW INT32 8\nphase PHASE data 1\n"
    "lincom LINCOM 3 data 1 3 phase 2 0 data 3 1\n";
  int32_t data_data[256];
  int32_t c[8];
  int fd, i, ret, error, n, r = 0;
  DIRFILE *D;
  const char* in_fields[2] = {"data", "phase"};
  const double m[2] = {2, 4};
  const double b[2] = {0, 2};

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_lincom(D, "lincom", 2, in_fields, m, b);
  error = gd_error(D);
  n = gd_getdata(D, "lincom", 5, 0, 1, 0, GD_INT32, c);

  gd_close(D);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], (40 + i) * 6 + 6);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);
  CHECKI(ret,0);

  return r;
}
