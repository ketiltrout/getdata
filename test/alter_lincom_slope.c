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
    "lincom LINCOM 2 data 1 0 data 1 0\n";
  int32_t data_data[256];
  int32_t c[8];
  int fd, i, r = 0;
  const char* in_fields[3] = {"data", "phase", NULL};
  double m[3] = {2, 3, 0};

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = gd_alter_lincom(D, "lincom", 0, in_fields, m, NULL);
  int error = gd_error(D);
  int n = gd_getdata(D, "lincom", 5, 0, 1, 0, GD_INT32, c);

  gd_close(D);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], i * 5 + 203);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);
  CHECKI(ret, 0);

  return r;
}
