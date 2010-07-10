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
    "div RECIP 230. data\n";
  int32_t data_data[256];
  double c[8];
  int fd, i, r = 0;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = gd_alter_crecip(D, "div", "phase", 1093. + _Complex_I * 3290.);
  int error = gd_error(D);
  int n = gd_getdata(D, "div", 5, 0, 1, 0, GD_FLOAT64, c);

  gd_close(D);

  for (i = 0; i < 8; ++i)
    CHECKFi(i,c[i], (1093. + _Complex_I * 3290.) / (i + 41.));

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);
  CHECKI(ret,0);

  return r;
}