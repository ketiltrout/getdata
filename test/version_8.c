/* Open a Standards Version 8 conformant dirfile */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/ar";
  const char* format_data =
    "/VERSION 8\n"
    "ar RAW UINT8 8\n"
    "q DIVIDE ar ar\n"
    "r RECIP 1. ar\n";
  double c[8];
  unsigned char data_data[256];
  int fd, i, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = gd_getdata(D, "r", 5, 0, 1, 0, GD_FLOAT64, c);
  int error = gd_error(D);

  int v = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  int l = gd_dirfile_standards(D, GD_VERSION_LATEST);
  int e = gd_dirfile_standards(D, GD_VERSION_EARLIEST);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);

  for (i = 0; i < 8; ++i)
    CHECKFi(i,c[i],1. / (40 + i));

  CHECKI(v,8);
  CHECKI(l,8);
  CHECKI(e,8);

  return r;
}
