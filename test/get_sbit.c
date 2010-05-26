/* Attempt to read SBIT */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <inttypes.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "bit SBIT data 7 4\ndata RAW UINT16 1\n";
  int16_t c[10];
  uint16_t data_data[256];
  int fd, r = 0;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = 0x0101 * (uint16_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(uint16_t));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = gd_getdata(D, "bit", 5, 0, 10, 0, GD_INT16, c);
  int error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 10);

  for (fd = 0; fd < 10; ++fd)
    CHECKIi(fd, c[fd], fd * 2 - ((fd > 6) ? 22 : 6));

  return r;
}
