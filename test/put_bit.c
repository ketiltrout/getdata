/* Attempt to write BIT */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
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
  const char* format_data = "bit BIT data 2 3\ndata RAW UINT8 8\n";
  uint8_t c[8];
  uint8_t d = 0xA5;
  int fd, i, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = i;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  for (i = 0; i < 50; ++i)
    write(fd, &d, sizeof(uint8_t));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "bit", 5, 0, 1, 0, GD_INT8, c);
  error = gd_error(D);

  gd_close(D);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    if (i < 40 || i >= 48) {
      CHECKXi(i,d, 0xA5);
    } else
      CHECKXi(i,d,(0xA1 | (i - 40) << 2));
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);

  return r;
}
