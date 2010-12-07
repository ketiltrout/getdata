/* Attempt to read PHASE */
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
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "phase PHASE data -2\ndata RAW UINT8 1\n";
  unsigned char c = 0;
  unsigned char data_data[256];
  int fd, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "phase", 5, 0, 1, 0, GD_UINT8, &c);
  error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "error=%i\n", error);
    r = 1;
  }
  if (n != 1) {
    fprintf(stderr, "n=%i\n", n);
    r = 1;
  }
  if (c != 3) {
    fprintf(stderr, "c=%i\n", c);
    r = 1;
  }
  CHECKI(error, 0);
  CHECKI(n, 1);
  CHECKI(c, 3);

  return r;
}
