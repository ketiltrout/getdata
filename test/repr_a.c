/* Attempt to read argument representation */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "lincom LINCOM 2 data 2;3 3;2 data 0;1 0\ndata RAW UINT8 1\n";
  double c = 0;
  unsigned char data_data[256];
  int fd;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = getdata(D, "lincom.a", 5, 0, 1, 0, GD_FLOAT64, &c);
  int error = get_error(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  const double complex v = 3 + _Complex_I * 2 + (2 + _Complex_I * 3) * 5
    + _Complex_I * 5;

  if (error)
    return 1;
  if (n != 1)
    return 1;
  if (fabs(c - carg(v)) > 1e-6) {
    fprintf(stderr, "%g = %g;%g\n", c, creal(v), cimag(v));
    return 1;
  }

  return 0;
}
