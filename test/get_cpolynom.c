/* Attempt to read POLYNOM */
#include "../src/getdata.h"

#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
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
  const char* format_data = "polynom POLYNOM data 3;2 2;4 0;1\ndata RAW UINT8 1\n";
  double complex c = 0;
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
  int n = getdata(D, "polynom", 5, 0, 1, 0, GD_COMPLEX128, &c);
  int error = get_error(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  const double complex v = (3 + _Complex_I * 2) + (2 + _Complex_I * 4) * 5
    + _Complex_I * 5 * 5;

  if (error) {
    printf("1=%i\n", error);
    return 1;
  }
  if (n != 1) {
    printf("2=%i\n", n);
    return 1;
  }
  if (c != v) {
    printf("%g;%g=%g;%g\n", creal(c), cimag(c), creal(v), cimag(v));
    return 1;
  }

  return 0;
}
