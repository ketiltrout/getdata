/* Attempt to read COMPLEX128 */
#include "test.h"

#include <math.h>
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
  const char* format_data = "data RAW COMPLEX128 8\n";
#ifdef GD_NO_C99_API
  double c[16];
  double data_data[256];
#else
  double complex c[8];
  double complex data_data[128];
#endif
  int fd, i, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd) {
#ifdef GD_NO_C99_API
    data_data[2 * fd] = data_data[2 * fd + 1] = 1.5 * fd;
#else
    data_data[fd] = (double complex)(1.5 * (1 + _Complex_I) * fd);
#endif
  }

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(double));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = gd_getdata(D, "data", 5, 0, 1, 0, GD_COMPLEX128, c);
  int error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i) {
#ifdef GD_NO_C99_API
    const double v[2] = {1.5 * (40 + i), 1.5 * (40 + i)};
    CHECKCi(i, c + 2 * i, v);
#else
    CHECKCi(i, c[i], 1.5 * (1 + _Complex_I) * (40 + i));
#endif
  }

  return r;
}
