/* Attempt to read complex constant */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "const CONST COMPLEX128 8.3;9.2\n";
#ifdef GD_NO_C99_API
  double c[2];
  const double v[2] = {8.3, 9.2};
#else
  double complex c;
  const double complex v = 8.3 + _Complex_I * 9.2;
#endif
  int fd, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
#ifdef GD_NO_C99_API
  n = gd_get_constant(D, "const", GD_COMPLEX128, c);
#else
  n = gd_get_constant(D, "const", GD_COMPLEX128, &c);
#endif
  error = gd_error(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 0);
  CHECKC(c, v);

  return r;
}
