/* Attempt to write COMPLEX128 */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW COMPLEX128 8\n";
#ifdef GD_NO_C99_API
  double c[8][2], d[2];
  const double zero[] = {0, 0};
#else
  double complex c[8], d;
  const double complex zero = 0;
#endif
  int fd, i, n, error, r = 0;
  struct stat buf;
  DIRFILE *D;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i) {
#ifdef GD_NO_C99_API
    c[i][0] = 40 + i;
    c[i][1] = i;
#else
    c[i] = 40 + i * (1 + _Complex_I);
#endif
  }

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX128, c);
  error = gd_error(D);

  gd_close(D);

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  }
  CHECKI(buf.st_size, 48 * 2 * sizeof(double));

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
#ifdef GD_NO_C99_API
  while (read(fd, d, 2 * sizeof(double)))
#else
  while (read(fd, &d, sizeof(double complex)))
#endif
  {
    if (i < 40 || i > 48) {
      CHECKCi(i,d,zero);
    } else {
#ifdef GD_NO_C99_API
      double v[] = {i, i - 40};
#else
      double complex v = i + _Complex_I * (i - 40);
#endif
      CHECKCi(i,d,v);
    }
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n,8);

  return r;
}
