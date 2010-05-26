/* Attempt to write COMPLEX128 */
#include "test.h"

#include <complex.h>
#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW COMPLEX128 8\n";
  double complex c[8], d;
  int fd, i, r = 0;
  struct stat buf;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = 40 + i * (1 + _Complex_I);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX128, c);
  int error = gd_error(D);

  gd_close(D);

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  }
  CHECKI(buf.st_size, 48 * sizeof(double complex));

  fd = open(data, O_RDONLY);
  i = 0;
  while (read(fd, &d, sizeof(double complex))) {
    if (i < 40 || i > 48) {
      CHECKCi(i,d,0);
    } else
      CHECKCi(i,d,i + _Complex_I * (i - 40));
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
