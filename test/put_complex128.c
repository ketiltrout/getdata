/* Attempt to write COMPLEX128 */
#include "../src/getdata.h"

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

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX128, c);
  int error = get_error(D);

  dirfile_close(D);

  if (stat(data, &buf)) {
    fputs("stat failed.\n", stderr);
    r = 1;
  }
  if (buf.st_size != 48 * sizeof(double complex)) {
    fprintf(stderr, "size=%lli", (long long)buf.st_size);
    r = 1;
  }

  fd = open(data, O_RDONLY);
  i = 0;
  while (read(fd, &d, sizeof(double complex))) {
    if (i < 40 || i > 48) {
      if (cabs(d) > 1e-10) {
        fprintf(stderr, "d[%i] = %g;%g\n", i, creal(d), cimag(d));
        r = 1;
      }
    } else if (cabs(d - i - _Complex_I * (i - 40)) > 1e-10) {
      fprintf(stderr, "d[%i] = %g;%g\n", i, creal(d), cimag(d));
      r = 1;
    }
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    r = 1;
    fprintf(stderr, "error=%i\n", error);
  }
  if (n != 8) {
    r = 1;
    fprintf(stderr, "n=%i\n", n);
  }

  return r;
}
