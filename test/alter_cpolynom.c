/* Test field modifying */
#include "../src/getdata.h"

#include <complex.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW INT32 8\n"
    "polynom POLYNOM data 1 2 1\n";
  int32_t data_data[256];
  double complex c[8];
  const double complex a[] = {2 + _Complex_I * 1,
    1 + _Complex_I * 2, 1 + _Complex_I * 3};
  int fd, i, we = 0;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_alter_cpolynom(D, "polynom", 0, NULL, a);
  int error = get_error(D);
  int n = getdata(D, "polynom", 5, 0, 1, 0, GD_COMPLEX128, c);

  dirfile_close(D);

  for (i = 0; i < 8; ++i) {
    int x = i + 40;
    double complex v = (2 + _Complex_I * 1) + (1 + _Complex_I * 2) * x
      + (1 + _Complex_I * 3) * x * x;
    if (c[i] != v) {
        printf("%g;%g = %g;%g\n", creal(v), cimag(v), creal(c[i]),
            cimag(c[i]));
        we++;
        exit(1);
      }
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (n != 8) {
    fprintf(stderr, "2=%lli\n", (long long)n);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "3=%i\n", ret);
    return 1;
  }
  if (we != 0) {
    fprintf(stderr, "4=%i\n", we);
    return 1;
  }

  return 0;
}
