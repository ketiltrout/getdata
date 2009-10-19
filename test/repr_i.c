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
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW COMPLEX128 1\n";
  double c[8];
  double complex data_data[100];
  int i, r = 0;

  mkdir(filedir, 0777);

  for (i = 0; i < 100; ++i)
    data_data[i] = cexp(_Complex_I * i * M_PI / 5.);

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, data_data, 100 * sizeof(double complex));
  close(i);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = getdata(D, "data.i", 5, 0, 8, 0, GD_FLOAT64, &c);
  int error = get_error(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "error=%i\n", error);
    r = 1;
  }
  if (n != 8) {
    fprintf(stderr, "n=%i\n", n);
    r = 1;
  }
  for (i = 0; i < 8; ++i)
    if (fabs(c[i] - cimag(data_data[5 + i])) > 1e-6) {
      fprintf(stderr, "%g = %g;%g = %g;%g\n", c[i], creal(data_data[5 + i]),
          cimag(data_data[5 + i]), cabs(data_data[5 + i]),
          carg(data_data[5 + i]));
      r = 1;
    }

  return r;
}
