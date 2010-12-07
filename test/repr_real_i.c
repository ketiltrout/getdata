/* Attempt to read imaginary representation */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW FLOAT64 1\n";
  double c[8];
  double data_data[100];
  int i, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  for (i = 0; i < 100; ++i)
    data_data[i] = sin(i * 3.14159265358979323846 / 5.);

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, data_data, 100 * sizeof(double));
  close(i);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data.i", 5, 0, 8, 0, GD_FLOAT64, &c);
  error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);
  for (i = 0; i < 8; ++i)
    CHECKFi(i,c[i],0);

  return r;
}
