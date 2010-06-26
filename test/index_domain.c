/* Frameindex look-up */
#include "test.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW FLOAT64 1\n";
  double d[1000];
  int i, r = 0;

  mkdir(filedir, 0777);

  for (i = 0; i < 1000; ++i)
    d[i] = 0;

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, d, sizeof(double));
  close(i);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  double f1 = gd_get_framenum(D, "data", 1.09);
  int error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_DOMAIN);
  CHECK(!isnan(f1),f1,"%.15g","%s",f1,"not a number");

  return r;
}
