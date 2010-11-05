/* Attempt to read constant */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "carray CARRAY FLOAT64 8.3 7.2 6.1 5.0 3.9 2.8 1.7\n";
  double c[7];
  int fd, i, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = gd_get_carray(D, "carray", GD_FLOAT64, c);
  int error = gd_error(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 0);
  for (i = 0; i < 7; ++i)
    CHECKFi(i, c[i], 8.3 - i * 1.1);

  return r;
}
