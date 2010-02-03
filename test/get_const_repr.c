/* Attempt to read constant with repr */
#include "../src/getdata.h"

#include <complex.h>
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
  const char* format_data = "const CONST COMPLEX128 8.3;9.2\n";
  double c;
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = get_constant(D, "const.m", GD_FLOAT64, &c);
  int error = get_error(D);

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;
  if (n != 0)
    return 1;
  if (fabs(c - cabs(8.3 + _Complex_I * 9.2)) > 1e-6)
    return 1;

  return 0;
}
