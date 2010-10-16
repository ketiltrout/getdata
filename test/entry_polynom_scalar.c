/* Try to read LINCOM entry */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <math.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data =
    "a0 CONST FLOAT64 1\n"
    "a1 CONST FLOAT64 2\n"
    "a2 CONST FLOAT64 3\n"
    "a3 CONST FLOAT64 4\n"
    "a4 CONST FLOAT64 5\n"
    "data POLYNOM in a0 a1 a2 a3 a4\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_entry_t E;

  int n = gd_entry(D, "data", &E);
  int error = gd_error(D);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKI(n, 0);
  CHECKS(E.field, "data");
  CHECKX(E.field_type, GD_POLYNOM_ENTRY);
  CHECKI(E.comp_scal, 0);
  CHECKI(E.EN(polynom,poly_ord), 4);
  CHECKS(E.in_fields[0], "in");
  CHECKS(E.scalar[0], "a0");
  CHECKS(E.scalar[1], "a1");
  CHECKS(E.scalar[2], "a2");
  CHECKS(E.scalar[3], "a3");
  CHECKS(E.scalar[4], "a4");
  for (fd = 0; fd < 4; ++fd)
    CHECKFi(fd,E.EN(polynom,a)[fd], fd + 1.);

  return r;
}
