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
    "m1 CONST COMPLEX128 1.1;3.2\n"
    "b1 CONST COMPLEX64 2.2;9.3\n"
    "data LINCOM 1 in1 m1.r b1.i\n";
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
  CHECKX(E.field_type, GD_LINCOM_ENTRY);
  CHECKI(E.u.lincom.n_fields, 1);
  CHECKS(E.in_fields[0], "in1");
  CHECKS(E.scalar[0], "m1.r");
  CHECKS(E.scalar[GD_MAX_LINCOM], "b1.i");
  CHECKF(E.u.lincom.m[0], 1.1);
  CHECKF(E.u.lincom.b[0], 9.3);

  return r;
}
