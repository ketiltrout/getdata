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
    "m1 CONST FLOAT64 1\n"
    "b1 CONST FLOAT64 2\n"
    "m2 CONST FLOAT64 3\n"
    "b2 CONST FLOAT64 4\n"
    "m3 CONST FLOAT64 5\n"
    "b3 CONST FLOAT64 6\n"
    "data LINCOM 3 in1 m1 b1 in2 m2 b2 in3 m3 b3\n";
  int fd, n, error, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  n = gd_entry(D, "data", &E);
  error = gd_error(D);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKI(n, 0);
  CHECKS(E.field, "data");
  CHECKI(E.field_type, GD_LINCOM_ENTRY);
  CHECKI(E.EN(lincom,n_fields), 3);
  CHECKS(E.in_fields[0], "in1");
  CHECKS(E.in_fields[1], "in2");
  CHECKS(E.in_fields[2], "in3");
  CHECKS(E.scalar[0], "m1");
  CHECKS(E.scalar[1], "m2");
  CHECKS(E.scalar[2], "m3");
  CHECKS(E.scalar[3], "b1");
  CHECKS(E.scalar[4], "b2");
  CHECKS(E.scalar[5], "b3");
  CHECKF(E.EN(lincom,m)[0], 1.);
  CHECKF(E.EN(lincom,b)[0], 2.);
  CHECKF(E.EN(lincom,m)[1], 3.);
  CHECKF(E.EN(lincom,b)[1], 4.);
  CHECKF(E.EN(lincom,m)[2], 5.);
  CHECKF(E.EN(lincom,b)[2], 6.);
  gd_free_entry_strings(&E);

  return r;
}
