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

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "data LINCOM 3 in1 1 2 in2 3 4 in3 5 6\n";
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
  CHECKI(E.field_type, GD_LINCOM_ENTRY);
  CHECKI(E.comp_scal, 0);
  CHECKI(E.u.lincom.n_fields, 3);
  CHECKS(E.in_fields[0], "in1");
  CHECKS(E.in_fields[1], "in2");
  CHECKS(E.in_fields[2], "in3");
  CHECKF(E.u.lincom.m[0], 1.);
  CHECKF(E.u.lincom.b[0], 2.);
  CHECKF(E.u.lincom.m[1], 3.);
  CHECKF(E.u.lincom.b[1], 4.);
  CHECKF(E.u.lincom.m[2], 5.);
  CHECKF(E.u.lincom.b[2], 6.);

  return r;
}
