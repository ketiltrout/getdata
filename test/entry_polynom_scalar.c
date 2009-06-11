/* Try to read LINCOM entry */
#include "../src/getdata.h"

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
  const char* format_data =
    "a0 CONST FLOAT64 1\n"
    "a1 CONST FLOAT64 2\n"
    "a2 CONST FLOAT64 3\n"
    "a3 CONST FLOAT64 4\n"
    "a4 CONST FLOAT64 5\n"
    "data POLYNOM in a0 a1 a2 a3 a4\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_entry_t E;

  int n = get_entry(D, "data", &E);
  int error = get_error(D);

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  if (error != GD_E_OK)
    return 1;
  if (n)
    return 1;
  if (strcmp(E.field, "data"))
    return 1;
  if (E.field_type != GD_POLYNOM_ENTRY)
    return 1;
  if (E.poly_ord != 4)
    return 1;
  if (strcmp(E.in_fields[0], "in"))
    return 1;
  if (fabs(E.a[0] - 1.) > 1e-10)
    return 1;
  if (fabs(E.a[1] - 2.) > 1e-10)
    return 1;
  if (fabs(E.a[2] - 3.) > 1e-10)
    return 1;
  if (fabs(E.a[3] - 4.) > 1e-10)
    return 1;
  if (fabs(E.a[4] - 5.) > 1e-10)
    return 1;

  return 0;
}
