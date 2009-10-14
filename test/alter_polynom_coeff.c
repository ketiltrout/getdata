/* Test field modifying */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW INT32 8\n"
    "polynom POLYNOM data 1 2 1\n";
  int32_t data_data[256];
  const double a[] = {2, 1, 3};
  int fd, i, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_alter_polynom(D, "polynom", 0, NULL, a);
  int error = get_error(D);

  if (error) {
    fprintf(stderr, "error=%i\n", error);
    r = 1;
  }

  int n = get_entry(D, "polynom", &E);
  error = get_error(D);

  dirfile_close(D);

  if (strcmp(E.field, "polynom")) {
    fprintf(stderr, "E.field = %s\n", E.field);
    r = 1;
  }

  if (E.field_type != GD_POLYNOM_ENTRY) {
    fprintf(stderr, "E.field_type = %i\n", E.field_type);
    r = 1;
  }

  if (E.poly_ord != 2) {
    fprintf(stderr, "E.poly_ord = %i\n", E.poly_ord);
    r = 1;
  }

  if (E.comp_scal != 0) {
    fprintf(stderr, "E.comp_scal = %i\n", E.comp_scal);
    r = 1;
  }
  
  if (strcmp(E.in_fields[0], "data")) {
    fprintf(stderr, "E.in_fields[0] = %s\n", E.in_fields[0]);
    r = 1;
  }

  for (i = 0; i < 3; ++i) 
    if (fabs(E.a[i] - a[i]) > 1e-6) {
      fprintf(stderr, "E.a[%i] = %g\n", i, E.a[i]);
      r = 1;
    }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    r = 1;
  }
  if (n != 0) {
    fprintf(stderr, "2=%lli\n", (long long)n);
    r = 1;
  }
  if (ret != 0) {
    fprintf(stderr, "3=%i\n", ret);
    r = 1;
  }

  return r;
}
