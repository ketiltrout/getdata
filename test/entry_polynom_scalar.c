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

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_entry_t E;

  int n = get_entry(D, "data", &E);
  int error = get_error(D);

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  if (error != GD_E_OK) {
    fprintf(stderr, "error = %i\n", error);
    r = 1;
  }

  if (n) {
    fprintf(stderr, "n = %i\n", n);
    r = 1;
  }

  if (strcmp(E.field, "data")) {
    fprintf(stderr, "E.field = %s\n", E.field);
    r = 1;
  }

  if (E.field_type != GD_POLYNOM_ENTRY) {
    fprintf(stderr, "E.field_type = %i\n", E.field_type);
    r = 1;
  }

  if (E.poly_ord != 4) {
    fprintf(stderr, "E.poly_ord = %i\n", E.poly_ord);
    r = 1;
  }

  if (strcmp(E.in_fields[0], "in")) {
    fprintf(stderr, "E.in_fields[0] = %s\n", E.in_fields[0]);
    r = 1;
  }

  if (strcmp(E.scalar[0], "a0")) {
    fprintf(stderr, "E.scalar[0] = %s\n", E.scalar[0]);
    r = 1;
  }

  if (strcmp(E.scalar[1], "a1")) {
    fprintf(stderr, "E.scalar[1] = %s\n", E.scalar[1]);
    r = 1;
  }

  if (strcmp(E.scalar[2], "a2")) {
    fprintf(stderr, "E.scalar[2] = %s\n", E.scalar[2]);
    r = 1;
  }

  if (strcmp(E.scalar[3], "a3")) {
    fprintf(stderr, "E.scalar[3] = %s\n", E.scalar[3]);
    r = 1;
  }

  if (strcmp(E.scalar[4], "a4")) {
    fprintf(stderr, "E.scalar[4] = %s\n", E.scalar[4]);
    r = 1;
  }

  if (E.scalar[5] != NULL) {
    fprintf(stderr, "E.scalar[5] = %p\n", E.scalar[5]);
    r = 1;
  }

  if (fabs(E.a[0] - 1.) > 1e-10) {
    fprintf(stderr, "E.a[0] = %g\n", E.a[0]);
    r = 1;
  }

  if (fabs(E.a[1] - 2.) > 1e-10) {
    fprintf(stderr, "E.a[1] = %g\n", E.a[1]);
    r = 1;
  }

  if (fabs(E.a[2] - 3.) > 1e-10) {
    fprintf(stderr, "E.a[2] = %g\n", E.a[2]);
    r = 1;
  }

  if (fabs(E.a[3] - 4.) > 1e-10) {
    fprintf(stderr, "E.a[3] = %g\n", E.a[3]);
    r = 1;
  }

  if (fabs(E.a[4] - 5.) > 1e-10) {
    fprintf(stderr, "E.a[4] = %g\n", E.a[4]);
    r = 1;
  }

  return r;
}
