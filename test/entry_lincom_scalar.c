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
    "m1 CONST FLOAT64 1\n"
    "b1 CONST FLOAT64 2\n"
    "m2 CONST FLOAT64 3\n"
    "b2 CONST FLOAT64 4\n"
    "m3 CONST FLOAT64 5\n"
    "b3 CONST FLOAT64 6\n"
    "data LINCOM 3 in1 m1 b1 in2 m2 b2 in3 m3 b3\n";
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

  if (E.field_type != GD_LINCOM_ENTRY) {
    fprintf(stderr, "E.field_type = %i\n", E.field_type);
    r = 1;
  }

  if (E.n_fields != 3) {
    fprintf(stderr, "E.n_fields = %i\n", E.n_fields);
    r = 1;
  }

  if (strcmp(E.in_fields[0], "in1")) {
    fprintf(stderr, "E.in_fields[0] = %s\n", E.in_fields[0]);
    r = 1;
  }

  if (strcmp(E.in_fields[1], "in2")) {
    fprintf(stderr, "E.in_fields[1] = %s\n", E.in_fields[1]);
    r = 1;
  }

  if (strcmp(E.in_fields[2], "in3")) {
    fprintf(stderr, "E.in_fields[2] = %s\n", E.in_fields[2]);
    r = 1;
  }

  if (strcmp(E.scalar[0], "m1")) {
    fprintf(stderr, "E.scalar[0] = %s\n", E.scalar[0]);
    r = 1;
  }

  if (strcmp(E.scalar[1], "m2")) {
    fprintf(stderr, "E.scalar[1] = %s\n", E.scalar[1]);
    r = 1;
  }

  if (strcmp(E.scalar[2], "m3")) {
    fprintf(stderr, "E.scalar[2] = %s\n", E.scalar[2]);
    r = 1;
  }

  if (strcmp(E.scalar[3], "b1")) {
    fprintf(stderr, "E.scalar[3] = %s\n", E.scalar[3]);
    r = 1;
  }

  if (strcmp(E.scalar[4], "b2")) {
    fprintf(stderr, "E.scalar[4] = %s\n", E.scalar[4]);
    r = 1;
  }

  if (strcmp(E.scalar[5], "b3")) {
    fprintf(stderr, "E.scalar[5] = %s\n", E.scalar[5]);
    r = 1;
  }

  if (fabs(E.m[0] - 1.) > 1e-10) {
    fprintf(stderr, "E.m[0] = %g\n", E.m[0]);
    r = 1;
  }

  if (fabs(E.b[0] - 2.) > 1e-10) {
    fprintf(stderr, "E.b[0] = %g\n", E.b[0]);
    r = 1;
  }

  if (fabs(E.m[1] - 3.) > 1e-10) {
    fprintf(stderr, "E.m[1] = %g\n", E.m[1]);
    r = 1;
  }

  if (fabs(E.b[1] - 4.) > 1e-10) {
    fprintf(stderr, "E.b[1] = %g\n", E.b[1]);
    r = 1;
  }

  if (fabs(E.m[2] - 5.) > 1e-10) {
    fprintf(stderr, "E.m[2] = %g\n", E.m[2]);
    r = 1;
  }

  if (fabs(E.b[2] - 6.) > 1e-10) {
    fprintf(stderr, "E.b[2] = %g\n", E.b[2]);
    r = 1;
  }

  return r;
}
