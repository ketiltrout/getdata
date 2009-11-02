/* Check if GETDATA_C89_API produces a useable API */
#define GETDATA_C89_API
#include "../src/getdata.h"

#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data =
    "lincom LINCOM data 3.3;4.4 5.5;6.6 data 7.7;8.8 9.9;1.1\n";
  int fd, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  get_entry(D, "lincom", &E);

  int error = get_error(D);
  if (error) {
    fprintf(stderr, "error[1]=%i\n", error);
    r = 1;
  }

  if (E.comp_scal != 1) {
    fprintf(stderr, "E.comp_scal=%i\n", E.comp_scal);
    r = 1;
  }

  if (fabs(E.cm[0][0] - 3.3) > 1e-10) {
    fprintf(stderr, "E.cm[0][0]=%g\n", E.cm[0][0]);
    r = 1;
  }

  if (fabs(E.cm[0][1] - 4.4) > 1e-10) {
    fprintf(stderr, "E.cm[0][1]=%g\n", E.cm[0][1]);
    r = 1;
  }

  if (fabs(E.cb[0][0] - 5.5) > 1e-10) {
    fprintf(stderr, "E.cb[0][0]=%g\n", E.cb[0][0]);
    r = 1;
  }

  if (fabs(E.cb[0][1] - 6.6) > 1e-10) {
    fprintf(stderr, "E.cb[0][1]=%g\n", E.cb[0][1]);
    r = 1;
  }

  if (fabs(E.cm[1][0] - 7.7) > 1e-10) {
    fprintf(stderr, "E.cm[1][0]=%g\n", E.cm[1][0]);
    r = 1;
  }

  if (fabs(E.cm[1][1] - 8.8) > 1e-10) {
    fprintf(stderr, "E.cm[1][1]=%g\n", E.cm[1][1]);
    r = 1;
  }

  if (fabs(E.cb[1][0] - 9.9) > 1e-10) {
    fprintf(stderr, "E.cb[1][0]=%g\n", E.cb[1][0]);
    r = 1;
  }

  if (fabs(E.cb[1][1] - 1.1) > 1e-10) {
    fprintf(stderr, "E.cb[1][1]=%g\n", E.cb[1][1]);
    r = 1;
  }

  dirfile_free_entry_strings(&E);

  const double ca[] = { 2.1, 3.2, 4.3, 5.4, 6.5, 7.6 };
  dirfile_add_cpolynom(D, "polynom", 2, "in", ca, 0);

  error = get_error(D);
  if (error) {
    fprintf(stderr, "error[2]=%i\n", error);
    r = 1;
  }

  get_entry(D, "polynom", &E);

  error = get_error(D);
  if (error) {
    fprintf(stderr, "error[3]=%i\n", error);
    r = 1;
  }

  if (E.poly_ord != 2) {
    fprintf(stderr, "E.poly_ord=%i\n", E.poly_ord);
    r = 1;
  }

  if (E.comp_scal != 1) {
    fprintf(stderr, "E.comp_scal=%i\n", E.comp_scal);
    r = 1;
  }

  if (fabs(E.ca[0][0] - ca[0]) > 1e-10) {
    fprintf(stderr, "E.ca[0][0]=%g\n", E.ca[0][0]);
    r = 1;
  }

  if (fabs(E.ca[0][1] - ca[1]) > 1e-10) {
    fprintf(stderr, "E.ca[0][1]=%g\n", E.ca[0][1]);
    r = 1;
  }

  if (fabs(E.ca[1][0] - ca[2]) > 1e-10) {
    fprintf(stderr, "E.ca[1][0]=%g\n", E.ca[1][0]);
    r = 1;
  }

  if (fabs(E.ca[1][1] - ca[3]) > 1e-10) {
    fprintf(stderr, "E.ca[1][1]=%g\n", E.ca[1][1]);
    r = 1;
  }

  if (fabs(E.ca[2][0] - ca[4]) > 1e-10) {
    fprintf(stderr, "E.ca[2][0]=%g\n", E.ca[2][0]);
    r = 1;
  }

  if (fabs(E.ca[2][1] - ca[5]) > 1e-10) {
    fprintf(stderr, "E.ca[2][1]=%g\n", E.ca[2][1]);
    r = 1;
  }

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
