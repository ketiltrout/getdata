/* Check if GETDATA_C89_API produces a useable API */
#define GETDATA_C89_API
#include "test.h"

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

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_get_entry(D, "lincom", &E);

  int error = gd_error(D);
  CHECKI(error, 0);
  CHECKIi(0,E.comp_scal, 1);
  CHECKFi(0,E.cm[0][0], 3.3);
  CHECKFi(0,E.cm[0][1], 4.4);
  CHECKFi(0,E.cb[0][0], 5.5);
  CHECKFi(0,E.cb[0][1], 6.6);
  CHECKFi(0,E.cm[1][0], 7.7);
  CHECKFi(0,E.cm[1][1], 8.8);
  CHECKFi(0,E.cb[1][0], 9.9);
  CHECKFi(0,E.cb[1][1], 1.1);

  gd_free_entry_strings(&E);

  const double ca[] = { 2.1, 3.2, 4.3, 5.4, 6.5, 7.6 };
  gd_add_cpolynom(D, "polynom", 2, "in", ca, 0);

  int error2 = gd_error(D);
  CHECKI(error2, 0);

  gd_get_entry(D, "polynom", &E);

  int error3 = gd_error(D);
  CHECKI(error3, 0);
  CHECKIi(1,E.poly_ord,2);
  CHECKIi(1,E.comp_scal,1);
  CHECKFi(1,E.ca[0][0], ca[0]);
  CHECKFi(1,E.ca[0][1], ca[1]);
  CHECKFi(1,E.ca[1][0], ca[2]);
  CHECKFi(1,E.ca[1][1], ca[3]);
  CHECKFi(1,E.ca[2][0], ca[4]);
  CHECKFi(1,E.ca[2][1], ca[5]);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
