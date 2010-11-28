/* Test include */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format2 = __TEST__ "dirfile/format2";
  const char* format_data = "/INCLUDE format1\na CONST UINT8 1\n";
  const char* format1_data = "b CONST UINT8 11\n/INCLUDE format2\n";
  const char* format2_data = "c CONST UINT8 11\n";
  int fd, ret1, ret2, error1, error2, r = 0;
  unsigned int nfields, nfragments;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(format2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format2_data, strlen(format2_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret1 = gd_uninclude(D, 2, 0);
  error1 = gd_error(D);
  ret2 = gd_include(D, "format2", 0, 0);
  error2 = gd_error(D);
  nfields = gd_nfields(D);
  nfragments = gd_nfragments(D);
  gd_close(D);

  unlink(format2);
  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1,0);
  CHECKI(error2,0);
  CHECKI(ret1,0);
  CHECKI(ret2,2);
  CHECKI(nfields,4);
  CHECKI(nfragments,3);

  return r;
}
