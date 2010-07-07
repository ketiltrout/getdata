#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format_data =
    "data RAW UINT16 2\n"
    "/FRAMEOFFSET 35\n"
    "lincom LINCOM 2 data2 1. 0. data 1. 0.\n"
    "lincom2 LINCOM 2 data 1. 0. data2 1. 0.\n"
    "INCLUDE format1\n";
  const char* format1_data = "data2 RAW UINT8 3\nFRAMEOFFSET 33\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  off_t bof_data = gd_get_bof(D, "data");
  int error1 = gd_error(D);
  off_t bof_data2 = gd_get_bof(D, "data2");
  int error2 = gd_error(D);
  off_t bof_lincom = gd_get_bof(D, "lincom");
  int error3 = gd_error(D);
  off_t bof_lincom2 = gd_get_bof(D, "lincom2");
  int error4 = gd_error(D);
  gd_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1, 0);
  CHECKI(bof_data, 70);
  CHECKI(error2, 0);
  CHECKI(bof_data2, 99);
  CHECKI(error3, 0);
  CHECKI(bof_lincom, 105);
  CHECKI(error4, 0);
  CHECKI(bof_lincom2, 70);

  return r;
}
