/* Parser check */
#include "test.h"

#include <inttypes.h>
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
  const char* format_data = "FRAMEOFFSET 1\nINCLUDE format1\nINCLUDE format2";

  const char* data1 = __TEST__ "dirfile/data1";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format1_data = "data1 RAW UINT8 1\n";

  const char* data2 = __TEST__ "dirfile/data2";
  const char* format2 = __TEST__ "dirfile/format2";
  const char* format2_data = "data2 RAW UINT8 1\nFRAMEOFFSET 2";

  int fd;
  int r = 0;
  uint8_t data_data[4] = { 0, 1, 2, 3 };

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

  fd = open(data1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 4);
  close(fd);

  fd = open(data2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 4);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  int error = gd_error(D);
  CHECKI(error, 0);

  gd_getdata(D, "data1", 3, 0, 1, 0, GD_UINT8, data_data);

  int error2 = gd_error(D);
  CHECKI(error2, 0);
  CHECKU(data_data[0], 2);

  gd_getdata(D, "data2", 3, 0, 1, 0, GD_UINT8, data_data);

  int error3 = gd_error(D);
  CHECKI(error3, 0);
  CHECKU(data_data[0], 1);

  gd_close(D);

  unlink(format);
  unlink(format1);
  unlink(format2);
  unlink(data1);
  unlink(data2);
  rmdir(filedir);

  return r;
}
