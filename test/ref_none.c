/* Parser check */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data =
    "data1 RAW UINT8 1\n"
    "data2 RAW UINT8 1\n"
    ;

  const char* data1 = __TEST__ "dirfile/data1";
  const char* data2 = __TEST__ "dirfile/data2";

  uint8_t data_data[4] = { 0, 1, 2, 3 };
  int fd, error, error2, r = 0;
  DIRFILE *D;
  off_t nf;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data1, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 4);
  close(fd);

  fd = open(data2, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 3);
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  error = gd_error(D);
  CHECKI(error, 0);

  nf = gd_nframes(D);

  error2 = gd_error(D);
  CHECKI(error2, 0);
  CHECKI(nf,4);

  gd_close(D);

  unlink(format);
  unlink(data1);
  unlink(data2);
  rmdir(filedir);

  return r;
}
