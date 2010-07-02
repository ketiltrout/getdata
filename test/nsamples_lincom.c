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
  const char* data = __TEST__ "dirfile/data";
  const char* data2 = __TEST__ "dirfile/data2";
  const char* format_data =
    "data RAW UINT16 1\n"
    "data2 RAW UINT8 1\n"
    "lincom LINCOM 2 data2 1. 0. data 1. 0.\n"
    "lincom2 LINCOM 2 data 1. 0. data2 1. 0.\n";
  int fd, r = 0;
  const size_t len = strlen(data);

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  fd = open(data2, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  off_t n = gd_get_nsamples(D, "lincom");
  int error = gd_error(D);
  off_t m = gd_get_nsamples(D, "lincom");
  int error2 = gd_error(D);
  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, (int)len / 2);
  CHECKI(error2, 0);
  CHECKI(m, (int)len / 2);

  return r;
}
