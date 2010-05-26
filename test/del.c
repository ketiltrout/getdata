/* Attempt to delete a field */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  unsigned char c[8];
  unsigned char data_data[256];
  int fd, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR);
  int ret = gd_delete(D, "data", 0);
  int error1 = gd_error(D);
  int n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  int error2 = gd_error(D);
  gd_close(D);

  fd = unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(fd, 0);
  CHECKI(error1, GD_E_OK);
  CHECKI(n, 0);
  CHECKI(ret, 0);
  CHECKI(error2, GD_E_BAD_CODE);

  return r;
}
