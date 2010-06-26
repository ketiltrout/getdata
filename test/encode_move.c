/* Test endianness */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* txtdata = __TEST__ "dirfile/data.txt";
  const char* format_data = "data RAW UINT16 8\nENCODING none\n";
  uint16_t data_data[128];
  uint16_t c[8];
  int fd, i, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = 0x201 * fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = gd_alter_encoding(D, GD_TEXT_ENCODED, 0, 1);
  int error = gd_error(D);
  int n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);

  gd_close(D);

  int unlink_txtdata = unlink(txtdata);
  int unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);

  for (i = 0; i < 8; ++i)
    CHECKXi(i,c[i], (40 + i) * 0x201);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKI(n, 8);
  CHECKI(unlink_txtdata, 0);
  CHECKI(unlink_data, -1);

  return r;
}
