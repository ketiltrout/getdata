/* Attempt to read UINT8 */
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

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  int n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UNKNOWN, c);

  int error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n, 0);
  CHECKI(error, GD_E_BAD_TYPE);

  return r;
}
