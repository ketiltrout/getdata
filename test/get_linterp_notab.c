/* Attempt to read LINTERP */
#include "test.h"

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
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "linterp LINTERP data ./table\ndata RAW UINT8 1\n";
  unsigned char c = 0;
  unsigned char data_data[64];
  int fd, r = 0;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 64; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 64);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  int n = gd_getdata(D, "linterp", 5, 0, 1, 0, GD_UINT8, &c);
  int error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n, 0);
  CHECKI(error, GD_E_OPEN_LINFILE);

  return r;
}
