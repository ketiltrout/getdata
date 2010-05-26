/* Attempt to read a non-existant field */
#include "test.h"

#include <stdio.h>
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
  const char* format_data = "\n";
  unsigned char c = 0;
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  int n = gd_getdata(D, "no-such-field", 5, 0, 1, 0, GD_UINT8, &c);

  int error = gd_error(D);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(n, 0);
  CHECKI(error, GD_E_BAD_CODE);
  return r;
}
