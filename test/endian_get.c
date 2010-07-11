/* Test endianness */
#include "../src/config.h"
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
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format_data = "ENDIAN little\nINCLUDE format1\n";
  const char* format_data1 = "ENDIAN big\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data1, strlen(format_data1));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  unsigned long n = gd_endianness(D, 0);
  unsigned long m = gd_endianness(D, 1);
  int error = gd_error(D);

  gd_close(D);

  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKX(n, GD_LITTLE_ENDIAN);
  CHECKX(m, GD_BIG_ENDIAN);

  return r;
}
