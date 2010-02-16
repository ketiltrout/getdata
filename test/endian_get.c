/* Test endianness */
#include "../src/config.h"
#include "../src/getdata.h"

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
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data1, strlen(format_data1));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  unsigned long n = get_endianness(D, 0);
  unsigned long m = get_endianness(D, 1);
  int error = get_error(D);

  dirfile_close(D);

  unlink(format);
  unlink(format1);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (n != GD_LITTLE_ENDIAN) {
    fprintf(stderr, "2=%lx\n", n);
    return 1;
  }
  if (m != GD_BIG_ENDIAN) {
    fprintf(stderr, "m=%lx\n", m);
    return 1;
  }

  return 0;
}
