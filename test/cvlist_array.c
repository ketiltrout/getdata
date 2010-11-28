#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data =
    "data1 CARRAY UINT8 1 2 3 4 5\n"
    "data2 CARRAY UINT8 2 4 6 8 10 12\n"
    "data3 CARRAY UINT8 3 6 9 12 15 18 21\n"
    "data4 RAW UINT8 1\n";
  int fd, error, r = 0;
  size_t i;
  struct uint8_carrays {
    size_t n;
    uint8_t *d;
  } *field_list;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = (struct uint8_carrays*)gd_carrays(D, GD_UINT8);

  error = gd_error(D);

  CHECKI(error, 0);

  if (!r)
    for (fd = 0; fd < 3; ++fd) {
      CHECKUi(fd,field_list[fd].n, (size_t)(5 + fd));
      for (i = 0; i < field_list[fd].n; ++i)
        CHECKUi(fd * 1000 + i,field_list[fd].d[i], (fd + 1) * (i + 1));
    }

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
