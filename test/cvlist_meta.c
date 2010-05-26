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
    "parent CONST UINT8 1\n"
    "META parent data1 CONST UINT8 1\n"
    "META parent data2 CONST UINT8 2\n"
    "META parent data3 CONST UINT8 3\n"
    "META parent data4 LINTERP UINT8 1\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  const uint8_t* field_list = gd_get_mconstants(D, "parent", GD_UINT8);

  int error = gd_error(D);
  CHECKI(error, 0);

  if (!r)
    for (fd = 0; fd < 3; ++fd)
      CHECKUi(fd,field_list[fd], fd + 1);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
