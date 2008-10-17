/* Retreiving the number of fields of a field should succeed cleanly */
#include "../src/getdata.h"

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
    "data1 CONST UINT8 1\n"
    "data2 CONST UINT8 2\n"
    "data3 CONST UINT8 3\n"
    "data4 RAW UINT8 1\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  const uint8_t* field_list = get_constants(D, GD_UINT8);

  if (get_error(D))
    r = 1;

  if (field_list == NULL)
    r = 1;

  fd = 0;
  if (!r)
    for (fd = 0; fd < 3; ++fd)
      if (field_list[fd] != fd + 1)
        r = 1;

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
