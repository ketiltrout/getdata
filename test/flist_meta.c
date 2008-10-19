/* Retreiving the number of fields of a field should succeed cleanly */
#include "../src/getdata.h"

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
    "parent RAW UINT8 1\n"
    "META parent data1 LINTERP UINT8 1\n"
    "META parent data2 LINTERP UINT8 1\n"
    "META parent data3 LINTERP UINT8 1\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  const char** field_list = get_mfield_list(D, "parent");

  if (get_error(D))
    r = 1;

  if (field_list == NULL)
    r = 1;

  if (!r)
    for (fd = 0; field_list[fd] != NULL; ++fd) {
      if (strlen(field_list[fd]) != 5)
        r = 1;

      if (field_list[fd][0] != 'd')
        r = 1;

      if (field_list[fd][1] != 'a')
        r = 1;

      if (field_list[fd][2] != 't')
        r = 1;

      if (field_list[fd][3] != 'a')
        r = 1;

      if (field_list[fd][4] < '1' || field_list[fd][4] > '3')
        r = 1;
    }

  if (fd != 3)
    r = 1;

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
