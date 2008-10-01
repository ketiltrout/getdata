/* Retreiving the number of fields of a field should succeed cleanly */
#include "../src/getdata.h"

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
  const char* format_data =
    "data1 RAW UINT8 1\n"
    "data2 RAW UINT8 1\n"
    "data3 RAW UINT8 1\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  const char** field_list = get_field_list(D);

  if (get_error(D))
    return 1;

  if (field_list == NULL)
    return 1;

  for (fd = 0; fd < 3; ++fd) {
    if (strlen(field_list[fd]) != 5)
      return 1;

    if (field_list[fd][0] != 'd')
      return 1;

    if (field_list[fd][1] != 'a')
      return 1;

    if (field_list[fd][2] != 't')
      return 1;

    if (field_list[fd][3] != 'a')
      return 1;

    if (field_list[fd][4] < '1' || field_list[fd][4] > '3')
      return 1;
  }

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  return 0;
}
