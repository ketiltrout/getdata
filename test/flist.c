/* Retreiving the number of fields of a field should succeed cleanly */
#include "test.h"

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
    "data1 RAW UINT8 1\n"
    "data2 RAW UINT8 1\n"
    "data3 RAW UINT8 1\n";
  int fd, i, error, r = 0;
  const char **field_list;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_field_list(D);

  error = gd_error(D);

  CHECKI(error, 0);
  CHECKPN(field_list);

  for (i = 0; ; ++i) {
    if (field_list[i] == NULL)
      break;

    if (strcmp(field_list[i], "data1") == 0)
      continue;
    else if (strcmp(field_list[i], "data2") == 0)
      continue;
    else if (strcmp(field_list[i], "data3") == 0)
      continue;
    else if (strcmp(field_list[i], "INDEX") == 0)
      continue;

    fprintf(stderr, "field_list[%i] = \"%s\"\n", i, field_list[i]);
    r = 1;
  }

  CHECKI(i, 4);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
