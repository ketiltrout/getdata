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
    "data1 STRING UINT8 1\n"
    "data2 STRING UINT8 1\n"
    "data3 STRING UINT8 1\n"
    "data4 CONST UINT8 1\n";
  int fd, i, error, r = 0;
  const char **field_list;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_field_list_by_type(D, GD_STRING_ENTRY);

  error = gd_error(D);
  CHECKI(error, GD_E_OK);
  CHECKPN(field_list);

  if (field_list == NULL)
    r = 1;

  for (i = 0; field_list[i]; ++i) {
    CHECKIi(i,strlen(field_list[i]), 5);

    CHECKIi(i,field_list[i][0], 'd');
    CHECKIi(i,field_list[i][1], 'a');
    CHECKIi(i,field_list[i][2], 't');
    CHECKIi(i,field_list[i][3], 'a');

    if (field_list[i][4] < '1' || field_list[i][4] > '3') {
      fprintf(stderr, "field_list[%i] = \"%s\"\n", i, field_list[i]);
      r = 1;
    }
  }

  CHECKI(i, 3);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
