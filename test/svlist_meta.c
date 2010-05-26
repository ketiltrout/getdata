/* Retreive a list of string metafields */
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
    "parent STRING valu1\n"
    "META parent data1 STRING valu1\n"
    "META parent data2 STRING valu2\n"
    "META parent data3 STRING valu3\n"
    "META parent data4 CONST UINT8 1\n";
  int fd, i, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_get_nfields(D);
  gd_get_nmfields(D, "parent");
  gd_get_nmfields_by_type(D, "parent", GD_STRING_ENTRY);
  const char** field_list = gd_get_mstrings(D, "parent");

  int error = gd_error(D);
  CHECKI(error, 0);
  CHECKPN(field_list);

  for (i = 0; field_list[i]; ++i) {
    int len = strlen(field_list[i]);
    CHECKIi(i,len,5);

    CHECKIi(i,field_list[i][0], 'v');
    CHECKIi(i,field_list[i][1], 'a');
    CHECKIi(i,field_list[i][2], 'l');
    CHECKIi(i,field_list[i][3], 'u');

    if (field_list[i][4] < '1' || field_list[i][4] > '3') {
      fprintf(stderr, "field_list[%i] = \"%s\"\n", i, field_list[i]);
      r = 1;
    }
  }

  CHECKI(i,3);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
