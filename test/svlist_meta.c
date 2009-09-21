/* Retreive a list of string metafields */
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
    "parent STRING valu1\n"
    "META parent data1 STRING valu1\n"
    "META parent data2 STRING valu2\n"
    "META parent data3 STRING valu3\n"
    "META parent data4 CONST UINT8 1\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  get_nfields(D);
  get_nmfields(D, "parent");
  get_nmfields_by_type(D, "parent", GD_STRING_ENTRY);
  const char** field_list = get_mstrings(D, "parent");

  if (get_error(D)) {
    fprintf(stderr, "error = %i\n", get_error(D));
    r = 1;
  }

  if (field_list == NULL) {
    fprintf(stderr, "field_list = %p\n", field_list);
    r = 1;
  }

  fd = 0;
  if (!r)
    for (fd = 0; ; ++fd) {
      if (field_list[fd] == NULL)
        break;

      if (strlen(field_list[fd]) != 5) {
        fprintf(stderr, "strlen(field_list[%i]) = %i\n", fd,
            strlen(field_list[fd]));
        r = 1;
      }

      if (field_list[fd][0] != 'v') {
        fprintf(stderr, "field_list[%i][0] = %i\n", fd, field_list[fd][0]);
        r = 1;
      }

      if (field_list[fd][1] != 'a') {
        fprintf(stderr, "field_list[%i][0] = %i\n", fd, field_list[fd][0]);
        r = 1;
      }

      if (field_list[fd][2] != 'l') {
        fprintf(stderr, "field_list[%i][0] = %i\n", fd, field_list[fd][0]);
        r = 1;
      }

      if (field_list[fd][3] != 'u') {
        fprintf(stderr, "field_list[%i][0] = %i\n", fd, field_list[fd][0]);
        r = 1;
      }

      if (field_list[fd][4] < '1' || field_list[fd][4] > '3') {
        fprintf(stderr, "field_list[%i][0] = %i\n", fd, field_list[fd][0]);
        r = 1;
      }
    }

  if (fd != 3) {
    fprintf(stderr, "fd = %i\n", fd);
    r = 1;
  }

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
