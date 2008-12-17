/* Test include */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format2 = __TEST__ "dirfile/format2";
  const char* format_data = "/INCLUDE format1\na CONST UINT8 1\n";
  const char* format1_data = "b CONST UINT8 11\n/INCLUDE format2\n";
  const char* format2_data = "c CONST UINT8 11\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(format2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format2_data, strlen(format2_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_uninclude(D, 1, 0);
  int error = get_error(D);
  unsigned int nfields = get_nfields(D);
  unsigned int nfragments = get_nfragments(D);
  dirfile_close(D);

  int unlink_format2 = unlink(format2);
  int unlink_format1 = unlink(format1);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "2=%i\n", ret);
    return 1;
  }
  if (nfields != 2) {
    fprintf(stderr, "3=%i\n", nfields);
    return 1;
  }
  if (nfragments != 1) {
    fprintf(stderr, "4=%i\n", nfragments);
    return 1;
  }
  if (unlink_format2 != 0) {
    fprintf(stderr, "5=%i\n", unlink_format2);
    return 1;
  }
  if (unlink_format1 != 0) {
    fprintf(stderr, "6=%i\n", unlink_format1);
    return 1;
  }

  return 0;
}
