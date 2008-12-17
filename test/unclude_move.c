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
  int ret1 = dirfile_uninclude(D, 2, 0);
  int error1 = get_error(D);
  int ret2 = dirfile_include(D, "format2", 0, 0);
  int error2 = get_error(D);
  unsigned int nfields = get_nfields(D);
  unsigned int nfragments = get_nfragments(D);
  dirfile_close(D);

  unlink(format2);
  unlink(format1);
  unlink(format);
  rmdir(filedir);

  if (error1) {
    fprintf(stderr, "1=%i\n", error1);
    return 1;
  }
  if (error2) {
    fprintf(stderr, "2=%i\n", error2);
    return 1;
  }
  if (ret1 != 0) {
    fprintf(stderr, "3=%i\n", ret1);
    return 1;
  }
  if (ret2 != 2) {
    fprintf(stderr, "4=%i\n", ret2);
    return 1;
  }
  if (nfields != 4) {
    fprintf(stderr, "5=%i\n", nfields);
    return 1;
  }
  if (nfragments != 3) {
    fprintf(stderr, "6=%i\n", nfragments);
    return 1;
  }

  return 0;
}
