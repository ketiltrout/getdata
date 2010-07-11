/* Test move */
#include "test.h"

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
  const char* subdir = __TEST__ "dirfile/subdir";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/subdir/format1";
  const char* data = __TEST__ "dirfile/data";
  const char* new_data = __TEST__ "dirfile/subdir/data";
  const char* format_data = "INCLUDE subdir/format1\ndata RAW UINT8 11\n";
  const char* format1_data = "#\n";
  int fd, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);
  mkdir(subdir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int ret = gd_move(D, "data", 1, 1);
  int error = gd_error(D);
  int ge_ret =  gd_entry(D, "data", &E);
  gd_close(D);

  int unlink_data = unlink(data);
  int unlink_new_data = unlink(new_data);
  unlink(format1);
  unlink(format);
  rmdir(subdir);
  rmdir(filedir);

  CHECKI(ret, 0);
  CHECKI(error, GD_E_OK);
  CHECKI(ge_ret, 0);
  CHECKI(E.fragment_index, 1);
  CHECKI(unlink_data, -1);
  CHECKI(unlink_new_data, 0);

  return r;
}
