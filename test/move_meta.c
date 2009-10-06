/* Test move */
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
  const char* format_data = "INCLUDE format1\ndata RAW UINT8 11\n"
    "data/meta CONST UINT8 11\n";
  const char* format1_data = "\n";
  int fd, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int ret = dirfile_move(D, "data", 1, 0);
  int error = get_error(D);
  int ge_ret =  get_entry(D, "data/meta", &E);
  dirfile_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  if (ret != 0) {
    fprintf(stderr, "1=%i\n", ret);
    r = 1;
  }
  if (error != GD_E_OK) {
    fprintf(stderr, "2=%i\n", error);
    r = 1;
  }
  if (ge_ret != 0) {
    fprintf(stderr, "3=%i\n", ge_ret);
    r = 1;
  }
  if (E.fragment_index != 1) {
    fprintf(stderr, "4=%i\n", E.fragment_index);
    r = 1;
  }

  return r;
}
