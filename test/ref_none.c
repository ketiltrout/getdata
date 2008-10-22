/* Parser check */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data =
    "data1 RAW UINT8 1\n"
    "data2 RAW UINT8 1\n"
    ;

  const char* data1 = __TEST__ "dirfile/data1";
  const char* data2 = __TEST__ "dirfile/data2";

  int fd;
  int r = 0;
  uint8_t data_data[4] = { 0, 1, 2, 3 };

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 4);
  close(fd);

  fd = open(data2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 3);
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);

  if (get_error(D))
    r = 1;

  off_t nf = get_nframes(D);

  if (get_error(D))
    r = 1;
  else if (nf != 4) {
    fprintf(stderr, "1=%llu\n", (unsigned long long)nf);
    r = 1;
  }

  dirfile_close(D);

  unlink(format);
  unlink(data1);
  unlink(data2);
  rmdir(filedir);

  return r;
}
