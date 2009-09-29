/* Parser check */
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
    "parent RAW UINT8 1\n"
    "parent/child CONST UINT8 1\n";
  int fd;
  signed char c;
  int r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  if (get_error(D))
    r = 1;

  get_constant(D, "parent/child", GD_INT8, &c);
  if (get_error(D))
    r = 1;
  if (c != 1)
    r = 1;

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
