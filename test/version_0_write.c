#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "FRAMEOFFSET RAW c 8\na&b RAW c 8";
  int fd, e, q, c, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e = gd_dirfile_standards(D, 0);
  q = gd_rewrite_fragment(D, 0);
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  c = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e,0);
  CHECKI(q,0);
  CHECKI(c,0);

  return r;
}
