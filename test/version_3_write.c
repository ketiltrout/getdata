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
  const char* format1 = __TEST__ "dirfile/RAW";
  const char* format_data = "ENDIAN RAW c 8\nINCLUDE RAW\n";
  const char* format_data1 = "m MULTIPLY ENDIAN ENDIAN\na&b RAW c 8\n";
  int fd, e, q, c, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data1, strlen(format_data1));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e = gd_dirfile_standards(D, 3);
  q = gd_rewrite_fragment(D, GD_ALL_FRAGMENTS);
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  c = gd_dirfile_standards(D, GD_VERSION_EARLIEST);
  gd_close(D);

  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(e,3);
  CHECKI(q,0);
  CHECKI(c,3);

  return r;
}
