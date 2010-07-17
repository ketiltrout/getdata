/* Open a Standards Version 4 conformant dirfile */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/RAW";
  const char* data = __TEST__ "dirfile/ENDIAN";
  const char* format_data = "ENDIAN RAW c 8\nINCLUDE RAW\n";
  const char* format_data1 = "VERSION PHASE ENDIAN 1\na&b RAW c 8\n";
  uint16_t c[8];
  unsigned char data_data[256];
  int fd, i, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data1, strlen(format_data1));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = gd_getdata(D, "VERSION", 5, 0, 1, 0, GD_UINT16, c);
  int error = gd_error(D);

  int v = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  int l = gd_dirfile_standards(D, GD_VERSION_LATEST);
  int e = gd_dirfile_standards(D, GD_VERSION_EARLIEST);

  gd_close(D);

  unlink(data);
  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);

  for (i = 0; i < 8; ++i)
    CHECKUi(i,c[i],41 + i);

  CHECKI(v,4);
  CHECKI(l,4);
  CHECKI(e,4);

  return r;
}
