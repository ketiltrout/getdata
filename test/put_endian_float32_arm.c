/* Attempt to write arm-endian FLOAT32 (which is just little endian) */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW FLOAT32 1\nENDIAN little arm\n";
  unsigned int i;
  const float c = 1.5;
  unsigned char x[sizeof(float)] = { 0x00, 0x00, 0xC0, 0x3F };
  unsigned char u[sizeof(float)];
  int fd, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777); 

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_FLOAT32, &c);
  error = gd_error(D);

  gd_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(float), SEEK_SET);
  read(fd, u, sizeof(float));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);
  
  for (i = 0; i < sizeof(float); ++i)
    CHECKXi(i, u[i], x[i]);

  return r;
}
