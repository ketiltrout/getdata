/* Attempt to write little-endian FLOAT64 */
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
  const char* format_data = "data RAW FLOAT64 1\nENDIAN little\n";
  int fd, r = 0;
  unsigned int i;
  const double c = 1.5;
  unsigned char x[sizeof(double)] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF8, 0x3F
  };
  unsigned char u[sizeof(double)];

  mkdir(filedir, 0777); 

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = gd_putdata(D, "data", 5, 0, 1, 0, GD_FLOAT64, &c);
  int error = gd_error(D);

  gd_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(double), SEEK_SET);
  read(fd, u, sizeof(double));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);
  
  for (i = 0; i < sizeof(double); ++i)
    CHECKXi(i, u[i], x[i]);

  return r;
}
