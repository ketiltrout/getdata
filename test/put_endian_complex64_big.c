/* Attempt to write big-endian COMPLEX64 */
#include "test.h"

#include <complex.h>
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
  const char* format_data = "data RAW COMPLEX64 1\nENDIAN big\n";
  int fd, i, r = 0;
  const float complex c = 1.5 + _Complex_I * 2.25;
  unsigned char x[sizeof(float complex)] = {
    0x3F, 0xC0, 0x00, 0x00, 0x40, 0x10, 0x00, 0x00
  };
  unsigned char u[sizeof(float complex)];

  mkdir(filedir, 0777); 

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, &c);
  int error = gd_error(D);

  gd_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(float complex), SEEK_SET);
  read(fd, u, sizeof(float complex));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);
  
  for (i = 0; i < sizeof(float complex); ++i)
    CHECKXi(i, u[i], x[i]);

  return r;
}
