/* Attempt to write big-endian COMPLEX64 */
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
  const char* format_data = "data RAW COMPLEX64 1\nENDIAN big\n";
  int fd, r = 0;
  unsigned int i;
#ifdef GD_NO_C99_API
  const float c[] = {1.5, 2.25};
#else
  const float complex c = 1.5 + _Complex_I * 2.25;
#endif
  unsigned char x[2 * sizeof(float)] = {
    0x3F, 0xC0, 0x00, 0x00, 0x40, 0x10, 0x00, 0x00
  };
  unsigned char u[2 * sizeof(float)];

  mkdir(filedir, 0777); 

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
#ifdef GD_NO_C99_API
  int n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, c);
#else
  int n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, &c);
#endif
  int error = gd_error(D);

  gd_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * 2 * sizeof(float), SEEK_SET);
  read(fd, u, 2 * sizeof(float));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);
  
  for (i = 0; i < 2 * sizeof(float); ++i)
    CHECKXi(i, u[i], x[i]);

  return r;
}
