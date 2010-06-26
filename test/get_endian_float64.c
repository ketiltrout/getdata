/* Attempt to read FLOAT64 with the opposite endianness */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

static int BigEndian(void)
{
  union {
    long int li;
    char ch[sizeof(long int)];
  } un;
  un.li = 1;
  return (un.ch[sizeof(long int) - 1] == 1);
}

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  char format_data[1000];
  union {
    double f;
    char b[8];
  } u;
  const char x[8] = {0x40, 0x26, 0xc8, 0x00, 0x00, 0x00, 0x00, 0x00};
  u.f = 0;
  double data_data[128];
  int fd, i, r = 0;
  const int big_endian = BigEndian();

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW FLOAT64 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  data_data[0] = 1.5;
  for (fd = 1; fd < 128; ++fd)
    data_data[fd] = data_data[fd - 1] * 1.5;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 128 * sizeof(double));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = gd_getdata(D, "data", 5, 0, 1, 0, GD_FLOAT64, &u.f);
  int error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);

  for (i = 0; i < 8; ++i)
    CHECKXi(i,u.b[i],x[(big_endian) ? 7 - i : i]);

  return r;
}
