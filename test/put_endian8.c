/* Attempt to write UINT8 with the opposite endianness */
#include "test.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
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
  uint8_t c = 0x21, d = 0;
  const int big_endian = BigEndian();
  int fd, n, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW UINT8 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, &c);
  error = gd_error(D);

  gd_close(D);

  fd = open(data, O_RDONLY | O_BINARY);
  lseek(fd, 5 * sizeof(uint8_t), SEEK_SET);
  read(fd, &d, sizeof(uint8_t));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKX(d,0x21);
  CHECKI(n,1);
  CHECKI(error,0);
  return r;
}
