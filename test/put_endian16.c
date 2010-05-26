/* Attempt to write UINT16 with the opposite endianness */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
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
  uint16_t c = 0x201, d = 0;
  int fd, r = 0;
  const int big_endian = BigEndian();

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW UINT16 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT16, &c);
  int error = gd_error(D);

  gd_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(uint16_t), SEEK_SET);
  read(fd, &d, sizeof(uint16_t));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,1);
  CHECKX(d,0x102);

  return r;
}
