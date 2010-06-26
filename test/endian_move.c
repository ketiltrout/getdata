/* Test endianness */
#include "../src/config.h"
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT16 8\nENDIAN little\n";
  uint16_t data_data[128];
  uint16_t c[8], d;
  int fd;
  int r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = 0x201 * fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = gd_alter_endianness(D, GD_BIG_ENDIAN, 0, 1);
  int error = gd_error(D);
  int n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);

  gd_close(D);

  fd = open(data, O_RDONLY | O_BINARY);
  int i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKX(d, i * 0x102);
      i++;
    }
    close(fd);
  } else {
    perror("open");
    r = 1;
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

#ifdef WORDS_BIGENDIAN
# define FACTOR 0x102
#else
# define FACTOR 0x201
#endif

  for (i = 0; i < 8; ++i)
    CHECKXi(i,c[i], (40 + i) * FACTOR);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKI(n, 8);

  return r;
}
