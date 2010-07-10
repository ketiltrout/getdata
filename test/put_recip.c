/* Attempt to write DIVIDE */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "div RECIP 1000. data\ndata RAW INT8 8\n";
  int8_t c[8], d;
  int fd, i, r = 0;
  struct stat buf;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (int8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = gd_putdata(D, "div", 5, 0, 1, 0, GD_INT8, c);
  int error = gd_error(D);

  gd_close(D);

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  }
  CHECKI(buf.st_size, 48 * sizeof(int8_t));

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
  while (read(fd, &d, sizeof(int8_t))) {
    if (i < 40 || i > 48) {
      CHECKIi(i,d,0);
    } else
      CHECKIi(i,d, 1000. / i);
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n,8);
  CHECKI(error, 0);

  return r;
}