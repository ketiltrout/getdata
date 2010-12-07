/* Attempt to read INT64 */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW INT64 8\n";
  int64_t c[8];
  int64_t data_data[64];
  int fd, i, n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 64; ++fd)
    data_data[fd] = fd * (0x0200000000000001LLU) * (2 * (fd % 2) - 1);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 64 * sizeof(int64_t));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT64, c);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKI(n, 8);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], (0x5000000000000028LL + i * 0x0200000000000001LL)
        * (2 * (i % 2) - 1));

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
