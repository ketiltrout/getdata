/* Attempt to write UINT8 via the legacy interface*/
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
#ifndef GD_LEGACY_API
  return 77; /* skipped */
#else
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  uint8_t c[8], d;
  int fd, i, n, error, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  n = PutData(filedir, "data", 5, 0, 1, 0, 'c', c, &error);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    if (i < 40 || i > 48) {
      CHECKUi(i,d,0);
    } else
      CHECKUi(i,d,i);
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);

  return r;
#endif
}
