/* Attempt a write following a read via the legacy API -- this requires
 * closing and then re-opening the legacy dirfile */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
#ifndef GETDATA_LEGACY_API
  return 77; /* skipped */
#else
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  uint8_t c[8];
  unsigned char data_data[256];
  int fd, i, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  int get_error;
  GetData(filedir, "data", 5, 0, 1, 0, 'c', c, &get_error);
  int put_error;
  int n = PutData(filedir, "data", 5, 0, 1, 0, 'c', c, &put_error);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(get_error,0);
  CHECKI(put_error,0);
  CHECKI(n,8);

  return r;
#endif
}
