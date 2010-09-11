/* Test field modifying */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  unsigned char data_data[256];
  uint16_t d;
  int fd, i, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_entry(D, "data", &E);
  E.u.raw.type = GD_UINT16;
  E.u.raw.spf = 11;
  int ret = gd_alter_entry(D, "data", &E, 1);
  int error = gd_error(D);
  off_t n = gd_nframes(D);

  gd_close(D);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKX(d, i * 8 / 11);
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

  CHECKI(error, 0);
  CHECKI(n, 32);
  CHECKI(ret, 0);

  return r;
}
