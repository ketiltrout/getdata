/* The number of frames should track the samples per frame */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

static void write_format(const char* format, int spf)
{
  char format_data[100];
  int fd;
  sprintf(format_data, "data RAW UINT8 %i\n", spf);

  fd = open(format, O_CREAT | O_TRUNC | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);
}

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  int fd, i, r = 0;
  const size_t len = strlen(data);

  mkdir(filedir, 0777);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  for (i = 1; i < (int)len + 1; ++i) {
    DIRFILE *D;
    gd_spf_t spf;
    size_t n;

    write_format(format, i);
    D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
    spf = gd_spf(D, "data");
    n = gd_nframes(D);
    CHECKUi(i, n, len / spf);
    gd_close(D);
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
