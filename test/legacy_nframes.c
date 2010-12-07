/* Retreiving the number of frames via the legacy API should succeed cleanly */
#include "test.h"

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
  const char* format_data = "data RAW UINT8 1\n";
  int fd, error, r = 0;
  const size_t len = strlen(data);
  size_t n;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  n = GetNFrames(filedir, &error, NULL);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKU(n,len);

  return r;
#endif
}
