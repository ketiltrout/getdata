/* Retreiving the samples-per-frame of a field via the legacy API should
 * succeed cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
#ifndef GD_LEGACY_API
  return 77; /* skip */
#else
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "data RAW UINT8 11\n";
  int fd, error, r = 0;
  unsigned int spf;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  spf = GetSamplesPerFrame(filedir, "data", &error);

  unlink(format);
  rmdir(filedir);

  CHECKU(spf, 11);
  return r;
#endif
}
