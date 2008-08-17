/* Retreiving the samples-per-frame of a field via the legacy API should
 * succeed cleanly */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
#ifndef GETDATA_LEGACY_API
  return 77; /* skip */
#else
  const char* filedir = __FILE__ "dirfile";
  const char* format = __FILE__ "dirfile/format";
  const char* format_data = "data RAW UINT8 11\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  int error;
  unsigned int spf = GetSamplesPerFrame(filedir, "data", &error);

  unlink(format);
  rmdir(filedir);

  return (spf != 11);
#endif
}
