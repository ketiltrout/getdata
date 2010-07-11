/* Retreiving the number of frames should succeed cleanly */
#if defined _FILE_OFFSET_BITS && _FILE_OFFSET_BITS == 64
#  define SKIP_TEST
#else
#  define _FILE_OFFSET_BITS 64
#endif

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
#ifdef SKIP_TEST
  return 77;
#else
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 1\n";
  int fd;
  const size_t len = strlen(data);

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  size_t n = gd_nframes(D);
  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return !(n == len);
#endif
}
