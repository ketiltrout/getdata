/* Retreiving the number of frames should succeed cleanly */
#ifdef HAVE_CONFIG_H
#include "config.h"
#undef GETDATA_LEGACY_API
#endif

#include "getdata/dirfile.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#if MKDIR_NO_MODE
#ifdef HAVE__MKDIR
#define mkdir(f,m) _mkdir(f)
#else
#define mkdir(f,m) mkdir(f)
#endif
#endif

using namespace GetData;

int main(void)
{
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

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data, len);
  close(fd);

  Dirfile dirfile(filedir, GD_RDONLY);
  size_t n = dirfile.NFrames();

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return !(n == len);
}
