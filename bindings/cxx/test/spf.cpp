/* Retreiving the samples-per-frame of a field should succeed cleanly */
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
  const char* format_data = "data RAW UINT8 11\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  Dirfile dirfile(filedir, GD_RDONLY);
  unsigned int spf = dirfile.SamplesPerFrame("data");

  unlink(format);
  rmdir(filedir);

  return (spf != 11);
}
