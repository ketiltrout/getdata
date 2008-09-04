#include "dirfile.h"
#include "entry.h"

#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

using namespace GetData;

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "data RAW UINT8 8\n";
  unsigned char c[8];
  int fd;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  Dirfile dirfile = Dirfile(filedir);
  Entry *entry = dirfile.Entry("data");
  int error = dirfile.Error();
  dirfile.Flush();

  unlink(format);
  rmdir(filedir);

  if (strcmp(entry->Code(), "data") != 0)
    return 1;

  if (entry->Type() != RawEntry)
    return 1;

  if (entry->SamplesPerFrame() != 8)
    return 1;

  if (strcmp(entry->File(), "./data") != 0)
    return 1;

  if (entry->RawType() != UInt8)
    return 1;

  if (error)
    return 1;

  return 0;
}
