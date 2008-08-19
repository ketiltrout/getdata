#include "dirfile.h"
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  unsigned char c[8];
  unsigned char data_data[256];
  int fd, i;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  Dirfile dirfile = Dirfile(filedir);
  size_t n = dirfile.GetData("data", 5, 0, 1, 0, Dirfile::UInt8, c);
  int error = dirfile.Error();

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;
  if (n != 8)
    return 1;
  for (i = 0; i < 8; ++i)
    if (c[i] != 40 + i)
      return 1;

  return 0;
}
