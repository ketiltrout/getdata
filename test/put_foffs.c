/* Attempt to write UINT8 with FRAMEOFFSET */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\nFRAMEOFFSET 2\n";
  uint8_t c[8], d;
  int fd, i;
  struct stat buf;

  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = 40 + i;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int n = putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  int error = D->error;

  dirfile_close(D);

  if (stat(data, &buf))
    return 1;
  if (buf.st_size != 32 * sizeof(uint8_t))
    return 1;

  fd = open(data, O_RDONLY);
  i = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    if (i < 24 || i >= 32) {
      if (d != 0)
        return 1;
    } else if (d != i + 16)
      return 1;
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;
  if (n != 8)
    return 1;

  return 0;
}
