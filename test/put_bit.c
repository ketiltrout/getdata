/* Attempt to write BIT */
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
  const char* format_data = "bit BIT data 2 3\ndata RAW UINT8 8\n";
  uint8_t c[8], d = 0xA5;
  int fd, i;

  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = i;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  for (i = 0; i < 50; ++i)
    write(fd, &d, sizeof(uint8_t));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int n = putdata(D, "bit", 5, 0, 1, 0, GD_INT8, c);
  int error = D->error;

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  i = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    if (i < 40 || i >= 48) {
      if (d != 0xA5)
        return 1;
    } else if (d != (0xA1 | (i - 40) << 2))
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
