/* Attempt to read SBIT */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <inttypes.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "bit SBIT data 7 4\ndata RAW UINT16 1\n";
  int16_t c[10];
  uint16_t data_data[256];
  int fd;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = 0x0101 * (uint16_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(uint16_t));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = getdata(D, "bit", 5, 0, 10, 0, GD_INT16, c);
  int error = get_error(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    printf("1=%i\n", n);
    return 1;
  }
  if (n != 10) {
    printf("2=%i\n", n);
    return 1;
  }
  for (fd = 0; fd < 10; ++fd)
    if (c[fd] != fd * 2 - ((fd > 6) ? 22 : 6)) {
      fprintf(stderr, "c[%i] = %2i %i\n", fd, c[fd],
          fd * 2 - ((fd > 6) ? 22 : 6));
      return 1;
    }

  return 0;
}
