/* Attempt to write BIT */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
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
  uint8_t c[8];
  uint8_t d = 0xA5;
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

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = putdata(D, "bit", 5, 0, 1, 0, GD_INT8, c);
  int error = get_error(D);

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  i = 0;
  int ne = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    if (i < 40 || i >= 48) {
      if (d != 0xA5) {
        ne++;
        fprintf(stderr, "%i=%2x A5\n", i, d);
      }
    } else if (d != (0xA1 | (i - 40) << 2)) {
      ne++;
      fprintf(stderr, "%i=%2x %2x\n", i, d, (0xA1 | (i - 40) << 2));
    }
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "n=%i\n", error);
    return 1;
  }
  if (n != 8) {
    fprintf(stderr, "n=%i\n", n);
    return 1;
  }
  if (ne)
    return 1;

  return 0;
}
