/* Attempt to write UINT8 */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data.txt";
  const char* format_data = "data RAW UINT8 8\n";
  uint8_t c[8];
  int d;
  int fd, i, r = 0;
  struct stat buf;
  FILE* stream;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_TEXT_ENCODED | GD_VERBOSE);
  int n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  int error = gd_error(D);

  gd_close(D);

  if (stat(data, &buf))
    return 1;

  stream = fopen(data, "r" FOPEN_TEXT);
  i = 0;
  for (;;) {
    fscanf(stream, "%i", &d);
    if (feof(stream))
      break;
    if (i < 40 || i > 48) {
      CHECKI(d, 0);
    } else
      CHECKI(d, i);
    i++;
  }
  fclose(stream);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);

  return r;
}
