/* Attempt to write UINT8 c/ an old-style type character should fail cleanly */
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
  const char* filedir = __FILE__ "dirfile";
  const char* format = __FILE__ "dirfile/format";
  const char* data = __FILE__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  uint8_t c[8], d;
  int fd, i;
  struct stat buf;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = 40 + i;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int n = putdata(D, "data", 5, 0, 1, 0, 'c', c);
  int error = D->error;

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  if (n != 0)
    return 1;

  return (error != GD_E_BAD_TYPE);
}