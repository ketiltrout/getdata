/* Attempt to read FLOAT32 as UINT16 */
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
  const char* format_data = "data RAW FLOAT32 8\n";
  float  data_data[256];
  uint16_t c[8];
  int fd, i;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(float));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  int n = getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);

  if (D->error)
    return 1;
  if (n != 8)
    return 1;
  for (i = 0; i < 8; ++i)
    if (c[i] != 40 + i)
      return 1;

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return 0;
}
