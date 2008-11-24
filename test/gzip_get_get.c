/* Attempt to read UINT8 */
#include "../src/config.h"
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* gzipdata = __TEST__ "dirfile/data.gz";
  const char* format_data = "data RAW UINT16 8\n";
  uint16_t c1[8], c2[8];
  char command[4096];
  uint16_t data_data[256];
  int fd, i;

  memset(c1, 0, 16);
  memset(c2, 0, 16);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(uint16_t));
  close(fd);

  /* compress */
  snprintf(command, 4096, "%s -f %s > /dev/null", GZIP, data);
  system(command);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n1 = getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c1);
  int error1 = get_error(D);
  dirfile_close(D);

  D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n2 = getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c2);
  int error2 = get_error(D);
  dirfile_close(D);

  unlink(gzipdata);
  unlink(format);
  rmdir(filedir);

  if (error1)
    return 1;
  if (error2)
    return 1;
  if (n1 != 8) {
    printf("n1 = %i\n", n1);
    return 1;
  }
  if (n2 != 8) {
    printf("n2 = %i\n", n2);
    return 1;
  }
  for (i = 0; i < 8; ++i) {
    if (c1[i] != 40 + i)
      return 1;
    if (c2[i] != 40 + i)
      return 1;
  }

  return 0;
}
