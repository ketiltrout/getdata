/* Attempt to read UINT8 */
#include "../src/config.h"
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

int main(void)
{
#if !defined USE_GZIP || !defined TEST_GZIP
  return 77; /* skip test */
#else
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* gzipdata = __TEST__ "dirfile/data.gz";
  const char* format_data = "data RAW UINT16 8\n";
  uint16_t c1[8], c2[8];
  char command[4096];
  uint16_t data_data[256];
  int fd, i, n1, error1, n2, error2, r = 0;
  DIRFILE *D;

  memset(c1, 0, 16);
  memset(c2, 0, 16);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(uint16_t));
  close(fd);

  /* compress */
  snprintf(command, 4096, "%s -f %s > /dev/null", GZIP, data);
  if (gd_system(command))
    return 1;

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c1);
  error1 = gd_error(D);
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n2 = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c2);
  error2 = gd_error(D);
  gd_close(D);

  unlink(gzipdata);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1, 0);
  CHECKI(error2, 0);
  CHECKI(n1, 8);
  CHECKI(n2, 8);
  for (i = 0; i < 8; ++i) {
    CHECKUi(i,c1[i], 40 + i);
    CHECKUi(i,c2[i], 40 + i);
  }

  return r;
#endif
}
