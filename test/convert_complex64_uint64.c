/* Attempt to read COMPLEX64 as UINT64 */
#include "test.h"

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
  const char* format_data = "data CONST UINT64 0\n";
  uint64_t c = 0;
  float complex d = 8;
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR);
  int n1 = gd_put_constant(D, "data", GD_COMPLEX64, &d);
  int n2 = gd_get_constant(D, "data", GD_UINT64, &c);
  int error = gd_error(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n1, 0);
  CHECKI(n2, 0);
  CHECKU(c, 8);

  return r;
}
