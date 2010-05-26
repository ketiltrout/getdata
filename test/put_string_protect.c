/* Attempt to write UINT8 */
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
  const char* format_data = "data STRING UINT8\nPROTECT all\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  int n = gd_put_string(D, "data", "some string");
  int error = gd_error(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(n,0);
  CHECKI(error, GD_E_PROTECTED);

  return r;
}
