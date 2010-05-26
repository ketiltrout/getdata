/* Attempt to delete a field */
#include "test.h"

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
  const char* format_data = "data CONST UINT8 13\n"
    "raw RAW UINT8 data\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR);
  int ret = gd_delete(D, "data", GD_DEL_DEREF);
  int error = gd_error(D);
  unsigned int spf = gd_get_spf(D, "raw");
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKU(spf, 13);
  CHECKI(ret, 0);

  return r;
}
