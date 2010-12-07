/* Test field modifying */
#define GD_C89_API
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW INT32 8\nphase PHASE data 1\n"
    "div RECIP data 230.\n";
  int32_t data_data[256];
  int fd, ret, error, n, r = 0;
  DIRFILE *D;
  double div[2] = {1093., 3290.};
  gd_entry_t E;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_crecip(D, "div", "phase", div);
  error = gd_error(D);
  n = gd_entry(D, "div", &E);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,0);
  CHECKI(ret,0);
  CHECKF(E.EN(recip,cdividend)[0], div[0]);
  CHECKF(E.EN(recip,cdividend)[1], div[1]);
  CHECKS(E.in_fields[0], "phase");
  gd_free_entry_strings(&E);

  return r;
}
