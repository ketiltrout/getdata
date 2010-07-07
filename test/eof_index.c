#include "test.h"

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
  const char* data = __TEST__ "dirfile/data";
  const char* format_data =
    "data RAW UINT16 1\n"
    "mult1 MULTIPLY data INDEX\n"
    "mult2 MULTIPLY INDEX INDEX\n"
    "mult3 MULTIPLY INDEX data\n";
  int fd, r = 0;
  const size_t len = strlen(data);

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  off_t eof_INDEX = gd_get_eof(D, "INDEX");
  int error0 = gd_error(D);
  off_t eof_mult1 = gd_get_eof(D, "mult1");
  int error1 = gd_error(D);
  off_t eof_mult2 = gd_get_eof(D, "mult2");
  int error2 = gd_error(D);
  off_t eof_mult3 = gd_get_eof(D, "mult3");
  int error3 = gd_error(D);
  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error0, GD_E_BAD_FIELD_TYPE);
  CHECKI(eof_INDEX, -1);
  CHECKI(error1, GD_E_OK);
  CHECKI(eof_mult1, (int)len / 2);
  CHECKI(error2, GD_E_BAD_FIELD_TYPE);
  CHECKI(eof_mult2, -1);
  CHECKI(error3, GD_E_OK);
  CHECKI(eof_mult3, (int)len / 2);

  return r;
}
