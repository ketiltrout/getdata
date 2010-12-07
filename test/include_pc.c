/* Test include */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int saw_callback = 0;
int callback(gd_parser_data_t *pdata __attribute__ (( unused )),
    void *extra __attribute__ (( unused )))
{
  saw_callback++;

  return GD_SYNTAX_IGNORE;
}

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format_data = "data PAW UINT8 1\n";
  const char* format1_data = "data ROW UINT8 11\n";
  int fd, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  D = gd_cbopen(filedir, GD_RDWR, callback, NULL);
  gd_parser_callback(D, NULL, NULL);
  gd_include(D, "format1", 0, 0);
  error = gd_error(D);
  gd_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_FORMAT);
  CHECKI(saw_callback, 1);

  return r;
}
