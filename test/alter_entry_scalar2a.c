/* Test field modifying */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "data BIT in c1 3\nc1 CONST INT64 3\n"
    "c2 CONST INT64 11\n";
  int fd, ret, error, n, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_entry(D, "data", &E);
  free(E.scalar[0]);
  E.scalar[0] = "c2";
  ret = gd_alter_entry(D, "data", &E, 0);
  error = gd_error(D);

  E.scalar[0] = NULL;
  gd_free_entry_strings(&E);
  n = gd_entry(D, "data", &E);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 0);
  CHECKI(ret, 0);
  CHECKI(E.EN(bit,bitnum), 11);
  gd_free_entry_strings(&E);

  return r;
}
