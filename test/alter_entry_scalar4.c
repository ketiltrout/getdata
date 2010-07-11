/* Test field modifying */
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
  const char* format_data = "data BIT in 3 c1\nc1 CONST INT64 3\n";
  int fd, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_entry(D, "data", &E);
  free(E.scalar[1]);
  E.scalar[1] = NULL;
  E.numbits = 11;
  int ret = gd_alter_entry(D, "data", &E, 0);
  int error = gd_error(D);

  gd_free_entry_strings(&E);
  int n = gd_entry(D, "data", &E);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 0);
  CHECKI(ret, 0);
  CHECKI(E.numbits, 11);
  CHECKP(E.scalar[1]);

  return r;
}
