/* Truncating a read-only dirfile should fail cleanly */
#include "test.h"

#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  int error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0666));
  chmod(filedir, 0555);

  /* ensure filesystem honours read-onlyness */
  if (!unlink(format) || errno != EACCES) {
    rmdir(filedir);
    return 77;
  }

  D = gd_open(filedir, GD_RDWR | GD_TRUNC);
  error = gd_error(D);
  gd_discard(D);

  chmod(filedir, 0777);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_TRUNC);
  return r;
}
