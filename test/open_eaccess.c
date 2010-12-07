/* Opening an dirfile with no read permission should fail cleanly */
#include "test.h"

#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  int fd, error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0000));

  /* ensure filesystem honours access */
  if ((fd = open(format, O_RDONLY)) >= 0 || errno != EACCES) {
    if (fd >= 0)
      close(fd);
    unlink(format);
    rmdir(filedir);
    return 77;
  }

  D = gd_open(filedir, GD_RDONLY);
  error = gd_error(D);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OPEN);
  return r;
}
