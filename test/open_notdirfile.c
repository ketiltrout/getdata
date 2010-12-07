/* Opening an non-dirfile should fail cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  int error, r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);

  D = gd_open(filedir, GD_RDONLY);

  rmdir(filedir);
  error = gd_error(D);
  CHECKI(error, GD_E_OPEN);
  gd_discard(D);

  return r;
}
