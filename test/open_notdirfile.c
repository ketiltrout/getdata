/* Opening an non-dirfile should fail cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  int r = 0;

  mkdir(filedir, 0777);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);

  rmdir(filedir);
  int error = gd_error(D);
  CHECKI(error, GD_E_OPEN);
  gd_discard(D);

  return r;
}
