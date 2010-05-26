/* Getting data from an invalid dirfile should fail cleanly */
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

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  gd_add_bit(D, "new", "input", 1, 1, 0);
  int error = gd_error(D);
  int r = 0;
  gd_close(D);

  CHECKI(error,GD_E_BAD_DIRFILE);

  return r;
}
