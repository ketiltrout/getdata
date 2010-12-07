/* Writing data to an invalid dirfile should fail cleanly */
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

  DIRFILE* D = gd_open(filedir, GD_RDWR);
  gd_flush(D, NULL);
  error = gd_error(D);
  gd_close(D);

  CHECKI(error, GD_E_BAD_DIRFILE);

  return r;
}
