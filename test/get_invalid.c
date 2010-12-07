/* Getting data from an invalid dirfile should fail cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  char c[8];
  int r = 0;

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  size_t n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  int error = gd_error(D);
  gd_close(D);

  CHECKI(n, 0);
  CHECKI(error, GD_E_BAD_DIRFILE);

  return r;
}
