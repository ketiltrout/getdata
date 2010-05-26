/* Closing an invalid dirfile should succeed cleanly */
#include "test.h"

int main(void)
{
  DIRFILE* D = gd_open("a non_existant dirfile", 0);
  int r = 0;

  int ret = gd_close(D);

  CHECKI(ret, 0);
  return r;
}
