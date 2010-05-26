/* Opening a non-existent dirfile should fail cleanly */
#include "test.h"

int main(void)
{
  DIRFILE* D = gd_open("a non_existant dirfile", 0);
  int r = 0;

  int error = gd_error(D);
  CHECKI(error, GD_E_OPEN);

  return r;
}
