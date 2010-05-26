/* Opening a non-existent dirfile should fail cleanly */
#include "test.h"

#include <string.h>

int main(void)
{
  char string[1000];
  int r = 0;

  DIRFILE* D = gd_open("a non_existant dirfile", 0);

  gd_error_string(D, string, 1000);
  int error = gd_error(D);
  gd_close(D);

  CHECKI(error, GD_E_OPEN);
  CHECKS(string, "Dirfile does not exist: a non_existant dirfile");

  return r;
}
