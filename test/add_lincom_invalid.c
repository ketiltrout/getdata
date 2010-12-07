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
  int error, r = 0;

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  const char* in_fields[2] = {"in1", "in2"};
  const double m[2] = {1, 0.3};
  const double b[2] = {0, 0.9};
  gd_add_lincom(D, "new", 2, in_fields, m, b, 0);
  error = gd_error(D);
  gd_close(D);

  CHECKI(error, GD_E_BAD_DIRFILE);

  return r;
}
