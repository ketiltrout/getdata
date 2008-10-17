/* Getting data from an invalid dirfile should fail cleanly */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  const char* in_fields[2] = {"in1", "in2"};
  const double m[2] = {1, 0.3};
  const double b[2] = {0, 0.9};
  dirfile_madd_lincom(D, "new", "meta", 2, in_fields, m, b);
  int error = get_error(D);
  dirfile_close(D);

  return (error != GD_E_BAD_DIRFILE);
}
