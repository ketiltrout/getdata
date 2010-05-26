/* Requesting the number of fields from an invalid dirfile should fail cleanly */
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
  unsigned int n = gd_get_nmvectors(D, "raw1");
  int error = gd_error(D);
  gd_close(D);

  if (n != 0)
    return 1;

  return (error != GD_E_BAD_DIRFILE);
}
