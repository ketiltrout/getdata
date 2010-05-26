/* Writing data to an invalid dirfile should fail cleanly */
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

  DIRFILE* D = gd_open(filedir, GD_RDWR);
  gd_flush(D, NULL);
  int error = gd_error(D);
  gd_close(D);

  return (error != GD_E_BAD_DIRFILE);
}
