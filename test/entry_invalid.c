/* Try to read entry from an invalid DIRFILE */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  DIRFILE* D = gd_open("not a dirfile", GD_RDONLY);
  gd_entry_t E;
  int r = 0;

  int n = gd_entry(D, "data", &E);
  int error = gd_error(D);
  gd_close(D);

  CHECKI(error, GD_E_BAD_DIRFILE);
  CHECKI(n, -1);

  return r;
}
