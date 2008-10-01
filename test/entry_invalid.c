/* Try to read entry from an invalid DIRFILE */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  DIRFILE* D = dirfile_open("not a dirfile", GD_RDONLY);
  gd_entry_t E;

  int n = get_entry(D, "data", &E);
  int error = get_error(D);
  dirfile_close(D);

  if (error != GD_E_BAD_DIRFILE)
    return 1;
  if (n == 0)
    return 1;

  return 0;
}
