/* Requesting the number of fields from an invalid dirfile should fail cleanly */
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
  const char* fl = get_constant_values(D, GD_UINT8);
  int error = get_error(D);
  dirfile_close(D);

  if (fl != NULL)
    return 1;

  return (error != GD_E_BAD_DIRFILE);
}
