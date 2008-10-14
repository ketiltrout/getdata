/* Add a RAW field */
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
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT);
  dirfile_add_spec(D, 0, "INCLUDE RAW UINT8 2");
  int error = get_error(D);

  /* check */
  int n = get_nfields(D);

  dirfile_close(D);

  if (!unlink(data))
    return 1;
  unlink(format);
  rmdir(filedir);

  if (n != 1)
    return 1;

  return (error != GD_E_FORMAT);
}
