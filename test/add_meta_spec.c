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

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT);
  dirfile_add_metaspec(D, "meta CONST UINT8 2", "INDEX");
  int error = get_error(D);

  /* check */
  int n = get_nfields(D);
  int m = get_nmetafields(D, "INDEX");

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (n != 1)
    return 1;

  if (m != 1)
    return 1;

  return (error != GD_E_OK);
}
