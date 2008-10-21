/* Add a RAW field */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  uint8_t val = 0;

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add_const(D, "data", GD_UINT8, GD_UINT8, &val, 0);
  val = 23;
  put_constant(D, "data", GD_UINT8, &val);
  int error = get_error(D);
  dirfile_close(D);

  /* check */
  val = 0;
  D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  get_constant(D, "data", GD_UINT8, &val);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (val != 23) {
    fprintf(stderr, "val=%i\n", val);
    return 1;
  }

  return (error != GD_E_OK);
}
