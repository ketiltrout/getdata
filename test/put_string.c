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
  char string[1024] = "";

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add_string(D, "data", "some string", 0);
  put_string(D, "data", "some other string");
  int error = get_error(D);
  dirfile_close(D);

  /* check */
  D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  get_string(D, "data", 1023, string);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (strcmp(string, "some other string")) {
    fprintf(stderr, "string=%s\n", string);
    return 1;
  }

  return (error != GD_E_OK);
}
