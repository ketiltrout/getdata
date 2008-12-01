/* Closing a dirfile should succeed cleanly */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";

  mkdir(filedir, 0777);
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0666));

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  dirfile_add_spec(D, "data RAW UINT8 1", 0);
  int ret = dirfile_discard(D);
  D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = get_nfields(D);
  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (ret != 0) {
    fprintf(stderr, "1=%i\n", ret);
    return 1;
  }
  if (n != 1) {
    fprintf(stderr, "1=%i\n", n);
    return 1;
  }

  return 0;
}
