/* Add a dirfile field to a read-only dirfile*/
#include "test.h"

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
  const char* format_data = "data RAW UINT8 8\n";
  int fd, r = 0;

  gd_entry_t E;
  E.field =  "new";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.u.raw.spf = 2;
  E.u.raw.type = GD_UINT8;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDONLY);
  gd_add(D, &E);
  int error = gd_error(D);

  /* check */
  int n = gd_nfields(D);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(n, 2);
  CHECKI(error, GD_E_ACCMODE);

  return r;
}
