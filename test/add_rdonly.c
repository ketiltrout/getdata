/* Add a dirfile field to a read-only dirfile*/
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
  const char* format_data = "data RAW UINT8 8\n";
  int fd;

  gd_entry_t E;
  E.field =  "new";
  E.field_type = GD_RAW_ENTRY;
  E.format_file = 0;
  E.spf = 2;
  E.data_type = GD_UINT8;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  dirfile_add(D, &E);
  int error = D->error;

  /* check */
  int n = get_nfields(D);

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (n != 1)
    return 1;

  return (error != GD_E_ACCMODE);
}
