/* Retreiving the number of fields of a field should succeed cleanly */
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
  const char* format_data =
    "data1 RAW UINT8 1\n"
    "data2 RAW UINT8 1\n"
    "data3 RAW UINT8 1\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  unsigned int nfields = get_nfields(D);
  int error = D->error;
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;

  return (nfields != 3);
}
