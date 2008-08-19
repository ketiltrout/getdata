/* Attempting to resove a recursively defined field should fail cleanly */
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
    "in1 RAW UINT8 11\n"
    "lincom LINCOM 2 lincom 1 0 in1 1 0\n";
  int fd;
  unsigned char c[8];

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  int n = getdata(D, "lincom", 5, 0, 1, 0, GD_UINT8, c);
  int error = D->error;
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (n != 0)
    return 1;

  return (error != GD_E_RECURSE_LEVEL);
}
