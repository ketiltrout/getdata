/* Truncating a dirfile should succeed cleanly */
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
  int fd;
  struct stat buf;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format, strlen(format));
  close(fd);

  close(open(data, O_CREAT | O_EXCL | O_WRONLY, 0666));

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_TRUNC | GD_VERBOSE);
  int error = get_error(D);
  dirfile_close(D);

  if (!unlink(data))
    return 1;

  if (stat(format, &buf))
    return 1;

  if (buf.st_size > 0)
    return 1;

  unlink(format);
  rmdir(filedir);

  return (error != GD_E_OK);
}
