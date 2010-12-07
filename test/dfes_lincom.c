/* Try to free the strings from a LINCOM entry */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "data LINCOM 2 in1 2 1 in2 2 1\n";
  int fd;
  DIRFILE *D;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  gd_entry(D, "data", &E);
  gd_free_entry_strings(&E);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return 0;
}
