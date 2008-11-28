/* Attempt to delete a field */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "data RAW UINT8 8\n"
    "META data e CONST UINT8 1\n"
    "META data q CONST UINT8 2\n"
    "META data a CONST UINT8 3\n"
    "META data b CONST UINT8 4\n"
    "META data z CONST UINT8 5\n"
    "META data l CONST UINT8 6\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int ret = dirfile_delete(D, "data", 0);
  int error = get_error(D);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (error != GD_E_DELETE) {
    fprintf(stderr, "2=%i\n", error);
    return 1;
  }
  if (ret != -1) {
    fprintf(stderr, "4=%i\n", ret);
    return 1;
  }

  return 0;
}
