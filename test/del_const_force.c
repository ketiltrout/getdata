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
  const char* format_data = "data CONST UINT8 13\n"
    "raw RAW UINT8 data\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int ret = dirfile_delete(D, "data", GD_DEL_FORCE);
  int error1 = get_error(D);
  unsigned int spf = get_spf(D, "raw");
  int error2 = get_error(D);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (error1 != GD_E_OK) {
    fprintf(stderr, "2=%i\n", error1);
    return 1;
  }
  if (error2 != GD_E_BAD_SCALAR) {
    fprintf(stderr, "2=%i\n", error2);
    return 1;
  }
  if (spf != 0) {
    fprintf(stderr, "3=%i\n", spf);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "4=%i\n", ret);
    return 1;
  }

  return 0;
}
