/* Attempt to write UINT8 */
#include "../src/getdata.h"

#include <inttypes.h>
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
  const char* format_data = "data CONST UINT8 8\nPROTECT all\n";
  uint8_t d = 3;
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED);
  int n = put_constant(D, "data", GD_UINT8, &d);
  int error = get_error(D);

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (error != GD_E_PROTECTED) {
    fprintf(stderr, "error=%i\n", error);
    return 1;
  }
  if (n != -1) {
    fprintf(stderr, "n=%i\n", n);
    return 1;
  }

  return 0;
}
