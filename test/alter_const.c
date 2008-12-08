/* Test field modifying */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "const CONST FLOAT32 8.3\n";
  int fd;
  double d;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_alter_const(D, "const", GD_UINT8);
  int error = get_error(D);
  int n = get_constant(D, "const", GD_FLOAT64, &d);

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (n != 0) {
    fprintf(stderr, "2=%lli\n", (long long)n);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "3=%i\n", ret);
    return 1;
  }
  if (d != 8) {
    fprintf(stderr, "4=%g\n", d);
    return 1;
  }

  return 0;
}
