/* Attempt to read a non-existant field */
#include "../src/getdata.h"

#include <stdio.h>
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
  const char* format_data = "\n";
  unsigned char c = 0;
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  int n = getdata(D, "no-such-field", 5, 0, 1, 0, GD_UINT8, &c);

  int error = get_error(D);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (n != 0) {
    fprintf(stderr, "n = %i\n", n);
    r = 1;
  }

  if (error != GD_E_BAD_CODE) {
    fprintf(stderr, "error = %i\n", error);
    r = 1;
  }

  return r;
}
