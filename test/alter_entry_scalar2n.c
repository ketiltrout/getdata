/* Test field modifying */
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
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\nconst CONST INT64 11\n";
  unsigned char data_data[256];
  int fd, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  get_entry(D, "data", &E);
  E.scalar[0] = "const";
  int ret = dirfile_alter_entry(D, "data", &E, 0);
  int error = get_error(D);

  E.scalar[0] = NULL;
  dirfile_free_entry_strings(&E);
  int n = get_entry(D, "data", &E);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    r = 1;
  }
  if (n != 0) {
    fprintf(stderr, "n=%i\n", n);
    r = 1;
  }
  if (ret != 0) {
    fprintf(stderr, "ret=%i\n", ret);
    r = 1;
  }
  if (E.spf != 11) {
    fprintf(stderr, "E.spf=%i\n", E.spf);
    r = 1;
  }

  return r;
}
