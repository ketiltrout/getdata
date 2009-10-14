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
  const char* format_data = "data BIT in 3 c1\nc1 CONST INT64 3\n";
  int fd, r = 0;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  get_entry(D, "data", &E);
  free(E.scalar[1]);
  E.scalar[1] = NULL;
  E.numbits = 11;
  int ret = dirfile_alter_entry(D, "data", &E, 0);
  int error = get_error(D);

  dirfile_free_entry_strings(&E);
  int n = get_entry(D, "data", &E);

  dirfile_close(D);

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
  if (E.numbits != 11) {
    fprintf(stderr, "E.numbits=%i\n", E.numbits);
    r = 1;
  }
  if (E.scalar[1] != NULL) {
    fprintf(stderr, "E.scalar[1]=%p\n", E.scalar[1]);
    r = 1;
  }

  return r;
}
