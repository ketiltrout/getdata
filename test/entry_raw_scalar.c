/* Try to read RAW entry */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "const CONST UINT32 8\ndata RAW UINT8 const\n";
  int fd, r = 0;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_entry_t E;

  int n = get_entry(D, "data", &E);
  int error = get_error(D);

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  if (error != GD_E_OK) {
    fprintf(stderr, "error = %i\n", error);
    r = 1;
  }

  if (n) {
    fprintf(stderr, "n = %i\n", n);
    r = 1;
  }

  if (strcmp(E.field, "data")) {
    fprintf(stderr, "E.field = %s\n", E.field);
    r = 1;
  }

  if (E.field_type != GD_RAW_ENTRY) {
    fprintf(stderr, "E.field_type = %i\n", E.field_type);
    r = 1;
  }

  if (strcmp(E.scalar[0], "const")) {
    fprintf(stderr, "E.scalar[0] = %s\n", E.scalar[0]);
    r = 1;
  }

  if (E.spf != 8) {
    fprintf(stderr, "E.spf = %i\n", E.spf);
    r = 1;
  }

  if (E.data_type != GD_UINT8) {
    fprintf(stderr, "E.data_type = %i\n", E.data_type);
    r = 1;
  }

  return r;
}
