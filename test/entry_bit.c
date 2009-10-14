/* Try to read BIT entry */
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
  const char* format_data = "data BIT in1 3 4\n";
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
  if (E.field_type != GD_BIT_ENTRY) {
    fprintf(stderr, "E.field_type = %i\n", E.field_type);
    r = 1;
  }
  if (strcmp(E.in_fields[0], "in1")) {
    fprintf(stderr, "E.in_fields[0] = %s\n", E.in_fields[0]);
    r = 1;
  }
  if (E.bitnum != 3) {
    fprintf(stderr, "E.bitnum = %i\n", E.bitnum);
    r = 1;
  }
  if (E.numbits != 4) {
    fprintf(stderr, "E.numbits = %i\n", E.numbits);
    r = 1;
  }
  if (E.scalar[0] != NULL) {
    fprintf(stderr, "E.scalar[0] = %s\n", E.scalar[0]);
    r = 1;
  }
  if (E.scalar[1] != NULL) {
    fprintf(stderr, "E.scalar[1] = %s\n", E.scalar[1]);
    r = 1;
  }

  return r;
}
