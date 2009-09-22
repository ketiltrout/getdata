/* Add a MULTIPLY field */
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
  int r = 0;
  gd_entry_t e;

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add_multiply(D, "new", "in1", "in2", 0);
  int error = get_error(D);

  /* check */
  get_entry(D, "new", &e);
  if (get_error(D))
    r = 1;
  else {
    if (e.field_type != GD_MULTIPLY_ENTRY) {
      fprintf(stderr, "field_type = %i\n", e.field_type);
      r = 1;
    }
    if (strcmp(e.in_fields[0], "in1")) {
      fprintf(stderr, "in_field[0] = %s\n", e.in_fields[0]);
      r = 1;
    }
    if (strcmp(e.in_fields[1], "in2")) {
      fprintf(stderr, "in_field[1] = %s\n", e.in_fields[1]);
      r = 1;
    }
    if (e.fragment_index != 0) {
      fprintf(stderr, "fragment_index = %i\n", e.fragment_index);
      r = 1;
    }
    dirfile_free_entry_strings(&e);
  }

  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (r)
    return 1;

  return (error != GD_E_OK);
}
