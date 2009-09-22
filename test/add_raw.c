/* Add a RAW field */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  gd_entry_t e;
  int r = 0;

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add_raw(D, "data", GD_UINT8, 2, 0);
  int error = get_error(D);

  /* check */
  get_entry(D, "data", &e);
  if (get_error(D))
    r = 1;
  else {
    if (e.field_type != GD_RAW_ENTRY) {
      fprintf(stderr, "field_type = %i\n", e.field_type);
      r = 1;
    }
    if (e.fragment_index != 0) {
      fprintf(stderr, "fragment_index = %i\n", e.fragment_index);
      r = 1;
    }
    if (e.spf != 2) {
      fprintf(stderr, "spf = %i\n", e.spf);
      r = 1;
    }
    if (e.data_type != GD_UINT8) {
      fprintf(stderr, "data_type = %i\n", e.data_type);
      r = 1;
    }
    dirfile_free_entry_strings(&e);
  }

  dirfile_close(D);

  if (unlink(data))
    return 1;
  unlink(format);
  rmdir(filedir);

  if (r)
    return 1;

  return (error != GD_E_OK);
}
