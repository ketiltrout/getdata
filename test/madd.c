/* Add a dirfile field */
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
  const char* data = __TEST__ "dirfile/data";
  int r = 0;
  unsigned char val;
  gd_entry_t e;

  gd_entry_t E;
  memset(&E, 0, sizeof(E));
  E.field = "data";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.spf = 2;
  E.data_type = GD_UINT8;

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  dirfile_add(D, &E);
  E.field_type = GD_CONST_ENTRY;
  E.const_type = GD_UINT8;
  dirfile_madd(D, &E, "data");
  int error = get_error(D);

  /* check */
  int n = get_nfields(D);
  get_entry(D, "data/data", &e);
  if (get_error(D))
    r = 1;
  else {
    if (e.field_type != GD_CONST_ENTRY) {
      fprintf(stderr, "field_type = %i\n", e.field_type);
      r = 1;
    }
    if (e.fragment_index != 0) {
      fprintf(stderr, "fragment_index = %i\n", e.fragment_index);
      r = 1;
    }
    if (e.const_type != GD_UINT8) {
      fprintf(stderr, "const_type = %i\n", e.const_type);
      r = 1;
    }
    get_constant(D, "data/data", GD_UINT8, &val);
    if (val != 0) {
      fprintf(stderr, "val = %i\n", val);
      r = 1;
    }
    dirfile_free_entry_strings(&e);
  }


  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (n != 2)
    return 1;
  if (r)
    return 1;

  return (error != GD_E_OK);
}
