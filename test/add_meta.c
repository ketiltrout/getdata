/* Add a dirfile field */
#include "../src/getdata.h"

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
  const char* data = __TEST__ "dirfile/data";

  gd_entry_t E;
  E.field = "data";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.spf = 2;
  E.data_type = GD_UINT8;

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_CREAT);
  dirfile_add(D, &E);
  E.field_type = GD_CONST_ENTRY;
  E.const_type = GD_UINT8;
  dirfile_add_meta(D, &E, "data");
  int error = get_error(D);

  /* check */
  int n = get_nfields(D);
  int m = get_nmetafields(D, "data");

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (n != 2)
    return 1;
  if (m != 1)
    return 1;

  return (error != GD_E_OK);
}
