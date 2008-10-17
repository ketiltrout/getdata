/* Getting data from an invalid dirfile should fail cleanly */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";

  gd_entry_t E;
  E.field =  "data";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.spf = 2;
  E.data_type = GD_UINT8;

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  dirfile_add(D, &E);
  int error = get_error(D);
  dirfile_close(D);

  return (error != GD_E_BAD_DIRFILE);
}
