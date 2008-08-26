/* Requesting the number of fields from an invalid dirfile should fail cleanly */
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

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  unsigned int n = get_n_frames(D);
  int error = D->error;
  dirfile_close(D);

  if (n != 0)
    return 1;

  return (error != GD_E_BAD_DIRFILE);
}
