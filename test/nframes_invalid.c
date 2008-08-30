/* Requesting the number of frames from an invalid dirfile should fail cleanly */
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
  size_t n = get_nframes(D);
  int error = D->error;
  dirfile_close(D);

  if (n != 0)
    return 1;

  return (error != GD_E_BAD_DIRFILE);
}
