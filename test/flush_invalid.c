/* Writing data to an invalid dirfile should fail cleanly */
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
  char c[8];

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  dirfile_flush(D, NULL);
  int error = D->error;
  dirfile_close(D);

  return (error != GD_E_BAD_DIRFILE);
}
