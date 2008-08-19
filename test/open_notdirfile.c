/* Opening an non-dirfile should fail cleanly */
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

  mkdir(filedir, 0777);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);

  rmdir(filedir);

  return (D->error != GD_E_OPEN);
}
