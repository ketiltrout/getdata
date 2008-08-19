/* Opening an dirfile with no read permission should fail cleanly */
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
  const char* format = __TEST__ "dirfile/format";

  mkdir(filedir, 0777);
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0000));

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  int error = D->error;
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  return (error != GD_E_OPEN);
}
