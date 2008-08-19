/* Truncating a read-only dirfile should fail cleanly */
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
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0666));
  chmod(filedir, 0555);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_TRUNC);

  chmod(filedir, 0777);
  unlink(format);
  rmdir(filedir);

  return (D->error != GD_E_TRUNC);
}
