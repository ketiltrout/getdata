/* a short error string should still be NULL-terminated */
#include "test.h"

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  char string[1000] = "abc";
  int r = 0;
  DIRFILE *D;

  mkdir(filedir, 0777);
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0666));

  D = gd_open(filedir, GD_RDONLY);
  gd_error_string(D, string, 2);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(string[1], 0);
  
  return r;
}
