/* a short error string should still be NULL-terminated */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  char string[1000] = "";

  mkdir(filedir, 0777);
  close(open(format, O_CREAT | O_EXCL | O_WRONLY, 0666));

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  get_error_string(D, string, 2);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (string[0] == 0)
    return 1;
  if (string[1] != 0)
    return 1;
  
  return 0;
}
