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

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  dirfile_madd_multiply(D, "new", "meta", "in1", "in2");
  int error = get_error(D);
  dirfile_close(D);

  return (error != GD_E_BAD_DIRFILE);
}
