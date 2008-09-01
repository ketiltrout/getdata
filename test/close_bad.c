/* Closing an invalid dirfile should succeed cleanly */
#include "../src/getdata.h"

int main(void)
{
  DIRFILE* D = dirfile_open("a non_existant dirfile", 0);

  return dirfile_close(D);
}
