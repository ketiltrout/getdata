/* Opening a non-existent dirfile should fail cleanly */
#include "../src/getdata.h"

int main(void)
{
  DIRFILE* D = dirfile_open("a non_existant dirfile", 0);

  return D->error != GD_E_OPEN;
}
