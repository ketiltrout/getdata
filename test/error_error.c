/* Opening a non-existent dirfile should fail cleanly */
#include "../src/getdata.h"

#include <string.h>

int main(void)
{
  char string[1000];

  DIRFILE* D = dirfile_open("a non_existant dirfile", 0);

  get_error_string(D, string, 1000);

  if (strstr(string, "a non_existant dirfile") == NULL)
      return 1;

  return get_error(D) != GD_E_OPEN;
}
