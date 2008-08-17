/* Requesting the number of frames from a NULL dirfile shouldn't crash (If it
 * works here, it should work whenever NULL is passed for a DIRFILE pointer.) */
#include "../src/getdata.h"

int main(void)
{
  size_t n = get_n_frames(NULL);

  return 0;
}
