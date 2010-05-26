/* Passing NULL to dirfile_close should succeed cleanly */
#include "test.h"

int main(void)
{
  int r = 0;
  int ret = gd_close(NULL);

  CHECKI(ret, 0);

  return r;
}
