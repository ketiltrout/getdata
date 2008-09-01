/* Passing NULL to dirfile_close should succeed cleanly */
#include "../src/getdata.h"

int main(void)
{
  return dirfile_close(NULL);
}
