/* Reference to a non-existent dirfile from the legacy API should fail cleanly
 */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
#ifndef GD_LEGACY_API
  return 77; /* skipped */
#else
  int error, r = 0;
  size_t n = GetNFrames("no such dirfile", &error, NULL);

  CHECKI(n,0);
  CHECKI(error, GD_E_OPEN);

  return r;
#endif
}
