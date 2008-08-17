/* Reference to a non-existent dirfile from the legacy API should fail cleanly
 */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
#ifndef GETDATA_LEGACY_API
  return 77; /* skipped */
#else
  int error;
  size_t n = GetNFrames("no such dirfile", &error, NULL);

  if (n != 0)
    return 1;
  return !(error == GD_E_OPEN);
#endif
}
