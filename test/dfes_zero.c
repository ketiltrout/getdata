/* Try to free the strings from a zeroed entry */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  gd_entry_t E;
  memset(&E, 0, sizeof(E));
  gd_free_entry_strings(&E);

  return 0;
}
