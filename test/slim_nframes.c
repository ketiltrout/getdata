/* Retreiving the number of frames should succeed cleanly */
#include "../src/config.h"
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* slimdata = __TEST__ "dirfile/data.slm";
  const char* format_data = "data RAW UINT16 1\n";
  char command[4096];
  uint16_t data_data[256];
  int i, error, r = 0;
  size_t n;
  DIRFILE *D;

  mkdir(filedir, 0777);

  for (i = 0; i < 256; ++i)
    data_data[i] = (uint16_t)i;

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, data_data, 256 * sizeof(uint16_t));
  close(i);

  /* compress */
  snprintf(command, 4096, "%s -k %s > /dev/null", SLIM, data);
  if (gd_system(command)) {
    perror("command");
    r = 1;
  }

#ifdef USE_SLIM
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
#else
  * D = gd_open(filedir, GD_RDONLY);
#endif
  n = gd_nframes(D);
  error = gd_error(D);
  gd_close(D);

  unlink(slimdata);
  unlink(format);
  rmdir(filedir);

#ifdef USE_SLIM
  CHECKI(error, 0);
  CHECKI(n,256);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
  CHECKI(n,0);
#endif

  return r;
}
