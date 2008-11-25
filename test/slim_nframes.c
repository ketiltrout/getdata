/* Retreiving the number of frames should succeed cleanly */
#include "../src/config.h"
#include "../src/getdata.h"

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
  int i;

  mkdir(filedir, 0777);

  for (i = 0; i < 256; ++i)
    data_data[i] = (uint16_t)i;

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, data_data, 256 * sizeof(uint16_t));
  close(i);

  /* compress */
  snprintf(command, 4096, "%s -k %s > /dev/null", SLIM, data);
  system(command);

#ifdef USE_SLIM
  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
#else
  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
#endif
  size_t n = get_nframes(D);
  int error = get_error(D);
  dirfile_close(D);

  unlink(slimdata);
  unlink(format);
  rmdir(filedir);

#ifdef USE_SLIM
  if (error)
    return 1;
  if (n != 256)
    return 1;
#else
  if (error != GD_E_UNSUPPORTED)
    return 1;
  if (n != 0)
    return 1;
#endif

  return 0;
}
