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
  const char* gzipdata = __TEST__ "dirfile/data.gz";
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
  snprintf(command, 4096, "%s -f %s > /dev/null", GZIP, data);
  system(command);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  size_t n = get_nframes(D);
  dirfile_close(D);

  unlink(gzipdata);
  unlink(format);
  rmdir(filedir);

  return !(n == 256);
}
