/* Retreiving the number of frames should succeed cleanly */
#include "../src/getdata.h"
#include "../src/config.h"

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* slimdata = __TEST__ "dirfile/data.slm";
  const char* format_data = "data RAW UINT8 1\n";
  char command[4096];
  unsigned char data_data[256];
  int i;

  mkdir(filedir, 0777);

  for (i = 0; i < 256; ++i)
    data_data[i] = (unsigned char)i;

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, data_data, 256);
  close(i);

  /* compress */
  snprintf(command, 4096, "%s %s > /dev/null", SLIM, data);
  system(command);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  size_t n = get_nframes(D);
  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return !(n == 256);
}
