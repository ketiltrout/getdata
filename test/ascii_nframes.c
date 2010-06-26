/* Retreiving the number of frames should succeed cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data.txt";
  const char* format_data = "data RAW UINT8 1\n";
  int i, r = 0;
  FILE* stream;

  mkdir(filedir, 0777);

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  stream = fopen(data, "w" FOPEN_TEXT);
  for (i = 0; i < 256; ++i)
    fprintf(stream, "%i\n", i);
  fclose(stream);

  DIRFILE* D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  size_t n = gd_get_nframes(D);
  size_t m = gd_get_nframes(D);
  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n, 256);
  CHECKI(m, 256);

  return r;
}
