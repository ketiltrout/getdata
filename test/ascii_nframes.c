/* Retreiving the number of frames should succeed cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data.txt";
  const char* format_data = "data RAW UINT8 1\n";
  int i, r = 0;
  FILE* stream;
  size_t n, m;
  DIRFILE *D;

  mkdir(filedir, 0777);

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  stream = fopen(data, "w" FOPEN_TEXT);
  for (i = 0; i < 256; ++i)
    fprintf(stream, "%i\n", i);
  fclose(stream);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_nframes(D);
  m = gd_nframes(D);
  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n, 256);
  CHECKI(m, 256);

  return r;
}
