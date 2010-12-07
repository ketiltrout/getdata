/* Attempt to write LINTERP */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* table = __TEST__ "dirfile/table";
  const char* format_data = "linterp LINTERP data ./table\ndata RAW INT8 8\n";
  int8_t c[8];
  int fd, i, n, error, r = 0;
  DIRFILE *D;
  FILE *t;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (int8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  t = fopen(table, "wt");
  for (i = 0; i < 10; ++i)
    fprintf(t, "%i %i\n", i * 6, (i - 5) * (i - 5));
  fclose(t);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  n = gd_putdata(D, "linterp", 5, 0, 1, 0, GD_INT8, c);
  error = gd_error(D);

  gd_close(D);

  unlink(table);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n,0);
  CHECKI(error, GD_E_DOMAIN);

  return r;
}
