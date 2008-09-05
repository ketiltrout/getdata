/* Attempt to write LINTERP */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* table = __TEST__ "dirfile/table";
  const char* format_data = "linterp LINTERP data ./table\ndata RAW INT8 8\n";
  int8_t c[8], d;
  int fd, i;
  struct stat buf;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (int8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  FILE* t = fopen(table, "wt");
  for (i = 0; i < 10; ++i)
    fprintf(t, "%i %i\n", i * 6, i * 3);
  fclose(t);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int n = putdata(D, "linterp", 5, 0, 1, 0, GD_INT8, c);
  int error = D->error;

  dirfile_close(D);

  if (stat(data, &buf))
    return 1;
  if (buf.st_size != 48 * sizeof(int8_t))
    return 1;

  fd = open(data, O_RDONLY);
  i = 0;
  while (read(fd, &d, sizeof(int8_t))) {
    if (i < 40 || i > 48) {
      if (d != 0)
        return 1;
    } else if (d != i * 2)
      return 1;
    i++;
  }
  close(fd);

  unlink(table);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;
  if (n != 8)
    return 1;

  return 0;
}
