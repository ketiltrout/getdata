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
  const char* filedir = __FILE__ "dirfile";
  const char* format = __FILE__ "dirfile/format";
  const char* table = __FILE__ "dirfile/table";
  const char* format_data = "linterp LINTERP data ./table\n";
  int8_t c[8], d;
  int fd, i;
  struct stat buf;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = 40 + i;

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

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (n != 0)
    return 1;

  return (error != GD_E_BAD_CODE);
}
