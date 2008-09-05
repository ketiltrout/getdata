/* Attempt to read LINTERP */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* table = __TEST__ "dirfile/table";
  const char* format_data = "linterp LINTERP data ./table\n";
  unsigned char c = 0;
  int fd, i;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  FILE* t = fopen(table, "wt");
  for (i = 0; i < 10; ++i)
    fprintf(t, "%i %i\n", i * 6, i * 12);
  fclose(t);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  int n = getdata(D, "linterp", 5, 0, 1, 0, GD_UINT8, &c);
  int error = D->error;

  dirfile_close(D);

  unlink(table);
  unlink(format);
  rmdir(filedir);

  if (n != 0)
    return 1;

  return (error != GD_E_BAD_CODE);
}
