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
  const char* data = __TEST__ "dirfile/data";
  const char* table = __TEST__ "dirfile/table";
  const char* format_data = "linterp LINTERP data ./table\ndata RAW UINT8 1\n";
  unsigned char c = 0;
  unsigned char data_data[64];
  int fd, i;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 64; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 64);
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
  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;
  if (n != 1)
    return 1;
  if (c != 10)
    return 1;

  return 0;
}
