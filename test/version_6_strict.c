/* Check Standards Version 6 strictness */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int cb(gd_parser_data_t* pdata, void* ll)
{
  ((int*)ll)[pdata->linenum - 1] = 1;
  return GD_SYNTAX_IGNORE;
}

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/ar";
  const char* format_data =
    "/VERSION 6\n"
    "X<r RAW UINT8 8\n"
    "X#r RAW COMPLEX128 8\n"
    "X.r RAW UINT8 8\n"
    "Xr POLYNOM INDEX 8 3 1 2\n"
    "/ENCODING none\n"
    "/REFERENCE ar\n"
    "/PROTECT none\n"
    "c CONST UINT8 3\n"
    "ar RAW UINT8 8\n";
  uint16_t c[8];
  int ll[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  uint16_t d[8];
  unsigned char data_data[256];
  int fd, i, r = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = dirfile_cbopen(filedir, GD_RDONLY | GD_PEDANTIC, cb, ll);
  int n = getdata(D, "ar", 5, 0, 1, 0, GD_UINT16, c);
  int error = get_error(D);

  int m = getdata(D, "FILEFRAM", 5, 0, 8, 0, GD_UINT16, d);
  int error2 = get_error(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "error = %i\n", error);
    r = 1;
  }

  if (error2 != GD_E_BAD_CODE) {
    fprintf(stderr, "error2 = %i\n", error2);
    r = 1;
  }

  for (i = 0; i < 10; ++i) {
    if ((i == 1 || i == 2 || i == 3 || i == 4) && !ll[i]) {
      fprintf(stderr, "ll[%i] = %i\n", i, ll[i]);
      r = 1;
    } else if ((i < 1 || i > 4) && ll[i]) {
      fprintf(stderr, "ll[%i] = %i\n", i, ll[i]);
      r = 1;
    }
  }

  if (n != 8) {
    fprintf(stderr, "n = %i\n", n);
    r = 1;
  }

  for (i = 0; i < 8; ++i)
    if (c[i] != 40 + i) {
      fprintf(stderr, "c[%i] = %i\n", i, c[i]);
      r = 1;
    }

  if (m != 0) {
    fprintf(stderr, "m = %i\n", m);
    r = 1;
  }

  return r;
}
