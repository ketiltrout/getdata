/* Attempt to read COMPLEX128 with the opposite endianness */
#include "../src/getdata.h"

#include <complex.h>
#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

static int BigEndian(void)
{
  union {
    long int li;
    char ch[sizeof(long int)];
  } un;
  un.li = 1;
  return (un.ch[sizeof(long int) - 1] == 1);
}

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  char format_data[1000];
  union {
    double complex f;
    char b[16];
  } u;
  char x[16];
  u.f = 0;
  double complex data_data[128];
  int fd, i, j, r = 0;
  const int big_endian = BigEndian();

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW COMPLEX128 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  data_data[0] = 1.5;
  for (j = 1; j < 128; ++j) {
    data_data[j] =
      (double complex)(data_data[j - 1] * (0.2 + _Complex_I * 0.3));
    if (j == 5) {
      u.f = data_data[j];

      for (i = 0; i < 8; ++i)
        x[7 - i] = u.b[i];
      for (; i < 16; ++i)
        x[23 - i] = u.b[i];
    }
  }

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 128 * sizeof(double complex));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  int n = getdata(D, "data", 5, 0, 1, 0, GD_COMPLEX128, &u.f);
  int error = get_error(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "error = %i\n", error);
    r = 1;
  }
  if (n != 1) {
    fprintf(stderr, "n = %i\n", n);
    r = 1;
  }

  for (i = 0; i < 16; ++i)
    if (x[i] != u.b[i]) {
      fprintf(stderr, "x[%i] = %2x, u.b[%i] = %2x (%g;%g)\n", i, x[i], i,
          u.b[i], creal(u.f), cimag(u.f));
      r = 1;
    }

  return r;
}
