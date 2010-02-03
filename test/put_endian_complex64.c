/* Attempt to write COMPLEX64 with the opposite endianness */
#include "../src/getdata.h"

#include <complex.h>
#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
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
  float complex c = 4. / (3. + _Complex_I);
  int fd, i, r = 0;
  const int big_endian = BigEndian();
  union {
    float complex f;
    char b[8];
  } u;
  char x[8];

  u.f = c;
  for (i = 0; i < 4; ++i)
    x[3 - i] = u.b[i];
  for (; i < 8; ++i)
    x[11 - i] = u.b[i];

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW COMPLEX64 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int n = putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, &c);
  int error = get_error(D);

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(float complex), SEEK_SET);
  read(fd, &u.f, sizeof(float complex));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (n != 1) {
    fprintf(stderr, "n=%i\n", n);
    r = 1;
  }
  if (error) {
    fprintf(stderr, "error=%i\n", error);
    r = 1;
  }
  
  for (i = 0; i < 8; ++i)
    if (x[i] != u.b[i]) {
      fprintf(stderr, "x[%i] = %2x, u.b[%i] = %2x (%g;%g)\n", i, x[i], i,
          u.b[i], creal(u.f), cimag(u.f));
      r = 1;
    }

  return r;
}
