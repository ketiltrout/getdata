/* Attempt to write COMPLEX64 */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW COMPLEX64 8\n";
#ifdef GD_NO_C99_API
  float c[8][2], d[2];
  const float zero[] = {0, 0};
#else
  float complex c[8], d;
  const float complex zero = 0;
#endif
  struct stat buf;
  int fd, i, n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i) {
#ifdef GD_NO_C99_API
    c[i][0] = 40 + i;
    c[i][1] = i;
#else
    c[i] = 40 + i * (1 + _Complex_I);
#endif
  }

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, c);
  error = gd_error(D);

  gd_close(D);

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  }
  CHECKI(buf.st_size, 48 * 2 * sizeof(float));

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
#ifdef GD_NO_C99_API
  while (read(fd, d, 2 * sizeof(float)))
#else
  while (read(fd, &d, sizeof(float complex)))
#endif
  {
    if (i < 40 || i > 48) {
      CHECKCi(i,d,zero);
    } else {
#ifdef GD_NO_C99_API
      float v[] = {i, i - 40};
#else
      float complex v = i + _Complex_I * (i - 40);
#endif
      CHECKCi(i,d,v);
    }
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);

  return r;
}
