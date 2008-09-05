/* Attempt to write FLOAT64 with the opposite endianness */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
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
  double c = 4. / 3.;
  int fd, i;
  const int big_endian = BigEndian();
  union {
    double f;
    char b[8];
  } u;
  const char x[8] = { 0x3f, 0xf5, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55 };

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW FLOAT64 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int n = putdata(D, "data", 5, 0, 1, 0, GD_FLOAT64, &c);
  int error = D->error;

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(double), SEEK_SET);
  read(fd, &u.f, sizeof(double));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (n != 1)
    return 1;
  if (error)
    return 1;
  
  for (i = 0; i < 8; ++i)
    if (x[(big_endian) ? 7 - i : i] != u.b[i])
      return 1;

  return 0;
}
