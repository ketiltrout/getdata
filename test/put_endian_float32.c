/* Attempt to write FLOAT32 with the opposite endianness */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int BigEndian(void)
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
  const char* filedir = __FILE__ "dirfile";
  const char* format = __FILE__ "dirfile/format";
  const char* data = __FILE__ "dirfile/data";
  char format_data[1000];
  float c = 4. / 3.;
  int fd, i;
  const int big_endian = BigEndian();
  union {
    float f;
    char b[4];
  } u;
  const char x[4] = { 0x3f, 0xaa, 0xaa, 0xab };

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW FLOAT32 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int n = putdata(D, "data", 5, 0, 1, 0, GD_FLOAT32, &c);
  int error = D->error;

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(float), SEEK_SET);
  read(fd, &u.f, sizeof(float));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (n != 1)
    return 1;
  if (error)
    return 1;
  
  for (i = 0; i < 4; ++i)
    if (x[(big_endian) ? 3 - i : i] != u.b[i])
      return 1;

  return 0;
}
