/* Attempt to write UINT64 with the opposite endianness */
#include "../src/getdata.h"

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
  uint64_t c = 0x0203000000040001LLU, d = 0;
  int fd;
  const int big_endian = BigEndian();

  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW UINT64 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED);
  int n = putdata(D, "data", 5, 0, 1, 0, GD_UINT64, &c);
  int error = get_error(D);

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * sizeof(uint64_t), SEEK_SET);
  read(fd, &d, sizeof(uint64_t));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (d != 0x0100040000000302LLU)
    return 1;
  if (n != 1)
    return 1;
  if (error)
    return 1;

  return 0;
}
