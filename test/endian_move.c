/* Test endianness */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT16 8\nENDIAN little\n";
  uint16_t data_data[128];
  uint16_t c[8], d;
  int fd;
  int we = 0;
  int xe = 0;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = 0x201 * fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_alter_endianness(D, GD_BIG_ENDIAN, 0, 1);
  int error = get_error(D);
  int n = getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  int i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      if (d != i * 0x102) {
        printf("%x = %x\n", i, d);
        xe++;
      }

      i++;
    }
    close(fd);
  } else
    xe = -1;

  unlink(data);
  unlink(format);
  rmdir(filedir);

  for (i = 0; i < 8; ++i)
    if (c[i] != (40 + i) * 0x201) {
      fprintf(stderr, "%x - %x\n", c[i], (40 + i) * 0x201);
      we++;
    }

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "2=%i\n", ret);
    return 1;
  }
  if (n != 8) {
    fprintf(stderr, "3=%i\n", n);
    return 1;
  }
  if (we != 0) {
    fprintf(stderr, "4=%i\n", we);
    return 1;
  }
  if (xe != 0) {
    fprintf(stderr, "5=%i\n", xe);
    return 1;
  }

  return 0;
}
