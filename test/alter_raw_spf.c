/* Test field modifying */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  unsigned char data_data[256];
  uint8_t d;
  int fd, i, we = 0;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_alter_raw(D, "data", GD_NULL, 11, 1);
  int error = get_error(D);
  off_t n = get_nframes(D);

  dirfile_close(D);

  fd = open(data, O_RDONLY);
  i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint8_t))) {
      if (d != i * 8 / 11) {
        printf("%4x = %4x\n", i * 8 / 11, d);
        we++;
      }

      i++;
    }
    close(fd);
  } else
    we = -1;

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (n != 32) {
    fprintf(stderr, "2=%lli\n", (long long)n);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "3=%i\n", ret);
    return 1;
  }
  if (we != 0) {
    fprintf(stderr, "4=%i\n", we);
    return 1;
  }

  return 0;
}
