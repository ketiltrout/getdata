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
  const char* format_data = "data RAW UINT32 8\nphase PHASE data 1\n"
    "lincom LINCOM 2 data 1 0 data 1 0\n";
  uint32_t data_data[256];
  uint32_t c[8];
  int fd, i, we = 0;
  const char* in_fields[3] = {"data", "phase", NULL};

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (uint32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(uint32_t));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_alter_lincom(D, "lincom", 0, in_fields, NULL, NULL);
  int error = get_error(D);
  int n = getdata(D, "lincom", 5, 0, 1, 0, GD_UINT32, c);

  dirfile_close(D);

  for (i = 0; i < 8; ++i)
    if (c[i] != i * 2 + 81) {
        printf("%i = %i\n", i * 2 + 81, c[i]);
        we++;
      }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (n != 8) {
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
