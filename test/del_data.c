/* Attempt to delete a field */
#include "../src/getdata.h"


#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
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
  int fd;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int ret = dirfile_delete(D, "data", GD_DEL_DATA);
  int error1 = get_error(D);
  int n = getdata(D, "data", 5, 0, 1, 0, GD_UINT8, data_data);
  int error2 = get_error(D);
  dirfile_close(D);

  fd = unlink(data);
  unlink(format);
  rmdir(filedir);

  if (fd == 0) {
    fprintf(stderr, "1=%i\n", fd);
    return 1;
  }
  if (error1 != GD_E_OK) {
    fprintf(stderr, "2=%i\n", error1);
    return 1;
  }
  if (n != 0) {
    fprintf(stderr, "3=%i\n", n);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "4=%i\n", ret);
    return 1;
  }
  if (error2 != GD_E_BAD_CODE) {
    fprintf(stderr, "5=%i\n", error2);
    return 1;
  }

  return 0;
}
