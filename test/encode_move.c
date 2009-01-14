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
  const char* txtdata = __TEST__ "dirfile/data.txt";
  const char* format_data = "data RAW UINT16 8\nENCODING none\n";
  uint16_t data_data[128];
  uint16_t c[8];
  int fd, i, we = 0;

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
  int ret = dirfile_alter_encoding(D, GD_TEXT_ENCODED, 0, 1);
  int error = get_error(D);
  int n = getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);

  dirfile_close(D);

  int unlink_txtdata = unlink(txtdata);
  int unlink_data = unlink(data);
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
  if (unlink_txtdata != 0) {
    fprintf(stderr, "5=%i\n", unlink_txtdata);
    return 1;
  }
  if (unlink_data == 0) {
    fprintf(stderr, "6=%i\n", unlink_data);
    return 1;
  }

  return 0;
}
