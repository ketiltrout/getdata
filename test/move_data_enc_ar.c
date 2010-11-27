/* Test move */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <inttypes.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* data = __TEST__ "dirfile/data";
  const char* txtdata = __TEST__ "dirfile/data.txt";
  const char* format_data =
    "/INCLUDE format1\ndata RAW UINT16 11\nENCODING text\n";
  const char* format1_data = "ENCODING none\n";
  int r = 0;
  uint16_t d;
  int fd, i;
  FILE* stream;
  gd_entry_t E;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  stream = fopen(txtdata, "w");
  for (i = 0; i < 128; ++i)
    fprintf(stream, "%i\n", i * 0x201);
  fclose(stream);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int ret = gd_move(D, "data", 1, 1);
  int error = gd_error(D);
  int ge_ret =  gd_entry(D, "data", &E);
  gd_close(D);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKXi(i,d,i * 0x201);
      i++;
    }
    close(fd);
  } else {
    perror("open");
    r = 1;
  }

  unlink(format1);
  unlink(format);
  int unlink_data = unlink(data);
  int unlink_txtdata = unlink(txtdata);
  rmdir(filedir);

  CHECKI(ret, 0);
  CHECKI(error, GD_E_OK);
  CHECKI(ge_ret, 0);
  CHECKI(E.fragment_index, 1);
  CHECKI(unlink_data, 0);
  CHECKI(unlink_txtdata, -1);
  gd_free_entry_strings(&E);

  return 0;
}
