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
  const char* format_data = "/INCLUDE format1\ndata RAW UINT16 11";
  const char* format1_data = "ENCODING text\n";
  uint16_t data_data[128];
  int r = 0;
  uint16_t d;
  char line[100];
  int fd, i;
  FILE* stream;
  gd_entry_t E;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = fd * 0x201;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  int ret = gd_move(D, "data", 1, 1);
  int error = gd_error(D);
  int ge_ret =  gd_entry(D, "data", &E);
  gd_close(D);

  stream = fopen(txtdata, "rt");

  if (stream != NULL) {
    i = 0;

    while (fgets(line, 100, stream)) {
      d = strtoul(line, NULL, 10);
      CHECKXi(i, d, (unsigned)i * 0x201);
      i++;
    }
    fclose(stream);
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
  CHECKI(unlink_data, -1);
  CHECKI(unlink_txtdata, 0);

  return r;
}
