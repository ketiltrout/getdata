/* Attempt to rename a field */
#include "test.h"

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
  const char* zata = __TEST__ "dirfile/zata";
  const char* format_data = "cata RAW UINT8 8\ndata RAW UINT8 8\n"
    "eata RAW UINT8 8\n";
  unsigned char data_data[256];
  int fd, r = 0;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = gd_rename(D, "data", "zata", 0);
  int error = gd_error(D);
  const char** fl = gd_field_list(D);

  const char* field_list[4];

  field_list[0] = strdup(fl[0]);
  field_list[1] = strdup(fl[1]);
  field_list[2] = strdup(fl[2]);
  field_list[3] = strdup(fl[3]);

  gd_close(D);

  int unlink_data = unlink(data);
  int unlink_zata = unlink(zata);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(ret,0);
  CHECKS(field_list[0], "INDEX");
  CHECKS(field_list[1], "cata");
  CHECKS(field_list[2], "eata");
  CHECKS(field_list[3], "zata");
  CHECKI(unlink_data, 0);
  CHECKI(unlink_zata, -1);
  free(field_list[0]);
  free(field_list[1]);
  free(field_list[2]);
  free(field_list[3]);

  return r;
}
