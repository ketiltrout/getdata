/* Attempt to rename a field */
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
  const char* zata = __TEST__ "dirfile/zata";
  const char* format_data = "cata RAW UINT8 8\ndata RAW UINT8 8\n"
    "eata RAW UINT8 8\n";
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

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int r = dirfile_rename(D, "data", "zata", 0);
  int error = get_error(D);
  const char** fl = get_field_list(D);

  const char* field_list[4];

  field_list[0] = strdup(fl[0]);
  field_list[1] = strdup(fl[1]);
  field_list[2] = strdup(fl[2]);
  field_list[3] = strdup(fl[3]);

  dirfile_close(D);

  int unlink_data = unlink(data);
  int unlink_zata = unlink(zata);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (r != 0) {
    fprintf(stderr, "2=%i\n", r);
    return 1;
  }
  if (strcmp(field_list[0], "INDEX")) {
    fprintf(stderr, "3=%s\n", field_list[0]);
    return 1;
  }
  if (strcmp(field_list[1], "cata")) {
    fprintf(stderr, "4=%s\n", field_list[1]);
    return 1;
  }
  if (strcmp(field_list[2], "eata")) {
    fprintf(stderr, "5=%s\n", field_list[2]);
    return 1;
  }
  if (strcmp(field_list[3], "zata")) {
    fprintf(stderr, "6=%s\n", field_list[3]);
    return 1;
  }
  if (unlink_data != 0) {
    fprintf(stderr, "7=%i\n", unlink_data);
    return 1;
  }
  if (unlink_zata == 0) {
    fprintf(stderr, "7=%i\n", unlink_zata);
    return 1;
  }

  return 0;
}
