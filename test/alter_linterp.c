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
  const char* table = __TEST__ "dirfile/table";
  const char* table1 = __TEST__ "dirfile/table1";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW INT32 8\nlut LINTERP data table\n";
  const char* tabledata = "0 0\n1000 5000\n";
  const char* table1data = "0 0\n1000 10000\n";
  int32_t data_data[256];
  int32_t c[8];
  gd_entry_t e;
  int fd, i, r = 0;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(table, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, tabledata, strlen(tabledata));
  close(fd);

  fd = open(table1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, table1data, strlen(table1data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = dirfile_alter_linterp(D, "lut", NULL, "table1", 0);
  int error = get_error(D);
  get_entry(D, "lut", &e);
  int error2 = get_error(D);
  int n = getdata(D, "lut", 5, 0, 1, 0, GD_INT32, c);

  dirfile_close(D);

  unlink(data);
  int unlink_table = unlink(table);
  unlink(table1);
  //unlink(format);
  rmdir(filedir);

  for (i = 0; i < 8; ++i)
    if (c[i] != (i + 40) * 10) {
        fprintf(stderr, "%i = %i\n", (i + 40) * 10, c[i]);
        r = 1;
      }

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    r = 1;
  }
  if (n != 8) {
    fprintf(stderr, "2=%lli\n", (long long)n);
    r = 1;
  }
  if (ret != 0) {
    fprintf(stderr, "3=%i\n", ret);
    r = 1;
  }
  if (unlink_table != 0) {
    fprintf(stderr, "5=%i\n", unlink_table);
    r = 1;
  }
  if (error2) {
    fprintf(stderr, "6=%i\n", error2);
    r = 1;
  }
  if (strcmp(e.in_fields[0], "data")) {
    fprintf(stderr, "7=%s\n", e.in_fields[0]);
    r = 1;
  }
  if (strcmp(e.table, "table1")) {
    fprintf(stderr, "7=%s\n", e.table);
    r = 1;
  }

  return r;
}
