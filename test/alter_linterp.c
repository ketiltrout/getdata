/* Test field modifying */
#include "test.h"

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

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  int ret = gd_alter_linterp(D, "lut", NULL, "table1", 0);
  int error = gd_error(D);
  gd_entry(D, "lut", &e);
  int error2 = gd_error(D);
  int n = gd_getdata(D, "lut", 5, 0, 1, 0, GD_INT32, c);

  gd_close(D);

  unlink(data);
  int unlink_table = unlink(table);
  unlink(table1);
  unlink(format);
  rmdir(filedir);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], (i + 40) * 10);

  CHECKI(error, 0);
  CHECKI(n, 8);
  CHECKI(ret, 0);
  CHECKI(unlink_table, 0);
  CHECKI(error2, 0);
  CHECKS(e.in_fields[0], "data");
  CHECKS(e.u.linterp.table, "table1");

  return r;
}
