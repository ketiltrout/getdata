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
  int fd, i, we = 0;

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
  int ret = dirfile_alter_linterp(D, "lut", NULL, "table1", 1);
  int error = get_error(D);
  int n = getdata(D, "lut", 5, 0, 1, 0, GD_INT32, c);

  dirfile_close(D);

  for (i = 0; i < 8; ++i)
    if (c[i] != (i + 40) * 5) {
        printf("%i = %i\n", (i + 40) * 5, c[i]);
        we++;
      }

  unlink(data);
  int unlink_table = unlink(table);
  unlink(table1);
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
  if (unlink_table == 0) {
    fprintf(stderr, "5=%i\n", unlink_table);
    return 1;
  }

  return 0;
}
