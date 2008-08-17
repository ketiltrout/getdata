/* Attempt to write FLOAT64 */
#include "../src/getdata.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __FILE__ "dirfile";
  const char* format = __FILE__ "dirfile/format";
  const char* data = __FILE__ "dirfile/data";
  const char* format_data = "data RAW FLOAT64 8\n";
  double c[8], d;
  int fd, i;
  struct stat buf;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = 40 + i;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR);
  int n = putdata(D, "data", 5, 0, 1, 0, GD_FLOAT64, c);
  int error = D->error;

  dirfile_close(D);

  if (stat(data, &buf))
    return 1;
  if (buf.st_size != 48 * sizeof(double))
    return 1;

  fd = open(data, O_RDONLY);
  i = 0;
  while (read(fd, &d, sizeof(double))) {
    if (i < 40 || i > 48) {
      if (d != 0)
        return 1;
    } else if (d != i)
      return 1;
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;
  if (n != 8)
    return 1;

  return 0;
}
