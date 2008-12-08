/* Test frameoffset */
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
  const char* format_data = "data RAW UINT8 8\nFRAMEOFFSET 13\n";
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
  int ret = dirfile_alter_frameoffset(D, 16, 0, 0);
  int error = get_error(D);
  off_t fo = get_frameoffset(D, 0);
  off_t nf = get_nframes(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (ret != 0) {
    fprintf(stderr, "2=%i\n", ret);
    return 1;
  }
  if (fo != 16) {
    fprintf(stderr, "3=%lli\n", (long long)fo);
    return 1;
  }
  if (nf != 48) {
    fprintf(stderr, "4=%lli\n", (long long)nf);
    return 1;
  }

  return 0;
}
