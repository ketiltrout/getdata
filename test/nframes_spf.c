/* The number of frames should track the samples per frame */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

void write_format(const char* format, int spf)
{
  char format_data[100];
  int fd;
  sprintf(format_data, "data RAW UINT8 %i\n", spf);

  fd = open(format, O_CREAT | O_TRUNC | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);
}

int main(void)
{
  const char* filedir = __FILE__ "dirfile";
  const char* format = __FILE__ "dirfile/format";
  const char* data = __FILE__ "dirfile/data";
  int fd;
  int i;
  const int len = strlen(data);

  mkdir(filedir, 0777);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data, len);
  close(fd);

  for (i = 1; i < len + 1; ++i) {
    write_format(format, i);
    DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
    unsigned int spf = get_samples_per_frame(D, "data");
    size_t n = get_n_frames(D);
    if (n != len / spf)
      return 1;
    dirfile_close(D);
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return 0;
}
