/* Test include */
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
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format_data = "data1 RAW UINT8 1\n";
  const char* format1_data = "data RAW UINT8 11\nREFERENCE data\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE);
  dirfile_include(D, "format1", 0, GD_VERBOSE);
  int error1 = get_error(D);
  const char* reference = strdup(dirfile_reference(D, NULL));
  int error2 = get_error(D);
  unsigned int spf = get_spf(D, "data");
  dirfile_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  if (error1) {
    fprintf(stderr, "1=%i\n", error1);
    return 1;
  }
  if (error2) {
    fprintf(stderr, "2=%i\n", error2);
    return 1;
  }
  if (strcmp(reference, "data")) {
    fprintf(stderr, "3=%s\n", reference);
    return 1;
  }

  return (spf != 11);
}
