/* Test include */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format_data = "data1 RAW UINT8 1\n";
  const char* format1_data = "data RAW UINT8 11\nREFERENCE data\n";
  int fd, error1, error2, r = 0;
  char *reference;
  gd_spf_t spf;
  DIRFILE *D;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_include(D, "format1", 0, GD_VERBOSE);
  error1 = gd_error(D);
  reference = strdup(gd_reference(D, NULL));
  error2 = gd_error(D);
  spf = gd_spf(D, "data");
  gd_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1, 0);
  CHECKI(error2, 0);
  CHECKS(reference, "data");
  CHECKU(spf, 11);
  free(reference);

  return r;
}
