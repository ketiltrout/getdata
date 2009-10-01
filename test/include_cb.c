/* Test include */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

int saw_callback = 0;
int callback(gd_parser_data_t *pdata __attribute__ (( unused )), 
    void *extra __attribute__ (( unused )))
{
  saw_callback++;

  return GD_SYNTAX_IGNORE;
}


int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* format_data = "\n";
  const char* format1_data = "data ROW UINT8 11\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  DIRFILE* D = dirfile_cbopen(filedir, GD_RDWR, callback, NULL);
  dirfile_include(D, "format1", 0, 0);
  int error = get_error(D);
  dirfile_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  if (error != GD_E_OK) {
    fprintf(stderr, "1=%i\n", error);
    return 1;
  }
  if (saw_callback != 1) {
    fprintf(stderr, "2=%i\n", saw_callback);
    return 1;
  }

  return 0;
}
