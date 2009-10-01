/* Parser check */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

static int saw_callback = 0;

int callback(gd_parser_data_t *pdata __attribute__ (( unused )),
    void* extra __attribute__ (( unused )))
{
  saw_callback++;

  return -1;
}

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data =
    "BADDIRECTIVE BADTYPE\n"
    "BADDIRECTIVE BADTYPE\n"
    "BADDIRECTIVE BADTYPE\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_cbopen(filedir, GD_RDONLY, callback, NULL);
  int error = get_error(D);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  if (saw_callback != 1) {
    fprintf(stderr, "1=%i\n", saw_callback);
    return 1;
  }

  return (error != GD_E_CALLBACK);
}
