/* VERSION should cross INCLUDEs */
#include "../src/getdata.h"

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
  const char* format1 = __FILE__ "dirfile/format1";
  const char* format_data = "VERSION 999999\nINCLUDE format1\n";
  const char* format1_data = "BADDIRECTIVE BADTYPE\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  int error = D->error;
  dirfile_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return (error != GD_E_OK);
}
