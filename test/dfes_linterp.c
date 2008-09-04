/* Try to free the strings from a LINTERP entry */
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
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data = "data LINTERP in table\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  gd_entry_t E;

  int n = get_entry(D, "data", &E);
  dirfile_free_entry_strings(&E);

  dirfile_close(D);
  unlink(format);
  rmdir(filedir);

  return 0;
}
