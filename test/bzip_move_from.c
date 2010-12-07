/* Test move */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <inttypes.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format1 = __TEST__ "dirfile/format1";
  const char* data = __TEST__ "dirfile/data";
  const char* bz2data = __TEST__ "dirfile/data.bz2";
  const char* format_data =
    "/INCLUDE format1\ndata RAW UINT16 11\nENCODING bzip2\n";
  const char* format1_data = "ENCODING none\n";
  uint16_t data_data[128];
  int fd, ret, error, ge_ret, unlink_data, unlink_bz2data, r = 0, i = 0;
  char command[4096];
  gd_entry_t E;
  DIRFILE *D;
  uint16_t d;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = fd * 0x201;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 128 * sizeof(uint16_t));
  close(fd);

  /* compress */
  snprintf(command, 4096, "%s -f %s > /dev/null", BZIP2, data);
  if (gd_system(command))
    return 1;

#ifdef USE_BZIP2
  D = gd_open(filedir, GD_RDWR | GD_VERBOSE | GD_UNENCODED);
#else
  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
#endif
  ret = gd_move(D, "data", 1, 1);
  error = gd_error(D);
  ge_ret =  gd_entry(D, "data", &E);
  gd_close(D);

#ifdef USE_BZIP2
  fd = open(data, O_RDONLY | O_BINARY);

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKI(d, i * 0x201);
      i++;
    }
    close(fd);
  } else {
    perror("open");
    r = 1;
  }
#endif

  unlink(format1);
  unlink(format);
  unlink_data = unlink(data);
  unlink_bz2data = unlink(bz2data);
  rmdir(filedir);

#ifdef USE_BZIP2
  CHECKI(ret, 0);
  CHECKI(error, GD_E_OK);
  CHECKI(ge_ret, 0);
  CHECKI(E.fragment_index, 1);
  CHECKI(unlink_data, 0);
  CHECKI(unlink_bz2data, -1);
#else
  CHECKI(ret, -1);
  CHECKI(error, GD_E_UNSUPPORTED);
  CHECKI(ge_ret, 0);
  CHECKI(E.fragment_index, 0);
  CHECKI(unlink_data, -1);
  CHECKI(unlink_bz2data, 0);
#endif
  gd_free_entry_strings(&E);

  return r;
}
