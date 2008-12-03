/* Test move */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <inttypes.h>
#include <unistd.h>

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
  int fd;
  char command[4096];
  gd_entry_t E;

  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = fd * 0x201;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, data_data, 128 * sizeof(uint16_t));
  close(fd);

  /* compress */
  snprintf(command, 4096, "%s -f %s > /dev/null", BZIP2, data);
  if (system(command))
    return 1;

#ifdef USE_BZIP2
  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_VERBOSE | GD_UNENCODED);
#else
  DIRFILE* D = dirfile_open(filedir, GD_RDWR | GD_UNENCODED);
#endif
  int ret = dirfile_move(D, "data", 1, 1);
  int error = get_error(D);
  int ge_ret =  get_entry(D, "data", &E);
  dirfile_close(D);

#ifdef USE_BZIP2
  int we = 0;
  uint16_t d;

  fd = open(data, O_RDONLY);
  int i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      if (d != i * 0x201) {
        printf("%i = %4x\n", i, d);
        we++;
      }

      i++;
    }
    close(fd);
  } else
    we = -1;
#endif

  unlink(format1);
  unlink(format);
  int unlink_data = unlink(data);
  int unlink_bz2data = unlink(bz2data);
  rmdir(filedir);

#ifdef USE_BZIP2
  if (ret != 0) {
    fprintf(stderr, "1=%i\n", ret);
    return 1;
  }
  if (error != GD_E_OK) {
    fprintf(stderr, "2=%i\n", error);
    return 1;
  }
  if (ge_ret != 0) {
    fprintf(stderr, "3=%i\n", ge_ret);
    return 1;
  }
  if (E.fragment_index != 1) {
    fprintf(stderr, "4=%i\n", E.fragment_index);
    return 1;
  }
  if (we != 0) {
    fprintf(stderr, "5=%i\n", we);
    return 1;
  }
  if (unlink_data != 0) {
    fprintf(stderr, "6=%i\n", unlink_data);
    return 1;
  }
  if (unlink_bz2data != -1) {
    fprintf(stderr, "7=%i\n", unlink_bz2data);
    return 1;
  }
#else
  if (ret != -1) {
    fprintf(stderr, "1=%i\n", ret);
    return 1;
  }
  if (error != GD_E_UNSUPPORTED) {
    fprintf(stderr, "2=%i\n", error);
    return 1;
  }
  if (ge_ret != 0) {
    fprintf(stderr, "3=%i\n", ge_ret);
    return 1;
  }
  if (E.fragment_index != 0) {
    fprintf(stderr, "4=%i\n", E.fragment_index);
    return 1;
  }
  if (unlink_data != -1) {
    fprintf(stderr, "6=%i\n", unlink_data);
    return 1;
  }
  if (unlink_bz2data != 0) {
    fprintf(stderr, "7=%i\n", unlink_bz2data);
    return 1;
  }
#endif

  return 0;
}
