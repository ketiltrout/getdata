/* Attempt to write UINT8 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#undef GETDATA_LEGACY_API
#endif

#include "getdata/dirfile.h"

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#ifdef HAVE_IO_H
#include <io.h>
#endif

#if MKDIR_NO_MODE
#ifdef HAVE__MKDIR
#define mkdir(f,m) _mkdir(f)
#else
#define mkdir(f,m) mkdir(f)
#endif
#endif
#ifndef O_BINARY
#define O_BINARY 0
#endif

using namespace GetData;

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW UINT8 8\n";
  uint8_t c[8], d;
  int fd, i, r = 0;
  struct stat buf;

  memset(c, 0, 8);
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  Dirfile dirfile(filedir, GD_RDWR | GD_UNENCODED);
  size_t n = dirfile.PutData("data", 5, 0, 1, 0, UInt8, c);
  int error = dirfile.Error();
  dirfile.Close();

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  }
  if (buf.st_size != 40 + 8 * sizeof(uint8_t)) {
    fprintf(stderr, "buf.st_size = %i (expected %i)\n",
        (int)buf.st_size, 40 + 8 * sizeof(uint8_t));
    r = 1;
  }

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    if (i < 40 || i > 48) {
      if (d != 0) {
        fprintf(stderr, "%i: d = %i (expected 0)\n", i, d);
        r = 1;
      }
    } else if (d != i) {
      fprintf(stderr, "%i: d = %i (expected %i)\n", i, d, i);
      r = 1;
    }
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "error = %i (exptected 0)\n", error);
    r = 1;
  }
  if (n != 8) {
    fprintf(stderr, "n = %i (exptected 8)\n", n);
    r = 1;
  }

  return r;
}
