/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#ifdef HAVE_ZLIB_H
#include <zlib.h>
#endif

#ifdef USE_MODULES
#define _GD_GzipOpen libgetdatagzip_LTX_GD_GzipOpen
#define _GD_GzipSeek libgetdatagzip_LTX_GD_GzipSeek
#define _GD_GzipRead libgetdatagzip_LTX_GD_GzipRead
#define _GD_GzipClose libgetdatagzip_LTX_GD_GzipClose
#define _GD_GzipSize libgetdatagzip_LTX_GD_GzipSize
#endif

/* The zlib encoding scheme uses edata as a gzFile object.  If a file is
 * open, fp = 0 otherwise fp = -1. */

int _GD_GzipOpen(struct _gd_raw_file* file, int mode __gd_unused,
    int creat __gd_unused)
{
  dtrace("%p, <unused>, <unused>", file);

  file->edata = gzopen(file->name, "rb" /* writing not supported */);

  if (file->edata != NULL) {
    file->fp = 0;
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

off64_t _GD_GzipSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  dtrace("%p, %lli, %x, <unused>", file, (long long)count, data_type);

  off64_t n = (off64_t)gzseek(file->edata, (off_t)count * GD_SIZE(data_type),
      SEEK_SET);

  if (n == -1) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%lli", (long long)(n / GD_SIZE(data_type)));
  return n / GD_SIZE(data_type);
}

ssize_t _GD_GzipRead(struct _gd_raw_file *file, void *ptr, gd_type_t data_type,
    size_t nmemb)
{
  dtrace("%p, %p, %x, %zi", file, ptr, data_type, nmemb);

  ssize_t n = gzread(file->edata, ptr, GD_SIZE(data_type) * nmemb);

  if (n >= 0)
    n /= GD_SIZE(data_type);

  dreturn("%zi", n);
  return n;
}

int _GD_GzipClose(struct _gd_raw_file *file)
{
  dtrace("%p", file);

  int ret = gzclose(file->edata);
  if (!ret) {
    file->fp = -1;
    file->edata = NULL;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_GzipSize(struct _gd_raw_file *file, gd_type_t data_type)
{
  uint32_t size = 0;

  dtrace("%p, %x", file, data_type);

  int fd = open(file->name, O_RDONLY);
  if (fd < 0) {
    dreturn("%i", -1);
    return -1;
  }

  /* seek to the end */
  if (lseek64(fd, -4, SEEK_END) == -1) {
    dreturn("%i", -1);
    return -1;
  }
  if (read(fd, &size, 4) < 4) {
    dreturn("%i", -1);
    return -1;
  }

  /* the checksum size is stored little endian */
#ifdef WORDS_BIGENDIAN
  size = (size << 24) | ((size << 8) & 0x00ff0000)
       | (size >> 24) | ((size >> 8) & 0x0000ff00);
#endif

  close(fd);

  size /= GD_SIZE(data_type);

  dreturn("%lli", (long long)size);
  return size;
}
