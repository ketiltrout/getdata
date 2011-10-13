/* Copyright (C) 2008-2011 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
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
 * open, idata = 0 otherwise idata = -1. */

int _GD_GzipOpen(int dirfd, struct _gd_raw_file* file, int swap __gd_unused,
    int mode __gd_unused, int creat __gd_unused)
{
  int fd;

  dtrace("%i, %p, <unused>, <unused>, <unused>", dirfd, file);

  fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666);

  if (fd == -1) { 
    dreturn("%i", 0);
    return 0;
  }

  file->edata = gzdopen(fd, "r");

  if (file->edata != NULL) {
    file->pos = 0;
    file->idata = 0;
    dreturn("%i", 0);
    return 0;
  }

  close(fd);
  dreturn("%i", 1);
  return 1;
}

off64_t _GD_GzipSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  off64_t n;

  dtrace("%p, %lli, %x, <unused>", file, (long long)count, data_type);

  if (file->pos == count) {
    dreturn("%lli", (long long)count);
    return count;
  }

  n = (off64_t)gzseek(file->edata, (off_t)count * GD_SIZE(data_type), SEEK_SET);

  if (n == -1) {
    dreturn("%i", -1);
    return -1;
  }
  
  n /= GD_SIZE(data_type);
  file->pos = n;

  dreturn("%lli", (long long)n);
  return n;
}

ssize_t _GD_GzipRead(struct _gd_raw_file *file, void *ptr, gd_type_t data_type,
    size_t nmemb)
{
  ssize_t n;

  dtrace("%p, %p, %x, %zu", file, ptr, data_type, nmemb);

  n = gzread(file->edata, ptr, GD_SIZE(data_type) * nmemb);

  if (n >= 0) {
    n /= GD_SIZE(data_type);
    file->pos += n;
  }

  dreturn("%zu", n);
  return n;
}

int _GD_GzipClose(struct _gd_raw_file *file)
{
  int ret;

  dtrace("%p", file);

  ret = gzclose(file->edata);
  if (!ret) {
    file->idata = -1;
    file->edata = NULL;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_GzipSize(int dirfd, struct _gd_raw_file *file, gd_type_t data_type,
    int swap __gd_unused)
{
  int fd;
  uint32_t size = 0;

  dtrace("%i, %p, %x, <unused>", dirfd, file, data_type);

  fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666);
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
