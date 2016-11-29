/* Copyright (C) 2008-2016 D. V. Wiebe
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

#ifdef HAVE_ZLIB_H
#include <zlib.h>
#endif

#ifdef HAVE_GZSEEK64
#define gd_gzseek gzseek64
#else
#define gd_gzseek gzseek
#endif

#ifdef HAVE_GZTELL64
#define gd_gztell gztell64
#else
#define gd_gztell gztell
#endif

/* The gzip encoding scheme uses edata as a gzFile object.  If a file is
 * open, idata >= 0 otherwise idata = -1.  Writes occur out-of-place. */

int _GD_GzipOpen(int fd, struct gd_raw_file_* file,
    gd_type_t data_type gd_unused_, int swap gd_unused_, unsigned int mode)
{
  const char *gzmode = "w";
  dtrace("%i, %p, <unused>, <unused>, 0x%X", fd, file, mode);

  if (mode & GD_FILE_READ) {
    file->idata = gd_OpenAt(file->D, fd, file->name, O_RDONLY | O_BINARY, 0666);
    gzmode = "r";
  } else if (mode & GD_FILE_TEMP) {
    file->idata = _GD_MakeTempFile(file->D, fd, file->name);
  } else { /* internal error */
    dreturn("%i", 1);
    errno = EINVAL; /* I guess ... ? */
    return 1;
  }

  if (file->idata == -1) {
    dreturn("%i", 1);
    return 1;
  }

  file->edata = gzdopen(file->idata, gzmode);

  if (file->edata == NULL) {
    close(file->idata);
    errno = ENOMEM;
    file->idata = -1;
    dreturn("%i", 1);
    return 1;
  }

  file->mode = mode;
  file->pos = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_GzipSeek(struct gd_raw_file_* file, off64_t offset,
    gd_type_t data_type, unsigned int mode)
{
  off64_t n = 0;

  dtrace("%p, %" PRId64 ", 0x%X, 0x%X", file, (int64_t)offset, data_type, mode);

  if (file->pos == offset) {
    dreturn("%" PRId64, (int64_t)offset);
    return offset;
  }

  offset *= GD_SIZE(data_type);

  n = gd_gzseek((gzFile)file->edata, offset, SEEK_SET);

  if (n == -1) {
    /* some implementations of gzseek return error on attempts to seek past the
     * EOF in read mode, and set the position to the EOF. */
    if (mode != GD_FILE_WRITE && gzeof((gzFile)file->edata))
      n = gd_gztell((gzFile)file->edata);
    else {
      dreturn("%i", -1);
      return -1;
    }
  }

  n /= GD_SIZE(data_type);
  file->pos = n;

  dreturn("%" PRId64, (int64_t)n);
  return n;
}

ssize_t _GD_GzipRead(struct gd_raw_file_ *file, void *ptr, gd_type_t data_type,
    size_t nmemb)
{
  ssize_t n;
  int errnum;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nmemb);

  n = gzread((gzFile)file->edata, ptr, GD_SIZE(data_type) * nmemb);

  if (n >= 0) {
    n /= GD_SIZE(data_type);
    file->pos += n;
  } else {
    gzerror((gzFile)file->edata, &errnum);
    if (errnum < 0)
      n = -1;
  }

  dreturn("%" PRIuSIZE, n);
  return n;
}

ssize_t _GD_GzipWrite(struct gd_raw_file_ *file, const void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;
  int errnum;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nmemb);

  n = gzwrite((gzFile)file->edata, ptr, GD_SIZE(data_type) * nmemb);

  if (n > 0) {
    n /= GD_SIZE(data_type);
    file->pos += n;
  } else {
    gzerror((gzFile)file->edata, &errnum);
    if (errnum < 0)
      n = -1;
  }

  dreturn("%" PRIdSIZE, n);
  return n;
}

int _GD_GzipClose(struct gd_raw_file_ *file)
{
  int ret;

  dtrace("%p", file);

  ret = gzclose((gzFile)file->edata);
  if (ret) {
    dreturn("%i", ret);
    return ret;
  }

  file->idata = -1;
  file->edata = NULL;
  file->mode = 0;

  dreturn("%i", 0);
  return 0;
}

off64_t _GD_GzipSize(int dirfd, struct gd_raw_file_ *file, gd_type_t data_type,
    int swap gd_unused_)
{
  int fd;
  uint32_t size = 0;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

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

  dreturn("%" PRIu32, size);
  return size;
}

int _GD_GzipStrerr(const struct gd_raw_file_ *file, char *buf, size_t buflen)
{
  int r = 0;
  int gzerrnum = 0;
  const char *gzerr = NULL;

  dtrace("%p, %p, %" PRIuSIZE, file, buf, buflen);

  if (file->edata)
    gzerr = gzerror((gzFile)file->edata, &gzerrnum);

  if (gzerrnum == Z_ERRNO || gzerr == NULL)
    r = gd_StrError(errno, buf, buflen);
  else {
    strncpy(buf, gzerr, buflen);
    buf[buflen - 1] = 0;
  }

  dreturn("%i", r);
  return r;
}
