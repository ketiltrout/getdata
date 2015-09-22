/* Copyright (C) 2008-2011, 2014 D. V. Wiebe
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

/* The gzip encoding scheme uses edata as a gzFile object.  If a file is
 * open, idata >= 0 otherwise idata = -1.  Writes occur out-of-place. */

int _GD_GzipOpen(int fd, struct gd_raw_file_* file, int swap gd_unused_,
    unsigned int mode)
{
  const char *gzmode = "w";
  dtrace("%i, %p, <unused>, 0x%X", fd, file, mode);

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
    file->idata = -1;
    dreturn("%i", 1);
    return 1;
  }

  file->mode = mode;
  file->pos = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_GzipSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int mode)
{
  off64_t n = 0;

  dtrace("%p, %lli, 0x%X, 0x%X", file, (long long)count, data_type, mode);

  if (file->pos == count) {
    dreturn("%lli", (long long)count);
    return count;
  }

  count *= GD_SIZE(data_type);

  if (count >= 0) {
    n = (off64_t)gzseek64((gzFile)file[(mode == GD_FILE_WRITE) ? 1 : 0].edata,
        (z_off64_t)count, SEEK_SET);

    if (n == -1) {
      /* gzseek returns error on attempts to seek past the EOF in read mode */
      if (mode != GD_FILE_WRITE && gzeof((gzFile)file[0].edata))
        n = gztell64((gzFile)file[0].edata);
      else {
        dreturn("%i", -1);
        return -1;
      }
    }

    n /= GD_SIZE(data_type);
    file->pos = n;
  }

  dreturn("%lli", (long long)n);
  return n;
}

ssize_t _GD_GzipRead(struct gd_raw_file_ *file, void *ptr, gd_type_t data_type,
    size_t nmemb)
{
  ssize_t n;
  int errnum;

  dtrace("%p, %p, 0x%X, %" PRNsize_t, file, ptr, data_type, nmemb);

  n = gzread((gzFile)file->edata, ptr, GD_SIZE(data_type) * nmemb);

  if (n >= 0) {
    n /= GD_SIZE(data_type);
    file->pos += n;
  } else {
    gzerror((gzFile)file->edata, &errnum);
    if (errnum < 0)
      n = -1;
  }

  dreturn("%" PRNsize_t, n);
  return n;
}

ssize_t _GD_GzipWrite(struct gd_raw_file_ *file, const void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;
  int errnum;

  dtrace("%p, %p, 0x%X, %" PRNsize_t, file, ptr, data_type, nmemb);

  n = gzwrite((gzFile)file->edata, ptr, GD_SIZE(data_type) * nmemb);

  if (n >= 0) {
    n /= GD_SIZE(data_type);
    file->pos += n;
  } else {
    gzerror((gzFile)file[1].edata, &errnum);
    if (errnum < 0)
      n = -1;
  }

  dreturn("%" PRNssize_t, n);
  return n;
}

/* Because calling gzflush can result in degredation of compression, we avoid
 * doing anything here */
int _GD_GzipSync(struct gd_raw_file_ *file gd_unused_)
{
  dtrace("<unused>");

  dreturn("%i", 0);
  return 0;
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

  dreturn("%lli", (long long)size);
  return size;
}
