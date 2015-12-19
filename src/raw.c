/* Copyright (C) 2008, 2010-2015 D. V. Wiebe
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

int _GD_RawOpen(int fd, struct gd_raw_file_* file,
    gd_type_t data_type gd_unused_, int swap gd_unused_, unsigned int mode)
{
  dtrace("%i, %p, <unused>, <unused>, 0x%X", fd, file, mode);

  if (!(mode & GD_FILE_TEMP)) {
    if (file->mode & mode) {
      dreturn("%i", 0);
      return 0;
    } else if (file->idata >= 0)
      close(file->idata);

    file->idata = gd_OpenAt(file->D, fd, file->name, ((mode & GD_FILE_WRITE) ?
          (O_RDWR | O_CREAT) : O_RDONLY) | O_BINARY, 0666);

  } else {
    file->idata = _GD_MakeTempFile(file->D, fd, file->name);
  }

  if (file->idata < 0) {
    dreturn("%i", 1);
    return 1;
  }

  file->pos = 0;
  file->mode = mode | GD_FILE_READ;

  dreturn("%i", 0);
  return 0;
}

off64_t _GD_RawSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int mode gd_unused_)
{
  off64_t pos;

  dtrace("%p, %" PRId64 ", 0x%X, <unused>", file, (int64_t)count, data_type);

  /* short circuit */
  if (file->pos == count) {
    dreturn("%" PRId64, (int64_t)count);
    return count;
  }

  pos = lseek64(file->idata, count * GD_SIZE(data_type), SEEK_SET);

  /* If we've landed in the middle of a sample, we have to back up */
  if (pos > 0 && (pos % GD_SIZE(data_type)))
    pos = lseek64(file->idata, -(pos % GD_SIZE(data_type)), SEEK_CUR);

  if (pos == -1) {
    dreturn("%i", -1);
    return -1;
  }

  pos /= GD_SIZE(data_type);
  file->pos = pos;

  dreturn("%" PRId64, (int64_t)pos);
  return pos;
}

ssize_t _GD_RawRead(struct gd_raw_file_ *restrict file, void *restrict ptr,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t nread;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nmemb);

  nread = read(file->idata, ptr, nmemb * GD_SIZE(data_type));

  if (nread >= 0) {
    nread /= GD_SIZE(data_type);
    file->pos += nread;
  }

  dreturn("%" PRIdSIZE, nread);
  return nread;
}

ssize_t _GD_RawWrite(struct gd_raw_file_ *restrict file,
    const void *restrict ptr, gd_type_t data_type, size_t nmemb)
{
  ssize_t nwrote;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nmemb);

  nwrote = write(file->idata, ptr, nmemb * GD_SIZE(data_type));

  if (nwrote >= 0) {
    nwrote /= GD_SIZE(data_type);
    file->pos += nwrote;
  }

  dreturn("%" PRIuSIZE, nwrote);
  return nwrote;
}

int _GD_RawSync(struct gd_raw_file_ *file)
{
  int r;

  dtrace("%p", file);

  r = fsync(file->idata);

  dreturn("%i", r);
  return r;
}

int _GD_RawClose(struct gd_raw_file_ *file)
{
  int ret;

  dtrace("%p", file);

  ret = close(file->idata);
  if (!ret) {
    file->idata = -1;
    file->mode = 0;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_RawSize(int dirfd, struct gd_raw_file_ *file, gd_type_t data_type,
    int swap gd_unused_)
{
  gd_stat64_t statbuf;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  if (gd_StatAt64(file->D, dirfd, file->name, &statbuf, 0) < 0)  {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%" PRId64, (int64_t)statbuf.st_size);
  return statbuf.st_size / GD_SIZE(data_type);
}
