/* Copyright (C) 2008, 2010, 2011 D. V. Wiebe
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

int _GD_RawOpen(int fd, struct _gd_raw_file* file, int swap __gd_unused,
    unsigned int mode)
{
  dtrace("%i, %p, <unused>, 0x%X", fd, file, mode);

  if (!(mode & GD_FILE_TEMP)) {
    if (file->mode & mode) {
      dreturn("%i", 0);
      return 0;
    } else if (file->idata >= 0)
      close(file->idata);

    file->idata = gd_OpenAt(file->D, fd, file->name, ((mode & GD_FILE_WRITE) ?
          (O_RDWR | O_CREAT) : O_RDONLY) | O_BINARY, 0666);
  } else
    file->idata = fd;

  file->pos = 0;
  file->mode = mode | GD_FILE_READ;

  dreturn("%i", file->idata < 0);
  return (file->idata < 0);
}

off64_t _GD_RawSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int mode __gd_unused)
{
  off64_t pos;

  dtrace("%p, %lli, 0x%X, <unused>", file, (long long)count, data_type);

  /* short circuit */
  if (file->pos == count) {
    dreturn("%lli", (long long)count);
    return count;
  }

  pos = lseek64(file->idata, count * GD_SIZE(data_type), SEEK_SET);

  if (pos == -1) {
    dreturn("%i", -1);
    return -1;
  }

  file->pos = count;

  dreturn("%lli", (long long)count);
  return count;
}

ssize_t _GD_RawRead(struct _gd_raw_file *file, void *ptr, gd_type_t data_type,
    size_t nmemb)
{
  ssize_t nread;

  dtrace("%p, %p, 0x%X, %zu", file, ptr, data_type, nmemb);

  nread = read(file->idata, ptr, nmemb * GD_SIZE(data_type));

  if (nread >= 0) {
    nread /= GD_SIZE(data_type);
    file->pos += nread;
  }

  dreturn("%zi", nread);
  return nread;
}

ssize_t _GD_RawWrite(struct _gd_raw_file *file, const void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t nwrote;

  dtrace("%p, %p, 0x%X, %zu", file, ptr, data_type, nmemb);

  nwrote = write(file->idata, ptr, nmemb * GD_SIZE(data_type));

  if (nwrote >= 0) {
    nwrote /= GD_SIZE(data_type);
    file->pos += nwrote;
  }

  dreturn("%zu", nwrote);
  return nwrote;
}

int _GD_RawSync(struct _gd_raw_file *file)
{
  return fsync(file->idata);
}

int _GD_RawClose(struct _gd_raw_file *file)
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

off64_t _GD_RawSize(int dirfd, struct _gd_raw_file *file, gd_type_t data_type,
    int swap __gd_unused)
{
  gd_stat64_t statbuf;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  if (gd_StatAt64(file->D, dirfd, file->name, &statbuf, 0) < 0)  {
    dreturn("%lli", -1LL);
    return -1;
  }

  dreturn("%lli", (long long)statbuf.st_size);
  return statbuf.st_size / GD_SIZE(data_type);
}
