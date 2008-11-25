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
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#endif

int _GD_RawOpen(struct _gd_raw_file* file, const char* base, int mode,
    int creat)
{
  dtrace("%p, \"%s\", %i, %i", file, base, mode, creat);

  if (GD_SetEncodedName(file, base, 0)) {
    dreturn("%i", -1);
    return -1;
  }

  file->fp = open(file->name, ((mode == GD_RDWR) ? O_RDWR : O_RDONLY) |
      (creat ? O_CREAT : 0), 0666);

  dreturn("%i", file->fp < 0);
  return (file->fp < 0);
}

off64_t _GD_RawSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  dtrace("%p, %lli, %x, <unused>", file, (long long)count, data_type);

  off64_t pos =  lseek64(file->fp, count * GD_SIZE(data_type), SEEK_SET);

  if (pos == -1) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%lli", (long long)count);
  return count;
}

ssize_t _GD_RawRead(struct _gd_raw_file *file, void *ptr, gd_type_t data_type,
    size_t nmemb)
{
  dtrace("%p, %p, %x, %zi", file, ptr, data_type, nmemb);

  int nread = read(file->fp, ptr, nmemb * GD_SIZE(data_type));

  if (nread >= 0)
    nread /= GD_SIZE(data_type);

  dreturn("%zi", nread);
  return nread;
}

ssize_t _GD_RawWrite(struct _gd_raw_file *file, const void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  dtrace("%p, %p, %x, %zi", file, ptr, data_type, nmemb);

  ssize_t nwrote = write(file->fp, ptr, nmemb * GD_SIZE(data_type));

  if (nwrote >= 0)
    nwrote /= GD_SIZE(data_type);

  dreturn("%zi", nwrote);
  return nwrote;
}

int _GD_RawSync(struct _gd_raw_file *file)
{
  return fsync(file->fp);
}

int _GD_RawClose(struct _gd_raw_file *file)
{
  dtrace("%p", file);

  int ret = close(file->fp);
  if (!ret)
    file->fp = -1;

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_RawSize(struct _gd_raw_file *file, const char *base,
    gd_type_t data_type)
{
  struct stat64 statbuf;
  dtrace("%p, \"%s\", %x", file, base, data_type);

  if (GD_SetEncodedName(file, base, 0)) {
    dreturn("%i", -1);
    return -1;
  }

  if (stat64(file->name, &statbuf) < 0)  {
    dreturn("%lli", -1LL);
    return -1;
  }

  dreturn("%lli", (long long)statbuf.st_size);
  return statbuf.st_size / GD_SIZE(data_type);
}

int _GD_RawTemp(struct _gd_raw_file *file, int mode)
{
  dtrace("%p, %i", file, mode);

  int move_error = 0;

  switch(mode) {
    case GD_TEMP_OPEN:
      if (GD_SetEncodedName(file + 1, file->name, 1)) {
        dreturn("%i", -1);
        return -1;
      }

      file[1].fp = mkstemp(file[1].name);

      if (file[1].fp == -1) {
        dreturn("%i", -1);
        return -1;
      }
      break;
    case GD_TEMP_MOVE:
      if (!rename(file[1].name, file[0].name)) {
        free(file[1].name);
        file[1].name = NULL;
        dreturn("%i", 0);
        return 0;
      }
      /* fallthrough on error */
    case GD_TEMP_DESTROY:
      if (file[1].name != NULL) {
        if (file[1].fp >= 0)
          if (_GD_AsciiClose(file + 1)) {
            dreturn("%i", -1);
            return -1;
          }

        if (unlink(file[1].name)) {
          dreturn("%i", -1);
          return -1;
        }

        if (mode == GD_TEMP_MOVE) {
          errno = move_error;
          dreturn("%i", -1);
          return -1;
        }
        free(file[1].name);
        file[1].name = NULL;
      }
      break;
  }

  dreturn("%i", 0);
  return 0;
}
