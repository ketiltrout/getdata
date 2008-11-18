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

int _GD_RawOpen(struct _gd_private_entry* entry, const char* name, int mode,
    int creat)
{
  dtrace("%p, \"%s\", %i, %i", entry, name, mode, creat);
  entry->fp = open(name,
      ((mode == GD_RDWR) ? O_RDWR : O_RDONLY) | (creat ? O_CREAT : 0), 0666);

  dreturn("%i", entry->fp < 0);
  return (entry->fp < 0);
}

off64_t _GD_RawSeek(struct _gd_private_entry* entry, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  return lseek64(entry->fp, count * GD_SIZE(data_type), SEEK_SET);
}

ssize_t _GD_RawRead(struct _gd_private_entry *entry, void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  return read(entry->fp, ptr, nmemb * GD_SIZE(data_type));
}

ssize_t _GD_RawWrite(struct _gd_private_entry *entry, const void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  return write(entry->fp, ptr, nmemb * GD_SIZE(data_type));
}

int _GD_RawSync(struct _gd_private_entry *entry)
{
  return fsync(entry->fp);
}

int _GD_RawClose(struct _gd_private_entry *entry)
{
  dtrace("%p", entry);

  int ret = close(entry->fp);
  if (!ret)
    entry->fp = -1;

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_RawSize(const char *name, gd_type_t data_type)
{
  struct stat64 statbuf;
  dtrace("\"%s\"", name);

  if (stat64(name, &statbuf) < 0)  {
    dreturn("%lli", -1LL);
    return -1;
  }

  dreturn("%lli", (long long)statbuf.st_size);
  return statbuf.st_size / GD_SIZE(data_type);
}

int _GD_RawTouch(const char* name)
{
  return _GD_GenericTouch(name, "");
}

int _GD_RawUnlink(const char* name)
{
  return _GD_GenericUnlink(name, "");
}
