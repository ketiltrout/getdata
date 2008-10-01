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
#endif

#ifdef HAVE_SLIMLIB_H
#include <slimlib.h>
#endif

int _GD_SlimOpen(union _gd_private_entry* entry, const char* name, int mode,
    int creat)
{
  (void)mode;
  (void)creat;
  char slimname[FILENAME_MAX];
  snprintf(slimname, FILENAME_MAX, "%s.slm", name);
  entry->stream = slimopen(slimname, "r" /* writing not supported */);

  if (entry->stream != NULL) {
    entry->fp = 0;
    return 0;
  }

  return 1;
}

off64_t _GD_SlimSeek(union _gd_private_entry* entry, off64_t count,
    gd_type_t data_type, int pad)
{
  (void)pad;
  return (off64_t)slimseek(entry->stream, (off_t)count * GD_SIZE(data_type),
      SEEK_SET);
}

ssize_t _GD_SlimRead(union _gd_private_entry *entry, void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  return slimread(ptr, GD_SIZE(data_type), nmemb, entry->stream);
}

int _GD_SlimClose(union _gd_private_entry *entry)
{
  int ret = slimclose(entry->stream);
  if (!ret) {
    entry->fp = -1;
    entry->stream = NULL;
  }
  return ret;
}

off64_t _GD_SlimSize(const char *name, gd_type_t data_type)
{
  char slimname[FILENAME_MAX];
  snprintf(slimname, FILENAME_MAX, "%s.slm", name);
  return slimrawsize(slimname) / GD_SIZE(data_type);
}
