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

int _GD_SlimOpen(struct _gd_private_entry* entry, const char* name,
    int mode __gd_unused, int creat __gd_unused)
{
  char slimname[FILENAME_MAX];

  dtrace("%p, \"%s\"", entry, name);

  snprintf(slimname, FILENAME_MAX, "%s.slm", name);
  entry->edata = slimopen(slimname, "r" /* writing not supported */);

  if (entry->edata != NULL) {
    entry->fp = 0;
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

off64_t _GD_SlimSeek(struct _gd_private_entry* entry, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  dtrace("%p, %lli, %x", entry, (long long)count, data_type);

  off64_t n = (off64_t)slimseek(entry->edata, (off_t)count *
      GD_SIZE(data_type), SEEK_SET);

  dreturn("%lli", (long long)n);
  return n;
}

ssize_t _GD_SlimRead(struct _gd_private_entry *entry, void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  dtrace("%p, %p, %x, %zi", entry, ptr, data_type, nemb);

  ssize_t n = slimread(ptr, GD_SIZE(data_type), nmemb, entry->edata);

  dreturn("%zi", n);
  return n;
}

int _GD_SlimClose(struct _gd_private_entry *entry)
{
  dtrace("%p", entry);

  int ret = slimclose(entry->edata);
  if (!ret) {
    entry->fp = -1;
    entry->edata = NULL;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_SlimSize(const char *name, gd_type_t data_type)
{
  char slimname[FILENAME_MAX];
  off64_t size;

  dtrace("\"%s\", %x", name, data_type);

  snprintf(slimname, FILENAME_MAX, "%s.slm", name);
  size = slimrawsize(slimname) / GD_SIZE(data_type);

  dreturn("%lli", (long long)size);
  return size;
}

int _GD_SlimUnlink(const char* name)
{
  return _GD_GenericUnlink(name, ".slm");
}
