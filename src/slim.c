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

#ifdef USE_MODULES
#define _GD_SlimOpen libgetdataslim_LTX_GD_SlimOpen
#define _GD_SlimSeek libgetdataslim_LTX_GD_SlimSeek
#define _GD_SlimRead libgetdataslim_LTX_GD_SlimRead
#define _GD_SlimClose libgetdataslim_LTX_GD_SlimClose
#define _GD_SlimSize libgetdataslim_LTX_GD_SlimSize
#endif

/* The slim encoding scheme uses edata as a slimfile pointer.  If a file is
 * open, fp = 0 otherwise fp = -1. */

int _GD_SlimOpen(struct _gd_raw_file* file, int mode __gd_unused,
    int creat __gd_unused)
{
  dtrace("%p, <unused>, <unused>", file);

  file->edata = slimopen(file->name, "r" /* writing not supported */);

  if (file->edata != NULL) {
    file->fp = 0;
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

off64_t _GD_SlimSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  dtrace("%p, %lli, %x, <unused>", file, (long long)count, data_type);

  off64_t n = (off64_t)slimseek(file->edata, (off_t)count * GD_SIZE(data_type),
      SEEK_SET);

  if (n == -1) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%lli", (long long)(n / GD_SIZE(data_type)));
  return n;
}

ssize_t _GD_SlimRead(struct _gd_raw_file *file, void *ptr, gd_type_t data_type,
    size_t nmemb)
{
  dtrace("%p, %p, %x, %zi", file, ptr, data_type, nmemb);

  ssize_t n = slimread(ptr, GD_SIZE(data_type), nmemb, file->edata);

  dreturn("%zi", n);
  return n;
}

int _GD_SlimClose(struct _gd_raw_file *file)
{
  dtrace("%p", file);

  int ret = slimclose(file->edata);
  if (!ret) {
    file->fp = -1;
    file->edata = NULL;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_SlimSize(struct _gd_raw_file *file, gd_type_t data_type)
{
  off64_t size;

  dtrace("%p, %x", file, data_type);

  size = slimrawsize(file->name);

  if (size < 0) {
    dreturn("%i", -1);
    return -1;
  }
  
  size /= GD_SIZE(data_type);

  dreturn("%lli", (long long)size);
  return size;
}
