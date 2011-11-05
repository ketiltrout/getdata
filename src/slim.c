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
 * open, idata = 0 otherwise idata = -1. */

int _GD_SlimOpen(int dirfd, int fd2 __gd_unused, struct _gd_raw_file* file,
    int swap __gd_unused, unsigned int mode __gd_unused)
{
  dtrace("%i, <unused>, %p, <unused>, <unused>", dirfd, file);

  {
#ifdef HAVE_SLIMDOPEN
    int fd;
    fd = gd_OpenAt(file->D, dirfd, file->name, ((mode & GD_FILE_WRITE) ?
          (O_RDWR | O_CREAT) : O_RDONLY) | O_BINARY, 0666);

    if (fd < 0) {
      dreturn("%i", 1);
      return 1;
    }
    file->edata = slimdopen(fd, "r");
#else
    char *filepath;
    /* this is easily broken, but the best we can do for now... */
    filepath = gd_MakeFullPath(file->D, dirfd, file->name);
    if (filepath == NULL) {
      dreturn("%i", 1);
      return 1;
    }

    file->edata = slimopen(filepath, "r");
    free(filepath);
#endif
  }

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->mode = GD_RDONLY;
  file->idata = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_SlimSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int mode __gd_unused)
{
  off64_t n;

  dtrace("%p, %lli, 0x%X, <unused>", file, (long long)count, data_type);

  n = (off64_t)slimseek((SLIMFILE *)file->edata, (off_t)count *
      GD_SIZE(data_type), SEEK_SET);

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
  ssize_t n;

  dtrace("%p, %p, 0x%X, %zu", file, ptr, data_type, nmemb);

  n = slimread(ptr, GD_SIZE(data_type), nmemb, (SLIMFILE *)file->edata);

  dreturn("%zu", n);
  return n;
}

int _GD_SlimClose(struct _gd_raw_file *file)
{
  int ret;

  dtrace("%p", file);

  ret = slimclose((SLIMFILE *)file->edata);
  if (!ret) {
    file->idata = -1;
    file->edata = NULL;
    file->mode = 0;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_SlimSize(int dirfd, struct _gd_raw_file *file, gd_type_t data_type,
    int swap __gd_unused)
{
  char *filepath;
  off64_t size;

  dtrace("%i, %p, 0x%X", dirfd, file, data_type);

  /* this is easily broken, but the best we can do for now... */
  filepath = gd_MakeFullPath(file->D, dirfd, file->name);
  if (filepath == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  size = slimrawsize(filepath);
  free(filepath);

  if (size < 0) {
    dreturn("%i", -1);
    return -1;
  }

  size /= GD_SIZE(data_type);

  dreturn("%lli", (long long)size);
  return size;
}
