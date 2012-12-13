/* Copyright (C) 2008, 2010, 2011, 2012 D. V. Wiebe
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

#ifdef HAVE_SLIMLIB_H
#include <slimlib.h>
#endif

#ifdef USE_MODULES
#define _GD_ZzslimName libgetdatazzslim_LTX_GD_ZzslimName
#define _GD_ZzslimOpen libgetdatazzslim_LTX_GD_ZzslimOpen
#define _GD_ZzslimSeek libgetdatazzslim_LTX_GD_ZzslimSeek
#define _GD_ZzslimRead libgetdatazzslim_LTX_GD_ZzslimRead
#define _GD_ZzslimClose libgetdatazzslim_LTX_GD_ZzslimClose
#define _GD_ZzslimSize libgetdatazzslim_LTX_GD_ZzslimSize
#endif

/* The zzslim encoding scheme uses edata as a slimfile pointer.  If a file is
 * open, idata = 0 otherwise idata = -1. */

int _GD_ZzslimName(DIRFILE *restrict D, const char *restrict enc_data,
    struct _gd_raw_file *restrict file, const char *restrict base,
    int temp __gd_unused, int resolv)
{
  size_t enc_len;

  dtrace("%p, \"%s\", %p, \"%s\", <unused>, %i", D, enc_data, file, base,
      resolv);

  if (enc_data == NULL)
    enc_data = "raw";

  enc_len = strlen(enc_data);
  
  /* Resolution is degenerate with the zzip encoding; so skip it for now */
  if (resolv) {
    dreturn("%i", 1);
    return 1;
  }

  if (file->name == NULL) {
    file->D = D;
    file->name = (char *)malloc(strlen(base) + strlen(enc_data) + 6);
    if (file->name == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }

    strcpy(file->name, enc_data);
    file->name[enc_len] = '/';
    strcat(strcpy(file->name + enc_len + 1, base), ".slm");
  }

  dreturn("%i (%s)", 0, file->name);
  return 0;
}

int _GD_ZzslimOpen(int dirfd, struct _gd_raw_file* file, int swap __gd_unused,
    unsigned int mode __gd_unused)
{
  char *filepath;

  dtrace("%i, %p, <unused>, <unused>", dirfd, file);

  /* this is easily broken, but the best we can do for now... */
  filepath = gd_MakeFullPathOnly(file->D, dirfd, file->name);
  if (filepath == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->edata = slimopen(filepath, "r");
  free(filepath);

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->mode = GD_RDONLY;
  file->idata = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_ZzslimSeek(struct _gd_raw_file* file, off64_t count,
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

ssize_t _GD_ZzslimRead(struct _gd_raw_file *restrict file, void *restrict ptr,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;

  dtrace("%p, %p, 0x%X, %" PRNsize_t, file, ptr, data_type, nmemb);

  n = slimread(ptr, GD_SIZE(data_type), nmemb, (SLIMFILE *)file->edata);

  dreturn("%" PRNsize_t, n);
  return n;
}

int _GD_ZzslimClose(struct _gd_raw_file *file)
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

off64_t _GD_ZzslimSize(int dirfd, struct _gd_raw_file *file,
    gd_type_t data_type, int swap __gd_unused)
{
  char *filepath;
  off64_t size;

  dtrace("%i, %p, 0x%X", dirfd, file, data_type);

  /* this is easily broken, but the best we can do for now... */
  filepath = gd_MakeFullPathOnly(file->D, dirfd, file->name);
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
