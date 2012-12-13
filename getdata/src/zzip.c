/* Copyright (C) 2011 D. V. Wiebe
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

#ifdef HAVE_ZZIP_LIB_H
#include <zzip/lib.h>
#endif

#ifdef USE_MODULES
#define _GD_ZzipName libgetdatazzip_LTX_GD_ZzipName
#define _GD_ZzipOpen libgetdatazzip_LTX_GD_ZzipOpen
#define _GD_ZzipSeek libgetdatazzip_LTX_GD_ZzipSeek
#define _GD_ZzipRead libgetdatazzip_LTX_GD_ZzipRead
#define _GD_ZzipClose libgetdatazzip_LTX_GD_ZzipClose
#define _GD_ZzipSize libgetdatazzip_LTX_GD_ZzipSize
#endif

/* The zzip encoding scheme looks just like the regular ol' C IO. */

int _GD_ZzipName(DIRFILE *restrict D, const char *restrict enc_data,
    struct _gd_raw_file *restrict file, const char *restrict base,
    int temp __gd_unused, int resolv)
{
  size_t enc_len;

  dtrace("%p, \"%s\", %p, \"%s\", <unused>, %i", D, enc_data, file, base,
      resolv);

  if (enc_data == NULL)
    enc_data = "raw";

  enc_len = strlen(enc_data);
  
  if (resolv) {
    free(file->name);
    file->name = (char*)malloc(enc_len + 5);
    if (file->name == NULL) {
      dreturn("%i", -1);
      return -1;
    }

    strcpy(file->name, enc_data);
    strcpy(file->name + enc_len, ".zip");

    dreturn("%i (%s)", 0, file->name);
    return 0;
  }

  if (file->name == NULL) {
    file->D = D;
    file->name = (char *)malloc(strlen(base) + strlen(enc_data) + 2);
    if (file->name == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }

    strcpy(file->name, enc_data);
    file->name[enc_len] = '/';
    strcpy(file->name + enc_len + 1, base);
  }

  dreturn("%i (%s)", 0, file->name);
  return 0;
}

int _GD_ZzipOpen(int dirfd, struct _gd_raw_file* file, int swap __gd_unused,
    unsigned int mode __gd_unused)
{
  char *ptr1, *ptr2;
  size_t len;

  dtrace("%i, %p, <unused>, <unused>", dirfd, file);

  /* zziplib doesn't seem to have a way to do these path shenannigans nicely */
  ptr1 = gd_MakeFullPathOnly(file->D, dirfd, "");
  if (ptr1 == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  len = strlen(ptr1);
  ptr2 = (char*)realloc(ptr1, len + strlen(file->name) + 2);
  if (ptr2 == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  ptr2[len] = '/';
  strcpy(ptr2 + len + 1, file->name);

  file->edata = zzip_open(ptr2, O_RDONLY);
  free(ptr2);

  if (file->edata == NULL) {
    file->idata = -1;
    dreturn("%i", 1);
    return 1;
  }

  file->idata = 0;
  file->mode = GD_FILE_READ;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_ZzipSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int mode __gd_unused)
{
  off64_t n;

  dtrace("%p, %lli, 0x%X, <unused>", file, (long long)count, data_type);

  n = file->pos = (off64_t)zzip_seek((ZZIP_FILE*)file->edata,
      (off_t)(count * GD_SIZE(data_type)), SEEK_SET);

  if (n >= 0)
    n /= GD_SIZE(data_type);

  dreturn("%lli", (long long)n);
  return n;
}

ssize_t _GD_ZzipRead(struct _gd_raw_file *restrict file, void *restrict data,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;

  dtrace("%p, %p, 0x%X, %" PRNsize_t, file, data, data_type, nmemb);

  n = zzip_read((ZZIP_FILE*)file->edata, data, GD_SIZE(data_type) * nmemb);

  if (n >= 0)
    n /= GD_SIZE(data_type);

  dreturn("%lli", (long long)n);
  return n;
}

int _GD_ZzipClose(struct _gd_raw_file *file)
{
  int ret;

  dtrace("%p", file);

  ret = zzip_close((ZZIP_FILE*)file->edata);

  if (!ret) {
    file->idata = -1;
    file->edata = NULL;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_ZzipSize(int dirfd, struct _gd_raw_file *file, gd_type_t data_type,
    int swap __gd_unused)
{
  ssize_t len;
  char *ptr1, *ptr2;
  ZZIP_FILE *fp;
  off64_t size = 0;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  /* zziplib doesn't seem to have a way to do these path shenannigans nicely */
  ptr1 = gd_MakeFullPathOnly(file->D, dirfd, "");
  if (ptr1 == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  len = strlen(ptr1);
  ptr2 = (char*)realloc(ptr1, len + strlen(file->name) + 2);
  if (ptr2 == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  ptr2[len] = '/';
  strcpy(ptr2 + len + 1, file->name);

  fp = zzip_open(ptr2, O_RDONLY);
  free(ptr2);

  if (fp == NULL) {
    file->idata = -1;
    dreturn("%i", -1);
    return -1;
  }

  if ((size = (off64_t)zzip_seek(fp, 0, SEEK_END)) == -1) {
    dreturn("%i", -1);
    return -1;
  }

  zzip_close(fp);

  size /= GD_SIZE(data_type);

  dreturn("%lli", (long long)size);
  return size;
}
