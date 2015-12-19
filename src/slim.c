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
#ifdef ZZSLIM
#define GD_SLIM(x) _GD_Zzslim ## x
#else
#define GD_SLIM(x) _GD_Slim ## x
#include "internal.h"
#endif

#ifdef HAVE_SLIMLIB_H
#include <slimlib.h>
#endif

struct gd_slimdata {
  SLIMFILE *f;
  char *filepath;
};

static struct gd_slimdata *GD_SLIM(DoOpen)(int dirfd, struct gd_raw_file_* file)
{
  struct gd_slimdata *gdsl;

  dtrace("%i, %p", dirfd, file);

  /* slimdopen in slimlib-2.6.7 and earlier contains a bug resulting in a SEGV,
   * so disable this for now */
#if 0
  /* #ifdef HAVE_SLIMDOPEN */
  {
    int fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666);
    if (fd < 0) {
      dreturn("%i", -1);
      return -1;
    }

    file->edata = slimdopen(fd, "r");

    if (file->edata == NULL) {
      close(fd);
      dreturn("%i", 1);
      return 1;
    }
  }
#else
  {
    gdsl = malloc(sizeof *gdsl);
    if (gdsl == NULL) {
      file->error = ENOMEM;
      dreturn("%p", NULL);
      return NULL;
    }

    /* this is easily broken, but the best we can do in this case */
    gdsl->filepath = gd_MakeFullPathOnly(file->D, dirfd, file->name);
    if (gdsl->filepath == NULL) {
      file->error = errno;
      free(gdsl);
      dreturn("%p", NULL);
      return NULL;
    }

    gdsl->f = slimopen(gdsl->filepath, "r");
  }
#endif

  if (gdsl->f == NULL) {
    free(gdsl->filepath);
    free(gdsl);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", gdsl);
  return gdsl;
}

int GD_SLIM(Open)(int dirfd, struct gd_raw_file_* file,
    gd_type_t data_type gd_unused_, int swap gd_unused_,
    unsigned int mode gd_unused_)
{
  dtrace("%i, %p, <unused>, <unused>, <unused>", dirfd, file);

  file->edata = GD_SLIM(DoOpen)(dirfd, file);

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->mode = GD_RDONLY;
  file->idata = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t GD_SLIM(Seek)(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int mode gd_unused_)
{
  struct gd_slimdata *gdsl = file->edata;

  dtrace("%p, %" PRId64 ", 0x%X, <unused>", file, (int64_t)count, data_type);

  /* slimlib appears to do a rewind before every SEEK_SET ! */
  if (slimseek(gdsl->f, (long)count * GD_SIZE(data_type), SEEK_SET))
  {
    /* Handle seeks past the EOF */
    off64_t size = slimrawsize(gdsl->filepath) / GD_SIZE(data_type);
    if (count < size || slimseek(gdsl->f, size, SEEK_SET)) {
      dreturn("%i", -1);
      return -1;
    }
    count = size;
  }

  file->pos = count;
  dreturn("%" PRId64, (int64_t)count);
  return count;
}

ssize_t GD_SLIM(Read)(struct gd_raw_file_ *restrict file, void *restrict ptr,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;
  struct gd_slimdata *gdsl = file->edata;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nmemb);

  n = slimread(ptr, GD_SIZE(data_type), nmemb, gdsl->f);

  dreturn("%" PRIuSIZE, n);
  return n;
}

int GD_SLIM(Close)(struct gd_raw_file_ *file)
{
  int ret;
  struct gd_slimdata *gdsl = file->edata;

  dtrace("%p", file);

  ret = slimclose(gdsl->f);
  if (!ret) {
    file->idata = -1;
    free(gdsl->filepath);
    free(file->edata);
    file->edata = NULL;
    file->mode = 0;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t GD_SLIM(Size)(int dirfd, struct gd_raw_file_ *file, gd_type_t data_type,
    int swap gd_unused_)
{
  off64_t size;

  dtrace("%i, %p, 0x%X", dirfd, file, data_type);

  /* slimdrawsize in slimlib-2.6.7 and earlier contains a bug resulting in a
   * SEGV, so disable this for now */
#if 0
  /* #ifdef HAVE_SLIMDRAWSIZE */
  {
    int fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666);
    if (fd < 0) {
      dreturn("%i", -1);
      return -1;
    }

    size = slimdrawsize(fd);
    close(fd);
  }
#else
  {
    /* this is easily broken, but the best we can do in this case */
    char *filepath = gd_MakeFullPathOnly(file->D, dirfd, file->name);
    if (filepath == NULL) {
      dreturn("%i", -1);
      return -1;
    }

    size = slimrawsize(filepath);
    free(filepath);
  }
#endif

  if (size < 0) {
    dreturn("%i", -1);
    return -1;
  }

  size /= GD_SIZE(data_type);

  dreturn("%" PRId64, (int64_t)size);
  return size;
}

int GD_SLIM(Strerr)(const struct gd_raw_file_ *file, char *buf, size_t buflen)
{
  int r = 0;

  dtrace("%p, %p, %" PRIuSIZE, file, buf, buflen);

  if (file->error)
    r = gd_StrError(file->error, buf, buflen);
  else /* the slimlib C API has no error reporting */
    strncpy(buf, "SLIMLIB: Unspecified error", buflen);

  dreturn("%i", r);
  return r;
}
