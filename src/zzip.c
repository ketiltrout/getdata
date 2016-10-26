/* Copyright (C) 2011, 2012, 2013, 2015, 2016 D. V. Wiebe
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

struct gd_zzipdata {
  ZZIP_DIR *dir;
  ZZIP_FILE *file;
  zzip_error_t err;
};

/* A ZZip "name" looks like this:
 *
 * <enc_data>.zip\0<base>
 */
int _GD_ZzipName(DIRFILE *restrict D gd_unused_, const char *restrict enc_data,
    struct gd_raw_file_ *restrict file, const char *restrict base,
    int temp gd_unused_, int resolv gd_unused_)
{
  size_t enc_len;

  dtrace("%p, \"%s\", %p, \"%s\", <unused>, <unused>", D, enc_data, file, base);

  if (file->name) {
    dreturn("%i (%s)", 0, file->name);
    return 0;
  }

  if (enc_data == NULL)
    enc_data = "raw";

  enc_len = strlen(enc_data);

  file->name = (char*)malloc(enc_len + 5 + strlen(base) + 1);
  if (file->name == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  sprintf(file->name, "%s.zip_%s", enc_data, base);
  /* Now replace the '_' with a NUL */
  file->name[enc_len + 4] = '\0';

  dreturn("%i (%s\\0%s)", 0, file->name, file->name + enc_len + 5);
  return 0;
}

/* This function opens both a ZZIP_DIR (the containing archive) and a ZZIP_FILE,
 * the target data file
 */
static struct gd_zzipdata *_GD_ZzipDoOpen(int dirfd, struct gd_raw_file_* file)
{
  struct gd_zzipdata *gdzz;
  int fd;

  dtrace("%i, %p", dirfd, file);

  /* open the zip file.  */
  if ((fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666)) < 0) {
    file->error = -1;
    dreturn("%p", NULL);
    return NULL;
  }

  if ((gdzz = malloc(sizeof *gdzz)) == NULL) {
    close(fd);
    file->error = ENOMEM;
    dreturn("%p", NULL);
    return NULL;
  }

  /* Pass file to libzzip. */
  if ((gdzz->dir = zzip_dir_fdopen(fd, &gdzz->err)) == NULL) {
    file->error = gdzz->err;
    close(fd);
    free(gdzz);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Now open the data file inside the archive, it's name is at the tail end of
   * file->name after the NUL */
  if ((gdzz->file = zzip_file_open(gdzz->dir,
          file->name + strlen(file->name) + 1, O_RDONLY)) == NULL)
  {
    file->error = gdzz->err;
    zzip_dir_close(gdzz->dir);
    free(gdzz);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", gdzz);
  return gdzz;
}

int _GD_ZzipOpen(int dirfd, struct gd_raw_file_* file,
    gd_type_t data_type gd_unused_, int swap gd_unused_,
    unsigned int mode gd_unused_)
{
  dtrace("%i, %p, <unused>, <unused>, <unused>", dirfd, file);

  file->edata = _GD_ZzipDoOpen(dirfd, file);

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

off64_t _GD_ZzipSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int mode gd_unused_)
{
  off64_t n;

  struct gd_zzipdata *gdzz = file->edata;

  dtrace("%p, %" PRId64 ", 0x%X, <unused>", file, (int64_t)count, data_type);

  n = (off64_t)zzip_seek(gdzz->file, (zzip_off_t)(count * GD_SIZE(data_type)),
      SEEK_SET);

  if (n < 0) {
    /* Maybe we tried to seek past the EOF? */
    n = (off64_t)zzip_seek(gdzz->file, 0, SEEK_END);

    if (n > count * GD_SIZE(data_type)) {
      /* we didn't: there was just an error; try again */
      n = (off64_t)zzip_seek(gdzz->file,
          (zzip_off_t)(count * GD_SIZE(data_type)), SEEK_SET);
    }
  }
    
  if (n < 0)
    file->error = gdzz->err;
  else
    n /= GD_SIZE(data_type);

  file->pos = n;

  dreturn("%" PRId64, (int64_t)n);
  return n;
}

ssize_t _GD_ZzipRead(struct gd_raw_file_ *restrict file, void *restrict data,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;

  struct gd_zzipdata *gdzz = file->edata;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, data, data_type, nmemb);

  n = zzip_read(gdzz->file, data, GD_SIZE(data_type) * nmemb);

  if (n < 0)
    file->error = gdzz->err;
  else 
    n /= GD_SIZE(data_type);

  file->pos += n;

  dreturn("%" PRId64, (int64_t)n);
  return n;
}

int _GD_ZzipClose(struct gd_raw_file_ *file)
{
  struct gd_zzipdata *gdzz = file->edata;

  dtrace("%p", file);

  if (zzip_file_close(gdzz->file) || zzip_dir_close(gdzz->dir)) {
    file->error = gdzz->err;
    dreturn("%i", 1);
    return 1;
  }

  file->idata = -1;
  free(file->edata);
  file->edata = NULL;

  dreturn("%i", 0);
  return 0;
}

off64_t _GD_ZzipSize(int dirfd, struct gd_raw_file_ *file, gd_type_t data_type,
    int swap gd_unused_)
{
  off64_t size = 0;
  struct gd_zzipdata *gdzz;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  gdzz = _GD_ZzipDoOpen(dirfd, file);

  if (gdzz == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  if ((size = (off64_t)zzip_seek(gdzz->file, 0, SEEK_END)) == -1)
    file->error = gdzz->err;
  else
    size /= GD_SIZE(data_type);

  zzip_file_close(gdzz->file);
  zzip_dir_close(gdzz->dir);
  free(gdzz);

  dreturn("%" PRId64, (int64_t)size);
  return size;
}

int _GD_ZzipStrerr(const struct gd_raw_file_ *file, char *buf, size_t buflen)
{
  int r = 0;

  dtrace("%p, %p, %" PRIuSIZE, file, buf, buflen);

  switch (file->error) {
    case ZZIP_OUTOFMEM:
      strncpy(buf, "ZZIP: Out of memory", buflen);
      break;
    case ZZIP_DIR_OPEN:
    case ZZIP_DIR_STAT:
    case ZZIP_DIR_SEEK:
    case ZZIP_DIR_READ:
    case -1:
      r = gd_StrError(errno, buf, buflen);
      break;
    default:
      snprintf(buf, buflen, "ZZIP: Unkown error 0x%X", file->error);
      break;
  }

  dreturn("%i", r);
  return r;
}
