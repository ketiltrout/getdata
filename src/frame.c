/* Copyright (C) 2012 D. V. Wiebe
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

struct gd_frame_data_t {
  uint64_t frame_size;
  uint64_t chunk_size;
  uint64_t offset;
  int have_format;
  int chunk_num;
  int dirfd;
  uint32_t cadence;
  gd_spf_t spf;
  FILE *fp;
};

int _GD_FrameName(DIRFILE *restrict D, unsigned int n_encdata,
    char *const *restrict encdata, unsigned int n_rawform,
    char *const *restrict rawform, struct _gd_raw_file *restrict file,
    const char *restrict base, gd_type_t type, gd_spf_t spf,
    int temp __gd_unused, int resolv)
{
  int int_len = 0;
  int have_format = 0;
  struct gd_frame_data_t *f;
  char *ptr;
  dtrace("%p, %u, %p, %u, %p, %p, \"%s\", 0x%X, %u, <unused>, %i", D, n_encdata,
      encdata, n_rawform, rawform, file, base, type, spf, resolv);

  /* Automatic detection of frame encoded data is not supported */
  if (resolv) {
    dreturn("%i", 1);
    return 1;
  }

  if (file->name == NULL) {
    if (n_encdata < 1)
      /* no filename specified */
      _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 0, NULL);
    else if (n_encdata < 2)
      /* no framelength specified */
      _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 1, NULL);
  
    if (D->error) {
      dreturn("%i", -1);
      return -1;
    }

    /* Check for invalid filename form */
    ptr = encdata[0];
    for (;;) {
      ptr = strchr(ptr, '%');
      if (ptr == NULL)
        break;
      ptr++;
      if (*ptr == '%')
        continue;
      else if (have_format) {
        _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 0, NULL);
        break;
      } else if (*ptr >= '0' && *ptr <= '9') {
        int_len = *ptr++ - '0';
        if (*ptr >= '0' && *ptr <= '9') {
          int_len = 10 * int_len + *ptr++ - '0';
          if (*ptr >= '0' && *ptr <= '9') {
            int_len *= 10 * int_len + *ptr++ - '0';
            if (*ptr >= '0' && *ptr <= '9')
              int_len = -1;
          }
        }

        if (int_len > 20 || int_len < 1) {
          _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 0, NULL);
          break;
        }
      }

      if (*ptr == 'x' || *ptr == 'u' || *ptr == 'X' || *ptr == 'o') {
        have_format = 1;
        continue;
      } else {
        _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 0, NULL);
        break;
      }
    }

    if (D->error) {
      dreturn("%i", -1);
      return -1;
    }

    file->D = D;
    file->name = strdup(encdata[0]);
    file->edata = malloc(sizeof(struct gd_frame_data_t));
    if (file->name == NULL || file->edata == NULL) {
      free(file->name);
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }

    /* save the metadata */
    f = (struct gd_frame_data_t*)file->edata;
    f->frame_size = gd_strtoll(encdata[1], &ptr, 0);
    if (*ptr)
      _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 1, NULL);

    if (n_rawform > 1) {
      f->chunk_size = gd_strtoll(rawform[2], &ptr, 0);
      if (*ptr || (!have_format && f->chunk_size > 0))
        _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 2, NULL);
    } else if (have_format)
      _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_GLOBAL, base, 2, NULL);
    else
      f->chunk_size = 0;

    if (n_rawform > 0) {
      f->offset = gd_strtoll(rawform[0], &ptr, 0);
      if (*ptr)
        _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_FIELD, base, 0, NULL);
    } else
      f->offset = 0;

    if (n_rawform > 1) {
      f->cadence = strtol(rawform[1], &ptr, 0);
      if (*ptr)
        _GD_SetError(D, GD_E_ENCDATA, GD_E_ENCDATA_FIELD, base, 1, NULL);
    } else
      f->cadence = GD_SIZE(type);
    
    f->have_format = have_format;
    f->spf = spf;
      
    if (D->error) {
      free(file->name);
      free(file->edata);
      dreturn("%i", -1);
      return -1;
    }
  }

  dreturn("%i (%s)", 0, file->name);
  return 0;
}

static int _GD_FrameOpenChunk(int dirfd, struct _gd_raw_file *file,
    struct gd_frame_data_t *f, int n)
{
  char *filename;
  int fd;
  size_t len = strlen(file->name) + 20;
  dtrace("%i, %p, %p, %i", dirfd, file, f, n);

  if (f->have_format) {
    filename = (char *)malloc(len);
    if (filename == NULL) {
      dreturn("%i", -1);
      return -1;
    }
    if (snprintf(filename, len, file->name, n) < 0) {
      free(filename);
      dreturn("%i", -1);
      return -1;
    }
  } else
    filename = file->name;

  fd = gd_OpenAt(file->D, dirfd, filename, O_RDONLY | O_BINARY, 0666);

  if (f->have_format)
    free(filename);

  if (fd >= 0) {
    f->chunk_num = n;
    f->dirfd = dirfd;
  }

  dreturn("%i", fd);
  return fd;
}

int _GD_FrameOpen(int dirfd, struct _gd_raw_file *file, int swap __gd_unused,
    unsigned int mode __gd_unused)
{
  struct gd_frame_data_t *f = file->edata;
  dtrace("%i, %p, <unused>, <unused>", dirfd, file);

  /* opening chunk zero is probably the wrong thing to do, but we've got to
   * start somewhere. */
  file->idata = _GD_FrameOpenChunk(dirfd, file, f, 0);

  if (file->idata < 0) {
    dreturn("%i", 1);
    return 1;
  }

  file->mode = GD_FILE_READ;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_FrameSeek(struct _gd_raw_file *file, off64_t count,
    gd_type_t data_type, unsigned int mode __gd_unused)
{
  off64_t frame_num, n;
  struct gd_frame_data_t *f = file->edata;

  dtrace("%p, %lli, 0x%X, <unused>", file, (long long)count, data_type);

  /* calculate chunk number */
  if (f->chunk_size > 0) {
    int chunk_num = count / (f->spf * f->chunk_size);
    if (chunk_num != f->chunk_num) {
      int old_fd = file->idata;
      file->idata = _GD_FrameOpenChunk(f->dirfd, file, f, 0);
      if (file->idata < 0) {
        dreturn("%i", 0);
        return 0;
      }
      close(old_fd);
    }
  }

  /* find the frame we're interested in */
  frame_num = count / f->spf - f->chunk_num * f->chunk_size;

  n = lseek64(file->idata, frame_num * f->frame_size, SEEK_SET);

  if (n < 0) {
    dreturn("%i", -1);
    return -1;
  }

  /* if we ended up in the middle of a frame, go back to the beginning */
  if (n % f->frame_size) {
    n = lseek64(file->idata, -(n % f->frame_size), SEEK_CUR);
    /* don't put up with flakey I/O */
    if (n < 0 || n % f->frame_size) {
      dreturn("%i", -1);
      return -1;
    }
  }

  /* calculate resultant sample number */
  file->pos = (n / f->frame_size) * f->spf;


  dreturn("%lli", (long long)file->pos);
  return file->pos;
}

ssize_t _GD_FrameRead(struct _gd_raw_file *restrict file, void *restrict data,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;

  dtrace("%p, %p, 0x%X, %zu", file, data, data_type, nmemb);

  n = zzip_read(file->edata, data, GD_SIZE(data_type) * nmemb);

  if (n >= 0)
    n /= GD_SIZE(data_type);

  dreturn("%lli", (long long)n);
  return n;
}

int _GD_FrameClose(struct _gd_raw_file *file)
{
  int ret;

  dtrace("%p", file);

  ret = zzip_close(file->edata);

  if (!ret) {
    file->idata = -1;
    file->edata = NULL;
  }

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_FrameSize(int dirfd, struct _gd_raw_file *file, gd_type_t data_type,
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
  ptr2 = realloc(ptr1, len + strlen(file->name) + 2);
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
