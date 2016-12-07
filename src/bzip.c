/* Copyright (C) 2008-2011, 2013, 2014, 2015, 2016 D. V. Wiebe
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

#ifdef HAVE_BZLIB_H
#include <bzlib.h>
#endif

#if SIZEOF_INT < 4
#define GD_BZIP_BUFFER_SIZE 32767
#else
#define GD_BZIP_BUFFER_SIZE 1000000
#endif

struct gd_bzdata {
  BZFILE* bzfile;
  FILE* stream;
  int bzerror;
  int stream_end;
  int pos, end;
  off64_t base;
  char data[GD_BZIP_BUFFER_SIZE];
};

/* The bzip encoding scheme uses edata as a gd_bzdata pointer.  If a file is
 * open, idata = 0 otherwise idata = -1. */

static struct gd_bzdata *_GD_Bzip2DoOpen(int dirfd, struct gd_raw_file_* file,
    unsigned int mode)
{
  int fd;
  struct gd_bzdata *ptr;
  FILE *stream;
  const char *fdmode = "rb";

  dtrace("%i, %p, 0x%X", dirfd, file, mode);

  file->error = BZ_IO_ERROR;

  if (mode & GD_FILE_READ) {
    fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666);
  } else if (mode & GD_FILE_TEMP) {
    fd = _GD_MakeTempFile(file->D, dirfd, file->name);
    fdmode = "wb";
  } else { /* internal error */
    errno = EINVAL; /* I guess ... ? */
    dreturn("%p", NULL);
    return NULL;
  }

  if (fd < 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  if ((stream = fdopen(fd, fdmode)) == NULL) {
    close(fd);
    dreturn("%p", NULL);
    return NULL;
  }

  if ((ptr = malloc(sizeof(struct gd_bzdata))) == NULL) {
    fclose(stream);
    dreturn("%p", NULL);
    return NULL;
  }

  ptr->stream = stream;
  ptr->bzerror = ptr->stream_end = 0;
  if (mode & GD_FILE_READ) {
    ptr->bzfile = BZ2_bzReadOpen(&ptr->bzerror, stream, 0, 0, NULL, 0);
  } else {
    ptr->bzfile = BZ2_bzWriteOpen(&ptr->bzerror, stream, 9, 0, 30);
    memset(ptr->data, 0, GD_BZIP_BUFFER_SIZE);
  }

  if (ptr->bzerror != BZ_OK) {
    if (mode & GD_FILE_READ)
      BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
    else
      BZ2_bzWriteClose(&ptr->bzerror, ptr->bzfile, 0, NULL, NULL);
    fclose(stream);
    file->error = ptr->bzerror;
    free(ptr);
    dreturn("%p", NULL);
    return NULL;
  }

  ptr->pos = ptr->end = 0;
  ptr->base = 0;
  file->pos = 0;

  dreturn("%p", ptr);
  return ptr;
}

int _GD_Bzip2Open(int dirfd, struct gd_raw_file_* file,
    gd_type_t type gd_unused_, int swap gd_unused_,
    unsigned int mode)
{
  dtrace("%i, %p, <unused>, <unused>, 0x%X", dirfd, file, mode);

  file->edata = _GD_Bzip2DoOpen(dirfd, file, mode);

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->mode = mode;
  file->idata = 0;
  dreturn("%i", 0);
  return 0;
}

ssize_t _GD_Bzip2Read(struct gd_raw_file_ *restrict file, void *restrict data,
    gd_type_t data_type, size_t nmemb)
{
  char* output = (char*)data;
  struct gd_bzdata *ptr = (struct gd_bzdata *)file->edata;
  uint64_t nbytes = nmemb * GD_SIZE(data_type);

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, data, data_type, nmemb);

  while (nbytes > (uint64_t)(ptr->end - ptr->pos)) {
    int n;

    memcpy(output, ptr->data + ptr->pos, ptr->end - ptr->pos);
    output += ptr->end - ptr->pos;
    nbytes -= ptr->end - ptr->pos;
    ptr->pos = ptr->end;

    if (ptr->stream_end) {
      dreturn("%li", (long)(nmemb - nbytes / GD_SIZE(data_type)));
      return nmemb - nbytes / GD_SIZE(data_type);
    }

    ptr->bzerror = 0;
    n = BZ2_bzRead(&ptr->bzerror, ptr->bzfile, ptr->data,
        GD_BZIP_BUFFER_SIZE);

    if (ptr->bzerror == BZ_OK || ptr->bzerror == BZ_STREAM_END) {
      ptr->base += ptr->end;
      ptr->pos = 0;
      ptr->end = n;
    } else {
      file->error = ptr->bzerror;
      dreturn("%i", -1);
      return -1;
    }

    /* eof */
    if (ptr->bzerror != BZ_OK) {
      ptr->stream_end = 1;
      break;
    }
  }

  if (nbytes > (uint64_t)(ptr->end - ptr->pos)) {
    memcpy(output, ptr->data + ptr->pos, ptr->end - ptr->pos);
    ptr->pos = ptr->end;
    nbytes -= ptr->end;
  } else {
    memcpy(output, ptr->data + ptr->pos, nbytes);
    ptr->pos += nbytes;
    nbytes = 0;
  }

  file->pos = (ptr->base + ptr->pos) / GD_SIZE(data_type);

  dreturn("%li", (long)(nmemb - nbytes / GD_SIZE(data_type)));
  return nmemb - nbytes / GD_SIZE(data_type);
}

ssize_t _GD_Bzip2Write(struct gd_raw_file_ *file, const void *data,
    gd_type_t data_type, size_t nmemb)
{
  struct gd_bzdata *ptr = (struct gd_bzdata *)file->edata;
  ssize_t n;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, data, data_type, nmemb);

  n = GD_SIZE(data_type) * nmemb;
  if (n > INT_MAX)
    n = INT_MAX;

  BZ2_bzWrite(&ptr->bzerror, ptr->bzfile, (void*)data, (int)n);

  if (ptr->bzerror) {
    file->error = ptr->bzerror;
    n = -1;
  } else {
    ptr->base += n;
    n /= GD_SIZE(data_type);
    file->pos += n;
  }

  dreturn("%" PRIdSIZE, n);
  return n;
}

off64_t _GD_Bzip2Seek(struct gd_raw_file_* file, off64_t offset,
    gd_type_t data_type, unsigned int mode)
{
  struct gd_bzdata *ptr;

  dtrace("%p, %" PRId64 ", 0x%X, 0x%X", file, (int64_t)offset, data_type, mode);

  ptr = (struct gd_bzdata *)(file->edata);

  /* nothing to do */
  if (file->pos == offset) {
    dreturn("%" PRId64, (int64_t)offset);
    return offset;
  }

  offset *= GD_SIZE(data_type);

  if (mode == GD_FILE_WRITE) {
    off64_t remaining = offset - file->pos * GD_SIZE(data_type);
    /* we only get here when we need to pad */
    while (ptr->base + ptr->end < offset) {
      int n;
      if (remaining > GD_BZIP_BUFFER_SIZE)
        n = GD_BZIP_BUFFER_SIZE;
      else
        n = remaining;

      _GD_Bzip2Write(file, ptr->data, GD_UINT8, n);
      remaining -= n;
    }
  } else {
    /* seek forward the slow way */
    while (ptr->base + ptr->end < offset) {
      int n;

      /* eof */
      if (ptr->stream_end)
        break;

      ptr->bzerror = 0;
      n = BZ2_bzRead(&ptr->bzerror, ptr->bzfile, ptr->data,
          GD_BZIP_BUFFER_SIZE);

      if (ptr->bzerror == BZ_OK || ptr->bzerror == BZ_STREAM_END) {
        ptr->base += ptr->end;
        ptr->end = n;
        if (ptr->bzerror == BZ_STREAM_END)
          ptr->stream_end = 1;
      } else {
        file->error = ptr->bzerror;
        dreturn("%i", -1);
        return -1;
      }
    }

    ptr->pos = (ptr->stream_end && offset >= ptr->base + ptr->end) ? ptr->end :
      offset - ptr->base;
  }
  
  file->pos = (ptr->base + ptr->pos) / GD_SIZE(data_type);

  dreturn("%" PRId64, (int64_t)file->pos);
  return file->pos;
}

int _GD_Bzip2Close(struct gd_raw_file_ *file)
{
  struct gd_bzdata *ptr = (struct gd_bzdata *)file->edata;
  dtrace("%p", file);

  ptr->bzerror = 0;
  if (file->mode & GD_FILE_READ)
    BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
  else
    BZ2_bzWriteClose(&ptr->bzerror, ptr->bzfile, 0, NULL, NULL);

  if (ptr->bzerror || fclose(ptr->stream)) {
    file->error = ptr->bzerror;
    dreturn("%i", 1);

    return 1;
  }

  file->idata = -1;
  file->mode = 0;
  free(file->edata);
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_Bzip2Size(int dirfd, struct gd_raw_file_ *file, gd_type_t data_type,
    int swap gd_unused_)
{
  struct gd_bzdata *ptr;
  off_t n;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  ptr = _GD_Bzip2DoOpen(dirfd, file, GD_FILE_READ);

  if (ptr == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  /* seek forward the slow way to the end */
  while (ptr->bzerror != BZ_STREAM_END) {
    int n;

    ptr->bzerror = 0;
    n = BZ2_bzRead(&ptr->bzerror, ptr->bzfile, ptr->data,
        GD_BZIP_BUFFER_SIZE);

    if (ptr->bzerror == BZ_OK || ptr->bzerror == BZ_STREAM_END) {
      ptr->base += ptr->end;
      ptr->pos = 0;
      ptr->end = n;
    } else {
      file->error = ptr->bzerror;
      BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
      fclose(ptr->stream);
      free(ptr);
      dreturn("%i", -1);
      return -1;
    }
  }
  BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
  fclose(ptr->stream);

  n = (ptr->base + ptr->end) / GD_SIZE(data_type);
  free(ptr);

  dreturn("%" PRId64, (int64_t)n);
  return n;
}

int _GD_Bzip2Strerr(const struct gd_raw_file_ *file, char *buf, size_t buflen)
{
  int r = 0;

  dtrace("%p, %p, %" PRIuSIZE, file, buf, buflen);

  switch (file->error) {
    case BZ_OK:
    case BZ_IO_ERROR:
      r = gd_StrError(errno, buf, buflen);
      break;
    case BZ_SEQUENCE_ERROR:
    case BZ_PARAM_ERROR:
      /* these indicate bugs in the code */
      strncpy(buf, "Internal error in Bzip2 encoding", buflen);
      break;
    case BZ_MEM_ERROR:
      strncpy(buf, "libbz2: Out of memory", buflen);
      break;
    case BZ_DATA_ERROR:
      strncpy(buf, "libbz2: Data integrity error", buflen);
      break;
    case BZ_UNEXPECTED_EOF:
      strncpy(buf, "libbz2: Unexpected EOF", buflen);
      break;
    default:
      snprintf(buf, buflen, "libbz2: Unkown error %i", file->error);
      break;
  }
  buf[buflen - 1] = 0;

  dreturn("%i", r);
  return r;
}
