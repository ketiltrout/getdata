/* (C) 2008-2010 D. V. Wiebe
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
#include <stdio.h>
#include <string.h>
#endif

#ifdef HAVE_BZLIB_H
#include <bzlib.h>
#endif

#ifdef USE_MODULES
#define _GD_Bzip2Open libgetdatabzip2_LTX_GD_Bzip2Open
#define _GD_Bzip2Seek libgetdatabzip2_LTX_GD_Bzip2Seek
#define _GD_Bzip2Read libgetdatabzip2_LTX_GD_Bzip2Read
#define _GD_Bzip2Close libgetdatabzip2_LTX_GD_Bzip2Close
#define _GD_Bzip2Size libgetdatabzip2_LTX_GD_Bzip2Size
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
 * open, fp = 0 otherwise fp = -1. */

static struct gd_bzdata *_GD_Bzip2DoOpen(struct _gd_raw_file* file)
{
  struct gd_bzdata *ptr;

  dtrace("%p", file);

  if ((ptr = (struct gd_bzdata *)malloc(sizeof(struct gd_bzdata))) == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  if ((ptr->stream = fopen(file->name, "rb")) == NULL) {
    free(ptr);
    dreturn("%p", NULL);
    return NULL;
  }

  ptr->bzerror = ptr->stream_end = 0;
  ptr->bzfile = BZ2_bzReadOpen(&ptr->bzerror, ptr->stream, 0, 0, NULL, 0);

  if (ptr->bzfile == NULL || ptr->bzerror != BZ_OK) {
    fclose(ptr->stream);
    free(ptr);
    dreturn("%p", NULL);
    return NULL;
  }

  ptr->pos = ptr->end = 0;
  ptr->base = 0;

  dreturn("%p", ptr);
  return ptr;
}

int _GD_Bzip2Open(struct _gd_raw_file* file, int mode __gd_unused,
    int creat __gd_unused)
{
  dtrace("%p, <unused>, <unused>", file);

  file->edata = _GD_Bzip2DoOpen(file);

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->fp = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_Bzip2Seek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  dtrace("%p, %lli, %x, <unused>", file, (long long)count, data_type);

  struct gd_bzdata *ptr = (struct gd_bzdata *)file->edata;

  count *= GD_SIZE(data_type);

  if (ptr->base > count) {
    /* a backwards seek -- reopen the file */
    ptr->bzerror = 0;
    BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
    ptr->bzfile = BZ2_bzReadOpen(&ptr->bzerror, ptr->stream, 0, 0, NULL, 0);

    if (ptr->bzfile == NULL || ptr->bzerror != BZ_OK) {
      fclose(ptr->stream);
      dreturn("%i", 1);
      return 1;
    }
    ptr->pos = ptr->end = 0;
    ptr->base = ptr->stream_end = 0;
  }

  /* seek forward the slow way */
  while (ptr->base + ptr->end < count) {
    ptr->bzerror = 0;
    int n = BZ2_bzRead(&ptr->bzerror, ptr->bzfile, ptr->data,
        GD_BZIP_BUFFER_SIZE);

    if (ptr->bzerror == BZ_OK || ptr->bzerror == BZ_STREAM_END) {
      ptr->base += ptr->end;
      ptr->end = n;
    } else {
      dreturn("%i", -1);
      return -1;
    }

    /* eof */
    if (ptr->bzerror != BZ_OK) {
      ptr->stream_end = 1;
      break;
    }
  }

  ptr->pos = (ptr->bzerror == BZ_STREAM_END && count >= ptr->base + ptr->end) ?
    ptr->end : count - ptr->base;

  dreturn("%lli", (long long)((ptr->base + ptr->pos) / GD_SIZE(data_type)));
  return (ptr->base + ptr->pos) / GD_SIZE(data_type);
}

ssize_t _GD_Bzip2Read(struct _gd_raw_file *file, void *data,
    gd_type_t data_type, size_t nmemb)
{
  dtrace("%p, %p, %x, %zu", file, data, data_type, nmemb);

  char* output = (char*)data;
  struct gd_bzdata *ptr = (struct gd_bzdata *)file->edata;
  unsigned long long nbytes = nmemb * GD_SIZE(data_type);

  while (nbytes > (unsigned long long)(ptr->end - ptr->pos)) {
    memcpy(output, ptr->data + ptr->pos, ptr->end - ptr->pos);
    output += ptr->end - ptr->pos;
    nbytes -= ptr->end - ptr->pos;
    ptr->pos = ptr->end;

    if (ptr->stream_end) {
      dreturn("%li", (long)(nmemb - nbytes / GD_SIZE(data_type)));
      return nmemb - nbytes / GD_SIZE(data_type);
    }

    ptr->bzerror = 0;
    int n = BZ2_bzRead(&ptr->bzerror, ptr->bzfile, ptr->data,
        GD_BZIP_BUFFER_SIZE);

    if (ptr->bzerror == BZ_OK || ptr->bzerror == BZ_STREAM_END) {
      ptr->base += ptr->end;
      ptr->pos = 0;
      ptr->end = n;
    } else {
      dreturn("%i", -1);
      return -1;
    }

    /* eof */
    if (ptr->bzerror != BZ_OK) {
      ptr->stream_end = 1;
      break;
    }
  }

  if (nbytes > (unsigned long long)(ptr->end - ptr->pos)) {
    memcpy(output, ptr->data + ptr->pos, ptr->end - ptr->pos);
    ptr->pos = ptr->end;
    nbytes -= ptr->end;
  } else {
    memcpy(output, ptr->data + ptr->pos, nbytes);
    ptr->pos += nbytes;
    nbytes = 0;
  }

  dreturn("%li", (long)(nmemb - nbytes / GD_SIZE(data_type)));
  return nmemb - nbytes / GD_SIZE(data_type);
}

int _GD_Bzip2Close(struct _gd_raw_file *file)
{
  dtrace("%p", file);

  struct gd_bzdata *ptr = (struct gd_bzdata *)file->edata;

  ptr->bzerror = 0;
  BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
  if (!fclose(ptr->stream)) {
    file->fp = -1;
    free(file->edata);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

off64_t _GD_Bzip2Size(struct _gd_raw_file *file, gd_type_t data_type)
{
  dtrace("%p, %x", file, data_type);

  struct gd_bzdata *ptr = _GD_Bzip2DoOpen(file);

  if (ptr == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  /* seek forward the slow way  to the end */
  while (ptr->bzerror != BZ_STREAM_END) {
    ptr->bzerror = 0;
    int n = BZ2_bzRead(&ptr->bzerror, ptr->bzfile, ptr->data,
        GD_BZIP_BUFFER_SIZE);

    if (ptr->bzerror == BZ_OK || ptr->bzerror == BZ_STREAM_END) {
      ptr->base += ptr->end;
      ptr->pos = 0;
      ptr->end = n;
    } else {
      free(ptr);
      BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
      fclose(ptr->stream);
      dreturn("%i", -1);
      return -1;
    }
  }
  BZ2_bzReadClose(&ptr->bzerror, ptr->bzfile);
  fclose(ptr->stream);

  off_t n = (ptr->base + ptr->end) / GD_SIZE(data_type);
  free(ptr);

  dreturn("%lli", (long long)n);
  return n;
}
