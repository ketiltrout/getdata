/* Copyright (C) 2009-2011 D. V. Wiebe
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

#ifdef HAVE_LZMA_H
#include <lzma.h>
#endif

#ifdef USE_MODULES
#define _GD_LzmaOpen libgetdatalzma_LTX_GD_LzmaOpen
#define _GD_LzmaSeek libgetdatalzma_LTX_GD_LzmaSeek
#define _GD_LzmaRead libgetdatalzma_LTX_GD_LzmaRead
#define _GD_LzmaClose libgetdatalzma_LTX_GD_LzmaClose
#define _GD_LzmaSize libgetdatalzma_LTX_GD_LzmaSize
#endif

#if SIZEOF_INT < 4
#define GD_LZMA_BUFFER_SIZE 32767
#else
#define GD_LZMA_BUFFER_SIZE 1000000
#endif

struct gd_lzmadata {
  lzma_stream xzfile;
  FILE* stream;
  int xzerror;
  int stream_end; /* uncompressed file ran out of data */
  int input_eof; /* compressed file ran out of data */
  int read_in; /* location of the end of valid data in data_in */
  int out_pos; /* library's current position relative to the start of
                  data_out */
  int end; /* location of the end of valid data in data_out */
  off64_t base; /* position of the start of data_out in the uncompressed
                   stream */
  uint8_t data_in[GD_LZMA_BUFFER_SIZE];
  uint8_t data_out[GD_LZMA_BUFFER_SIZE];
};

/* The bzip encoding scheme uses edata as a gd_lzmadata pointer.  If a file is
 * open, idata = 0 otherwise idata = -1. */

static struct gd_lzmadata *_GD_LzmaDoOpen(int dirfd, struct _gd_raw_file* file)
{
  struct gd_lzmadata *ptr;
  int fd;

  dtrace("%i, %p", dirfd, file);

  if ((ptr = (struct gd_lzmadata *)malloc(sizeof(struct gd_lzmadata))) == NULL)
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if ((fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666))
      == -1)
  {
    free(ptr);
    dreturn("%p", NULL);
    return NULL;
  }

  if ((ptr->stream = fdopen(fd, "rb")) == NULL) {
    close(fd);
    free(ptr);
    dreturn("%p", NULL);
    return NULL;
  }
  dprintf("%p = %s", ptr->stream, file->name);

  memset(&ptr->xzfile, 0, sizeof(lzma_stream));
  ptr->xzfile.next_in = ptr->data_in;
  ptr->xzfile.next_out = ptr->data_out;
  ptr->xzfile.avail_in = 0;
  ptr->xzfile.avail_out = GD_LZMA_BUFFER_SIZE;
  ptr->xzerror = lzma_auto_decoder(&ptr->xzfile, 1000000000, 0);
  ptr->xzfile.total_in = 0;

  if (ptr->xzerror != LZMA_OK) {
    fclose(ptr->stream);
    free(ptr);
    dreturn("%p", NULL);
    return NULL;
  }

  ptr->read_in = ptr->out_pos = ptr->end = 0;
  ptr->base = ptr->stream_end = ptr->input_eof = 0;

  dreturn("%p", ptr);
  return ptr;
}

int _GD_LzmaOpen(int dirfd, struct _gd_raw_file* file, int swap __gd_unused,
    int mode __gd_unused, int creat __gd_unused)
{
  struct gd_lzmadata *ptr;

  dtrace("%i, %p, <unused>, <unused>, <unused>", dirfd, file);

  file->edata = ptr = _GD_LzmaDoOpen(dirfd, file);

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->idata = 0;
  dreturn("%i", 0);
  return 0;
}

/* Read as much data as possible, and then run the converter on it, leaving
 * unused input at the front of the input buffer, and updating read_in as
 * appropriate.  Returns -1 on error */
static int _GD_LzmaDecode(struct gd_lzmadata *ptr)
{
  int n = 0;

  dtrace("%p", ptr);

  if (!ptr->input_eof) {
    n = fread(ptr->data_in + ptr->read_in, 1, GD_LZMA_BUFFER_SIZE -
        ptr->read_in, ptr->stream);
    dprintf("n=%i for %i on %p", n, GD_LZMA_BUFFER_SIZE - ptr->read_in,
        ptr->stream);

    if (n < GD_LZMA_BUFFER_SIZE - ptr->read_in) {
      if (feof(ptr->stream))
        ptr->input_eof = 1;
      else {
        dreturn("%i", -1);
        return -1;
      }
    }
  }

  dprintf("read_in=%i  n=%i", ptr->read_in, n);
  ptr->xzfile.avail_in = ptr->read_in + n;
  dprintf("avail_in=%zu   total_in=%llu", ptr->xzfile.avail_in,
      ptr->xzfile.total_in);
  dprintf("avail_out=%zu  total_out=%llu", ptr->xzfile.avail_out,
      ptr->xzfile.total_out);

  /* no more data to convert -- end of stream reached */
  if (ptr->xzfile.avail_in == 0) {
    ptr->stream_end = 1;
    dreturn("%i", 0);
    return 0;
  }

  /* amount of data = amount already in buffer + amount just now read */
  ptr->xzerror = lzma_code(&ptr->xzfile, LZMA_RUN);
  dprintf("avail_in=%zu   total_in=%llu", ptr->xzfile.avail_in,
      ptr->xzfile.total_in);
  dprintf("avail_out=%zu  total_out=%llu", ptr->xzfile.avail_out,
      ptr->xzfile.total_out);

  if (ptr->xzerror == LZMA_OK || ptr->xzerror == LZMA_STREAM_END) {
    ptr->base += ptr->end;
    ptr->end = ptr->xzfile.total_out;
    /* shift unused input to start of buffer */
    memmove(ptr->data_in, ptr->data_in + ptr->xzfile.total_in,
        ptr->xzfile.avail_in);
    ptr->read_in = ptr->xzfile.avail_in;
  } else {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

off64_t _GD_LzmaSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad __gd_unused)
{
  struct gd_lzmadata *ptr = (struct gd_lzmadata *)file->edata;

  dtrace("%p, %lli, %x, <unused>", file, (long long)count, data_type);

  count *= GD_SIZE(data_type);

  if (ptr->base > count) {
    /* a backwards seek -- rewind to the beginning */
    lzma_end(&ptr->xzfile);
    ptr->xzfile.avail_in = 0;
    ptr->xzfile.avail_out = GD_LZMA_BUFFER_SIZE;
    ptr->xzerror = lzma_auto_decoder(&ptr->xzfile, 1000000000, 0);
    ptr->xzfile.total_in = GD_LZMA_BUFFER_SIZE;
    if (ptr->xzerror != LZMA_OK) {
      file->idata = -1;
      fclose(ptr->stream);
      dreturn("%i", 1);
      return 1;
    }
    rewind(ptr->stream);
    ptr->read_in = ptr->out_pos = ptr->end = 0;
    ptr->base = ptr->input_eof = ptr->stream_end = 0;
  }

  /* seek forward the slow way */
  while (ptr->base + ptr->end < count) {
    if (_GD_LzmaDecode(ptr)) {
      dreturn("%i", -1);
      return -1;
    }

    /* eof */
    if (ptr->stream_end)
      break;
  }

  ptr->out_pos = (ptr->stream_end && count >= ptr->base + ptr->end) ?  ptr->end
    : count - ptr->base;

  dreturn("%lli", (long long)((ptr->base + ptr->out_pos) / GD_SIZE(data_type)));
  return (ptr->base + ptr->out_pos) / GD_SIZE(data_type);
}

ssize_t _GD_LzmaRead(struct _gd_raw_file *file, void *data, gd_type_t data_type,
    size_t nmemb)
{
  char* output = (char *)data;
  struct gd_lzmadata *ptr = (struct gd_lzmadata *)file->edata;
  uint64_t nbytes = nmemb * GD_SIZE(data_type);

  dtrace("%p, %p, %x, %zu", file, data, data_type, nmemb);

  /* this loops over chunks of uncompressed data of size data_out until we
   * have as much data as we need in data_out, or until EOF */
  while (nbytes > (uint64_t)(ptr->end - ptr->out_pos)) {
    memcpy(output, ptr->data_out + ptr->out_pos, ptr->end - ptr->out_pos);
    output += ptr->end - ptr->out_pos;
    nbytes -= ptr->end - ptr->out_pos;
    ptr->out_pos = ptr->end;

    if (ptr->stream_end) {
      dreturn("%li", (long)(nmemb - nbytes / GD_SIZE(data_type)));
      return nmemb - nbytes / GD_SIZE(data_type);
    }

    if (_GD_LzmaDecode(ptr)) {
      dreturn("%i", -1);
      return -1;
    }

    /* eof */
    if (ptr->stream_end)
      break;
  }

  /* If we still have more data than we need (no EOF), copy it all, otherwise
   * (reached EOF) copy as much as we have */
  if (nbytes > (uint64_t)(ptr->end - ptr->out_pos)) {
    memcpy(output, ptr->data_out + ptr->out_pos, ptr->end - ptr->out_pos);
    ptr->out_pos = ptr->end;
    nbytes -= ptr->end;
  } else {
    memcpy(output, ptr->data_out + ptr->out_pos, nbytes);
    ptr->out_pos += nbytes;
    nbytes = 0;
  }

  dreturn("%li", (long)(nmemb - nbytes / GD_SIZE(data_type)));
  return nmemb - nbytes / GD_SIZE(data_type);
}

int _GD_LzmaClose(struct _gd_raw_file *file)
{
  struct gd_lzmadata *ptr = (struct gd_lzmadata *)file->edata;

  dtrace("%p", file);

  ptr->xzerror = 0;
  lzma_end(&ptr->xzfile);
  if (!fclose(ptr->stream)) {
    file->idata = -1;
    free(file->edata);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

off64_t _GD_LzmaSize(int dirfd, struct _gd_raw_file *file, gd_type_t data_type,
    int swap __gd_unused)
{
  struct gd_lzmadata *ptr;
  off_t n;

  dtrace("%i, %p, %x, <unused>", dirfd, file, data_type);

  ptr = _GD_LzmaDoOpen(dirfd, file);

  if (ptr == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  /* seek forward the slow way  to the end */
  while (!ptr->stream_end) {
    if (_GD_LzmaDecode(ptr)) {
      lzma_end(&ptr->xzfile);
      fclose(ptr->stream);
      dreturn("%i", -1);
      return -1;
    }
  }
  lzma_end(&ptr->xzfile);
  fclose(ptr->stream);

  n = (ptr->base + ptr->end) / GD_SIZE(data_type);
  free(ptr);

  dreturn("%lli", (long long)n);
  return n;
}
