/* Copyright (C) 2014 D. V. Wiebe
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
#define GD_LZMA_DATA_OUT 32767
#else
#define GD_LZMA_DATA_OUT 1000000
#endif
#define GD_LZMA_DATA_IN 4096
#define GD_LZMA_LOOKBACK 4096

struct gd_lzmadata {
  lzma_stream xz;
  FILE* stream;
  int stream_end;
  int input_eof;
  int offset;
  uint8_t data_in[GD_LZMA_DATA_IN];
  uint8_t data_out[GD_LZMA_DATA_OUT];
};

/* total bytes in the output buffer */
#define NOUT(p) (GD_LZMA_DATA_OUT - (p).xz.avail_out)

/* byte position of the start of the output buffer */
#define BASE(p) ((p).xz.total_out - NOUT(p))

/* bytes of unused data in output buffer */
#define READY(p) (NOUT(p) - (p).offset)

/* At EOF */
#define LZEOF(p) ((p).stream_end || (p).input_eof)

/* The lzma encoding scheme uses edata as a gd_lzmadata pointer.  If a file is
 * open, idata = 0 otherwise idata = -1. */

static struct gd_lzmadata *_GD_LzmaDoOpen(int dirfd, struct gd_raw_file_* file,
    unsigned int mode)
{
  struct gd_lzmadata *lzd;
  int fd;
  lzma_ret e;
  const char *fdmode = "rb";
  FILE *stream;
  const lzma_stream stream_init = LZMA_STREAM_INIT;

  dtrace("%i, %p, %i", dirfd, file, mode);

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

  if ((lzd = (struct gd_lzmadata *)malloc(sizeof(struct gd_lzmadata))) == NULL)
  {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(lzd, 0, sizeof(*lzd));

  lzd->xz = stream_init;
  lzd->stream = stream;
  lzd->xz.next_in = lzd->data_in;
  lzd->xz.next_out = lzd->data_out;
  lzd->xz.avail_out = GD_LZMA_DATA_OUT;
  if (mode & GD_FILE_READ)
    e = lzma_auto_decoder(&lzd->xz, UINT64_MAX, 0);
  else {
    e = lzma_easy_encoder(&lzd->xz, 9, LZMA_CHECK_CRC64);
    memset(lzd->data_in, 0, GD_LZMA_DATA_IN);
  }

  if (e != LZMA_OK) {
    fclose(lzd->stream);
    free(lzd);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", lzd);
  return lzd;
}

int _GD_LzmaOpen(int dirfd, struct gd_raw_file_* file, int swap gd_unused_,
    unsigned int mode)
{
  dtrace("%i, %p, <unused>, 0x%X", dirfd, file, mode);

  file->edata = _GD_LzmaDoOpen(dirfd, file, mode);

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->mode = mode;
  file->pos = 0;
  file->idata = 0;
  dreturn("%i", 0);
  return 0;
}

/* Read and decode until the there's enough data ready in the output buffer, or
 * it's full, or we hit EOF.  Returns -1 on error, data ready size otherwise.
 *
 * This doesn't update next_out, so we don't have to worry about updating
 * offset.
 */
static int _GD_LzmaReady(struct gd_lzmadata *lzd, size_t nreq)
{
  lzma_ret e;
  int ready = READY(*lzd);

  dtrace("%p, %" PRNsize_t, lzd, nreq);

  /* already have enough data, or no more data to read */
  if (LZEOF(*lzd) || (size_t)ready >= nreq) {
    dreturn("%i", ready);
    return ready;
  }

  /* coding loop */
  while (lzd->xz.avail_out > 0 && (size_t)ready < nreq) {
    /* No input data, so read some */
    if (lzd->xz.avail_in == 0) {
      size_t n = fread(lzd->data_in, 1, GD_LZMA_DATA_IN, lzd->stream);
      if (n == 0) {
        if (ferror(lzd->stream)) {
          dreturn("%i", -1);
          return -1;
        }
        /* end of input */
        lzd->input_eof = 1;
        break;
      }
      lzd->xz.avail_in = n;
      lzd->xz.next_in = lzd->data_in;
    }

    /* code */
    e = lzma_code(&lzd->xz, LZMA_RUN);
    if (e != LZMA_OK && e != LZMA_STREAM_END) {
      dreturn("%i", -1);
      return -1;
    }

    ready = READY(*lzd);
    if (e == LZMA_STREAM_END) {
      lzd->stream_end = 1;
      break;
    }
  }

  dreturn("%i", ready);
  return ready;
}

/* clear the output buffer, retaining a bit of data for lookback purposes */
static void _GD_LzmaClear(struct gd_lzmadata *lzd)
{
  int n;

  dtrace("%p", lzd);
  
  /* amount of data to keep */
  n = NOUT(*lzd);
  if (n > GD_LZMA_LOOKBACK)
    n = GD_LZMA_LOOKBACK;

  memmove(lzd->data_out, lzd->xz.next_out - n, n);
  lzd->xz.next_out = lzd->data_out + n; 
  lzd->xz.avail_out = GD_LZMA_DATA_OUT - n;

  dreturnvoid();
}

off64_t _GD_LzmaSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int mode)
{
  struct gd_lzmadata *lzd;
  lzma_ret e;
  uint64_t bcount;

  dtrace("%p, %lli, 0x%X, 0x%X", file, (long long)count, data_type, mode);

  bcount = count * GD_SIZE(data_type);

  if (mode == GD_FILE_WRITE)
    lzd = (struct gd_lzmadata *)file[1].edata;
  else
    lzd = (struct gd_lzmadata *)file[0].edata;

  /* the easy case -- position is somewhere within our current output buffer */
  if (bcount < lzd->xz.total_out && bcount >= BASE(*lzd)) {
    lzd->offset = bcount - BASE(*lzd);
    file->pos = count;

    dreturn("%lli", (long long)(file->pos));
    return file->pos;
  }

  if (mode != GD_FILE_WRITE) {
    if (BASE(*lzd) > bcount) {
      /* a backwards seek -- rewind to the beginning */
      lzd->xz.avail_in = 0;
      lzd->xz.avail_out = GD_LZMA_DATA_OUT;
      lzd->xz.total_in = lzd->xz.total_out = 0;
      lzd->xz.next_in = lzd->data_in;
      lzd->xz.next_out = lzd->data_out;
      e = lzma_auto_decoder(&lzd->xz, UINT64_MAX, 0);
      if (e != LZMA_OK) {
        file->idata = -1;
        free(lzd);
        file->edata = NULL;
        fclose(lzd->stream);
        dreturn("%i", 1);
        return 1;
      }
      rewind(lzd->stream);
      lzd->input_eof = lzd->stream_end = 0;
    }

    /* seek forward the slow way */
    while (lzd->xz.total_out < bcount) {
      /* discard output */
      _GD_LzmaClear(lzd);

      if (_GD_LzmaReady(lzd, lzd->xz.avail_out) < 0) {
        dreturn("%i", -1);
        return -1;
      }

      /* eof */
      if (LZEOF(*lzd))
        break;
    }

    if (lzd->xz.total_out < bcount) {
      /* ran out of data */
      lzd->offset = NOUT(*lzd);
      file->pos = lzd->xz.total_out / GD_SIZE(data_type);
    } else {
      lzd->offset = bcount - BASE(*lzd);
      file->pos = count;
    }
  } else {
    /* we only get here when we need to pad */
    while (lzd->xz.total_in < bcount) {
      int n = bcount - lzd->xz.total_in;
      if (n > GD_LZMA_DATA_IN)
        n = GD_LZMA_DATA_IN;

      _GD_LzmaWrite(file + 1, lzd->data_in, GD_UINT8, n);
    }
    lzd->offset = 0;
    file->pos = lzd->xz.total_in / GD_SIZE(data_type);
  }

  dreturn("%lli", (long long)(file->pos));
  return file->pos;
}

ssize_t _GD_LzmaRead(struct gd_raw_file_ *file, void *data, gd_type_t data_type,
    size_t nmemb)
{
  uint64_t bcount;
  struct gd_lzmadata *lzd = (struct gd_lzmadata *)file->edata;
  ssize_t nread = 0;

  dtrace("%p, %p, 0x%X, %" PRNsize_t, file, data, data_type, nmemb);

  if (nmemb > GD_SSIZE_T_MAX / GD_SIZE(data_type))
    nmemb = GD_SSIZE_T_MAX / GD_SIZE(data_type);
  bcount = nmemb * GD_SIZE(data_type);

  /* decoding loop */
  while (bcount > 0) {
    int bready, nready;

    /* clear the output buffer if it's full */
    if (lzd->xz.avail_out == 0)
      _GD_LzmaClear(lzd);
    
    bready = _GD_LzmaReady(lzd, bcount);
    if (bready < 0) {
      dreturn("%i", -1);
      return -1;
    }

    /* copy whole samples */
    nready = bready / GD_SIZE(data_type);
    if (nready > (ssize_t)nmemb)
      nready = nmemb;

    bready = nready * GD_SIZE(data_type);

    memcpy(data, lzd->data_out + lzd->offset, bready);
    lzd->offset += bready;
    bcount -= bready;
    data += bready;
    nread += nready;

    if (LZEOF(*lzd))
      break;
  }

  dreturn("%" PRNssize_t, nread);
  return nread;
}

/* flush the output buffer to the stream */
static int _GD_LzmaFlush(struct gd_lzmadata *lzd)
{
  uint8_t *ptr;

  dtrace("%p", lzd);

  ptr = lzd->data_out;
  while (NOUT(*lzd) > 0) {
    ssize_t nw = fwrite(ptr, 1, NOUT(*lzd), lzd->stream);
    if (nw == 0 && ferror(lzd->stream)) {
      dreturn("%i", 1);
      return 1;
    }

    ptr += nw;
    lzd->xz.avail_out += nw;
  }

  /* reset output buffer */
  lzd->xz.next_out = lzd->data_out;

  dreturn("%i", 0);
  return 0;
}

ssize_t _GD_LzmaWrite(struct gd_raw_file_ *file, const void *data,
    gd_type_t data_type, size_t nmemb)
{
  lzma_ret e;
  size_t n;
  struct gd_lzmadata *lzd = (struct gd_lzmadata *)file->edata;

  dtrace("%p, %p, 0x%X, %" PRNsize_t, file, data, data_type, nmemb);

  n = nmemb * GD_SIZE(data_type);

  /* We let liblzma read directly from the caller's buffer */
  lzd->xz.next_in = data;
  lzd->xz.avail_in = n;

  /* code */
  while (lzd->xz.avail_in > 0) {
    e = lzma_code(&lzd->xz, LZMA_RUN);
    if (e != LZMA_OK) {
      dreturn("%i", -1);
      return -1;
    }

    if (_GD_LzmaFlush(lzd)) {
      dreturn("%i", -1);
      return -1;
    }
  }

  /* we always write all the input, if successful */
  dreturn("%" PRNssize_t, (ssize_t)nmemb);
  return nmemb;
}

int _GD_LzmaClose(struct gd_raw_file_ *file)
{
  lzma_ret e;
  struct gd_lzmadata *lzd = (struct gd_lzmadata *)file->edata;

  dtrace("%p", file);

  /* finialise */
  if (!(file->mode & GD_FILE_READ)) {
    /* ensure we don't read any more data */
    lzd->xz.avail_in = 0;
    for (;;) {
      e = lzma_code(&lzd->xz, LZMA_FINISH);
      if (e != LZMA_OK && e != LZMA_STREAM_END) {
        dreturn("%i", 1);
        return 1;
      }

      if (_GD_LzmaFlush(lzd)) {
        dreturn("%i", 1);
        return 1;
      }

      if (e == LZMA_STREAM_END)
        break;
    }
  }

  /* shutdown */
  lzma_end(&lzd->xz);
  if (fclose(lzd->stream)) {
    dreturn("%i", 1);
    return 1;
  }

  file->idata = -1;
  free(file->edata);
  file->edata = NULL;
  dreturn("%i", 0);
  return 0;
}

/* We don't flush the encoder since it may degrade compression, but we can
 * flush the stdio buffer */
int _GD_LzmaSync(struct gd_raw_file_ *file)
{
  int r = 0;
  dtrace("%p", file);

  if (file->mode & GD_FILE_WRITE) {
    struct gd_lzmadata *lzd = (struct gd_lzmadata *)file[1].edata;

    r = fflush(lzd->stream);
  }

  dreturn("%i", r);
  return r;
}

off64_t _GD_LzmaSize(int dirfd, struct gd_raw_file_ *file, gd_type_t data_type,
    int swap gd_unused_)
{
  struct gd_lzmadata *lzd;
  off64_t n;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  lzd = _GD_LzmaDoOpen(dirfd, file, GD_FILE_READ);

  if (lzd == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  /* read until EOF */
  while (!LZEOF(*lzd)) {
    if (_GD_LzmaReady(lzd, GD_LZMA_DATA_OUT) < 0) {
      dreturn("%i", -1);
      return -1;
    }

    /* discard output */
    lzd->xz.next_out = lzd->data_out;
    lzd->xz.avail_out = GD_LZMA_DATA_OUT;
  }

  /* liblzma very graciously does the bookkeeping for us */
  n = lzd->xz.total_out / GD_SIZE(data_type);

  lzma_end(&lzd->xz);
  fclose(lzd->stream);

  free(lzd);

  dreturn("%lli", (long long)n);
  return n;
}
