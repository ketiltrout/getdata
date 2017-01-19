/* Copyright (C) 2009-2017 D. V. Wiebe
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

#if SIZEOF_INT < 4
#define GD_LZMA_DATA_OUT 32752
#else
#define GD_LZMA_DATA_OUT 1000000
#endif
#define GD_LZMA_DATA_IN 32752
#define GD_LZMA_LOOKBACK 4096

struct gd_lzmadata {
  lzma_stream xz;
  FILE* stream;
  int stream_end;
  int input_eof;
  int offset; /* Offset into the output buffer */
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
    fclose(stream);
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
    file->error = e;
    fclose(lzd->stream);
    free(lzd);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", lzd);
  return lzd;
}

int _GD_LzmaOpen(int dirfd, struct gd_raw_file_* file,
    gd_type_t data_type gd_unused_, int swap gd_unused_, unsigned int mode)
{
  dtrace("%i, %p, <unused>, <unused>, 0x%X", dirfd, file, mode);

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
static int _GD_LzmaReady(struct gd_lzmadata *lzd, size_t nreq, int size,
    int *errnum)
{
  lzma_ret e;
  int ready;
  
  dtrace("%p, %" PRIuSIZE ", %i, %p", lzd, nreq, size, errnum);

  ready = READY(*lzd);

  /* already have some data, or no more data to read */
  if (LZEOF(*lzd) || ready >= size) {
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
      *errnum = e;
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

/* clear the output buffer, retaining a bit of data for lookback purposes
 * If "part" is non-zero, then it gives the length of a partial sample
 * found at the end of the buffer. */
static void _GD_LzmaClear(struct gd_lzmadata *lzd, int part)
{
  int n = 0;

  dtrace("%p, %i", lzd, part);
  
  /* amount of data to keep */
  n = NOUT(*lzd);
  if (n > GD_LZMA_LOOKBACK)
    n = GD_LZMA_LOOKBACK;

  memmove(lzd->data_out, lzd->xz.next_out - n, n);
  lzd->xz.next_out = lzd->data_out + n; 
  lzd->xz.avail_out = GD_LZMA_DATA_OUT - n;
  lzd->offset = n - part;

  dreturnvoid();
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

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, data, data_type, nmemb);

  n = nmemb * GD_SIZE(data_type);

  /* We let liblzma read directly from the caller's buffer */
  lzd->xz.next_in = data;
  lzd->xz.avail_in = n;

  /* code */
  while (lzd->xz.avail_in > 0) {
    e = lzma_code(&lzd->xz, LZMA_RUN);
    if (e != LZMA_OK) {
      file->error = e;
      dreturn("%i", -1);
      return -1;
    }

    if (_GD_LzmaFlush(lzd)) {
      dreturn("%i", -1);
      return -1;
    }
  }

  /* we always write all the input, if successful */
  file->pos += nmemb;
  dreturn("%" PRIdSIZE, (ssize_t)nmemb);
  return nmemb;
}

off64_t _GD_LzmaSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int mode)
{
  struct gd_lzmadata *lzd;
  lzma_ret e;
  uint64_t byte_count;
  const unsigned size = GD_SIZE(data_type);

  dtrace("%p, %" PRId64 ", 0x%X, 0x%X", file, (int64_t)count, data_type, mode);

  byte_count = count * size;

  lzd = (struct gd_lzmadata *)file->edata;

  if (mode != GD_FILE_WRITE) {
    /* the easy case -- position is somewhere within our current output buffer
     */
    if (byte_count < lzd->xz.total_out && byte_count >= BASE(*lzd)) {
      lzd->offset = byte_count - BASE(*lzd);
      file->pos = count;

      dreturn("%" PRId64, (int64_t)(file->pos));
      return file->pos;
    }

    if (BASE(*lzd) > byte_count) {
      /* a backwards seek -- rewind to the beginning */
      lzd->xz.avail_in = 0;
      lzd->xz.avail_out = GD_LZMA_DATA_OUT;
      lzd->xz.total_in = lzd->xz.total_out = 0;
      lzd->xz.next_in = lzd->data_in;
      lzd->xz.next_out = lzd->data_out;
      e = lzma_auto_decoder(&lzd->xz, UINT64_MAX, 0);
      if (e != LZMA_OK) {
        file->error = e;
        file->idata = -1;
        fclose(lzd->stream);
        free(lzd);
        file->edata = NULL;
        dreturn("%i", 1);
        return 1;
      }
      rewind(lzd->stream);
      lzd->input_eof = lzd->stream_end = 0;
    }

    /* seek forward the slow way */
    while (lzd->xz.total_out < byte_count) {
      /* discard output */
      _GD_LzmaClear(lzd, 0);

      if (_GD_LzmaReady(lzd, lzd->xz.avail_out, size, &file->error) < 0) {
        dreturn("%i", -1);
        return -1;
      }

      /* eof */
      if (LZEOF(*lzd))
        break;
    }

    if (lzd->xz.total_out < byte_count) {
      /* ran out of data */
      lzd->offset = NOUT(*lzd);
      file->pos = lzd->xz.total_out / size;
    } else {
      lzd->offset = byte_count - BASE(*lzd);
      file->pos = count;
    }
  } else {
    /* we only get here when we need to pad */
    while (lzd->xz.total_in < byte_count) {
      int n = byte_count - lzd->xz.total_in;
      if (n > GD_LZMA_DATA_IN)
        n = GD_LZMA_DATA_IN;

      _GD_LzmaWrite(file, lzd->data_in, data_type, n / size);
    }
    lzd->offset = 0;
  }

  dreturn("%" PRId64, (int64_t)file->pos);
  return file->pos;
}

ssize_t _GD_LzmaRead(struct gd_raw_file_ *file, void *data, gd_type_t data_type,
    size_t nmemb)
{
  uint64_t bytes_remaining;
  struct gd_lzmadata *lzd = (struct gd_lzmadata *)file->edata;
  ssize_t samples_read = 0;
  const unsigned size = GD_SIZE(data_type);

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, data, data_type, nmemb);

  if (nmemb > GD_SSIZE_T_MAX / size)
    nmemb = GD_SSIZE_T_MAX / size;
  bytes_remaining = nmemb * size;

  /* decoding loop */
  while (bytes_remaining > 0) {
    int bytes_ready, samples_ready;

    bytes_ready = _GD_LzmaReady(lzd, bytes_remaining, size, &file->error);
    if (bytes_ready < 0) {
      dreturn("%i", -1);
      return -1;
    } else if (bytes_ready < (int)size) {
      /* clear the output buffer */
      _GD_LzmaClear(lzd, bytes_ready);
    } else {
      /* copy whole samples */
      samples_ready = bytes_ready / size;

      if (samples_read + samples_ready > (ssize_t)nmemb)
        samples_ready = nmemb - samples_read;

      bytes_ready = samples_ready * size;

      memcpy(data, lzd->data_out + lzd->offset, bytes_ready);

      lzd->offset += bytes_ready;
      bytes_remaining -= bytes_ready; 
      data += bytes_ready;
      samples_read += samples_ready;
    }
    if (LZEOF(*lzd))
      break;
  }

  file->pos += samples_read;

  dreturn("%" PRIdSIZE, samples_read);
  return samples_read;
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
        file->error = e;
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
    struct gd_lzmadata *lzd = (struct gd_lzmadata *)file->edata;

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
  const unsigned size = GD_SIZE(data_type);

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  lzd = _GD_LzmaDoOpen(dirfd, file, GD_FILE_READ);

  if (lzd == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  /* read until EOF */
  while (!LZEOF(*lzd)) {
    if (_GD_LzmaReady(lzd, GD_LZMA_DATA_OUT, size, &file->error) < 0) {
      dreturn("%i", -1);
      return -1;
    }

    /* discard output */
    lzd->xz.next_out = lzd->data_out;
    lzd->xz.avail_out = GD_LZMA_DATA_OUT;
  }

  /* liblzma very graciously does the bookkeeping for us */
  n = lzd->xz.total_out / size;

  lzma_end(&lzd->xz);
  fclose(lzd->stream);

  free(lzd);

  dreturn("%" PRId64, (int64_t)n);
  return n;
}

int _GD_LzmaStrerr(const struct gd_raw_file_ *file, char *buf, size_t buflen)
{
  int r = 0;

  dtrace("%p, %p, %" PRIuSIZE, file, buf, buflen);

  switch(file->error) {
    case LZMA_OK:
      r = gd_StrError(errno, buf, buflen);
      break;
    case LZMA_MEM_ERROR:
    case LZMA_MEMLIMIT_ERROR:
      strncpy(buf, "LZMA: Out of memory", buflen);
      break;
    case LZMA_FORMAT_ERROR:
      strncpy(buf, "LZMA: File format not recognized", buflen);
      break;
    case LZMA_OPTIONS_ERROR:
      strncpy(buf, "LZMA: Invalid or unsupported options", buflen);
      break;
    case LZMA_DATA_ERROR:
      strncpy(buf, "LZMA: Data is corrupt", buflen);
      break;
    case LZMA_BUF_ERROR:
      strncpy(buf, "LZMA: No progress is possible", buflen);
      break;
    case LZMA_PROG_ERROR:
      /* this indicate bugs in the code */
      strncpy(buf, "Internal error in LZMA encoding", buflen);
      break;
    default:
      snprintf(buf, buflen, "LZMA: Unkown error %i", file->error);
      break;
  }

  dreturn("%i", r);
  return r;
}
