/* Copyright (C) 2015, 2016 D. V. Wiebe
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

#ifdef HAVE_FLAC_ALL_H
#include <FLAC/all.h>
#endif

/* errors and error disambiguation */
#define GD_FLAC_E_IO           0 /* I/O (stdlib) error */
#define GD_FLAC_E_EOS          1 /* seek past eof */
#define GD_FLAC_E_CPS          2 /* bad channels per sample */
#define GD_FLAC_E_BPS          3 /* bad bits per sample */
#define GD_FLAC_E_MEM          4 /* alloc error */
#define GD_FLAC_E_SDS    0x10000 /* FLAC__StreamDecoderState */
#define GD_FLAC_E_SDIS   0x20000 /* FLAC__StreamDecoderInitStatus */
#define GD_FLAC_E_SDES   0x20000 /* FLAC__StreamDecoderErrorStatus */
#define GD_FLAC_E_SES    0x30000 /* FLAC__StreamEncoderState */
#define GD_FLAC_E_SEIS   0x40000 /* FLAC__StreamEncoderInitStatus */

struct gd_flacdata {
  union {
    FLAC__StreamDecoder *d;
    FLAC__StreamEncoder *e;
  } codec;
  FILE* stream;
  unsigned bps; /* bits per sample */
  unsigned cps; /* channels per sample */
  int swap; /* Byte-swapping required */

  int stream_end;
  int error; /* error flag */
  int *errnum; /* file->error */

  /* Decoder (reader) only */
  char *data; /* Dechannelised frame of data */
  unsigned dlen; /* Length in samples of data */
  unsigned pos; /* offset in samples into the frame data */
  off64_t base; /* sample number of the start of the frame */
};

/* The flac encoding scheme uses edata as a gd_flacdata pointer.  If a file is
 * open, idata = 0 otherwise idata = -1. */

/* The decoder callback */
static FLAC__StreamDecoderWriteStatus _GD_FlacDecodeCallback(
    const FLAC__StreamDecoder *decoder gd_unused_, const FLAC__Frame *frame,
    const FLAC__int32 *const buffer[], void *client_data)
{
  struct gd_flacdata *gdfl = client_data;
  unsigned u, c;
  int16_t *ptr;

  dtrace("<unused>, %p, %p, %p", frame, buffer, client_data);

  /* Advance base for previous frame */
  gdfl->base += gdfl->dlen;

  /* Reset position */
  gdfl->pos = 0;

  /* check bps and cps */
  if (gdfl->cps != FLAC__stream_decoder_get_channels(gdfl->codec.d)) {
    gdfl->error = 1;
    *gdfl->errnum = GD_FLAC_E_CPS;
    dreturn("%s (cps)", "FLAC__STREAM_DECODER_WRITE_STATUS_ABORT");
    return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
  }

  if (gdfl->bps != FLAC__stream_decoder_get_bits_per_sample(gdfl->codec.d)) {
    gdfl->error = 1;
    *gdfl->errnum = GD_FLAC_E_BPS;
    dreturn("%s (bps)", "FLAC__STREAM_DECODER_WRITE_STATUS_ABORT");
    return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
  }

  if (gdfl->dlen != frame->header.blocksize) {
    /* Resize the data buffer to accommodate the new frame */
    free(gdfl->data);
    gdfl->data = malloc(frame->header.blocksize * gdfl->bps * gdfl->cps / 8);
    if (gdfl->data == NULL) {
      gdfl->error = 1;
      *gdfl->errnum = GD_FLAC_E_MEM;
      dreturn("%s (alloc)", "FLAC__STREAM_DECODER_WRITE_STATUS_ABORT");
      return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
    }
    gdfl->dlen = frame->header.blocksize;
  }

  ptr = (int16_t*)gdfl->data;

  /* Copy and dechannelise the decoded data to our local buffer */
  if (gdfl->bps == 8)
    for (u = 0; u < frame->header.blocksize; ++u)
      /* there's only one channel in this case */
      gdfl->data[u] = (int8_t)buffer[0][u];
  else if (gdfl->swap)
    for (u = 0; u < frame->header.blocksize; ++u)
      for (c = gdfl->cps; c > 0; --c)
        *(ptr++) = (int16_t)buffer[c - 1][u];
  else
    for (u = 0; u < frame->header.blocksize; ++u)
      for (c = 0; c < gdfl->cps; ++c)
        *(ptr++) = (int16_t)buffer[c][u];

  dreturn("%s", "FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE");
  return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

/* The decoder error callback */
static void _GD_FlacErrorCallback(const FLAC__StreamDecoder *decoder gd_unused_,
    FLAC__StreamDecoderErrorStatus status, void *client_data)
{
  struct gd_flacdata *gdfl = client_data;

  dtrace("<unused>, %i, %p", (int)status, client_data);

  gdfl->error = 1;
  *gdfl->errnum = GD_FLAC_E_SDES | status;

  dreturnvoid();
  return;
}

static struct gd_flacdata *_GD_FlacDoOpen(int dirfd, struct gd_raw_file_* file,
    gd_type_t data_type, int swap, unsigned int mode)
{
  int fd, status;
  struct gd_flacdata *gdfl;
  FILE *stream;
  const char *fdmode = "rb";

  dtrace("%i, %p, 0x%X, %i, 0x%X", dirfd, file, data_type, swap, mode);

  if (mode & GD_FILE_READ) {
    fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666);
  } else if (mode & GD_FILE_TEMP) {
    fd = _GD_MakeTempFile(file->D, dirfd, file->name);
    fdmode = "wb";
  } else { /* internal error */
    errno = EINVAL; /* I guess ... ? */
    fd = -1;
  }

  if (fd < 0) {
    file->error = GD_FLAC_E_IO;
    dreturn("%p", NULL);
    return NULL;
  }

  if ((stream = fdopen(fd, fdmode)) == NULL) {
    close(fd);
    file->error = GD_FLAC_E_IO;
    dreturn("%p", NULL);
    return NULL;
  }

  if ((gdfl = malloc(sizeof *gdfl)) == NULL) {
    fclose(stream);
    file->error = GD_FLAC_E_MEM;
    dreturn("%p", NULL);
    return NULL;
  }

  memset(gdfl, 0, sizeof *gdfl);
  gdfl->stream = stream;
  gdfl->data = NULL;

  /* libFLAC only supports up to 24-bits per sample (which isn't a helpful
   * number for us).  Except for 1-byte types, we use 16 bits-per-sample 
   * and increase the number of channels to accomodate the data type.  */
  if (GD_SIZE(data_type) <= 2) {
    gdfl->bps = GD_SIZE(data_type) * 8;
    gdfl->cps = 1;
    gdfl->swap = 0;
  } else {
    gdfl->bps = 16;
    gdfl->cps = GD_SIZE(data_type) / 2;
    gdfl->swap = swap;
  }

  if (mode & GD_FILE_READ) {
    if ((gdfl->codec.d = FLAC__stream_decoder_new()) == NULL) {
      file->error = GD_FLAC_E_MEM;
      goto OPEN_ERROR;
    }

    status = FLAC__stream_decoder_init_FILE(gdfl->codec.d, stream,
          _GD_FlacDecodeCallback, NULL, _GD_FlacErrorCallback, gdfl);

    if (status != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
      file->error = GD_FLAC_E_SDIS | status;
      goto OPEN_ERROR;
    }

    /* Advance to the end of the metadata */
    if (!FLAC__stream_decoder_process_until_end_of_metadata(gdfl->codec.d))
      goto OPEN_ERROR;
  } else {
    if ((gdfl->codec.e = FLAC__stream_encoder_new()) == NULL) {
      file->error = GD_FLAC_E_MEM;
      goto OPEN_ERROR;
    }

    FLAC__stream_encoder_set_channels(gdfl->codec.e, gdfl->cps);
    FLAC__stream_encoder_set_bits_per_sample(gdfl->codec.e, gdfl->bps);
    FLAC__stream_encoder_set_sample_rate(gdfl->codec.e, 1);

    status = FLAC__stream_encoder_init_FILE(gdfl->codec.e, stream, NULL, NULL);
    if (status != FLAC__STREAM_ENCODER_INIT_STATUS_OK) {
      if (status == FLAC__STREAM_ENCODER_INIT_STATUS_ENCODER_ERROR)
        file->error = GD_FLAC_E_SES |
          FLAC__stream_encoder_get_state(gdfl->codec.e);
      else
        file->error = GD_FLAC_E_SEIS | status;
      goto OPEN_ERROR;
    }
  }

  file->pos = 0;

  dreturn("%p", gdfl);
  return gdfl;

OPEN_ERROR:
  fclose(stream);
  free(gdfl);
  dreturn("%p", NULL);
  return NULL;
}

int _GD_FlacOpen(int dirfd, struct gd_raw_file_* file, gd_type_t data_type,
    int swap, unsigned int mode)
{
  dtrace("%i, %p, 0x%X, %i, 0x%X", dirfd, file, data_type, swap, mode);

  file->edata = _GD_FlacDoOpen(dirfd, file, data_type, swap, mode);

  if (file->edata == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  file->mode = mode;
  file->idata = 0;
  ((struct gd_flacdata*)file->edata)->errnum = &file->error;
  dreturn("%i", 0);
  return 0;
}

/* copy the decoded frame to the output */
static size_t _GD_FlacOutput(struct gd_flacdata *gdfl, gd_type_t data_type,
    char *output, size_t ns)
{
  dtrace("%p, 0x%X, %p, %" PRIuSIZE, gdfl, data_type, output, ns);

  if (ns > gdfl->dlen - gdfl->pos)
    ns = gdfl->dlen - gdfl->pos;

  if (ns > 0) {
    memcpy(output, gdfl->data + gdfl->pos * GD_SIZE(data_type),
        ns * GD_SIZE(data_type));
    gdfl->pos += ns;
  }

  dreturn("%" PRIuSIZE, ns);
  return ns;
}

ssize_t _GD_FlacRead(struct gd_raw_file_ *restrict file, void *restrict data,
    gd_type_t data_type, size_t nmemb)
{
  struct gd_flacdata *gdfl = (struct gd_flacdata *)file->edata;
  char *output = data;
  size_t s, ns = nmemb;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, data, data_type, nmemb);

  for (;;) {
    /* copy the currently loaded frame to the output */
    s = _GD_FlacOutput(gdfl, data_type, output, ns);
    output += s * GD_SIZE(data_type);

    if ((ns -= s) == 0)
      break;

    /* Decode one frame */
    if (!FLAC__stream_decoder_process_single(gdfl->codec.d)) {
      dreturn("%i", -1);
      return -1;
    }

    if (gdfl->error) {
      dreturn("%i", -1);
      return -1;
    }

    /* out of data */
    if (FLAC__stream_decoder_get_state(gdfl->codec.d) ==
        FLAC__STREAM_DECODER_END_OF_STREAM)
    {
      gdfl->stream_end = 1;
      break;
    }
  }

  file->pos = gdfl->base + gdfl->pos;

  dreturn("%li", (long)(nmemb - ns));
  return nmemb - ns;
}

ssize_t _GD_FlacWrite(struct gd_raw_file_ *file, const void *data,
    gd_type_t data_type, size_t nmemb)
{
  struct gd_flacdata *gdfl = (struct gd_flacdata *)file->edata;
  ssize_t n = nmemb;
  const int size = GD_SIZE(data_type);
  size_t i, remaining;
  unsigned c;
  int32_t *buffer[8] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
  const int8_t *i8 = data;
  const int16_t *i16 = data;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, data, data_type, nmemb);

  if (nmemb > 0 && size) {
    /* Allocate buffers */
    for (i = 0; i < gdfl->cps; ++i)
      if ((buffer[i] = malloc(nmemb * sizeof *buffer[i])) == NULL) {
        file->error = GD_FLAC_E_MEM;
        goto WRITE_ERROR;
      }

    /* Data marshalling -- split into 32-bit-wide channels of
     * 8- or 16-bit signed data */
    if (gdfl->bps == 8)
      for (i = 0; i < nmemb; ++i)
        buffer[0][i] = *(i8++);
    else if (gdfl->swap)
      for (i = 0; i < nmemb; ++i)
        for (c = gdfl->cps; c > 0; --c)
          buffer[c - 1][i] = *(i16++);
    else
      for (i = 0; i < nmemb; ++i)
        for (c = 0; c < gdfl->cps; ++c)
          buffer[c][i] = *(i16++);

    /* Write the data -- we have to loop here because libFLAC uses
     * unsigned int for sample count */
    remaining = nmemb;
    while (remaining > 0) {
      unsigned ns = UINT_MAX;
      if (ns > remaining)
        ns = remaining;

      if (!FLAC__stream_encoder_process(gdfl->codec.e, (void*)buffer, ns)) {
        file->error = GD_FLAC_E_SES
          | FLAC__stream_encoder_get_state(gdfl->codec.e);
        goto WRITE_ERROR;
      }
      remaining -= ns;
      file->pos += ns;
    }
  } else if (0) {
WRITE_ERROR:
    n = -1;
  }

  for (i = 0; i < gdfl->cps; ++i)
    free(buffer[i]);
  dreturn("%" PRIdSIZE, n);
  return n;
}

off64_t _GD_FlacSeek(struct gd_raw_file_* file, off64_t offset,
    gd_type_t data_type, unsigned int mode)
{
  struct gd_flacdata *gdfl;

  dtrace("%p, %" PRId64 ", 0x%X, 0x%X", file, (int64_t)offset, data_type, mode);

  gdfl = (struct gd_flacdata *)(file->edata);

  if (mode == GD_FILE_WRITE) {
    /* nothing to do */
    if (file->pos == offset) {
      dreturn("%" PRId64, (int64_t)offset);
      return offset;
    }

    /* we only get here when we need to pad */
    char *zero = malloc(GD_BUFFER_SIZE);
    if (zero == NULL) {
      *gdfl->errnum = GD_FLAC_E_MEM;
      dreturn("%i", -1);
      return -1;
    }
    memset(zero, 0, GD_BUFFER_SIZE);

    while (file->pos < offset) {
      off64_t remaining = offset - file->pos;
      int n = GD_BUFFER_SIZE / GD_SIZE(data_type);
      if (n > remaining)
        n = remaining;

      _GD_FlacWrite(file, zero, GD_UINT8, n);
      if (file->error) {
        free(zero);
        dreturn("%i", -1);
        return -1;
      }
    }
    free(zero);
  } else {
    /* nothing to do */
    if (gdfl->base + gdfl->pos == offset) {
      dreturn("%" PRId64, (int64_t)offset);
      return offset;
    }

    int eof = 0;
    off64_t last = FLAC__stream_decoder_get_total_samples(gdfl->codec.d) - 1;

    if (last < 0) { /* no data -- fake it */
      file->pos = 0;
      dreturn("%i", 0);
      return 0;
    }

    if (offset > last) {
      offset = last;
      eof = 1;
    }

    if (!FLAC__stream_decoder_seek_absolute(gdfl->codec.d, offset)) {
      file->error = GD_FLAC_E_SDS |
        FLAC__stream_decoder_get_state(gdfl->codec.d);
      dreturn("%i", -1);
      return -1;
    }

    if (eof) {
      /* advance to EOF by reading the last frame */
      if (!FLAC__stream_decoder_process_single(gdfl->codec.d)) {
        dreturn("%i", -1);
        return -1;
      }
      /* and "consume" it */
      gdfl->pos = gdfl->dlen;

      gdfl->stream_end = 1;
      offset++;
    }
  }
  
  file->pos = offset;

  dreturn("%" PRId64, (int64_t)file->pos);
  return file->pos;
}

int _GD_FlacClose(struct gd_raw_file_ *file)
{
  struct gd_flacdata *gdfl = (struct gd_flacdata *)file->edata;
  dtrace("%p", file);

  if (file->mode & GD_FILE_READ) {
    FLAC__stream_decoder_finish(gdfl->codec.d);
    FLAC__stream_decoder_delete(gdfl->codec.d);
  } else {
    FLAC__stream_encoder_finish(gdfl->codec.e);
    FLAC__stream_encoder_delete(gdfl->codec.e);
  }

  free(gdfl->data);
  file->idata = -1;
  file->mode = 0;
  free(file->edata);
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_FlacSize(int dirfd, struct gd_raw_file_ *file, gd_type_t data_type,
    int swap)
{
  struct gd_flacdata *gdfl;
  off_t n;

  dtrace("%i, %p, 0x%X, %i", dirfd, file, data_type, swap);

  gdfl = _GD_FlacDoOpen(dirfd, file, data_type, swap, GD_FILE_READ);

  if (gdfl == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  n = FLAC__stream_decoder_get_total_samples(gdfl->codec.d);

  FLAC__stream_decoder_finish(gdfl->codec.d);
  FLAC__stream_decoder_delete(gdfl->codec.d);
  free(gdfl);

  dreturn("%" PRId64, (int64_t)n);
  return n;
}

int _GD_FlacStrerr(const struct gd_raw_file_ *file, char *buf, size_t buflen)
{
  int r = 0;

  dtrace("%p, %p, %" PRIuSIZE, file, buf, buflen);

  switch(file->error) {
    case GD_FLAC_E_EOS:
      strncpy(buf, "Cannot seek past end of stream", buflen);
      break;
    case GD_FLAC_E_CPS:
      strncpy(buf, "Invalid channels per sample in FLAC stream", buflen);
      break;
    case GD_FLAC_E_BPS:
      strncpy(buf, "Invalid bits per sample in FLAC stream", buflen);
      break;
    case GD_FLAC_E_IO:
    case GD_FLAC_E_SES | FLAC__STREAM_ENCODER_IO_ERROR:
      r = gd_StrError(errno, buf, buflen);
      break;
    case GD_FLAC_E_MEM:
    case GD_FLAC_E_SDS | FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR:
    case GD_FLAC_E_SES | FLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR:
      strncpy(buf, "FLAC: Out of memory", buflen);
      break;
    case GD_FLAC_E_SDS | FLAC__STREAM_DECODER_SEEK_ERROR:
      strncpy(buf, "FLAC: Seek error", buflen);
      break;
    case GD_FLAC_E_SDIS 
        | FLAC__STREAM_DECODER_INIT_STATUS_UNSUPPORTED_CONTAINER:
    case GD_FLAC_E_SEIS 
        | FLAC__STREAM_ENCODER_INIT_STATUS_UNSUPPORTED_CONTAINER:
      strncpy(buf, "FLAC: Unsupported container", buflen);
      break;
    case GD_FLAC_E_SDIS | FLAC__STREAM_DECODER_INIT_STATUS_ALREADY_INITIALIZED:
    case GD_FLAC_E_SEIS | FLAC__STREAM_ENCODER_INIT_STATUS_ALREADY_INITIALIZED:
    case GD_FLAC_E_SDIS | FLAC__STREAM_DECODER_INIT_STATUS_INVALID_CALLBACKS:
    case GD_FLAC_E_SEIS | FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_CALLBACKS:
    case GD_FLAC_E_SEIS
      | FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_NUMBER_OF_CHANNELS:
    case GD_FLAC_E_SEIS
      | FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_BITS_PER_SAMPLE:
    case GD_FLAC_E_SEIS | FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_SAMPLE_RATE:
    case GD_FLAC_E_SEIS | FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_BLOCK_SIZE:
    case GD_FLAC_E_SES  | FLAC__STREAM_ENCODER_UNINITIALIZED:
    case GD_FLAC_E_SES  | FLAC__STREAM_ENCODER_CLIENT_ERROR:
      /* these indicate bugs in our code */
      snprintf(buf, buflen, "Internal error 0x%X in FLAC encoding",
          file->error);
      break;
    case GD_FLAC_E_SDIS | FLAC__STREAM_DECODER_INIT_STATUS_ERROR_OPENING_FILE:
      strncpy(buf, "FLAC: Error opening file", buflen);
      break;
    case GD_FLAC_E_SES | FLAC__STREAM_ENCODER_FRAMING_ERROR:
      strncpy(buf, "FLAC: Framing error", buflen);
      break;
    default:
      snprintf(buf, buflen, "FLAC: Unkown error 0x%X", file->error);
      break;
  }

  dreturn("%i", r);
  return r;
}
