/* Copyright (C) 2011-2016 D. V. Wiebe
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

/* define to debug this unit */
#undef DEBUG_SIE

/* Note: when using STDIO, glibc reads files in 4kiB chunks, so for small SIE
 * files (4kiB = 170-455 records, depending on data size), all the I/O in this
 * file happens in RAM, which probably means it's not terribly much slower than
 * an in-RAM implementation like daisie, with the added benefit of not having
 * to contort ourselves to play nice with the bookkeeping layer.
 *
 * On the other hand, this means we're basically unable to read SIE files
 * concurrently (which we really shouldn't have expected to work anyways).
 */

#ifdef DEBUG_SIE
#define dprintf_sie dprintf
#else
#define dprintf_sie(...)
#endif

#define DPRINTF dprintf_sie("F  r:%" PRIdSIZE " p:0x%" PRIX64 " s:%i,0x%" PRIX64 " " \
    "d:0x%" PRIX64 ",0x%" PRIX64 " l:%i/0x%" PRIX64 ",0x%" PRIX64 " @%li", \
    f->r, f->p, f->bof, f->s, f->d[0], f->d[1], f->have_l, f->l[0], f->l[1], \
    ftell(f->fp));

struct gd_siedata {
  int swap; /* byte swapping required */
  FILE *fp; /* stream */

  ssize_t r; /* current record number */
  int64_t p; /* I/O pointer in samples */
  int64_t s; /* record ending sample */
  int64_t d[3]; /* the raw record:
                   d[0] is the sample number in storage order 
                   d[1] and d[2] are space for the data */
  int64_t l[3]; /* the previous record */
  int have_l; /* a flag to indicate that l is initialised */
  int bof;    /* this is the first record */
  int header; /* non-zero if we have a header */
};

/* Header size in bytes */
#define HEADSIZE 9

/* correct for byte sex */
#define FIXSEX(swap,v) ((swap) ? (int64_t)gd_swap64(v) : (v))

static int _GD_SampIndDiscardHeader(FILE *stream)
{
  int have = 0;
  unsigned char header[HEADSIZE];
  static const unsigned char header_magic[8] = { 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff };

  dtrace("%p", stream);

  if (fread(header, HEADSIZE, 1, stream) < 1) {
    dreturn("%i", -1);
    return -1;
  }

  /* Check for magic */
#ifdef DEBUG_SIE
  dprintf("header: 0x%02X 0x%02X 0x%02X 0x%02X 0x%02X 0x%02X 0x%02X 0x%02X "
      "0x%02X", header[0], header[1], header[2], header[3], header[4],
      header[5], header[6], header[7], header[8]);
#endif
  if (memcmp(header, header_magic, 8) == 0) {
    int c;
    /* We have magic, check for trailing data, which indicates there's another
     * record (so what we have is indeed a header) */
    c = getc(stream);
    if (c == EOF) {
      if (ferror(stream)) {
        dreturn("%i", -1);
        return -1;
      } /* not a header; rewind so we can read it again */
      rewind(stream);
    } else { /* trailing data, unget it so it's available for the first read */
      if (ungetc(c, stream) == EOF) {
        dreturn("%i", -1);
        return -1;
      }
      have = 1;
    }
  } else /* Bad header magic = no header: rewind */
    rewind(stream);

  dreturn("%i", have);
  return have;
}

static int _GD_SampIndDoOpen(int fdin, struct gd_raw_file_ *file,
    struct gd_siedata *f, int swap, unsigned int mode)
{
  int fd;
  FILE *stream;

  dtrace("%i, %p, %i, 0x%X", fdin, file, swap, mode);

  if (!(mode & GD_FILE_TEMP)) {
    fd = gd_OpenAt(file->D, fdin, file->name, ((mode & GD_FILE_WRITE) ?
          (O_RDWR | O_CREAT) : O_RDONLY) | O_BINARY, 0666);
  } else {
    fd = _GD_MakeTempFile(file->D, fdin, file->name);
  }

  if (fd < 0) {
    dreturn("%i", -1);
    return -1;
  }
  stream = fdopen(fd, (mode & GD_FILE_WRITE) ? "rb+" : "rb");

  if (stream == NULL) {
    close(fd);
    dreturn("%i", -1);
    return -1;
  }

  if (!(mode & GD_FILE_WRITE)) {
    f->header = _GD_SampIndDiscardHeader(stream);
    if (f->header < 0) {
      fclose(stream);
      dreturn("%i", -1);
      return -1;
    }
  } else
    f->header = 0;

  memset(f, 0, sizeof(struct gd_siedata));
  f->r = f->s = f->p = f->d[0] = -1;
  f->fp = stream;
  f->swap = swap;

  dreturn("%i", fd);
  return fd;
}

int _GD_SampIndOpen(int fd, struct gd_raw_file_ *file,
    gd_type_t data_type gd_unused_, int swap, unsigned int mode)
{
  dtrace("%i, %p, <unused>, %i, 0x%X", fd, file, swap, mode);

  file->edata = malloc(sizeof(struct gd_siedata));

  if (file->edata == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  file->idata = _GD_SampIndDoOpen(fd, file, (struct gd_siedata*)file->edata,
      swap, mode);

  if (file->idata < 0) {
    free(file->edata);
    dreturn("%i", -1);
    return -1;
  }

  file->mode = mode;
  file->pos = 0;
  dreturn("%i", 0);
  return 0;
}

/* advance one record, with byte swapping and error checking; returns -1 on EOF,
 * -2 on error */
static int _GD_Advance(struct gd_siedata *f, size_t size)
{
  size_t n;
  int64_t p = f->s + 1;
  int64_t l[3];
  dtrace("%p, %" PRIuSIZE, f, size);

  /* save the current record */
  if (p > 0)
    memcpy(l, f->d, size);

  /* read the next record */
  n = fread(f->d, size, 1, f->fp);

  if (n != 1) {
    if (ferror(f->fp)) {
      dreturn("%i", -2);
      return -2;
    } else {
      /* we're past the end of the last record */
      f->p = f->s + 1;
      dreturn("%i", -1);
      return -1;
    }
  }

  /* handle newly read record */
  f->s = FIXSEX(f->swap, f->d[0]);
  f->p = p;
  f->r++;

  if (p > 0) {
    memcpy(f->l, l, size);
    f->have_l = 1;
    f->bof = 0;
  } else {
    f->have_l = 0;
    f->bof = 1;
  }

  dreturn("%i", 0);
  return 0;
}

off64_t _GD_SampIndSeek(struct gd_raw_file_ *file, off64_t sample,
    gd_type_t data_type, unsigned int mode)
{
  int r;
  const size_t size = sizeof(int64_t) + GD_SIZE(data_type);
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);

  dtrace("%p, 0x%" PRIX64 ", 0x%X, 0x%X", file, (uint64_t)sample, data_type,
      mode);

  if (file->pos == sample && f->p >= 0) {
    dreturn("0x%" PRIX64, (uint64_t)sample);
    return sample;
  }

  if (sample < f->p) {
    /* seek backwards -- reading a file backwards doesn't necessarily work
     * that well.  So, let's just rewind to the beginning and try again. */
    rewind(f->fp);

    /* Advance past header if necessary */
    if (f->header) {
      char header[HEADSIZE];
      if (fread(header, HEADSIZE, 1, f->fp) < 1) {
        dreturn("%i", -1);
        return -1;
      }
    }

    file->idata = 0;
    f->r = f->s = f->p = f->d[0] = -1;
    f->bof = 1;
    f->have_l = 0;
  }

  while (sample > f->s) {
    DPRINTF;

    /* seek ahead ... */
    r = _GD_Advance(f, size);
    if (r == -2) {
      dreturn("%i", -1);
      return -1;
    } else if (r == -1)
      break;
  }
  DPRINTF;

  if ((mode & GD_FILE_WRITE) && sample > f->s && sample > 0) {
    char zero[16];
    memset(zero, 0, 16);

    /* does the last record have a value of zero? */
    if (memcmp(f->d + 1, &zero, GD_SIZE(data_type)) == 0
        && ftell(f->fp) > 0)
    {
      /* in this case, just increase the current record's end */
      f->s = sample;
      f->d[0] = FIXSEX(f->swap, sample);

      /* back up and update the file */
      if (fseek(f->fp, -size, SEEK_CUR) || fwrite(f->d, size, 1, f->fp) < 1)
      {
        dreturn("%i", -1);
        return -1;
      }
      /* The MSVCRT's stdio seems to screw up without the following: */
      fflush(f->fp);

      DPRINTF;
    } else {
      /* add a new record */
      f->r++;
      f->bof = 0;
      f->s = sample;
      memcpy(f->l, f->d, size);
      f->have_l = 1;
      f->d[0] = FIXSEX(f->swap, sample);
      f->d[1] = f->d[2] = 0;
      if (fwrite(f->d, size, 1, f->fp) < 1) {
        dreturn("%i", -1);
        return -1;
      }
      fflush(f->fp);

      DPRINTF;
    }
  }
  file->pos = f->p = sample;

  DPRINTF;

  dreturn("0x%" PRIX64, (uint64_t)sample);
  return (off64_t)(sample);
}

/* store n copies of s, which is of length l, in d */
static void *_GD_Duplicate(void *restrict d, const void *restrict s, size_t l,
    int64_t n)
{
  int64_t i;
  dtrace("%p, %p, %" PRIuSIZE ", 0x%" PRIX64, d, s, l, (uint64_t)n);

  if (n > 0) {
    if (l == 1) {
      memset(d, *(char*)s, (size_t)n);
      d = (char*)d + n;
    } else if (l == 2) {
      uint16_t v = *(uint16_t*)s;
      uint16_t *p = (uint16_t*)d;
      for (i = 0; i < n; ++i)
        *(p++) = v;
      d = p;
    } else if (l == 4) {
      uint32_t v = *(uint32_t*)s;
      uint32_t *p = (uint32_t*)d;
      for (i = 0; i < n; ++i)
        *(p++) = v;
      d = p;
    } else if (l == 8) {
      uint64_t v = *(uint64_t*)s;
      uint64_t *p = (uint64_t*)d;
      for (i = 0; i < n; ++i)
        *(p++) = v;
      d = p;
    } else if (l == 16) {
#ifndef GD_NO_C99_API
      double complex v = *(double complex*)s;
      double complex *p = (double complex*)d;
      for (i = 0; i < n; ++i)
        *(p++) = v;
#else
      double v[2];
      double *p = (double *)d;
      v[0] = ((double*)s)[0];
      v[1] = ((double*)s)[1];
      for (i = 0; i < n; ++i) {
        *(p++) = v[0];
        *(p++) = v[1];
      }
#endif
      d = p;
    }
  }

  dreturn("%p", d);
  return d;
}

ssize_t _GD_SampIndRead(struct gd_raw_file_ *restrict file, void *restrict ptr,
    gd_type_t data_type, size_t nelem)
{
  int r;
  ssize_t count = 0;
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);
  void *cur = ptr;

  dtrace("%p, %p, 0x%03x, %" PRIuSIZE, file, ptr, data_type, nelem);

  /* not enough data in the current run */
  while (f->s - f->p < (int64_t)(nelem - count)) {
    /* copy what we've got */
    cur = _GD_Duplicate(cur, f->d + 1, GD_SIZE(data_type), f->s - f->p + 1);
    count += f->s - f->p + 1;

    DPRINTF;

    /* advance */
    r = _GD_Advance(f, sizeof(int64_t) + GD_SIZE(data_type));
    if (r == -2) {
      dreturn("%i", -1);
      return -1;
    } else if (r == -1)
      break;
  }
  DPRINTF;

  /* copy the remnant */
  if (f->s - f->p >= (int64_t)(nelem - count)) {
    _GD_Duplicate(cur, f->d + 1, GD_SIZE(data_type), nelem - count);
    f->p += nelem - count;
    count = nelem;
  } else {
    _GD_Duplicate(cur, f->d + 1, GD_SIZE(data_type), f->s - f->p + 1);
    count += f->s - f->p + 1;
    f->p = f->s + 1;
  }

  DPRINTF;

  file->pos = f->p;

  dreturn("%" PRIdSIZE, count);
  return count;
}

/* return the number of records in the file */
static ssize_t _GD_GetNRec(struct gd_siedata *f, size_t size)
{
  gd_stat64_t statbuf;
  dtrace("%p, %" PRIuSIZE, f, size);

  if (gd_fstat64(fileno(f->fp), &statbuf)) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%" PRIdSIZE, (ssize_t)(statbuf.st_size / size));
  return (ssize_t)(statbuf.st_size / size);
}

ssize_t _GD_SampIndWrite(struct gd_raw_file_ *restrict file,
    const void *restrict ptr, gd_type_t data_type, size_t nelem)
{
  ssize_t nrec;
  void *p;
  size_t i;
  ssize_t rin = 0, rout = 0, plen = 10;
  int64_t fr;
  int r;
  int64_t *cur_end;
  int64_t end;
  void *cur_datum, *buffer;
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);
  const size_t dlen = GD_SIZE(data_type);
  const size_t size = sizeof(int64_t) + dlen;
  dtrace("%p, %p, 0x%03x, %" PRIuSIZE, file, ptr, data_type, nelem);

  if ((nrec = _GD_GetNRec(f, size)) < 0) {
    dreturn("%i", -1);
    return -1;
  }

  /* compress the data in core first, including the current record. */
  p = malloc(plen * size);
  memcpy(p, f->d, size);
  cur_end = (int64_t*)p;
  cur_datum = ((int64_t*)p) + 1;
  DPRINTF;

  if ((f->r == -1 || f->bof) && f->p == 0) {
    /* we're overwriting everything from the start */
    memcpy(cur_datum, ptr, dlen);
  } else if (!f->bof) {
    int64_t ls;
    int need_advance = 0;

    /* need to check the previous record to see if we're overwriting this whole
     * record and we need to combine with the previous one */
    if (!f->have_l) {
      /* forced to back up to read the previous record */
      if (fseek(f->fp, -2 * size, SEEK_CUR) ||
          (fread(f->l, size, 1, f->fp) < 1))
      {
        free(p);
        dreturn("%i", -1);
        return -1;
      }
      need_advance = 1;

      DPRINTF;
    }

    /* the ending sample of the previous record */
    ls = FIXSEX(f->swap, f->l[0]);

    /* if we're not at the start of the current record, we don't need to
     * do anything fancy */
    if (f->p == ls + 1) {
      /* we're completely overwriting the current record, so update the value */
      memcpy(cur_datum, ptr, dlen);
      
      /* then check whether that's the same as the value of the old record; if
       * it is, combine them
       */
      if (memcmp(f->l + 1, ptr, dlen) == 0) {
        dprintf_sie("combine: 0x%" PRIX64, f->l[1]);
        /* the new value is the same as the value of the previous record, so
         * back up a record and combine */

        /* rewind if we haven't already done so */
        if (f->have_l) {
          if (fseek(f->fp, -size, SEEK_CUR)) {
            free(p);
            dreturn("%i", -1);
            return -1;
          }
        }
        need_advance = 0;

        f->r--;
        memcpy(f->d, f->l, size);
        memcpy(p, f->d, size);
        f->s = FIXSEX(f->swap, f->l[0]);
        f->have_l = 0;
      }
    }

    /* reset the file pointer if we didn't have to rewind but were forced to */
    if (need_advance) {
      if (fseek(f->fp, size, SEEK_CUR)) {
        free(p);
        dreturn("%i", -1);
        return -1;
      }
      f->have_l = 1;
    }

    DPRINTF;
  }

  for (i = 0; i < nelem; ++i) {
    if (memcmp(((const char*)ptr) + i * dlen, cur_datum, dlen)) {
      dprintf_sie("mismatch on sample %zu:", i);
      if (++rin == plen) {
        void *p2;
        plen *= 2;
        p2 = realloc(p, plen * size);
        if (p2 == NULL) {
          free(p);
          dreturn("%i", -1);
          return -1;
        }
        p = p2;
        cur_end = (int64_t*)((char*)p + size * (rin - 1));
      }
      end = f->p + i - 1;
      dprintf_sie("*cur_end:   0x%" PRIX64 " -> 0x%" PRIX64, FIXSEX(f->swap,
            *cur_end), end);
      gd_put_unaligned64(FIXSEX(f->swap, end), cur_end);
      dprintf_sie(" cur_end:   %p -> %p", cur_end, (char*)p + size * rin);
      cur_end = (int64_t*)((char*)p + size * rin);
      dprintf_sie(" cur_datum: %p -> %p", cur_datum, cur_end + 1);
      cur_datum = cur_end + 1;
      dprintf_sie("*cur_datum: %X -> %X", *((char*)cur_datum),
          *(((const char*)ptr) + i * dlen));
      memcpy(cur_datum, ((const char*)ptr) + i * dlen, dlen);
    }
  }
  end = f->p + nelem - 1;
  dprintf_sie("*cur_end:   0x%" PRIX64 " -> 0x%" PRIX64, FIXSEX(f->swap,
        *cur_end), end);
  gd_put_unaligned64(FIXSEX(f->swap, end), cur_end);
  rin++;

  for (i = 0; i < (size_t)rin; ++i) {
    dprintf_sie("%zu: 0x%X 0x%X 0x%X 0x%X 0x%X 0x%X 0x%X 0x%X 0x%X",
        i, ((char*)p)[size * i + 0], ((char*)p)[size * i + 1],
        ((char*)p)[size * i + 2], ((char*)p)[size * i + 3],
        ((char*)p)[size * i + 4], ((char*)p)[size * i + 5],
        ((char*)p)[size * i + 6], ((char*)p)[size * i + 7],
        ((char*)p)[size * i + 8]);
  }

  /* determine how many records we have to replace */
  fr = f->r;
  if (fr < 0) {
    fr = 0;
    rout--;
  }

  while (f->s <= end) {
    ++rout;

    DPRINTF;

    r = _GD_Advance(f, sizeof(int64_t) + GD_SIZE(data_type));
    if (r == -2) {
      free(p);
      dreturn("%i", -1);
      return -1;
    } else if (r == -1)
      break;
  }
  DPRINTF;

  dprintf_sie("nrec:%" PRIdSIZE " rin:%" PRIdSIZE " rout:%" PRIdSIZE
      " fr:%" PRId64, nrec, rin, rout, fr);

  /* now, do some moving: first, move the trailing records, forward by
   * (rin - rout) records */
  if (nrec - (fr + rout) > 0) {
    buffer = malloc((nrec - (fr + rout)) * size);
    if (buffer == NULL) {
      free(p);
      dreturn("%i", -1);
      return -1;
    }
    if (fseek(f->fp, (fr + rout) * size, SEEK_SET) ||
        (fread(buffer, size, nrec - (fr + rout), f->fp)
         < (size_t)(nrec - (fr + rout))))
    {
      free(buffer);
      free(p);
      dreturn("%i", -1);
      return -1;
    }
    if (fseek(f->fp, (fr + rin) * size, SEEK_SET) ||
        (fwrite(buffer, size, nrec - (fr + rout), f->fp)
         < (size_t)(nrec - (fr + rout))))
    {
      free(buffer);
      free(p);
      dreturn("%i", -1);
      return -1;
    }
    free(buffer);
  }

  /* now insert the new records */
  if (fseek(f->fp, fr * size, SEEK_SET) ||
      (fwrite(p, size, rin, f->fp) < (size_t)rin))
  {
    free(p);
    dreturn("%i", -1);
    return -1;
  }

  /* truncate the file if necessary */
  if (rin < rout) {
    if (fflush(f->fp)) {
      free(p);
      dreturn("%i", -1);
      return -1;
    }
    if (gd_truncate(fileno(f->fp), (nrec - rout + rin) * size)) {
      free(p);
      dreturn("%i", -1);
      return -1;
    }
  }

  /* update the current record */
  memcpy(f->d, (char *)p + (rin - 1) * size, size);
  if (rin > 1) 
    f->bof = 0;
  else
    f->bof = 1;

  f->p = f->s = FIXSEX(f->swap, f->d[0]);
  f->r = fr + rin - 1;
  f->have_l = 0;

  DPRINTF;

  free(p);

  file->pos = f->p;
  dreturn("%" PRIuSIZE, nelem);
  return nelem;
}

int _GD_SampIndSync(struct gd_raw_file_ *file)
{
  int ret;
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);

  dtrace("%p", file);

  ret = fflush(f->fp);

  if (!ret)
    ret = fsync(fileno(f->fp));

  dreturn("%i", ret);
  return ret;
}

int _GD_SampIndClose(struct gd_raw_file_* file)
{
  int ret;
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);

  dtrace("%p", file);

  ret = fclose(f->fp);
  if (ret != EOF) {
    file->mode = 0;
    file->idata = -1;
    free(file->edata);
    file->edata = NULL;
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

off64_t _GD_SampIndSize(int dirfd, struct gd_raw_file_* file,
    gd_type_t data_type, int swap)
{
  struct gd_siedata f;
  ssize_t last_rec;
  int64_t n;
  const size_t size = sizeof(int64_t) + GD_SIZE(data_type);

  dtrace("%i, %p, 0x%03x, %i", dirfd, file, data_type, swap);

  /* open */
  if (_GD_SampIndDoOpen(dirfd, file, &f, swap, GD_FILE_READ) < 0) {
    dreturn("%i", -1);
    return -1;
  }

  /* find the last record */
  last_rec = _GD_GetNRec(&f, size) - 1;

  /* seek to this record */
  if (fseeko64(f.fp, last_rec * size, SEEK_SET)) {
    fclose(f.fp);
    dreturn("%i", -1);
    return -1;
  }

  /* read the sample index */
  if (fread(&n, sizeof(uint64_t), 1, f.fp) != 1) {
    fclose(f.fp);
    dreturn("%i", -1);
    return -1;
  }

  if (swap)
    n = gd_swap64(n);

  fclose(f.fp);

  /* check for overflow */
  if (n > GD_INT64_MAX - 1) {
    errno = ERANGE;
    dreturn("%i", -1);
    return -1;
  }

  dreturn("0x%" PRIX64, (uint64_t)(n + 1));
  return n + 1;
}
