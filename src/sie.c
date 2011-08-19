/* Copyright (C) 2011 D. V. Wiebe
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
#include <inttypes.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#endif

struct gd_siedata {
  int swap;
  ssize_t r;
  int64_t p;
  int64_t s;
  FILE *fp;
  int64_t d[3];
};

static int _GD_SampIndDoOpen(int dirfd, struct _gd_raw_file *file,
    struct gd_siedata *f, int swap, int mode, int creat)
{
  int fd;
  FILE *stream;

  dtrace("%i, %p, %i, %i, %i", dirfd, file, swap, mode, creat);

  fd = gd_OpenAt(file->D, dirfd, file->name, ((mode == GD_RDWR) ? O_RDWR :
        O_RDONLY) | (creat ? O_CREAT : 0) | O_BINARY, 0666);
  
  if (fd < 0) {
    dreturn("%i", -1);
    return -1;
  }

  stream = fdopen(fd, (mode == GD_RDWR) ? "r+" : "r");

  if (stream == NULL) {
    close(fd);
    dreturn("%i", -1);
    return -1;
  }

  memset(f, 0, sizeof(struct gd_siedata));
  f->r = f->s = f->p = f->d[0] = -1;
  f->fp = stream;
  f->swap = swap;
  dreturn("%i", 0);
  return 0;
}

int _GD_SampIndOpen(int dirfd, struct _gd_raw_file *file, int swap, int mode,
    int creat)
{
  dtrace("%i, %p, %i, %i, %i", dirfd, file, swap, mode, creat);

  file->edata = malloc(sizeof(struct gd_siedata));

  if (file->edata == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  if (_GD_SampIndDoOpen(dirfd, file, file->edata, swap, mode, creat)) {
    free(file->edata);
    dreturn("%i", -1);
    return -1;
  }

  file->idata = 0;
  dreturn("%i", 0);
  return 0;
}

/* advance one record, with byte swapping and error checking; returns -1 on EOF,
 * -2 on error */
static int _GD_Advance(struct gd_siedata *f, size_t size)
{
  int64_t p[3];
  size_t n;
  dtrace("%p, %zi", f, size);

  /* save the current record */
  memcpy(p, f->d, 3 * sizeof(int64_t));

  /* read the next record */
  n = fread(f->d, size, 1, f->fp);
  if (f->swap)
    f->d[0] = gd_swap64(f->d[0]);

  if (ferror(f->fp)) {
    dreturn("%i", -2);
    return -2;
  } else if (n != 1) {
    /* ignore short reads, resulting from corrupted files .. go back to
     * the last good record */
    fseek(f->fp, -n, SEEK_CUR);
    /* and restore it */
    memcpy(f->d, p, 3 * sizeof(int64_t));
    f->s = f->d[0];
    f->p = f->d[0] + 1;
    dreturn("%i", -1);
    return -1;
  } else if (feof(f->fp)) {
    f->s = f->d[0];
    f->p = f->d[0] + 1;
    dreturn("%i", -1);
    return -1;
  }

  f->s = f->p = p[0] + 1;
  f->r++;

  dreturn("%i", 0);
  return 0;
}

off64_t _GD_SampIndSeek(struct _gd_raw_file *file, off64_t sample,
    gd_type_t data_type, int pad)
{
  int r;
  const size_t size = sizeof(int64_t) + GD_SIZE(data_type);
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);

  dtrace("%p, %llx, 0x%03x, %i", file, (long long)sample, data_type, pad);

  if (sample < f->p) {
    /* seek backwards -- reading a file backwards doesn't necessarily work
     * that well.  So, let's just rewind to the beginning and try again. */
    rewind(f->fp);
    file->idata = 0;
    f->r = f->p = f->d[0] = -1;
  }

  while (sample > f->d[0]) {
    /* seek ahead ... */
    r = _GD_Advance(f, size);
    if (r == -2) {
      dreturn("%i", -1);
      return -1;
    } else if (r == -1)
      break;
  }

  if (pad && sample > f->d[0]) {
    double complex p = 0;
    if (memcmp(f->d + 1, &p, GD_SIZE(data_type)) == 0) {
      /* in this case, just increase the current record's end */
      f->d[0] = sample;
      /* back up and update the file */
      fseek(f->fp, -size, SEEK_CUR);
      fwrite(f->d, size, 1, f->fp);
    } else {
      /* add a new record */
      f->d[0] = sample;
      f->d[1] = f->d[2] = 0;
      fwrite(f->d, size, 1, f->fp);
    }
    f->s = sample;
  }
  f->p = sample;

  dreturn("%llx", f->p);
  return (off64_t)(f->p);
}

/* store n copies of s, which is of length l, in d */
static void *_GD_Duplicate(void *d, void *s, size_t l, int64_t n)
{
  int64_t i;
  dtrace("%p, %p, %zi, 0x%llx", d, s, l, (long long)n);

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
    double complex v = *(double complex*)s;
    double complex *p = (double complex*)d;
    for (i = 0; i < n; ++i)
      *(p++) = v;
    d = p;
  }

  dreturn("%p", d);
  return d;
}

ssize_t _GD_SampIndRead(struct _gd_raw_file *file, void *ptr,
    gd_type_t data_type, size_t nelem)
{
  int r;
  ssize_t count = 0;
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);
  void *cur = ptr;

  dtrace("%p, %p, 0x%03x, %zu", file, ptr, data_type, nelem);

  /* not enough data in the current run */
  while (f->d[0] - f->p <= (int64_t)(nelem - count)) {
    /* copy what we've got */
    cur = _GD_Duplicate(cur, f->d + 1, GD_SIZE(data_type), f->d[0] - f->p + 1);
    count += f->d[0] - f->p + 1;

    /* advance */
    r = _GD_Advance(f, sizeof(int64_t) + GD_SIZE(data_type));
    if (r == -2) {
      dreturn("%i", -1);
      return -1;
    } else if (r == -1)
      break;
  }

  /* copy the remnant */
  if (f->d[0] - f->p >= (int64_t)(nelem - count)) {
    _GD_Duplicate(cur, f->d + 1, GD_SIZE(data_type), nelem - count);
    f->p += nelem - count;
    count = nelem;
  } else {
    cur = _GD_Duplicate(cur, f->d + 1, GD_SIZE(data_type), f->d[0] - f->p + 1);
    count += f->d[0] - f->p + 1;
    f->p = f->d[0] + 1;
  }

  dreturn("%llx", (long long)count);
  return count;
}

/* return the number of records in the file */
static ssize_t _GD_GetNRec(struct gd_siedata *f, size_t size)
{
  gd_stat64_t statbuf;
  dtrace("%p, %zi", f, size);

  if (gd_fstat64(fileno(f->fp), &statbuf)) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%llx", statbuf.st_size / size);
  return (ssize_t)(statbuf.st_size / size);
}

ssize_t _GD_SampIndWrite(struct _gd_raw_file *file, const void *ptr,
    gd_type_t data_type, size_t nelem)
{
  ssize_t nrec;
  void *p;
  size_t i;
  ssize_t rin = 0, rout = 0, plen = 10;
  int64_t fr;
  int r;
  int64_t *cur_end;
  void *cur_datum, *buffer;
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);
  const size_t dlen = GD_SIZE(data_type);
  const size_t size = sizeof(int64_t) + dlen;
  dtrace("%p, %p, 0x%03x, %zi", file, ptr, data_type, nelem);

  if ((nrec = _GD_GetNRec(f, size)) < 0) {
    dreturn("%i", -1);
    return -1;
  }

  /* compress the data in core first, including the current record. */
  p = malloc(plen * size);
  memcpy(p, f->d, size);
  cur_end = (int64_t*)p;
  cur_datum = ((int64_t*)p) + 1;

  /* to prevent weirdness... */
  if (f->p == f->s)
    memcpy(cur_datum, ptr, dlen);

  for (i = 0; i < nelem; ++i) {
    if (memcmp(((const char*)ptr) + i * dlen, cur_datum, dlen)) {
      if (++rin == plen) {
        plen += 10;
        void *p2 = realloc(p, plen * size);
        if (p2 == NULL) {
          free(p);
          dreturn("%i", -1);
          return -1;
        }
        p2 = p;
      }
      *cur_end = f->p + i - 1;
      cur_end = (int64_t*)((char*)p + size * rin);
      cur_datum = cur_end + 1;
      memcpy(cur_datum, ((const char*)ptr) + i * dlen, dlen);
    }
  }
  *cur_end = f->p + nelem - 1;
  rin++;

  /* determine how many records we have to replace */
  fr = f->r;
  if (fr < 0) {
    fr = 0;
    rout--;
  }

  while (f->d[0] <= *cur_end) {
    ++rout;

    r = _GD_Advance(f, sizeof(int64_t) + GD_SIZE(data_type));
    if (r == -2) {
      free(p);
      dreturn("%i", -1);
      return -1;
    } else if (r == -1)
      break;
  }

  /* fix the endianness */
  if (f->swap)
    for (i = 0; i < (size_t)rin; ++i)
      *((uint64_t*)(((char*)p) + size * i)) =
        gd_swap64(*((uint64_t*)(((char*)p) + size * i)));

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
  free(p);

  /* truncate the file if necessary */
  if (rin < rout)
    gd_truncate(fileno(f->fp), nrec - rout + rin);

  dreturn("%llu", (unsigned long long)nelem);
  return nelem;
}

int _GD_SampIndSync(struct _gd_raw_file *file)
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

int _GD_SampIndClose(struct _gd_raw_file* file)
{
  int ret;
  struct gd_siedata *f = (struct gd_siedata*)(file->edata);

  dtrace("%p", file);

  ret = fclose(f->fp);
  if (ret != EOF) {
    file->idata = -1;
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

off64_t _GD_SampIndSize(int dirfd, struct _gd_raw_file* file,
    gd_type_t data_type, int swap)
{
  struct gd_siedata f;
  ssize_t last_rec;
  int64_t n;
  const size_t size = sizeof(int64_t) + GD_SIZE(data_type);

  dtrace("%i, %p, 0x%03x, %i", dirfd, file, data_type, swap);

  /* open */
  if (_GD_SampIndDoOpen(dirfd, file, &f, swap, GD_RDONLY, 0)) {
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

  dreturn("%llx", (long long unsigned)n);
  return (off64_t)n;
}

int _GD_SampIndTOpen(int fd, struct _gd_raw_file *file, int swap)
{
  FILE *stream;
  struct gd_siedata *f;

  dtrace("%i, %p, %i", fd, file, swap);

  file->edata = malloc(sizeof(struct gd_siedata));
  if (file->edata == NULL) {
    close(fd);
    dreturn("%i", -1);
    return -1;
  }

  stream = fdopen(fd, "w+");

  if (stream == NULL) {
    close(fd);
    dreturn("%i", -1);
    return -1;
  }

  f = (struct gd_siedata*)(file->edata);
  memset(f, 0, sizeof(struct gd_siedata));
  f->r = f->s = f->p = f->d[0] = -1;
  f->fp = stream;
  f->swap = swap;
  file->idata = 0;

  dreturn("%i", 0);
  return 0;
}

int _GD_SampIndTUnlink(int dirfd, struct _gd_raw_file *file)
{
  dtrace("%i, %p", dirfd, file);

  if (file->name != NULL) {
    if (file->idata >= 0)
      if (_GD_SampIndClose(file)) {
        dreturn("%i", -1);
        return -1;
      }

    if (gd_UnlinkAt(file->D, dirfd, file->name, 0)) {
      dreturn("%i", -1);
      return -1;
    }

    free(file->name);
    file->name = NULL;
  }

  dreturn("%i", 0);
  return 0;
}
