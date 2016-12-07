/* Copyright (C) 2008-2013, 2015, 2016 D. V. Wiebe
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

/* The ASCII encoding uses file->edata for the stream pointer */

int _GD_AsciiOpen(int fd, struct gd_raw_file_* file, gd_type_t type gd_unused_,
    int swap gd_unused_, unsigned int mode)
{
  dtrace("%i, %p, <unused>, <unused>, %u", fd, file, mode);

  if (!(mode & GD_FILE_TEMP))
    file->idata = gd_OpenAt(file->D, fd, file->name, ((mode & GD_FILE_WRITE)
          ? (O_RDWR | O_CREAT) : O_RDONLY) | O_BINARY, 0666);
  else
    file->idata = _GD_MakeTempFile(file->D, fd, file->name);

  if (file->idata < 0) {
    dreturn("%i", -1);
    return -1;
  }

  file->edata = fdopen(file->idata, (mode & GD_FILE_WRITE) ? "rb+" : "rb");

  if (file->edata == NULL) {
    close(file->idata);
    file->idata = -1;
    dreturn("%i", -1);
    return -1;
  }

  file->mode = mode | GD_FILE_READ;
  file->pos = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_AsciiSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type gd_unused_, unsigned int mode)
{
  char line[64];

  dtrace("%p, %" PRId64 ", <unused>, 0x%X", file, (int64_t)count, mode);

  if (count < file->pos) {
    rewind((FILE *)file->edata);
    file->pos = 0;
  }

  for (; count > file->pos; ++file->pos)
    if (fgets(line, 64, (FILE *)file->edata) == NULL)
      break;

  if (mode & GD_FILE_WRITE && count > file->pos) {
    strcpy(line, "0\n");
    for (; count > file->pos; ++file->pos)
      fputs(line, (FILE *)file->edata);
  }

  dreturn("%" PRId64, (int64_t)file->pos);
  return file->pos;
}

static const char * _GD_ScanFormat(gd_type_t data_type)
{ 
  const char *fmt;
  dtrace("0x%X", data_type);

  switch(data_type) {
    case GD_UINT8:
#ifndef NO_8BIT_INT_PREFIX
      fmt = "%" SCNu8 "\n";
      break;
#else
      /* FALLTHROUGH */
#endif
    case GD_UINT16:
      fmt = "%" SCNu16 "\n";
      break;
    case GD_INT8:
#ifndef NO_8BIT_INT_PREFIX
      fmt = "%" SCNi8 "\n";
      break;
#else
      /* FALLTHROUGH */
#endif
    case GD_INT16:
      fmt = "%" SCNi16 "\n";
      break;
    case GD_UINT32:
      fmt = "%" SCNu32 "\n";
      break;
    case GD_INT32:
      fmt = "%" SCNi32 "\n";
      break;
    case GD_UINT64:
      fmt = "%" SCNu64 "\n";
      break;
    case GD_INT64:
      fmt = "%" SCNi64 "\n";
      break;
    case GD_FLOAT32:
      fmt = "%f\n";
      break;
    case GD_FLOAT64:
      fmt = "%lf\n";
      break;
    case GD_COMPLEX64:
      fmt = "%f;%f\n";
      break;
    case GD_COMPLEX128:
      fmt = "%lf;%lf\n";
      break;
    default:
      fmt = NULL;
      break;
  }

  dreturn("\"%s\"", fmt);
  return fmt;
}

ssize_t _GD_AsciiRead(struct gd_raw_file_ *restrict file, void *restrict ptr,
    gd_type_t data_type, size_t nmemb)
{
  const char *fmt;
  ssize_t n = 0;
#ifdef NO_8BIT_INT_PREFIX
  short int i16;
#endif

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nmemb);

  fmt = _GD_ScanFormat(data_type);
  if (data_type & GD_COMPLEX) {
    for (n = 0; n < (ssize_t)nmemb; ++n) {
      if (feof((FILE *)file->edata))
        break;

      if (fscanf((FILE *)file->edata, fmt, (char *)ptr + GD_SIZE(data_type) * n,
            (char *)ptr + GD_SIZE(data_type) * n + GD_SIZE(data_type) / 2) < 2)
      {
        if (ferror((FILE *)file->edata))
          n = -1;
        break;
      }
      file->pos++;
    }
  } else {
    for (n = 0; n < (ssize_t)nmemb; ++n) {
      if (ferror((FILE *)file->edata))
        break;


#ifdef NO_8BIT_INT_PREFIX
      if (GD_SIZE(data_type) == 1) {
        if (fscanf((FILE *)file->edata, fmt, &i16) < 1) {
          if (ferror((FILE *)file->edata))
            n = -1;
          break;
        }
        if (data_type & GD_SIGNED)
          *((int8_t *)ptr + GD_SIZE(data_type) * n) = (int8_t)i16;
        else
          *((uint8_t *)ptr + GD_SIZE(data_type) * n) = (uint8_t)i16;
      } else
#endif
      {
        if (fscanf((FILE *)file->edata, fmt, (char *)ptr + GD_SIZE(data_type) *
              n) < 1)
        {
          if (ferror((FILE *)file->edata))
            n = -1;
          break;
        }
      }
      file->pos++;
    }
  }

  dreturn("%" PRIdSIZE, n);
  return n;
}

#define WRITE_ASCII(fmt,t) do { \
  for (n = 0; n < (ssize_t)nmemb; ++n) \
    if (fprintf((FILE *)file->edata, "%" fmt "\n", ((t*)ptr)[n]) < 0) { n = -1; break; } \
} while (0)
#define WRITE_CASCII(fmt,t) do { \
  for (n = 0; n < (ssize_t)nmemb; ++n) \
    if (fprintf((FILE *)file->edata, "%" fmt ";%" fmt "\n", ((t*)ptr)[n*2], ((t*)ptr)[n*2+1]) < 0) \
      { n = -1; break; } \
} while (0)

ssize_t _GD_AsciiWrite(struct gd_raw_file_ *restrict file,
    const void *restrict ptr, gd_type_t data_type, size_t nmemb)
{
  ssize_t n = -1;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nmemb);

  switch (data_type) {
    case GD_UINT8:       WRITE_ASCII(PRIu8 ,  uint8_t); break;
    case GD_INT8:        WRITE_ASCII(PRId8 ,   int8_t); break;
    case GD_UINT16:      WRITE_ASCII(PRIu16, uint16_t); break;
    case GD_INT16:       WRITE_ASCII(PRId16,  int16_t); break;
    case GD_UINT32:      WRITE_ASCII(PRIu32, uint32_t); break;
    case GD_INT32:       WRITE_ASCII(PRId32,  int32_t); break;
    case GD_UINT64:      WRITE_ASCII(PRIu64, uint64_t); break;
    case GD_INT64:       WRITE_ASCII(PRId64,  int64_t); break;
    case GD_FLOAT32:     WRITE_ASCII(".7g",     float); break;
    case GD_FLOAT64:     WRITE_ASCII(".16g",   double); break;
    case GD_COMPLEX64:  WRITE_CASCII(".7g",     float); break;
    case GD_COMPLEX128: WRITE_CASCII(".16g",   double); break;
    default:                            errno = EINVAL; break;
  }
  
  file->pos += nmemb;

  dreturn("%" PRIdSIZE, n);
  return n;
}

int _GD_AsciiSync(struct gd_raw_file_ *file)
{
  int ret;

  dtrace("%p", file);

  ret = fflush((FILE *)file->edata);

#ifndef __MSVCRT__
  if (!ret)
    ret = fsync(fileno((FILE *)file->edata));
#endif

  dreturn("%i", ret);
  return ret;
}

int _GD_AsciiClose(struct gd_raw_file_* file)
{
  int ret;

  dtrace("%p", file);

  ret = fclose((FILE *)file->edata);
  if (ret == EOF) {
    dreturn("%i", 1);
    return 1;
  }

  file->idata = -1;
  file->mode = 0;
  dreturn("%i", 0);
  return 0;
}

off64_t _GD_AsciiSize(int dirfd, struct gd_raw_file_* file,
    gd_type_t data_type gd_unused_, int swap gd_unused_)
{
  FILE* stream;
  char *buffer = NULL;
  size_t len = 0;
  off64_t n = 0;
  int fd;

  dtrace("%i, %p, <unused>, <unused>", dirfd, file);

  fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY, 0666);
  if (fd < 0) {
    dreturn("%i", -1);
    return -1;
  }

  stream = fdopen(fd, "rb");

  if (stream == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  while (getdelim(&buffer, &len, '\n', stream) != -1)
    n++;
  free(buffer);

  fclose(stream);

  dreturn("%" PRId64, (int64_t)n);
  return n;
}
