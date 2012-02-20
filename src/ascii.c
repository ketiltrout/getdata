/* Copyright (C) 2008-2011 D. V. Wiebe
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

int _GD_AsciiOpen(int fd, struct _gd_raw_file* file, int swap __gd_unused,
    unsigned int mode)
{
  dtrace("%i, %p, <unused>, %u", fd, file, mode);

  if (!(mode & GD_FILE_TEMP)) {
    if (file->mode & mode) {
      dreturn("%i", 0);
      return 0;
    } else if (file->edata != NULL)
      fclose(file->edata);

    file->idata = gd_OpenAt(file->D, fd, file->name, ((mode & GD_FILE_WRITE)
          ? (O_RDWR | O_CREAT) : O_RDONLY) | O_BINARY, 0666);

    if (file->idata < 0) {
      dreturn("%i", -1);
      return -1;
    }
  } else
    file->idata = fd;

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

off64_t _GD_AsciiSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type __gd_unused, unsigned int mode)
{
  char line[64];

  dtrace("%p, %lli, <unused>, 0x%X", file, count, mode);

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

  dreturn("%lli", (long long)file->pos);
  return file->pos;
}

static void _GD_ScanFormat(char* fmt, gd_type_t data_type)
{
  dtrace("%p, 0x%X", fmt, data_type);

  switch(data_type) {
    case GD_UINT8:
#ifndef NO_8BIT_INT_PREFIX
      strcpy(fmt, "%" SCNu8);
      break;
#endif
    case GD_UINT16:
      strcpy(fmt, "%" SCNu16);
      break;
    case GD_INT8:
#ifndef NO_8BIT_INT_PREFIX
      strcpy(fmt, "%" SCNi8);
      break;
#endif
    case GD_INT16:
      strcpy(fmt, "%" SCNi16);
      break;
    case GD_UINT32:
      strcpy(fmt, "%" SCNu32);
      break;
    case GD_INT32:
      strcpy(fmt, "%" SCNi32);
      break;
    case GD_UINT64:
      strcpy(fmt, "%" SCNu64);
      break;
    case GD_INT64:
      strcpy(fmt, "%" SCNi64);
      break;
    case GD_FLOAT32:
      strcpy(fmt, "%f");
      break;
    case GD_FLOAT64:
      strcpy(fmt, "%lf");
      break;
    case GD_COMPLEX64:
      strcpy(fmt, "%f;%f");
      break;
    case GD_COMPLEX128:
      strcpy(fmt, "%lf;%lf");
      break;
    default:
      fmt[0] = 0;
      break;
  }

  dreturn("[\"%s\"]", fmt);
}

ssize_t _GD_AsciiRead(struct _gd_raw_file *restrict file, void *restrict ptr,
    gd_type_t data_type, size_t nmemb)
{
  char fmt[50];
  size_t n = 0;
  ssize_t ret = 0;
#ifdef NO_8BIT_INT_PREFIX
  short int i16;
#endif

  dtrace("%p, %p, 0x%X, %zu", file, ptr, data_type, nmemb);

  _GD_ScanFormat(fmt, data_type);
  if (data_type & GD_COMPLEX) {
    for (n = 0; n < nmemb; ++n) {
      if (feof((FILE *)file->edata))
        break;

      if (fscanf((FILE *)file->edata, fmt, (char *)ptr + GD_SIZE(data_type) * n,
            (char *)ptr + GD_SIZE(data_type) * n + GD_SIZE(data_type) / 2) < 2)
      {
        if (!feof((FILE *)file->edata))
          ret = -1;
        break;
      }
      file->pos++;
    }
  } else {
    for (n = 0; n < nmemb; ++n) {
      if (feof((FILE *)file->edata))
        break;


#ifdef NO_8BIT_INT_PREFIX
      if (GD_SIZE(data_type) == 1) {
        if (fscanf((FILE *)file->edata, fmt, &i16) < 1) {
          if (!feof((FILE *)file->edata))
            ret = -1;
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
          if (!feof((FILE *)file->edata))
            ret = -1;
          break;
        }
      }
      file->pos++;
    }
  }

  dreturn("%li", (ret) ? (long)ret : (long)n);
  return (ret) ? ret : (ssize_t)n;
}

ssize_t _GD_AsciiWrite(struct _gd_raw_file *restrict file,
    const void *restrict ptr, gd_type_t data_type, size_t nmemb)
{
  ssize_t ret = 0;
  size_t n = 0;

  dtrace("%p, %p, 0x%X, %zu", file, ptr, data_type, nmemb);

  switch(data_type) {
    case GD_UINT8:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIu8 "\n",
              ((uint8_t *)ptr)[n]) < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_INT8:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIi8 "\n",
              ((int8_t *)ptr)[n]) < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_UINT16:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIu16 "\n", ((uint16_t *)ptr)[n])
            < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_INT16:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIi16 "\n", ((int16_t *)ptr)[n])
            < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_UINT32:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIu32 "\n", ((uint32_t *)ptr)[n])
            < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_INT32:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIi32 "\n", ((int32_t *)ptr)[n])
            < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_UINT64:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIu64 "\n", ((uint64_t *)ptr)[n])
            < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_INT64:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%" PRIi64 "\n", ((int64_t *)ptr)[n])
            < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_FLOAT32:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%.7g\n", ((float *)ptr)[n]) < 0) {
          ret = -1;
          break;
        }
      break;
    case GD_FLOAT64:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%.16lg\n", ((double *)ptr)[n]) < 0) {
          ret = -1;
          break;
        }
      break;
    case GD_COMPLEX64:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%.16g;%.16g\n", ((float *)ptr)[n * 2],
              (((float *)ptr)[n * 2 + 1])) < 0)
        {
          ret = -1;
          break;
        }
      break;
    case GD_COMPLEX128:
      for (n = 0; n < nmemb; ++n)
        if (fprintf((FILE *)file->edata, "%.16lg;%.16lg\n",
              ((double *)ptr)[n * 2], (((double *)ptr)[n * 2 + 1])) < 0)
        {
          ret = -1;
          break;
        }
      break;
    default:
      ret = -1;
      break;
  }

  dreturn("%li", (ret) ? (long)ret : (long)n);
  return (ret) ? ret : (ssize_t)n;
}

int _GD_AsciiSync(struct _gd_raw_file *file)
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

int _GD_AsciiClose(struct _gd_raw_file* file)
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

off64_t _GD_AsciiSize(int dirfd, struct _gd_raw_file* file,
    gd_type_t data_type __gd_unused, int swap __gd_unused)
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

  dreturn("%lli", n);
  return n;
}
