/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
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
#endif

int _GD_AsciiOpen(union _gd_private_entry* entry, const char* name, int mode,
    int creat)
{
  char asciiname[FILENAME_MAX];

  dtrace("%p, \"%s\", %i, %i", entry, name, mode, creat);

  snprintf(asciiname, FILENAME_MAX, "%s.txt", name);
  int fp = open(asciiname,
      ((mode == GD_RDWR) ? O_RDWR : O_RDONLY) | (creat ? O_CREAT : 0), 0666);

  entry->stream = fdopen(fp, (mode == GD_RDWR) ? "r+" : "r");

  if (entry->stream != NULL)
    entry->fp = 0;

  dreturn("%i", entry->stream == NULL);
  return (entry->stream == NULL);
}

off64_t _GD_AsciiSeek(union _gd_private_entry* entry, off64_t count,
    gd_type_t data_type, int pad)
{
  char line[64];

  dtrace("%p, %lli, %x, %i", entry, count, data_type, pad);

  if (count < entry->fp) {
    rewind(entry->stream);
    entry->fp = 0;
  }

  for (; count > entry->fp; ++entry->fp)
    if (fgets(line, 64, entry->stream) == NULL)
      break;

  if (pad && count > entry->fp) {
    strcpy(line, "0\n");
    for (; count > entry->fp; ++entry->fp)
      fputs(line, entry->stream);
  }

  dreturn("%i", entry->fp);

  return entry->fp;
}

static void _GD_read_format(char* fmt, gd_type_t data_type)
{
  dtrace("%p, %x", fmt, data_type);

  switch(data_type) {
    case GD_UINT8:
      strcpy(fmt, "%" SCNu8);
      break;
    case GD_INT8:
      strcpy(fmt, "%" SCNi8);
      break;
    case GD_UINT16:
      strcpy(fmt, "%" SCNu16);
      break;
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
    default:
      fmt[0] = 0;
      break;
  }

  dreturn("[\"%s\"]", fmt);
}

ssize_t _GD_AsciiRead(union _gd_private_entry *entry, void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  char fmt[50];
  ssize_t n = 0;
  _GD_read_format(fmt, data_type);
  for (n = 0; n < nmemb; ++n) {
    if (feof(entry->stream))
      break;

    if (fscanf(entry->stream, fmt, ptr + GD_SIZE(data_type) * n) < 1) {
      n = -1;
      break;
    }
  }

  return n;
}

ssize_t _GD_AsciiWrite(union _gd_private_entry *entry, const void *ptr,
    gd_type_t data_type, size_t nmemb)
{
  ssize_t n;

  switch(data_type) {
    case GD_UINT8:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIu8 "\n",
              *(uint8_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_INT8:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIi8 "\n",
              *(int8_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_UINT16:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIu16 "\n",
              *(uint16_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_INT16:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIi16 "\n",
              *(int16_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_UINT32:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIu32 "\n",
              *(uint32_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_INT32:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIi32 "\n",
              *(int32_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_UINT64:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIu64 "\n",
              *(uint64_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_INT64:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%" PRIi64 "\n",
              *(int64_t*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_FLOAT32:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%.7g\n",
              *(float*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    case GD_FLOAT64:
      for (n = 0; n < nmemb; ++n)
        if (fprintf(entry->stream, "%.16lg\n",
              *(double*)(ptr + GD_SIZE(data_type) * n)) < 0)
        {
          n = -1;
          break;
        }
      break;
    default:
      n = -1;
      break;
  }

  return n;
}

int _GD_AsciiSync(union _gd_private_entry *entry)
{
  int ret = fflush(entry->stream);
  if (ret)
    return ret;

  return fsync(fileno(entry->stream));
}

int _GD_AsciiClose(union _gd_private_entry *entry)
{
  int ret = fclose(entry->stream);
  if (ret != EOF) {
    entry->fp = -1;
    return 0;
  }

  return 1;
}

off64_t _GD_AsciiSize(const char *name, gd_type_t data_type)
{
  (void)data_type;
  FILE* stream;
  char asciiname[FILENAME_MAX];
  snprintf(asciiname, FILENAME_MAX, "%s.txt", name);
  off64_t n = 0;

  stream = fopen(asciiname, "r");

  if (stream == NULL)
    return -1;

  while (!feof(stream))
    if (fgets(asciiname, FILENAME_MAX, stream) != NULL)
      n++;

  fclose(stream);

  return n;
}
