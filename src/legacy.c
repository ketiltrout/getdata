/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2003-2005 Theodore Kisner
 * (C) 2005-2008 D. V. Wiebe
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
 * You should have received a copy of the GNU General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif

#include "internal.h"

static struct {
  unsigned int n;
  DIRFILE** D;
} _GD_Dirfiles = {0, NULL};

/* Error-reporting kludge for deprecated API */
static char _GD_GlobalErrorString[MAX_LINE_LENGTH + 6];
static char _GD_GlobalErrorFile[MAX_LINE_LENGTH + 6];
static DIRFILE _GD_GlobalErrors = {
  .error = 0,
  .suberror = 0,
  .error_string = _GD_GlobalErrorString,
  .error_file = _GD_GlobalErrorFile,
  .flags = GD_INVALID
};

/* old error strings */
const char const*GD_ERROR_CODES[GD_N_ERROR_CODES] = {
  "Success",
  "Error opening dirfile",
  "Error in Format file",
  "Error truncating dirfile",
  "Error creating dirfile",
  "Field code not found in File Format",
  "Unrecognized data type",
  "I/O error accessing field file",
  "Could not open included Format file",
  "Internal error",
  "Memory allocation failed",
  "No RAW fields defined",
  "Could not open interpolation file",
  "Too many levels of recursion",
  "Bad DIRFILE",
  "Cannot write to specified field",
  "Read-only dirfile",
  "Request out-of-range"
};

static struct FormatType Format;

/* _GD_CopyGlobalError: Copy the last error message to the global error buffer.
 */
static int _GD_CopyGlobalError(DIRFILE* D)
{
  _GD_GlobalErrors.suberror = D->suberror;
  _GD_GlobalErrors.error_line = D->error_line;
  strncpy(_GD_GlobalErrors.error_file, D->error_file, FILENAME_MAX);
  strncpy(_GD_GlobalErrors.error_string, D->error_string, FILENAME_MAX);

  return _GD_GlobalErrors.error = D->error;
}

/* legacy wrapper for get_error_string()
 */
char* GetDataErrorString(char* buffer, size_t buflen)
{
  return get_error_string(&_GD_GlobalErrors, buffer, buflen);
}

/* _GD_GetDirfile: Locate the legacy DIRFILE given the filespec.  This started
 * life as GetFormat...
 */
static DIRFILE* _GD_GetDirfile(const char *filename_in)
{
  int i_dirfile;

  char filedir[FILENAME_MAX];
  strncpy(filedir, filename_in, FILENAME_MAX);
  if (filedir[strlen(filedir) - 1] == '/')
    filedir[strlen(filedir) - 1] = '\0';

  /* first check to see if we have already read it */
  for (i_dirfile = 0; i_dirfile < _GD_Dirfiles.n; i_dirfile++) {
    if (strncmp(filedir, _GD_Dirfiles.D[i_dirfile]->name, FILENAME_MAX) == 0) {
      _GD_ClearError(_GD_Dirfiles.D[i_dirfile]);
      return _GD_Dirfiles.D[i_dirfile];
    }
  }

  /* if we get here, the file has not yet been read */
  /* Allocate the memory, then fill.  If we have an error, */
  /*  we will have to free the memory... */
  _GD_Dirfiles.n++;
  _GD_Dirfiles.D = realloc(_GD_Dirfiles.D, _GD_Dirfiles.n * sizeof(DIRFILE*));

  /* Legacy dirfiles must be opened read-write, since we never know if
   * they'll be used with putdata at some point */
  _GD_Dirfiles.D[_GD_Dirfiles.n - 1] = dirfile_open(filedir, GD_RDWR);

  /* Error encountered -- the dirfile will shortly be deleted */
  if (_GD_Dirfiles.D[_GD_Dirfiles.n - 1]->error != GD_E_OK)
    return _GD_Dirfiles.D[--_GD_Dirfiles.n];

  return _GD_Dirfiles.D[_GD_Dirfiles.n - 1];
}

/* We're not going to go through all the bother of attempting to reconstruct
 * the old-style FormatType, since we have neither the fields sorted into
 * types, nor the old FooEntryType structs.  Instead, just return a combination
 * of get_nfields and get_field_list, plus a few odds and ends */
const struct FormatType *GetFormat(const char *filedir, int *error_code) {
  DIRFILE *D = _GD_GetDirfile(filedir);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return NULL;
  }
  
  /* fill the structure -- like everything about the legacy API, this is
   * not thread-safe */
  Format.FileDirName = filedir; 
  Format.file_offset = (int)D->frame_offset;
  Format.first_field = (D->first_field) ? D->first_field->field : NULL;
  Format.Entries = get_field_list(D);
  Format.n_entries = get_nfields(D);

  return &Format;
}

/* legacy interface to getdata() */
int GetData(const char *filename, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char return_type, void *data_out, int *error_code)
{
  DIRFILE* D;
  int nread;

  D = _GD_GetDirfile(filename);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return 0;
  }

  nread = (int)getdata64(D, field_code, (off64_t)first_frame,
      (off64_t)first_samp, (size_t)num_frames, (size_t)num_samp,
      _GD_LegacyType(return_type), data_out);
  *error_code = _GD_CopyGlobalError(D);

  return nread;
}

/* legacy interface to get_nframes() --- the third argument to this function
 * has been ignored since at least 2005 (and why does it come after
 * error_code?)
 */
int GetNFrames(const char *filename, int *error_code, const void *unused)
{
  DIRFILE* D;
  int nf;
  (void)unused;

  D = _GD_GetDirfile(filename);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return 0;
  }

  nf = (int)get_nframes(D);
  *error_code = _GD_CopyGlobalError(D);

  return nf;
}

/* legacy interface to get_spf()
 */
int GetSamplesPerFrame(const char *filename, const char *field_code,
    int *error_code)
{
  DIRFILE* D;

  D = _GD_GetDirfile(filename);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return 0;
  }

  int spf = (int)get_spf(D, field_code);
  *error_code = _GD_CopyGlobalError(D);

  return spf;
}

/* legacy interface to putdata()
 */
int PutData(const char *filename, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char data_type, const void *data_in, int *error_code)
{
  DIRFILE* D;
  int n_write = 0;

  D = _GD_GetDirfile(filename);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return 0;
  }

  n_write = (int)putdata64(D, field_code, (off64_t)first_frame,
      (off64_t)first_samp, (size_t)num_frames, (size_t)num_samp,
      _GD_LegacyType(data_type), data_in);
  *error_code = _GD_CopyGlobalError(D);

  return n_write;
}
/* vim: ts=2 sw=2 et
 */
