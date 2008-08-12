/*                           (C) 2002 C. Barth Netterfield */
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#include "getdata_internal.h"

#define MAX_LINE_LENGTH FILENAME_MAX
#define MAX_IN_COLS 12 /* lincom needs = 3 * MAX_LINCOM + 3 ; */

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
  .error_file = _GD_GlobalErrorFile
};

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

/* GetDataErrorString: Write a descriptive message in the supplied buffer
 *       describing the last library error.  The message may be truncated but
 *       will be null terminated.  Returns buffer, or NULL if buflen < 1.
 */
char* GetDataErrorString(char* buffer, size_t buflen)
{
  return getdata_error_string(&_GD_GlobalErrors, buffer, buflen);
}

/* _GD_GetDirfile: Locate the legacy DIRFILE given the filespec.
 */
static DIRFILE* _GD_GetDirfile(const char *filedir)
{
  int i_dirfile;

  /** first check to see if we have already read it **/
  for (i_dirfile = 0; i_dirfile < _GD_Dirfiles.n; i_dirfile++) {
    if (strncmp(filedir, _GD_Dirfiles.D[i_dirfile]->name, FILENAME_MAX) == 0) {
      _GD_ClearGetDataError(_GD_Dirfiles.D[i_dirfile]);
      return _GD_Dirfiles.D[i_dirfile];
    }
  }

  /** if we get here, the file has not yet been read */
  /** Allocate the memory, then fill.  If we have an error, */
  /*  we will have to free the memory... */
  _GD_Dirfiles.n++;
  _GD_Dirfiles.D = realloc(_GD_Dirfiles.D, _GD_Dirfiles.n * sizeof(DIRFILE*));

  _GD_Dirfiles.D[_GD_Dirfiles.n - 1] = dirfile_open(filedir, 0);

  /* Error encountered, free the memory */ 
  if (_GD_Dirfiles.D[_GD_Dirfiles.n - 1]->error != GD_E_OK) {
    dirfile_close(_GD_Dirfiles.D[_GD_Dirfiles.n - 1]);
    _GD_Dirfiles.n--; /* no need to free.  The next realloc will just do
                         nothing */
    return NULL;
  }

  return _GD_Dirfiles.D[_GD_Dirfiles.n - 1];
}

/***************************************************************************/
/*                                                                         */
/*  GetData: read BLAST format files.                                      */
/*    filename_in: the name of the file directory (raw files are in here)  */
/*    field_code: the name of the field you want to read                   */
/*    first_frame, first_samp: the first sample read is                    */
/*              first_samp + samples_per_frame*first_frame                 */
/*    num_frames, num_samps: the number of samples read is                 */
/*              num_samps + samples_per_frame*num_frames                   */
/*    return_type: data type of *data_out:                                 */
/*           GD_INT8,  GD_INT16,   GD_INT32,  GD_INT64                     */
/*          GD_UINT8, GD_UINT16,  GD_UINT32, GD_UINT64                     */
/*                               GD_FLOAT32, GD_FLOAT64                    */
/*    void *data_out: array to put the data                                */
/*    *error_code: error code is returned here.                            */
/*                                                                         */
/*    return value: returns number of samples actually read into data_out  */
/*                                                                         */
/***************************************************************************/
int GetData(const char *filename_in, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t return_type, void *data_out, int *error_code)
{
  DIRFILE* D;
  char filename[FILENAME_MAX];
  int nread;

  strncpy(filename, filename_in, FILENAME_MAX);
  if (filename[strlen(filename) - 1] == '/')
    filename[strlen(filename) - 1] = '\0';

  D = _GD_GetDirfile(filename);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return 0;
  }

  nread = getdata(D, field_code, first_frame, first_samp, num_frames,
      num_samp, return_type, data_out);
  *error_code = _GD_CopyGlobalError(D);

  return nread;
}

/***************************************************************************/
/*                                                                         */
/*    Get the number of frames available                                   */
/*                                                                         */
/***************************************************************************/
int GetNFrames(const char *filename_in, int *error_code, const void *unused)
{
  DIRFILE* D;
  char filename[FILENAME_MAX];
  int nf;

  strncpy(filename, filename_in, FILENAME_MAX);
  if (filename[strlen(filename) - 1] == '/')
    filename[strlen(filename) - 1] = '\0';

  D = _GD_GetDirfile(filename);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return 0;
  }

  nf = get_n_frames(D);
  *error_code = _GD_CopyGlobalError(D);

  return nf;
}

/***************************************************************************/
/*                                                                         */
/*    Get the number of samples for each frame for the given field         */
/*                                                                         */
/***************************************************************************/
int GetSamplesPerFrame(const char *filename_in, const char *field_code,
    int *error_code)
{
  DIRFILE* D = malloc(sizeof(DIRFILE));
  char filename[FILENAME_MAX];

  strncpy(filename, filename_in, FILENAME_MAX);
  if (filename[strlen(filename) - 1] == '/')
    filename[strlen(filename) - 1] = '\0';

  D = _GD_GetDirfile(filename);

  if (D->error) {
    *error_code = _GD_CopyGlobalError(D);
    return 0;
  }

  int spf = get_samples_per_frame(D, field_code);
  *error_code = _GD_CopyGlobalError(D);

  return spf;
}

int PutData(const char *filename_in, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t data_type, void *data_in, int *error_code)
{
  DIRFILE* D;
  int n_write = 0;
  char filename[FILENAME_MAX];

  strncpy(filename, filename_in, FILENAME_MAX);
  if (filename[strlen(filename) - 1] == '/')
    filename[strlen(filename) - 1] = '\0';

  D = _GD_GetDirfile(filename);

  if (D->error != GD_E_OK) {
    *error_code = _GD_CopyGlobalError(D);
    dirfile_close(D);
    return 0;
  }

  n_write = putdata(D, field_code, first_frame, first_samp, num_frames,
      num_samp, data_type, data_in);
  *error_code = _GD_CopyGlobalError(D);

  return n_write;
}
/* vim: ts=2 sw=2 et
 */
