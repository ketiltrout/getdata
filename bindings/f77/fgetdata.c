/* (C) 2008 D. V. Wiebe
 *
 *************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the GetData; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "fgetdata.h"

#include <stdio.h>
#include <stdlib.h>

/* Fortran 77 has no facility to take a pointer to a DIRFILE* object.
 * Instead, we keep a list of them here.  If we ever run out of these,
 * the caller will be abort()ed. */
static DIRFILE* f77dirfiles[GDF_N_DIRFILES];
static int f77dirfiles_initialised = 0;

/* initialise the f77dirfiles array */
static void _GDF_InitDirfiles(void)
{
  int i;

  for (i = 1; i < GDF_N_DIRFILES; ++i)
    f77dirfiles[i] = NULL;

  /* we keep entry zero as a generic, invalid dirfile to return if
   * dirfile lookup fails */
  f77dirfiles[0] = (DIRFILE*)malloc(sizeof(DIRFILE));
  f77dirfiles[0]->flags = GD_INVALID;

  f77dirfiles_initialised = 1;
}

/* make a C string */
static char* _GDF_CString(char* out, const char* in, int l)
{
  int i;
  for (i = 0; i < l; ++i)
    out[i] = in[i];
  out[l] = '\0';

  return out;
}

/* convert an int to a DIRFILE* */
static DIRFILE* _GDF_GetDirfile(int d)
{
  if (f77dirfiles[d] == NULL)
    return f77dirfiles[0];

  return f77dirfiles[d];
}

/* convert a new DIRFILE* into an int */
static int _GDF_SetDirfile(DIRFILE* D)
{
  int i;

  if (!f77dirfiles_initialised)
    _GDF_InitDirfiles();

  for (i = 1; i < GDF_N_DIRFILES; ++i)
    if (f77dirfiles[i] == NULL) {
      f77dirfiles[i] = D;
      return i;
    }

  /* out of f77dirfiles space: complain and abort */
  fputs("libfgetdata: DIRFILE space exhausted.", stderr);
  abort();
}

/* delete the supplied dirfile */
static void _GDF_ClearDirfile(int d)
{
  if (d == 0)
    return;

  DIRFILE* D = f77dirfiles[d];

  if (D) {
    dirfile_close(D);
    f77dirfiles[d] = NULL;
  }
}

/* dirfile_open wrapper */
void F77_FUNC(gdfopn, GDFOPN) (int* dirfile, const char* dirfilename,
    int* dirfilename_l, int* flags)
{
  char* out = malloc(*dirfilename_l + 1);

  *dirfile = _GDF_SetDirfile(dirfile_open(_GDF_CString(out, dirfilename,
          *dirfilename_l), *flags));

  free(out);
}

/* dirfile_close wrapper */
void F77_FUNC(gdfcls, GDFCLS) (int* dirfile)
{
  _GDF_ClearDirfile(*dirfile);
}

/* getdata wrapper */
void F77_FUNC(gdfget, GDFGET) (int* n_read, int* dirfile, char* field_code,
    const int* field_code_l, int* first_frame, int* first_sample,
    int* num_frames, int* num_samples, int* return_type, void* data_out)
{
  char* out = malloc(*field_code_l + 1);
  *n_read = getdata(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*return_type, data_out);
  free(out);
}

/* get_n_frames wrapper */
void F77_FUNC(gdfnfr, GDFNFR) (int* nframes, int* dirfile)
{
  *nframes = get_n_frames(_GDF_GetDirfile(*dirfile));
}

/* get_samples_per_frame wrapper */
void F77_FUNC(gdfspf, GDFSPF) (int* spf, int* dirfile, const char* field_code,
    int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  *spf = get_samples_per_frame(_GDF_GetDirfile(*dirfile),
      _GDF_CString(out, field_code, *field_code_l));
  free(out);
}

/* putdata wrapper */
void F77_FUNC(gdfput, GDFPUT) (int* n_wrote, int* dirfile,
    const char* field_code, int* field_code_l, int* first_frame,
    int* first_sample, int* num_frames, int* num_samples, int* data_type,
    void* data_in)
{
  char* out = malloc(*field_code_l + 1);
  *n_wrote = getdata(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*data_type, data_in);
  free(out);
}

/* return the error number */
void F77_FUNC(gdferr, GDFERR) (int* error, int* dirfile)
{
  *error = _GDF_GetDirfile(*dirfile)->error;
}

/* getdata_error_string wrapper */
void F77_FUNC(gdfstr, GDFSTR) (int* dirfile, char* buffer, int* len)
{
  getdata_error_string(_GDF_GetDirfile(*dirfile), buffer, *len);
}
