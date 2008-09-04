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
 * along with GetData; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "fgetdata.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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

/* create a Fortran space padded string */
static int _GDF_FString(char* dest, int *dlen, const char* src)
{
  int slen = strlen(src);
  if (slen < *dlen) {
    sprintf(dest, "%-*s", *dlen, src);
    dest[*dlen - 1] = ' ';
    return 0;
  }

  *dlen = slen + 1;
  return -1;
}

/* dirfile_open wrapper */
void F77_FUNC(gdfopn, GDFOPN) (int* dirfile, const char* dirfilename,
    const int* dirfilename_l, const int* flags)
{
  char* out = malloc(*dirfilename_l + 1);

  *dirfile = _GDF_SetDirfile(dirfile_open(_GDF_CString(out, dirfilename,
          *dirfilename_l), *flags));

  free(out);
}

/* dirfile_close wrapper */
void F77_FUNC(gdfcls, GDFCLS) (const int* dirfile)
{
  _GDF_ClearDirfile(*dirfile);
}

/* dirfile_flush wrapper */
void F77_FUNC(gdffls, GDFFLS) (const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out;

  if (field_code_l == 0)
    dirfile_flush(_GDF_GetDirfile(*dirfile), NULL);
  else {
    out = malloc(*field_code_l + 1);
    dirfile_flush(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l));
    free(out);
  }
}

/* getdata wrapper */
void F77_FUNC(gdfget, GDFGET) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l,
    const int* first_frame, const int* first_sample,
    const int* num_frames, const int* num_samples, const int* return_type,
    void* data_out)
{
  char* out = malloc(*field_code_l + 1);
  *n_read = getdata(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*return_type, data_out);
  free(out);
}

/* Return the maximum field name length */
void F77_FUNC(gdffnx, GDFFNX) (int* max, const int* dirfile)
{
  int i, len = 0;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int nfields = get_nfields(D);
  if (D->error)
    return;

  const char** fl = get_field_list(D);

  for (i = 0; i < nfields; ++i)
    if (strlen(fl[i]) > len)
      len = strlen(fl[i]);

  *max = len;
}

/* get_field_list wrapper -- this only returns one field name */
void F77_FUNC(gdffdn, GDFFDN) (char* name, int* name_l, const int* dirfile,
    const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int nfields = get_nfields(D);
  if (D->error)
    return;

  if (*field_num <= nfields) {
    fl = get_field_list(D);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;
}

/* get_nfields wrapper */
void F77_FUNC(gdfnfd, GDFNFD) (int* nfields, const int* dirfile)
{
  *nfields = get_nfields(_GDF_GetDirfile(*dirfile));
}

/* get_nframes wrapper */
void F77_FUNC(gdfnfr, GDFNFR) (int* nframes, const int* dirfile)
{
  *nframes = get_nframes(_GDF_GetDirfile(*dirfile));
}

/* get_spf wrapper */
void F77_FUNC(gdfspf, GDFSPF) (int* spf, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  *spf = get_spf(_GDF_GetDirfile(*dirfile),
      _GDF_CString(out, field_code, *field_code_l));
  free(out);
}

/* putdata wrapper */
void F77_FUNC(gdfput, GDFPUT) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* first_frame,
    const int* first_sample, const int* num_frames, const int* num_samples,
    const int* data_type, const void* data_in)
{
  char* out = malloc(*field_code_l + 1);
  *n_wrote = putdata(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*data_type, data_in);
  free(out);
}

/* return the error number */
void F77_FUNC(gdferr, GDFERR) (int* error, const int* dirfile)
{
  *error = _GDF_GetDirfile(*dirfile)->error;
}

/* get_error_string wrapper */
void F77_FUNC(gdfstr, GDFSTR) (const int* dirfile, char* buffer, const int* len)
{
  get_error_string(_GDF_GetDirfile(*dirfile), buffer, *len);
}

/* returns the field type */
void F77_FUNC(gdffdt, GDFFDT) (int* type, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E))
    *type = GD_NO_ENTRY;
  else
    *type = E.field_type; 

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for RAW */
void F77_FUNC(gdferw, GDFERW) (int* spf, int* dtype, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_RAW_ENTRY)
    *spf = 0;
  else {
    *spf = E.spf;
    *dtype = E.data_type;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for LINCOM */
void F77_FUNC(gdfelc, GDFELC) (int* nfields,
    char* infield1, int* infield1_l, double* m1, double* b1,
    char* infield2, int* infield2_l, double* m2, double* b2,
    char* infield3, int* infield3_l, double* m3, double* b3,
    const int* dirfile, const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_LINCOM_ENTRY)
    *nfields = 0;
  else {
    *nfields = E.n_fields;
    _GDF_FString(infield1, infield1_l, E.in_fields[0]);
    *m1 = E.m[0];
    *b1 = E.b[0];

    if (E.n_fields > 1) {
      _GDF_FString(infield2, infield2_l, E.in_fields[1]);
      *m2 = E.m[1];
      *b2 = E.b[1];
    }

    if (E.n_fields > 2) {
      _GDF_FString(infield3, infield3_l, E.in_fields[2]);
      *m3 = E.m[2];
      *b3 = E.b[2];
    }
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for LINTERP */
void F77_FUNC(gdfelt, GDFELT) (char* in_field, int* in_field_l, char* table,
    int* table_l, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_LINTERP_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(table, table_l, E.table);
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for BIT */
void F77_FUNC(gdfebt, GDFEBT) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_BIT_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *bitnum = E.bitnum;
    *numbits = E.numbits;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for MULTIPLY */
void F77_FUNC(gdfemt, GDFEMT) (char* in_field1, int* in_field1_l,
    char* in_field2, int* in_field2_l, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_MULTIPLY_ENTRY)
    *in_field1_l = 0;
  else {
    _GDF_FString(in_field1, in_field1_l, E.in_fields[0]);
    _GDF_FString(in_field2, in_field2_l, E.in_fields[1]);
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for PHASE */
void F77_FUNC(gdfeph, GDFEPH) (char* in_field, int* in_field_l, int* shift,
    const int* dirfile, const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_PHASE_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *shift = E.shift;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}
