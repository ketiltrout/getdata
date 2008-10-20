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
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
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
  size_t len = 0;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int i, nfields = get_nfields(D);
  if (D->error)
    return;

  const char** fl = get_field_list(D);

  for (i = 0; i < nfields; ++i)
    if (strlen(fl[i]) > len)
      len = strlen(fl[i]);

  *max = len;
}

/* Return the maximum field name length for a meta list */
void F77_FUNC(gdfmfx, GDFMFX) (int* max, const int* dirfile, const char* parent,
    const int* parent_l)
{
  size_t len = 0;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int i, nfields = get_nmfields(D, pa);

  if (D->error) {
    free(pa);
    return;
  }

  const char** fl = get_mfield_list(D, pa);

  for (i = 0; i < nfields; ++i)
    if (strlen(fl[i]) > len)
      len = strlen(fl[i]);

  *max = len;

  free(pa);
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

  if (*field_num <= (int)nfields) {
    fl = get_field_list(D);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;
}

/* get_mfield_list wrapper -- this only returns one field name */
void F77_FUNC(gdfmfn, GDFMFN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int nfields = get_nmfields(D, pa);
  if (D->error) {
    free(pa);
    return;
  }

  if (*field_num <= (int)nfields) {
    fl = get_mfield_list(D, pa);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
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
  *error = get_error(_GDF_GetDirfile(*dirfile));
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
void F77_FUNC(gdferw, GDFERW) (int* spf, int* dtype, int* fragment_index,
    const int* dirfile, const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_RAW_ENTRY)
    *spf = 0;
  else {
    *spf = E.spf;
    *dtype = E.data_type;
    *fragment_index = E.fragment_index;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for LINCOM */
void F77_FUNC(gdfelc, GDFELC) (int* nfields,
    char* infield1, int* infield1_l, double* m1, double* b1,
    char* infield2, int* infield2_l, double* m2, double* b2,
    char* infield3, int* infield3_l, double* m3, double* b3,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_LINCOM_ENTRY)
    *nfields = 0;
  else {
    *nfields = E.n_fields;
    *fragment_index = E.fragment_index;

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
    int* table_l, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_LINTERP_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(table, table_l, E.table);
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *fragment_index = E.fragment_index;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for BIT */
void F77_FUNC(gdfebt, GDFEBT) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
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
    *fragment_index = E.fragment_index;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for MULTIPLY */
void F77_FUNC(gdfemt, GDFEMT) (char* in_field1, int* in_field1_l,
    char* in_field2, int* in_field2_l, int* fragment_index, const int* dirfile,
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
    *fragment_index = E.fragment_index;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for PHASE */
void F77_FUNC(gdfeph, GDFEPH) (char* in_field, int* in_field_l, int* shift,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_PHASE_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *shift = E.shift;
    *fragment_index = E.fragment_index;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get_entry wrapper for CONST */
void F77_FUNC(gdfeco, GDFECO) (int* data_type, int* fragment_index,
    const int* dirfile, const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_CONST_ENTRY)
    *data_type = 0;
  else {
    *data_type = E.const_type;
    *fragment_index = E.fragment_index;
  }

  dirfile_free_entry_strings(&E);
  free(out);
}

/* get fragment index for field */
void F77_FUNC(gdffgi, GDFFGI) (int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E))
    *fragment_index = -1;
  else
    *fragment_index = E.fragment_index;

  dirfile_free_entry_strings(&E);
  free(out);
}

/* dirfile_add_raw wrapper */
void F77_FUNC(gdfarw, GDFARW) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* data_type, const int* spf,
    const int* fragment_index)
{
  char* out = malloc(*field_code_l + 1);
  dirfile_add_raw(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), (gd_type_t)(*data_type), *spf, *fragment_index);
  free(out);
}

/* dirfile_add_lincom wrapper */
void F77_FUNC(gdfalc, GDFALC) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const double* m1, const double* b1,
    const char* in_field2, const int* in_field2_l, const double* m2,
    const double* b2, const char* in_field3, const int* in_field3_l,
    const double* m3, const double* b3, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in_fields[3] = {NULL, NULL, NULL};
  double m[3] = {0, 0, 0};
  double b[3] = {0, 0, 0};
  int nf = *n_fields;

  if (nf > 0) {
    in_fields[0] = malloc(*in_field1_l + 1);
    _GDF_CString(in_fields[0], in_field1, *in_field1_l);
    m[0] = *m1;
    b[0] = *b1;
  }

  if (nf > 1) {
    in_fields[1] = malloc(*in_field2_l + 1);
    _GDF_CString(in_fields[1], in_field2, *in_field2_l);
    m[1] = *m2;
    b[1] = *b2;
  }

  if (nf > 2) {
    in_fields[2] = malloc(*in_field3_l + 1);
    _GDF_CString(in_fields[2], in_field3, *in_field3_l);
    m[2] = *m3;
    b[2] = *b3;
  }

  dirfile_add_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), nf, (const char**)in_fields, m, b, *fragment_index);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

/* dirfile_add_linterp wrapper */
void F77_FUNC(gdfalt, GDFALT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const char* table, const int* table_l, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);
  char* tab = malloc(*table_l + 1);

  dirfile_add_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l),
      _GDF_CString(tab, table, *table_l), *fragment_index);
  free(fc);
  free(in);
  free(tab);
}

/* dirfile_add_bit wrapper */
void F77_FUNC(gdfabt, GDFABT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* bitnum, const int* numbits, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  dirfile_add_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l), *bitnum,
      *numbits, *fragment_index);
  free(fc);
  free(in);
}

/* dirfile_add_multiply wrapper */
void F77_FUNC(gdfamt, GDFAMT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in1 = malloc(*in_field1_l + 1);
  char* in2 = malloc(*in_field2_l + 1);

  dirfile_add_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in1, in_field1, *in_field1_l),
      _GDF_CString(in2, in_field2, *in_field2_l), *fragment_index);

  free(fc);
  free(in1);
  free(in2);
}

/* dirfile_add_phase wrapper */
void F77_FUNC(gdfaph, GDFAPH) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* shift, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  dirfile_add_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l), *shift,
      *fragment_index);

  free(fc);
  free(in);
}

/* get_fragmentname wrapper */
void F77_FUNC(gdffgn, GDFFGN) (char* filename, int* filename_l,
    const int* dirfile, const int* index)
{
  _GDF_FString(filename, filename_l, get_fragmentname(_GDF_GetDirfile(*dirfile),
        *index));
}

/* get_nfragments wrapper */
void F77_FUNC(gdfnfg, GDFNFG) (int* nformats, const int* dirfile)
{
  *nformats = get_nfragments(_GDF_GetDirfile(*dirfile));
}

/* dirfile_metaflush wrapper */
void F77_FUNC(gdfmfl, GDFMFL) (const int* dirfile)
{
  dirfile_metaflush(_GDF_GetDirfile(*dirfile));
}

/* dirfile_include wrapper */
void F77_FUNC(gdfinc, GDFINC) (const int* dirfile, const char* file,
    const int* file_l, const int* fragment_index, const int* flags)
{
  char* fi = malloc(*file_l + 1);

  dirfile_include(_GDF_GetDirfile(*dirfile), _GDF_CString(fi, file, *file_l),
      *fragment_index, *flags);

  free(fi);
}

/* get_nfield_by_type wrapper */
void F77_FUNC(gdfnft, GDFNFT) (int* nfields, const int* dirfile,
    const int* type)
{
  *nfields = get_nfields_by_type(_GDF_GetDirfile(*dirfile), (gd_entype_t)*type);
}

/* get_nvectors wrapper */
void F77_FUNC(gdfnve, GDFNVE) (int* nvectors, const int* dirfile)
{
  *nvectors = get_nvectors(_GDF_GetDirfile(*dirfile));
}

/* get_field_list_by_type wrapper -- this only returns one field name */
void F77_FUNC(gdffnt, GDFFNT) (char* name, int* name_l, const int* dirfile,
    const int* type, const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int nfields = get_nfields_by_type(D, (gd_entype_t)*type);
  if (D->error)
    return;

  if (*field_num <= (int)nfields) {
    fl = get_field_list_by_type(D, (gd_entype_t)*type);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;
}

/* get_vector_list wrapper -- this only returns one field name */
void F77_FUNC(gdfven, GDFVEN) (char* name, int* name_l, const int* dirfile,
    const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int nfields = get_nvectors(D);
  if (D->error)
    return;

  if (*field_num <= (int)nfields) {
    fl = get_vector_list(D);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;
}

/* get_mfield_list_by_type wrapper -- this only returns one field name */
void F77_FUNC(gdfmft, GDFMFT) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* type,
    const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int nfields = get_nmfields_by_type(D, pa, (gd_entype_t)*type);
  if (D->error) {
    free(pa);
    return;
  }

  if (*field_num <= (int)nfields) {
    fl = get_mfield_list_by_type(D, pa, (gd_entype_t)*type);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
}

/* get_mvector_list wrapper -- this only returns one field name */
void F77_FUNC(gdfmvn, GDFMVN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int nfields = get_nmvectors(D, pa);
  if (D->error) {
    free(pa);
    return;
  }

  if (*field_num <= (int)nfields) {
    fl = get_mvector_list(D, pa);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
}

/* dirfile_madd_lincom wrapper */
void F77_FUNC(gdfmlc, GDFMLC) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* n_fields, const char* in_field1, const int* in_field1_l,
    const double* m1, const double* b1, const char* in_field2,
    const int* in_field2_l, const double* m2, const double* b2,
    const char* in_field3, const int* in_field3_l, const double* m3,
    const double* b3)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in_fields[3] = {NULL, NULL, NULL};
  double m[3] = {0, 0, 0};
  double b[3] = {0, 0, 0};
  int nf = *n_fields;

  if (nf > 0) {
    in_fields[0] = malloc(*in_field1_l + 1);
    _GDF_CString(in_fields[0], in_field1, *in_field1_l);
    m[0] = *m1;
    b[0] = *b1;
  }

  if (nf > 1) {
    in_fields[1] = malloc(*in_field2_l + 1);
    _GDF_CString(in_fields[1], in_field2, *in_field2_l);
    m[1] = *m2;
    b[1] = *b2;
  }

  if (nf > 2) {
    in_fields[2] = malloc(*in_field3_l + 1);
    _GDF_CString(in_fields[2], in_field3, *in_field3_l);
    m[2] = *m3;
    b[2] = *b3;
  }

  dirfile_madd_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l), nf,
      (const char**)in_fields, m, b);
  free(pa);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

/* dirfile_madd_linterp wrapper */
void F77_FUNC(gdfmlt, GDFMLT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const char* table,
    const int* table_l)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);
  char* tab = malloc(*table_l + 1);

  dirfile_madd_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in, in_field, *in_field_l), _GDF_CString(tab, table,
        *table_l));
  free(pa);
  free(fc);
  free(in);
  free(tab);
}

/* dirfile_add_bit wrapper */
void F77_FUNC(gdfmbt, GDFMBT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* bitnum,
    const int* numbits)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  dirfile_madd_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in, in_field, *in_field_l), *bitnum, *numbits);
  free(pa);
  free(fc);
  free(in);
}

/* dirfile_madd_multiply wrapper */
void F77_FUNC(gdfmmt, GDFMMT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field1, const int* in_field1_l, const char* in_field2,
    const int* in_field2_l)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in1 = malloc(*in_field1_l + 1);
  char* in2 = malloc(*in_field2_l + 1);

  dirfile_madd_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in1, in_field1, *in_field1_l), _GDF_CString(in2, in_field2,
        *in_field2_l));

  free(pa);
  free(fc);
  free(in1);
  free(in2);
}

/* dirfile_madd_phase wrapper */
void F77_FUNC(gdfmph, GDFMPH) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* shift)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  dirfile_madd_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in, in_field, *in_field_l), *shift);

  free(pa);
  free(fc);
  free(in);
}

/* dirfile_add_const wrapper */
void F77_FUNC(gdfaco, GDFACO) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* const_type, const int* data_type,
    const void* value, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);

  dirfile_add_const(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), (gd_type_t)*const_type, (gd_type_t)*data_type, value,
      *fragment_index);

  free(fc);
}

/* dirfile_madd_const wrapper */
void F77_FUNC(gdfmco, GDFMCO) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* const_type, const int* data_type, const void* value)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);

  dirfile_madd_const(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      (gd_type_t)*const_type, (gd_type_t)*data_type, value);

  free(pa);
  free(fc);
}

/* dirfile_add_string wrapper */
void F77_FUNC(gdfast, GDFAST) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* value, const int* value_l,
    const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* va = malloc(*value_l + 1);

  dirfile_add_string(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(va, value, *value_l), *fragment_index);

  free(fc);
  free(va);
}

/* dirfile_madd_string wrapper */
void F77_FUNC(gdfmst, GDFMST) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* value, const int* value_l)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* va = malloc(*value_l + 1);

  dirfile_madd_string(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(va, value, *value_l));

  free(pa);
  free(fc);
  free(va);
}

/* dirfile_add_spec wrapper */
void F77_FUNC(gdfasp, GDFASP) (const int* dirfile, const char* spec,
    const int* spec_l, const int* fragment_index)
{
  char* sp = malloc(*spec_l + 1);

  dirfile_add_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(sp, spec, *spec_l),
      *fragment_index);

  free(sp);
}

/* dirfile_madd_spec wrapper */
void F77_FUNC(gdfmsp, GDFMSP) (const int* dirfile, const char* spec,
    const int* spec_l, const char *parent, const int* parent_l)
{
  char* pa = malloc(*parent_l + 1);
  char* sp = malloc(*spec_l + 1);

  dirfile_madd_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(sp, spec,
        *spec_l), _GDF_CString(pa, parent, *parent_l));

  free(pa);
  free(sp);
}

/* get_constant wrapper */
void F77_FUNC(gdfgco, GDFGCO) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* return_type,
    void* data_out)
{
  char* fc = malloc(*field_code_l + 1);
  *n_read = get_constant(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), (gd_type_t)*return_type, data_out);
  free(fc);
}

/* get_string wrapper */
void F77_FUNC(gdfgst, GDFGST) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    char* data_out)
{
  char* fc = malloc(*field_code_l + 1);
  char* out = malloc(*len + 1);
  int l = *len;

  *n_read = get_string(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), (size_t)len, out);

  _GDF_FString(data_out, &l, out);
  free(fc);
  free(out);
}

/* put_constant wrapper */
void F77_FUNC(gdfpco, GDFPCO) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* data_type,
    const void* data_in)
{
  char* fc = malloc(*field_code_l + 1);
  *n_wrote = put_constant(_GDF_GetDirfile(*dirfile), _GDF_CString(fc,
        field_code, *field_code_l), (gd_type_t)*data_type, data_in);
  free(fc);
}

/* put_string wrapper */
void F77_FUNC(gdfpst, GDFPST) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    const char* data_in)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*len + 1);
  *n_wrote = put_string(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, data_in, *len));
  free(fc);
  free(in);
}

/* get_nmfields wrapper */
void F77_FUNC(gdfnmf, GDFNMF) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l)
{
  char* pa = malloc(*parent_l + 1);
  *nfields = get_nmfields(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l));
  free(pa);
}

/* get_nmfields_by_type wrapper */
void F77_FUNC(gdfnmt, GDFNMT) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l, const int* type)
{
  char* pa = malloc(*parent_l + 1);
  *nfields = get_nmfields_by_type(_GDF_GetDirfile(*dirfile), _GDF_CString(pa,
        parent, *parent_l), (gd_entype_t)*type);
  free(pa);
}

/* get_nmvectors wrapper */
void F77_FUNC(gdfnmv, GDFNMV) (int* nvectors, const int* dirfile,
    const char* parent, const int* parent_l)
{
  char* pa = malloc(*parent_l + 1);
  *nvectors = get_nmvectors(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l));
  free(pa);
}
