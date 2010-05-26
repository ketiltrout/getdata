/* (C) 2008 D. V. Wiebe
 *
 *************************************************************************
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
  dtracevoid();
  int i;

  for (i = 1; i < GDF_N_DIRFILES; ++i)
    f77dirfiles[i] = NULL;

  /* we keep entry zero as a generic, invalid dirfile to return if
   * dirfile lookup fails */
  f77dirfiles[0] = (DIRFILE*)malloc(sizeof(DIRFILE));
  memset(f77dirfiles[0], 0, sizeof(DIRFILE));
  f77dirfiles[0]->flags = GD_INVALID;

  f77dirfiles_initialised = 1;
  dreturnvoid();
}

/* make a C string */
static char* _GDF_CString(char* out, const char* in, int l)
{
  dtrace("%p, \"%s\", %i", out, in, l);
  int i;

  for (i = 0; i < l; ++i)
    out[i] = in[i];
  out[l] = '\0';

  dreturn("%p", out);
  return out;
}

/* convert an int to a DIRFILE* */
static DIRFILE* _GDF_GetDirfile(int d)
{
  dtrace("%i", d);

  if (!f77dirfiles_initialised)
    _GDF_InitDirfiles();

  if (d < 0 || d >= GDF_N_DIRFILES || f77dirfiles[d] == NULL) {
    dreturn("%p [0]", f77dirfiles[0]);
    return f77dirfiles[0];
  }

  dreturn("%p", f77dirfiles[d]);
  return f77dirfiles[d];
}

/* convert a new DIRFILE* into an int */
static int _GDF_SetDirfile(DIRFILE* D)
{
  int i;

  dtrace("%p", D);

  if (!f77dirfiles_initialised)
    _GDF_InitDirfiles();

  for (i = 1; i < GDF_N_DIRFILES; ++i)
    if (f77dirfiles[i] == NULL) {
      f77dirfiles[i] = D;
      dreturn("%i", i);
      return i;
    }

  /* out of f77dirfiles space: complain and abort */
  fputs("libfgetdata: DIRFILE space exhausted.", stderr);
  abort();
}

/* delete the supplied dirfile */
static void _GDF_ClearDirfile(int d)
{
  dtrace("%i", d);

  if (d != 0)
    f77dirfiles[d] = NULL;

  dreturnvoid();
}

/* create a Fortran space padded string */
static int _GDF_FString(char* dest, int *dlen, const char* src)
{
  dtrace("%p, %i, \"%s\"", dest, *dlen, src);
  int i;
  int slen = strlen(src);

  if (src == NULL) {
    *dlen = 0;
    dreturn("%i", -1);
    return -1;
  }

  if (slen <= *dlen) {
    for (i = 0; i < slen; ++i)
      dest[i] = src[i];

    for (; i < *dlen; ++i)
      dest[i] = ' ';
    dreturn("%i", 0);
    return 0;
  }

  *dlen = slen;
  dreturn("%i", -1);
  return -1;
}

/* callback wrapper */
static int _GDF_Callback(gd_parser_data_t* pdata, void* f77_callback)
{
  int unit;
  int r = GD_SYNTAX_ABORT;

  dtrace("%p, %p", pdata, f77_callback);

  if (f77_callback != NULL) {
    unit = _GDF_SetDirfile((DIRFILE*)pdata->dirfile);

    (*(void(*)(int*, const int*, const int*, char*, const int*,
               const char*))f77_callback)(&r, &unit, &pdata->suberror,
             pdata->line, &pdata->linenum, pdata->filename);

    pdata->line[GD_MAX_LINE_LENGTH - 1] = '\0';

    _GDF_ClearDirfile(unit);
  }

  dreturn("%i", r);
  return r;
}

/* gd_open wrapper */
void F77_FUNC(gdopen, GDOPEN) (int* dirfile, const char* dirfilename,
    const int* dirfilename_l, const int* flags)
{
  char* out = malloc(*dirfilename_l + 1);

  *dirfile = _GDF_SetDirfile(gd_open(_GDF_CString(out, dirfilename,
          *dirfilename_l), *flags));

  free(out);
}

/* gd_close wrapper */
void F77_FUNC(gdclos, GDCLOS) (const int* dirfile)
{
  if (*dirfile != 0) {
    gd_close(_GDF_GetDirfile(*dirfile));

    _GDF_ClearDirfile(*dirfile);
  }
}

/* gd_flush wrapper */
void F77_FUNC(gdflsh, GDFLSH) (const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out;

  if (field_code_l == 0)
    gd_flush(_GDF_GetDirfile(*dirfile), NULL);
  else {
    out = malloc(*field_code_l + 1);
    gd_flush(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l));
    free(out);
  }
}

/* getdata wrapper */
void F77_FUNC(gdgetd, GDGETD) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l,
    const int* first_frame, const int* first_sample,
    const int* num_frames, const int* num_samples, const int* return_type,
    void* data_out)
{
  char* out = malloc(*field_code_l + 1);
  *n_read = gd_getdata(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*return_type, data_out);
  free(out);
}

/* Return the maximum field name length */
void F77_FUNC(gdfdnx, GDFDNX) (int* max, const int* dirfile)
{
  size_t len = 0;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int i, nfields = gd_get_nfields(D);
  if (D->error)
    return;

  const char** fl = gd_get_field_list(D);

  for (i = 0; i < nfields; ++i)
    if (strlen(fl[i]) > len)
      len = strlen(fl[i]);

  *max = len;
}

/* Return the maximum field name length for a meta list */
void F77_FUNC(gdmfnx, GDMFNX) (int* max, const int* dirfile, const char* parent,
    const int* parent_l)
{
  size_t len = 0;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int i, nfields = gd_get_nmfields(D, pa);

  if (D->error) {
    free(pa);
    return;
  }

  const char** fl = gd_get_mfield_list(D, pa);

  for (i = 0; i < nfields; ++i)
    if (strlen(fl[i]) > len)
      len = strlen(fl[i]);

  *max = len;

  free(pa);
}

/* gd_get_field_list wrapper -- this only returns one field name */
void F77_FUNC(gdfldn, GDFLDN) (char* name, int* name_l, const int* dirfile,
    const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int nfields = gd_get_nfields(D);
  if (D->error)
    return;

  if (*field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_get_field_list(D);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;
}

/* gd_get_mfield_list wrapper -- this only returns one field name */
void F77_FUNC(gdmfdn, GDMFDN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int nfields = gd_get_nmfields(D, pa);
  if (D->error) {
    free(pa);
    return;
  }

  if (*field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_get_mfield_list(D, pa);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
}

/* gd_get_nfields wrapper */
void F77_FUNC(gdnfld, GDNFLD) (int* nfields, const int* dirfile)
{
  *nfields = gd_get_nfields(_GDF_GetDirfile(*dirfile));
}

/* gd_get_nframes wrapper */
void F77_FUNC(gdnfrm, GDNFRM) (int* nframes, const int* dirfile)
{
  *nframes = gd_get_nframes(_GDF_GetDirfile(*dirfile));
}

/* gd_get_spf wrapper */
void F77_FUNC(gdgspf, GDGSPF) (int* spf, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  *spf = gd_get_spf(_GDF_GetDirfile(*dirfile),
      _GDF_CString(out, field_code, *field_code_l));
  free(out);
}

/* putdata wrapper */
void F77_FUNC(gdputd, GDPUTD) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* first_frame,
    const int* first_sample, const int* num_frames, const int* num_samples,
    const int* data_type, const void* data_in)
{
  char* out = malloc(*field_code_l + 1);
  *n_wrote = gd_putdata(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*data_type, data_in);
  free(out);
}

/* return the error number */
void F77_FUNC(gderor, GDEROR) (int* error, const int* dirfile)
{
  *error = gd_error(_GDF_GetDirfile(*dirfile));
}

/* gd_error_string wrapper */
void F77_FUNC(gdestr, GDESTR) (const int* dirfile, char* buffer, const int* len)
{
  int i;
  gd_error_string(_GDF_GetDirfile(*dirfile), buffer, *len);

  /* space pad */
  for (i = 0; i < *len && buffer[i]; ++i)
    ;

  for (; i < *len; ++i)
    buffer[i] = ' ';
}

/* gd_get_entry_type wrapper */
void F77_FUNC(gdenty, GDENTY) (int* type, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* fc = malloc(*field_code_l + 1);

  *type = (int)gd_get_entry_type(_GDF_GetDirfile(*dirfile), _GDF_CString(fc,
        field_code, *field_code_l));

  free(fc);
}

/* gd_get_entry wrapper for RAW */
void F77_FUNC(gdgerw, GDGERW) (int* spf, int* dtype, int* fragment_index,
    const int* dirfile, const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_RAW_ENTRY)
    *spf = 0;
  else {
    *spf = E.spf;
    *dtype = E.data_type;
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_entry wrapper for LINCOM */
void F77_FUNC(gdgelc, GDGELC) (int* nfields,
    char* infield1, int* infield1_l, double* m1, double* b1,
    char* infield2, int* infield2_l, double* m2, double* b2,
    char* infield3, int* infield3_l, double* m3, double* b3,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  dtrace("%p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %i, %s, %i",
      nfields, infield1, infield1_l, m1, b1, infield2, infield2_l, m2, b2,
      infield3, infield3_l, m3, b3, fragment_index, *dirfile, field_code,
      *field_code_l);
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
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

    gd_free_entry_strings(&E);
  }
  free(out);

  dreturnvoid();
}

void F77_FUNC(gdgecl, GDGECL) (int* nfields,
    char* infield1, int* infield1_l, double complex* m1, double complex* b1,
    char* infield2, int* infield2_l, double complex* m2, double complex* b2,
    char* infield3, int* infield3_l, double complex* m3, double complex* b3,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  dtrace("%p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %i, \"%s\", "
      "%i", nfields, infield1, infield1_l, m1, b1, infield2, infield2_l, m2, b2,
      infield3, infield3_l, m3, b3, fragment_index, *dirfile, field_code,
      *field_code_l);
  char* fc = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
          *field_code_l), &E))
    *nfields = 0;
  else if (E.field_type != GD_LINCOM_ENTRY) {
    *nfields = 0;
    gd_free_entry_strings(&E);
  } else {
    *nfields = E.n_fields;
    *fragment_index = E.fragment_index;

    _GDF_FString(infield1, infield1_l, E.in_fields[0]);
    *m1 = E.cm[0];
    *b1 = E.cb[0];

    if (E.n_fields > 1) {
      _GDF_FString(infield2, infield2_l, E.in_fields[1]);
      *m2 = E.cm[1];
      *b2 = E.cb[1];
    }

    if (E.n_fields > 2) {
      _GDF_FString(infield3, infield3_l, E.in_fields[2]);
      *m3 = E.cm[2];
      *b3 = E.cb[2];
    }
    gd_free_entry_strings(&E);
  }

  free(fc);

  dreturnvoid();
}

/* gd_get_entry wrapper for POLYNOM */
void F77_FUNC(gdgepn, GDGEPN) (int* poly_ord,
    char* infield, int* infield_l, double* a0, double* a1,
    double* a2, double* a3, double* a4, double* a5,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_POLYNOM_ENTRY)
    *poly_ord = 0;
  else {
    *poly_ord = E.poly_ord;
    *fragment_index = E.fragment_index;

    _GDF_FString(infield, infield_l, E.in_fields[0]);

    switch (E.poly_ord) {
      case 5:
        *a5 = E.a[5];
      case 4:
        *a4 = E.a[4];
      case 3:
        *a3 = E.a[3];
      case 2:
        *a2 = E.a[2];
      case 1:
        *a1 = E.a[1];
        *a0 = E.a[0];
    }
    gd_free_entry_strings(&E);
  }

  free(out);
}

void F77_FUNC(gdgecp, GDGECP) (int* poly_ord,
    char* infield, int* infield_l, double complex* a0, double complex* a1,
    double complex* a2, double complex* a3, double complex* a4,
    double complex* a5, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_POLYNOM_ENTRY)
    *poly_ord = 0;
  else {
    *poly_ord = E.poly_ord;
    *fragment_index = E.fragment_index;

    _GDF_FString(infield, infield_l, E.in_fields[0]);

    switch (E.poly_ord) {
      case 5:
        *a5 = E.ca[5];
      case 4:
        *a4 = E.ca[4];
      case 3:
        *a3 = E.ca[3];
      case 2:
        *a2 = E.ca[2];
      case 1:
        *a1 = E.ca[1];
        *a0 = E.ca[0];
    }
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_entry wrapper for LINTERP */
void F77_FUNC(gdgelt, GDGELT) (char* in_field, int* in_field_l, char* table,
    int* table_l, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_LINTERP_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(table, table_l, E.table);
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_entry wrapper for BIT */
void F77_FUNC(gdgebt, GDGEBT) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_BIT_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *bitnum = E.bitnum;
    *numbits = E.numbits;
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_entry wrapper for SBIT */
void F77_FUNC(gdgesb, GDGESB) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_SBIT_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *bitnum = E.bitnum;
    *numbits = E.numbits;
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_entry wrapper for MULTIPLY */
void F77_FUNC(gdgemt, GDGEMT) (char* in_field1, int* in_field1_l,
    char* in_field2, int* in_field2_l, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_MULTIPLY_ENTRY)
    *in_field1_l = 0;
  else {
    _GDF_FString(in_field1, in_field1_l, E.in_fields[0]);
    _GDF_FString(in_field2, in_field2_l, E.in_fields[1]);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_entry wrapper for PHASE */
void F77_FUNC(gdgeph, GDGEPH) (char* in_field, int* in_field_l, int* shift,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_PHASE_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *shift = E.shift;
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_entry wrapper for CONST */
void F77_FUNC(gdgeco, GDGECO) (int* data_type, int* fragment_index,
    const int* dirfile, const char* field_code, const int* field_code_l)
{
  char* out = malloc(*field_code_l + 1);
  gd_entry_t E;

  if (gd_get_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
          *field_code_l), &E) || E.field_type != GD_CONST_ENTRY)
    *data_type = 0;
  else {
    *data_type = E.const_type;
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
}

/* gd_get_fragment_index wrapper */
void F77_FUNC(gdfrgi, GDFRGI) (int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* fc = malloc(*field_code_l + 1);

  *fragment_index = gd_get_fragment_index(_GDF_GetDirfile(*dirfile),
      _GDF_CString(fc, field_code, *field_code_l));

  free(fc);
}

/* gd_add_raw wrapper */
void F77_FUNC(gdadrw, GDADRW) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* data_type, const int* spf,
    const int* fragment_index)
{
  char* out = malloc(*field_code_l + 1);
  gd_add_raw(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), (gd_type_t)(*data_type), (gd_spf_t)*spf,
      *fragment_index);
  free(out);
}

/* gd_add_lincom wrapper */
void F77_FUNC(gdadlc, GDADLC) (const int* dirfile, const char* field_code,
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

  gd_add_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), nf, (const char**)in_fields, m, b, *fragment_index);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

void F77_FUNC(gdadcl, GDADCL) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const double complex* m1, const double complex* b1,
    const char* in_field2, const int* in_field2_l, const double complex* m2,
    const double complex* b2, const char* in_field3, const int* in_field3_l,
    const double complex* m3, const double complex* b3,
    const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in_fields[3] = {NULL, NULL, NULL};
  double complex cm[3] = {0, 0, 0};
  double complex cb[3] = {0, 0, 0};
  int nf = *n_fields;

  if (nf > 0) {
    in_fields[0] = malloc(*in_field1_l + 1);
    _GDF_CString(in_fields[0], in_field1, *in_field1_l);
    cm[0] = *m1;
    cb[0] = *b1;
  }

  if (nf > 1) {
    in_fields[1] = malloc(*in_field2_l + 1);
    _GDF_CString(in_fields[1], in_field2, *in_field2_l);
    cm[1] = *m2;
    cb[1] = *b2;
  }

  if (nf > 2) {
    in_fields[2] = malloc(*in_field3_l + 1);
    _GDF_CString(in_fields[2], in_field3, *in_field3_l);
    cm[2] = *m3;
    cb[2] = *b3;
  }

  gd_add_clincom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), nf, (const char**)in_fields, cm, cb, *fragment_index);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

/* gd_add_polynom wrapper */
void F77_FUNC(gdadpn, GDADPN) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const double* a0, const double* a1, const double* a2,
    const double* a3, const double* a4, const double* a5,
    const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* inf = NULL;
  double a[6] = {0, 0, 0, 0, 0, 0};
  int po = *poly_ord;
  if (po > 5)
    po = 5;

  inf = malloc(*in_field_l + 1);
  _GDF_CString(inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      a[5] = *a5;
    case 4:
      a[4] = *a4;
    case 3:
      a[3] = *a3;
    case 2:
      a[2] = *a2;
    case 1:
      a[1] = *a1;
      a[0] = *a0;
  }

  gd_add_polynom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), po, inf, a, *fragment_index);
  free(fc);
  free(inf);
}

void F77_FUNC(gdadcp, GDADCP) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const double complex* a0, const double complex* a1,
    const double complex* a2, const double complex* a3,
    const double complex* a4, const double complex* a5,
    const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* inf = NULL;
  double complex ca[6] = {0, 0, 0, 0, 0, 0};
  int po = *poly_ord;
  if (po > 5)
    po = 5;

  inf = malloc(*in_field_l + 1);
  _GDF_CString(inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      ca[5] = *a5;
    case 4:
      ca[4] = *a4;
    case 3:
      ca[3] = *a3;
    case 2:
      ca[2] = *a2;
    case 1:
      ca[1] = *a1;
      ca[0] = *a0;
  }

  gd_add_cpolynom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), po, inf, ca, *fragment_index);
  free(fc);
  free(inf);
}

/* gd_add_linterp wrapper */
void F77_FUNC(gdadlt, GDADLT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const char* table, const int* table_l, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);
  char* tab = malloc(*table_l + 1);

  gd_add_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l),
      _GDF_CString(tab, table, *table_l), *fragment_index);
  free(fc);
  free(in);
  free(tab);
}

/* gd_add_bit wrapper */
void F77_FUNC(gdadbt, GDADBT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* bitnum, const int* numbits, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_add_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l),
      (gd_bit_t)*bitnum, (gd_bit_t)*numbits, *fragment_index);
  free(fc);
  free(in);
}

/* gd_add_sbit wrapper */
void F77_FUNC(gdadsb, GDADSB) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* bitnum, const int* numbits, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_add_sbit(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l),
      (gd_bit_t)*bitnum, (gd_bit_t)*numbits, *fragment_index);
  free(fc);
  free(in);
}

/* gd_add_multiply wrapper */
void F77_FUNC(gdadmt, GDADMT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in1 = malloc(*in_field1_l + 1);
  char* in2 = malloc(*in_field2_l + 1);

  gd_add_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in1, in_field1, *in_field1_l),
      _GDF_CString(in2, in_field2, *in_field2_l), *fragment_index);

  free(fc);
  free(in1);
  free(in2);
}

/* gd_add_phase wrapper */
void F77_FUNC(gdadph, GDADPH) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* shift, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_add_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l), *shift,
      *fragment_index);

  free(fc);
  free(in);
}

/* gd_get_fragmentname wrapper */
void F77_FUNC(gdfrgn, GDFRGN) (char* filename, int* filename_l,
    const int* dirfile, const int* index)
{
  _GDF_FString(filename, filename_l, gd_get_fragmentname(_GDF_GetDirfile(*dirfile),
        *index));
}

/* gd_get_nfragments wrapper */
void F77_FUNC(gdnfrg, GDNFRG) (int* nformats, const int* dirfile)
{
  *nformats = gd_get_nfragments(_GDF_GetDirfile(*dirfile));
}

/* gd_metaflush wrapper */
void F77_FUNC(gdmfls, GDMFLS) (const int* dirfile)
{
  gd_metaflush(_GDF_GetDirfile(*dirfile));
}

/* gd_include wrapper */
void F77_FUNC(gdincl, GDINCL) (const int* dirfile, const char* file,
    const int* file_l, const int* fragment_index, const int* flags)
{
  char* fi = malloc(*file_l + 1);

  gd_include(_GDF_GetDirfile(*dirfile), _GDF_CString(fi, file, *file_l),
      *fragment_index, *flags);

  free(fi);
}

/* gd_get_nfield_by_type wrapper */
void F77_FUNC(gdnfdt, GDNFDT) (int* nfields, const int* dirfile,
    const int* type)
{
  *nfields = gd_get_nfields_by_type(_GDF_GetDirfile(*dirfile), (gd_entype_t)*type);
}

/* gd_get_nvectors wrapper */
void F77_FUNC(gdnvec, GDNVEC) (int* nvectors, const int* dirfile)
{
  *nvectors = gd_get_nvectors(_GDF_GetDirfile(*dirfile));
}

/* gd_get_field_list_by_type wrapper -- this only returns one field name */
void F77_FUNC(gdfdnt, GDFDNT) (char* name, int* name_l, const int* dirfile,
    const int* type, const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int nfields = gd_get_nfields_by_type(D, (gd_entype_t)*type);
  if (D->error)
    return;

  if (*field_num <= (int)nfields) {
    fl = gd_get_field_list_by_type(D, (gd_entype_t)*type);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;
}

/* gd_get_vector_list wrapper -- this only returns one field name */
void F77_FUNC(gdvecn, GDVECN) (char* name, int* name_l, const int* dirfile,
    const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  unsigned int nfields = gd_get_nvectors(D);
  if (D->error)
    return;

  if (*field_num <= (int)nfields) {
    fl = gd_get_vector_list(D);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;
}

/* gd_get_mfield_list_by_type wrapper -- this only returns one field name */
void F77_FUNC(gdmfdt, GDMFDT) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* type,
    const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int nfields = gd_get_nmfields_by_type(D, pa, (gd_entype_t)*type);
  if (D->error) {
    free(pa);
    return;
  }

  if (*field_num <= (int)nfields) {
    fl = gd_get_mfield_list_by_type(D, pa, (gd_entype_t)*type);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
}

/* gd_get_mvector_list wrapper -- this only returns one field name */
void F77_FUNC(gdmven, GDMVEN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num)
{
  const char** fl;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);

  char* pa = malloc(*parent_l + 1);
  _GDF_CString(pa, parent, *parent_l);

  unsigned int nfields = gd_get_nmvectors(D, pa);
  if (D->error) {
    free(pa);
    return;
  }

  if (*field_num <= (int)nfields) {
    fl = gd_get_mvector_list(D, pa);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
}

/* gd_madd_lincom wrapper */
void F77_FUNC(gdmdlc, GDMDLC) (const int* dirfile, const char* parent,
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

  gd_madd_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l), nf,
      (const char**)in_fields, m, b);
  free(pa);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

void F77_FUNC(gdmdcl, GDMDCL) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* n_fields, const char* in_field1, const int* in_field1_l,
    const double complex* m1, const double complex* b1, const char* in_field2,
    const int* in_field2_l, const double complex* m2, const double complex* b2,
    const char* in_field3, const int* in_field3_l, const double complex* m3,
    const double complex* b3)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in_fields[3] = {NULL, NULL, NULL};
  double complex cm[3] = {0, 0, 0};
  double complex cb[3] = {0, 0, 0};
  int nf = *n_fields;

  if (nf > 0) {
    in_fields[0] = malloc(*in_field1_l + 1);
    _GDF_CString(in_fields[0], in_field1, *in_field1_l);
    cm[0] = *m1;
    cb[0] = *b1;
  }

  if (nf > 1) {
    in_fields[1] = malloc(*in_field2_l + 1);
    _GDF_CString(in_fields[1], in_field2, *in_field2_l);
    cm[1] = *m2;
    cb[1] = *b2;
  }

  if (nf > 2) {
    in_fields[2] = malloc(*in_field3_l + 1);
    _GDF_CString(in_fields[2], in_field3, *in_field3_l);
    cm[2] = *m3;
    cb[2] = *b3;
  }

  gd_madd_clincom(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l), nf,
      (const char**)in_fields, cm, cb);
  free(pa);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

/* gd_madd_polynom wrapper */
void F77_FUNC(gdmdpn, GDMDPN) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* poly_ord, const char* in_field, const int* in_field_l,
    const double* a0, const double* a1, const double* a2, const double* a3,
    const double* a4, const double* a5)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* inf = NULL;
  double a[6] = {0, 0, 0, 0, 0, 0};
  int po = *poly_ord;
  if (po > 5)
    po = 5;

  inf = malloc(*in_field_l + 1);
  _GDF_CString(inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      a[5] = *a5;
    case 4:
      a[4] = *a4;
    case 3:
      a[3] = *a3;
    case 2:
      a[2] = *a2;
    case 1:
      a[1] = *a1;
      a[0] = *a0;
  }

  gd_madd_polynom(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l), po, inf, a);
  free(pa);
  free(fc);
  free(inf);
}

void F77_FUNC(gdmdcp, GDMDCP) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* poly_ord, const char* in_field, const int* in_field_l,
    const double complex* a0, const double complex* a1,
    const double complex* a2, const double complex* a3,
    const double complex* a4, const double complex* a5)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* inf = NULL;
  double complex ca[6] = {0, 0, 0, 0, 0, 0};
  int po = *poly_ord;
  if (po > 5)
    po = 5;

  inf = malloc(*in_field_l + 1);
  _GDF_CString(inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      ca[5] = *a5;
    case 4:
      ca[4] = *a4;
    case 3:
      ca[3] = *a3;
    case 2:
      ca[2] = *a2;
    case 1:
      ca[1] = *a1;
      ca[0] = *a0;
  }

  gd_madd_cpolynom(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l), po, inf, ca);
  free(pa);
  free(fc);
  free(inf);
}

/* gd_madd_linterp wrapper */
void F77_FUNC(gdmdlt, GDMDLT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const char* table,
    const int* table_l)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);
  char* tab = malloc(*table_l + 1);

  gd_madd_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in, in_field, *in_field_l), _GDF_CString(tab, table,
        *table_l));
  free(pa);
  free(fc);
  free(in);
  free(tab);
}

/* gd_madd_bit wrapper */
void F77_FUNC(gdmdbt, GDMDBT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* bitnum,
    const int* numbits)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_madd_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in, in_field, *in_field_l), (gd_bit_t)*bitnum,
      (gd_bit_t)*numbits);
  free(pa);
  free(fc);
  free(in);
}

/* gd_madd_sbit wrapper */
void F77_FUNC(gdmdsb, GDMDSB) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* bitnum,
    const int* numbits)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_madd_sbit(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in, in_field, *in_field_l), (gd_bit_t)*bitnum,
      (gd_bit_t)*numbits);
  free(pa);
  free(fc);
  free(in);
}

/* gd_madd_multiply wrapper */
void F77_FUNC(gdmdmt, GDMDMT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field1, const int* in_field1_l, const char* in_field2,
    const int* in_field2_l)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in1 = malloc(*in_field1_l + 1);
  char* in2 = malloc(*in_field2_l + 1);

  gd_madd_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in1, in_field1, *in_field1_l), _GDF_CString(in2, in_field2,
        *in_field2_l));

  free(pa);
  free(fc);
  free(in1);
  free(in2);
}

/* gd_madd_phase wrapper */
void F77_FUNC(gdmdph, GDMDPH) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* shift)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_madd_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(in, in_field, *in_field_l), *shift);

  free(pa);
  free(fc);
  free(in);
}

/* gd_add_const wrapper */
void F77_FUNC(gdadco, GDADCO) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* const_type, const int* data_type,
    const void* value, const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);

  gd_add_const(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), (gd_type_t)*const_type, (gd_type_t)*data_type, value,
      *fragment_index);

  free(fc);
}

/* gd_madd_const wrapper */
void F77_FUNC(gdmdco, GDMDCO) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* const_type, const int* data_type, const void* value)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);

  gd_madd_const(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      (gd_type_t)*const_type, (gd_type_t)*data_type, value);

  free(pa);
  free(fc);
}

/* gd_add_string wrapper */
void F77_FUNC(gdadst, GDADST) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* value, const int* value_l,
    const int* fragment_index)
{
  char* fc = malloc(*field_code_l + 1);
  char* va = malloc(*value_l + 1);

  gd_add_string(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(va, value, *value_l), *fragment_index);

  free(fc);
  free(va);
}

/* gd_madd_string wrapper */
void F77_FUNC(gdmdst, GDMDST) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* value, const int* value_l)
{
  char* pa = malloc(*parent_l + 1);
  char* fc = malloc(*field_code_l + 1);
  char* va = malloc(*value_l + 1);

  gd_madd_string(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l), _GDF_CString(fc, field_code, *field_code_l),
      _GDF_CString(va, value, *value_l));

  free(pa);
  free(fc);
  free(va);
}

/* gd_add_spec wrapper */
void F77_FUNC(gdadsp, GDADSP) (const int* dirfile, const char* spec,
    const int* spec_l, const int* fragment_index)
{
  char* sp = malloc(*spec_l + 1);

  gd_add_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(sp, spec, *spec_l),
      *fragment_index);

  free(sp);
}

/* gd_madd_spec wrapper */
void F77_FUNC(gdmdsp, GDMDSP) (const int* dirfile, const char* spec,
    const int* spec_l, const char *parent, const int* parent_l)
{
  char* pa = malloc(*parent_l + 1);
  char* sp = malloc(*spec_l + 1);

  gd_madd_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(sp, spec,
        *spec_l), _GDF_CString(pa, parent, *parent_l));

  free(pa);
  free(sp);
}

/* gd_get_constant wrapper */
void F77_FUNC(gdgtco, GDGTCO) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* return_type,
    void* data_out)
{
  char* fc = malloc(*field_code_l + 1);
  *n_read = gd_get_constant(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), (gd_type_t)*return_type, data_out);
  free(fc);
}

/* gd_get_string wrapper */
void F77_FUNC(gdgtst, GDGTST) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    char* data_out)
{
  char* fc = malloc(*field_code_l + 1);
  char* out = malloc(*len + 1);
  int l = *len;

  *n_read = gd_get_string(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), (size_t)*len, out) - 1;

  _GDF_FString(data_out, &l, out);
  free(fc);
  free(out);
}

/* gd_put_constant wrapper */
void F77_FUNC(gdptco, GDPTCO) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* data_type,
    const void* data_in)
{
  char* fc = malloc(*field_code_l + 1);
  *n_wrote = gd_put_constant(_GDF_GetDirfile(*dirfile), _GDF_CString(fc,
        field_code, *field_code_l), (gd_type_t)*data_type, data_in);
  free(fc);
}

/* gd_put_string wrapper */
void F77_FUNC(gdptst, GDPTST) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    const char* data_in)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*len + 1);
  *n_wrote = gd_put_string(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, data_in, *len)) - 1;
  free(fc);
  free(in);
}

/* gd_get_nmfields wrapper */
void F77_FUNC(gdnmfd, GDNMFD) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l)
{
  char* pa = malloc(*parent_l + 1);
  *nfields = gd_get_nmfields(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l));
  free(pa);
}

/* gd_get_nmfields_by_type wrapper */
void F77_FUNC(gdnmft, GDNMFT) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l, const int* type)
{
  char* pa = malloc(*parent_l + 1);
  *nfields = gd_get_nmfields_by_type(_GDF_GetDirfile(*dirfile), _GDF_CString(pa,
        parent, *parent_l), (gd_entype_t)*type);
  free(pa);
}

/* gd_get_nmvectors wrapper */
void F77_FUNC(gdnmve, GDNMVE) (int* nvectors, const int* dirfile,
    const char* parent, const int* parent_l)
{
  char* pa = malloc(*parent_l + 1);
  *nvectors = gd_get_nmvectors(_GDF_GetDirfile(*dirfile), _GDF_CString(pa, parent,
        *parent_l));
  free(pa);
}

/* gd_discard wrapper */
void F77_FUNC(gddscd, GDDSCD) (const int* dirfile)
{
  if (*dirfile != 0) {
    gd_discard(_GDF_GetDirfile(*dirfile));

    _GDF_ClearDirfile(*dirfile);
  }
}

/* gd_cbopen wrapper */
void F77_FUNC(gdcopn, GDCOPN) (int* dirfile, const char* dirfilename,
    const int* dirfilename_l, const int* flags, const void* callback)
{
  dtrace("%p, \"%s\", %i, %x, %p", dirfile, dirfilename, *dirfilename_l, *flags,
      callback);

  char* out = malloc(*dirfilename_l + 1);

  *dirfile = _GDF_SetDirfile(gd_cbopen(_GDF_CString(out, dirfilename,
          *dirfilename_l), *flags, (callback == 0) ? NULL : _GDF_Callback,
        (callback == 0) ? NULL : (void*)callback));

  free(out);
  dreturn("%i", *dirfile);
}

/* gd_parser_callback wrapper */
void F77_FUNC(gdclbk, GDCLBK) (const int* dirfile, const void* callback)
{
  gd_parser_callback(_GDF_GetDirfile(*dirfile), (callback == 0) ? NULL
      : _GDF_Callback, (callback == 0) ?  NULL : (void*)callback);
}

/* gd_alter_bit wrapper */
void F77_FUNC(gdalbt, GDALBT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    int* bitnum, int* numbits)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_alter_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l),
      (gd_bit_t)*bitnum, (gd_bit_t)*numbits);

  free(fc);
  free(in);
}

/* gd_alter_sbit wrapper */
void F77_FUNC(gdalsb, GDALSB) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    int* bitnum, int* numbits)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_alter_sbit(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l),
      (gd_bit_t)*bitnum, (gd_bit_t)*numbits);

  free(fc);
  free(in);
}

/* gd_alter_const wrapper */
void F77_FUNC(gdalco, GDALCO) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* const_type)
{
  char* fc = malloc(*field_code_l + 1);

  gd_alter_const(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), (gd_type_t)*const_type);

  free(fc);
}

/* gd_alter_lincom wrapper */
void F77_FUNC(gdallc, GDALLC) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const double* m1, const double* b1,
    const char* in_field2, const int* in_field2_l, const double* m2,
    const double* b2, const char* in_field3, const int* in_field3_l,
    const double* m3, const double* b3)
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

  gd_alter_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), nf, (const char**)in_fields, m, b);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

/* gd_alter_clincom wrapper */
void F77_FUNC(gdalcl, GDALCL) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const double complex* m1, const double complex* b1,
    const char* in_field2, const int* in_field2_l, const double complex* m2,
    const double complex* b2, const char* in_field3, const int* in_field3_l,
    const double complex* m3, const double complex* b3)
{
  char* fc = malloc(*field_code_l + 1);
  char* in_fields[3] = {NULL, NULL, NULL};
  double complex m[3] = {0, 0, 0};
  double complex b[3] = {0, 0, 0};
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

  gd_alter_clincom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), nf, (const char**)in_fields, m, b);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
}

/* gd_alter_polynom wrapper */
void F77_FUNC(gdalpn, GDALPN) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const double* a0, const double* a1, const double* a2,
    const double* a3, const double* a4, const double* a5)
{
  char* fc = malloc(*field_code_l + 1);
  char* inf = NULL;
  double a[6] = {0, 0, 0, 0, 0, 0};
  int po = *poly_ord;
  if (po > 5)
    po = 5;

  inf = malloc(*in_field_l + 1);
  _GDF_CString(inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      a[5] = *a5;
    case 4:
      a[4] = *a4;
    case 3:
      a[3] = *a3;
    case 2:
      a[2] = *a2;
    case 1:
      a[1] = *a1;
      a[0] = *a0;
  }

  gd_alter_polynom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), po, inf, a);
  free(fc);
  free(inf);
}

/* gd_alter_cpolynom wrapper */
void F77_FUNC(gdalcp, GDALCP) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const double complex* a0, const double complex* a1,
    const double complex* a2, const double complex* a3,
    const double complex* a4, const double complex* a5)
{
  char* fc = malloc(*field_code_l + 1);
  char* inf = NULL;
  double complex a[6] = {0, 0, 0, 0, 0, 0};
  int po = *poly_ord;
  if (po > 5)
    po = 5;

  inf = malloc(*in_field_l + 1);
  _GDF_CString(inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      a[5] = *a5;
    case 4:
      a[4] = *a4;
    case 3:
      a[3] = *a3;
    case 2:
      a[2] = *a2;
    case 1:
      a[1] = *a1;
      a[0] = *a0;
  }

  gd_alter_cpolynom(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), po, inf, a);
  free(fc);
  free(inf);
}

/* gd_alter_multiply wrapper */
void F77_FUNC(gdalmt, GDALMT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l)
{
  char* fc = malloc(*field_code_l + 1);
  char* in1 = malloc(*in_field1_l + 1);
  char* in2 = malloc(*in_field2_l + 1);

  gd_alter_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in1, in_field1, *in_field1_l),
      _GDF_CString(in2, in_field2, *in_field2_l));

  free(fc);
  free(in1);
  free(in2);
}

/* gd_alter_phase wrapper */
void F77_FUNC(gdalph, GDALPH) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* shift)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);

  gd_alter_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l), *shift);

  free(fc);
  free(in);
}

/* gd_get_encoding wrapper */
void F77_FUNC(gdgenc, GDGENC) (int* encoding, const int* dirfile,
    const int* fragment)
{
  *encoding = gd_get_encoding(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_get_endianness wrapper */
void F77_FUNC(gdgend, GDGEND) (int* endianness, const int* dirfile,
    const int* fragment)
{
  *endianness = gd_get_endianness(_GDF_GetDirfile(*dirfile), *fragment);
}

/* dirfilename wrapper */
void F77_FUNC(gdname, GDNAME) (char* name, int* name_l, const int* dirfile)
{
  const char* dn = gd_dirfilename(_GDF_GetDirfile(*dirfile));
  _GDF_FString(name, name_l, dn);
}

/* gd_get_parent_fragment wrapper */
void F77_FUNC(gdpfrg, GDPFRG) (int* parent, const int* dirfile,
    const int* fragment)
{
  *parent = gd_get_parent_fragment(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_protect wrapper */
void F77_FUNC(gdprot, GDPROT) (const int* dirfile, const int* protection_level,
    const int* fragment)
{
  gd_protect(_GDF_GetDirfile(*dirfile), *protection_level, *fragment);
}

/* gd_get_protection wrapper */
void F77_FUNC(gdgprt, GDGPRT) (int* protection_level, const int* dirfile,
    const int* fragment)
{
  *protection_level = gd_get_protection(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_get_raw_filename wrapper */
void F77_FUNC(gdrwfn, GDRWFN) (char* name, int* name_l, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* fc = malloc(*field_code_l + 1);

  const char* fn = gd_get_raw_filename(_GDF_GetDirfile(*dirfile), _GDF_CString(fc,
        field_code, *field_code_l));

  _GDF_FString(name, name_l, fn);

  free(fc);
}

/* gd_reference wrapper */
void F77_FUNC(gdrefe, GDREFE) (char* name, int* name_l, const int* dirfile,
    const char* field_code, const int *field_code_l)
{
  char* fc = malloc(*field_code_l + 1);

  const char* ref = gd_reference(_GDF_GetDirfile(*dirfile),
      _GDF_CString(fc, field_code, *field_code_l));

  _GDF_FString(name, name_l, ref);

  free(fc);
}

/* gd_alter_encoding wrapper */
void F77_FUNC(gdaenc, GDAENC) (const int* dirfile, const int* encoding,
    const int* fragment, const int* recode)
{
  gd_alter_encoding(_GDF_GetDirfile(*dirfile), *encoding, *fragment,
      *recode);
}

/* gd_alter_endianness wrapper */
void F77_FUNC(gdaend, GDAEND) (const int* dirfile, const int* endianness,
    const int* fragment, const int* recode)
{
  gd_alter_endianness(_GDF_GetDirfile(*dirfile), *endianness, *fragment,
      *recode);
}

/* gd_alter_linterp wrapper */
void F77_FUNC(gdallt, GDALLT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const char* table, const int* table_l, const int* recode)
{
  char* fc = malloc(*field_code_l + 1);
  char* in = malloc(*in_field_l + 1);
  char* tab = malloc(*table_l + 1);

  gd_alter_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(in, in_field, *in_field_l),
      _GDF_CString(tab, table, *table_l), *recode);
  free(fc);
  free(in);
  free(tab);
}

/* gd_alter_raw wrapper */
void F77_FUNC(gdalrw, GDALRW) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* data_type, const int* spf,
    const int* recode)
{
  char* out = malloc(*field_code_l + 1);
  gd_alter_raw(_GDF_GetDirfile(*dirfile), _GDF_CString(out, field_code,
        *field_code_l), (gd_type_t)(*data_type), (gd_spf_t)*spf, *recode);
  free(out);
}

/* gd_alter_spec wrapper */
void F77_FUNC(gdalsp, GDALSP) (const int* dirfile, const char* spec,
    const int* spec_l, const int* move)
{
  char* sp = malloc(*spec_l + 1);

  gd_alter_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(sp, spec, *spec_l),
      *move);

  free(sp);
}

/* gd_delete wrapper */
void F77_FUNC(gddele, GDDELE) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* flags)
{
  char* fc = malloc(*field_code_l + 1);

  gd_delete(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), *flags);

  free(fc);
}

/* gd_malter_spec wrapper */
void F77_FUNC(gdmlsp, GDMLSP) (const int* dirfile, const char* spec,
    const int* spec_l, const char* parent, const int* parent_l,
    const int* move)
{
  char* sp = malloc(*spec_l + 1);
  char* pa = malloc(*parent_l + 1);

  gd_malter_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(sp, spec,
        *spec_l), _GDF_CString(pa, parent, *parent_l), *move);

  free(pa);
  free(sp);
}

/* gd_move wrapper */
void F77_FUNC(gdmove, GDMOVE) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* new_fragment, const int* move_data)
{
  char* fc = malloc(*field_code_l + 1);

  gd_move(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), *new_fragment, *move_data);

  free(fc);
}

/* gd_rename wrapper */
void F77_FUNC(gdrenm, GDRENM) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* new_name, const int* new_name_l,
    const int* move_data)
{
  char* fc = malloc(*field_code_l + 1);
  char* nn = malloc(*new_name_l + 1);

  gd_rename(_GDF_GetDirfile(*dirfile), _GDF_CString(fc, field_code,
        *field_code_l), _GDF_CString(nn, new_name, *new_name_l), *move_data);

  free(nn);
  free(fc);
}

/* gd_uninclude wrapper */
void F77_FUNC(gduinc, GDUINC) (const int* dirfile, const int* fragment,
    const int* del)
{
  gd_uninclude(_GDF_GetDirfile(*dirfile), *fragment, *del);
}

/* gd_alter_frameoffset wrapper */
void F77_FUNC(gdafof, GDAFOF) (const int* dirfile, const int* offset,
    const int* fragment, const int* recode)
{
  gd_alter_frameoffset(_GDF_GetDirfile(*dirfile), *offset, *fragment,
      *recode);
}

/* gd_get_frameoffset wrapper */
void F77_FUNC(gdgfof, GDGFOF) (int* offset, const int* dirfile,
    const int* fragment)
{
  *offset = gd_get_frameoffset(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_get_native_type wrapper */
void F77_FUNC(gdntyp, GDNTYP) (int* type, const int* dirfile,
    const char* field_code, const int* field_code_l)
{
  char* fc = malloc(*field_code_l + 1);

  *type = gd_get_native_type(_GDF_GetDirfile(*dirfile), _GDF_CString(fc,
        field_code, *field_code_l));

  free(fc);
}

/* returns the value of the comp_scal member */
void F77_FUNC(gdcscl, GDCSCL) (int *comp_scal, const int *dirfile,
    const char *field_code, const int *field_code_l)
{
  char *fc = malloc(*field_code_l + 1);
  gd_entry_t E;
  DIRFILE *D = _GDF_GetDirfile(*dirfile);

  *comp_scal = 0;

  gd_get_entry(D, _GDF_CString(fc, field_code, *field_code_l), &E);

  if (!gd_error(D) && (E.field_type == GD_LINCOM_ENTRY ||
        E.field_type == GD_POLYNOM_ENTRY))
    *comp_scal = E.comp_scal;

  gd_free_entry_strings(&E);
  free(fc);
}

/* gd_validate wrapper */
void F77_FUNC(gdvldt, GDVLDT) (int *valid, const int *dirfile,
    const char *field_code, const int *field_code_l)
{
  char *fc = malloc(*field_code_l + 1);

  *valid = gd_validate(_GDF_GetDirfile(*dirfile),
      _GDF_CString(fc, field_code, *field_code_l));

  free(fc);
}

/* gd_get_framenum wrapper */
void F77_FUNC(gdfnum, GDFNUM) (double *framenum, const int *dirfile,
    const char *field_code, const int *field_code_l, const double *value)
{
  char *fc = malloc(*field_code_l + 1);

  *framenum = gd_get_framenum(_GDF_GetDirfile(*dirfile),
      _GDF_CString(fc, field_code, *field_code_l), *value);

  free(fc);
}

/* gd_get_framenum_subset wrapper */
void F77_FUNC(gdfnss, GDFNSS) (double *framenum, const int *dirfile,
    const char *field_code, const int *field_code_l, const double *value,
    const int *start, const int *end)
{
  char *fc = malloc(*field_code_l + 1);

  *framenum = gd_get_framenum_subset64(_GDF_GetDirfile(*dirfile),
      _GDF_CString(fc, field_code, *field_code_l), *value, *start, *end);

  free(fc);
}

/* retrieve a scalar parameter */
void F77_FUNC(gdgsca, GDGSCA) (char* scalar, int* scalar_l, const int* dirfile,
    const char* field_code, const int *field_code_l, const int* index)
{
  dtrace("%p, %p, %i, \"%s\", %i, %i", scalar, scalar_l, *dirfile, field_code,
      *field_code_l, *index);

  char *fc = malloc(*field_code_l + 1);
  gd_entry_t E;
  DIRFILE *D = _GDF_GetDirfile(*dirfile);
  int ok = 0;

  gd_get_entry(D, _GDF_CString(fc, field_code, *field_code_l), &E);

  free(fc);

  if (!gd_error(D) && *index > 0) {
    ok = 1;
    switch (E.field_type) {
      case GD_NO_ENTRY:
      case GD_LINTERP_ENTRY:
      case GD_MULTIPLY_ENTRY:
      case GD_INDEX_ENTRY:
      case GD_CONST_ENTRY:
      case GD_STRING_ENTRY:
        ok = 0;
        break;
      case GD_LINCOM_ENTRY:
        if (*index > GD_MAX_LINCOM + E.n_fields ||
            (*index > E.n_fields && *index <= GD_MAX_LINCOM))
        {
          ok = 0;
        }
        break;
      case GD_POLYNOM_ENTRY:
        if (*index > E.poly_ord + 1)
          ok = 0;
        break;
      case GD_BIT_ENTRY:
      case GD_SBIT_ENTRY:
        if (*index > 2)
          ok = 0;
        break;
      case GD_RAW_ENTRY:
      case GD_PHASE_ENTRY:
        if (*index > 1)
          ok = 0;
        break;
    }
  }

  if (ok && E.scalar[*index - 1] == NULL)
    ok = 0;

  _GDF_FString(scalar, scalar_l, (ok) ? E.scalar[*index - 1] : "");

  gd_free_entry_strings(&E);

  dreturnvoid();
}

/* set a scalar parameter */
void F77_FUNC(gdasca, GDASCA) (const int* dirfile, const char* field_code,
    const int *field_code_l, const int *index, const char *scalar,
    const int *scalar_l, int* recode)
{
  dtrace("%i, %p, %i, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      *index, scalar, *scalar_l, *recode);

  char *fc = malloc(*field_code_l + 1);
  gd_entry_t E;
  DIRFILE *D = _GDF_GetDirfile(*dirfile);

  if (*index < 1) {
    dreturnvoid();
    return;
  }

  gd_get_entry(D, _GDF_CString(fc, field_code, *field_code_l), &E);

  int ok = 1;
  switch (E.field_type) {
    case GD_NO_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
      ok = 0;
      break;
    case GD_LINCOM_ENTRY:
      if (*index > GD_MAX_LINCOM + E.n_fields ||
          (*index > E.n_fields && *index <= GD_MAX_LINCOM))
      {
        ok = 0;
      }
      break;
    case GD_POLYNOM_ENTRY:
      if (*index > E.poly_ord + 1)
        ok = 0;
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      if (*index > 2)
        ok = 0;
      break;
    case GD_RAW_ENTRY:
    case GD_PHASE_ENTRY:
      if (*index > 1)
        ok = 0;
      break;
  }

  if (!ok || gd_error(D)) {
    dreturnvoid();
    return;
  }

  free(E.scalar[*index - 1]);
  E.scalar[*index - 1] = malloc(*scalar_l + 1);
  _GDF_CString(E.scalar[*index - 1], scalar, *scalar_l);

  gd_alter_entry(D, fc, &E, *recode);

  dreturnvoid();
}
