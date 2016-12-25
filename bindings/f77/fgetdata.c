/* Copyright (C) 2008-2015 D. V. Wiebe
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

/* convert scalar_ind from C to FORTRAN */
#define GDF_SCIND_C2F(out,in) do { \
  (out) = (in); if ((out) >= 0) (out)++; \
} while (0)

/* convert scalar ind from FORTRAN to C */
#define GDF_SCIND_F2C(out,in) do { \
  (out) = (in); if ((out) > 0) (out)--; \
} while (0)

/* Fortran 77 has no facility to take a pointer to a DIRFILE* object.
 * Instead, we keep a list of them here.  If we ever run out of these,
 * we'll just return the invalid dirfile */
static DIRFILE* f77dirfiles[GDF_N_DIRFILES];
static int f77dirfiles_initialised = 0;

/* casting data pointers to function pointers is prohibited in C.  This
 * container is used to get around that */
static struct _GDF_callback_container {
  _GDF_callback_t func;
} f77callbacks[GDF_N_DIRFILES];

/* initialise the f77dirfiles array */
static void _GDF_InitDirfiles(void)
{
  int i;

  dtracevoid();

  for (i = 1; i < GDF_N_DIRFILES; ++i)
    f77dirfiles[i] = NULL;

  /* we keep entry zero as a generic, invalid dirfile to return if
   * dirfile lookup fails */
  f77dirfiles[0] = gd_invalid_dirfile();

  f77dirfiles_initialised = 1;
  dreturnvoid();
}

/* make a C string */
static char *_GDF_CString(char **out, const char *in, int l)
{
  int i;

  dtrace("%p, %p, %i", out, in, l);

  if (l < 0) {
    *out = NULL;
    dreturn("%p", NULL);
    return NULL;
  }

  *out = (char*)malloc(l + 1);
  for (i = 0; i < l; ++i)
    (*out)[i] = in[i];
  (*out)[l] = '\0';

  dreturn("\"%s\"", *out);
  return *out;
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
static int _GDF_SetDirfile(DIRFILE* D, int close)
{
  int i;

  dtrace("%p, %i", D, close);

  if (!f77dirfiles_initialised)
    _GDF_InitDirfiles();

  for (i = 1; i < GDF_N_DIRFILES; ++i)
    if (f77dirfiles[i] == NULL) {
      f77dirfiles[i] = D;
      f77callbacks[i].func = NULL;
      dreturn("%i", i);
      return i;
    }

  /* out of f77dirfiles space: complain */
  fputs("libfgetdata: DIRFILE space exhausted.", stderr);
  if (close)
    gd_discard(D);
  return -1;
}

/* delete the supplied dirfile */
static void _GDF_ClearDirfile(int d)
{
  dtrace("%i", d);

  if (d != 0)
    f77dirfiles[d] = NULL;

  dreturnvoid();
}

/* create a gd_triple_t value */
static gd_triplet_t _GDF_SetTriplet(gd_windop_t op, const void *data)
{
  gd_triplet_t t;

  dtrace("%i, %p", op, data);

  switch(op) {
    case GD_WINDOP_EQ:
    case GD_WINDOP_NE:
      t.i = *(int32_t*)data;
      dreturn("%" PRId64, t.i);
      break;
    case GD_WINDOP_SET:
    case GD_WINDOP_CLR:
      t.u = *(int32_t*)data;
      dreturn("%" PRIu64, t.u);
      break;
    default:
      t.r = *(double*)data;
      dreturn("%.15g", t.r);
      break;
  }

  return t;
}

/* create a Fortran space padded string */
static int _GDF_FString(char *dest, int32_t *dlen, const char *src)
{
  int i, slen;

  dtrace("%p, %i, \"%s\"", dest, *dlen, src);

  if (src == NULL) {
    *dlen = 0;
    dreturn("%i", -1);
    return -1;
  }

  slen = strlen(src);

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
static int _GDF_Callback(gd_parser_data_t* pdata, void *f77_callback)
{
  struct _GDF_callback_container *c =
    (struct _GDF_callback_container*)f77_callback;

  int unit;
  int r = GD_SYNTAX_ABORT;

  dtrace("%p, %p", pdata, f77_callback);

  if (c != NULL && c->func != NULL) {
    unit = _GDF_SetDirfile((DIRFILE*)pdata->dirfile, 0);
    if (unit < 0) {
      dreturn("%i", r);
      return r;
    }

    (c->func)(&r, &unit, &pdata->suberror, pdata->line, &pdata->linenum,
        pdata->filename);

    pdata->line[GD_MAX_LINE_LENGTH - 1] = '\0';

    _GDF_ClearDirfile(unit);
  }

  dreturn("%i", r);
  return r;
}

/* gd_open wrapper */
void F77_FUNC(gdopen, GDOPEN) (int32_t *dirfile, const char *dirfilename,
    const int32_t *dirfilename_l, const int32_t *flags)
{
  char *out;

  dtrace("%p, %p, %i, %i", dirfile, dirfilename, *dirfilename_l, *flags);

  *dirfile = _GDF_SetDirfile(gd_open(_GDF_CString(&out, dirfilename,
          *dirfilename_l), *flags), 1);

  free(out);

  dreturn("%i", *dirfile);
}

/* gd_close wrapper */
void F77_FUNC(gdclos, GDCLOS) (const int32_t *dirfile)
{
  dtrace("%i", *dirfile);

  if (*dirfile != 0) {
    gd_close(_GDF_GetDirfile(*dirfile));

    _GDF_ClearDirfile(*dirfile);
  }

  dreturnvoid();
}

/* gd_flush wrapper */
void F77_FUNC(gdflsh, GDFLSH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  dtrace("%i, %p, %i", *dirfile, field_code, *field_code_l);

  if (*field_code_l == 0)
    gd_flush(_GDF_GetDirfile(*dirfile), NULL);
  else {
    char *out;
    gd_flush(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l));
    free(out);
  }

  dreturnvoid();
}

/* gd_getdata wrapper */
void F77_FUNC(gdgetd, GDGETD) (int32_t *n_read, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample,
    const int32_t *num_frames, const int32_t *num_samples,
    const int32_t *return_type, void *data_out)
{
  char *out;

  dtrace("%p, %i, %p, %i, %i, %i, %i, %i, 0x%X, %p", n_read, *dirfile,
      field_code, *field_code_l, *first_frame, *first_sample, *num_frames,
      *num_samples, *return_type, data_out);

  *n_read = gd_getdata(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
        *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*return_type, data_out);
  free(out);

  dreturn("%i", *n_read);
}

/* Return the maximum field name length */
void F77_FUNC(gdfdnx, GDFDNX) (int32_t *max, const int32_t *dirfile)
{
  const char **fl;
  size_t len = 0;
  DIRFILE* D;
  unsigned int i, nfields;

  dtrace("%p, %i", max, *dirfile);

  D = _GDF_GetDirfile(*dirfile);
  nfields = gd_nfields(D);

  if (!gd_error(D)) {
    fl = gd_field_list(D);

    for (i = 0; i < nfields; ++i)
      if (strlen(fl[i]) > len)
        len = strlen(fl[i]);
  }

  *max = len;
  dreturn("%i", *max);
}

/* Return the maximum field name length for a meta list */
void F77_FUNC(gdmfnx, GDMFNX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l)
{
  const char **fl;
  unsigned int i, nfields;
  size_t len = 0;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  char *pa;

  dtrace("%p, %i, %p, %i", max, *dirfile, parent, *parent_l);

  _GDF_CString(&pa, parent, *parent_l);
  nfields = gd_nmfields(D, pa);

  if (!gd_error(D)) {
    fl = gd_mfield_list(D, pa);

    for (i = 0; i < nfields; ++i)
      if (strlen(fl[i]) > len)
        len = strlen(fl[i]);
  }

  *max = len;

  free(pa);
  dreturn("%i", *max);
}

/* gd_field_list wrapper -- this only returns one field name */
void F77_FUNC(gdfldn, GDFLDN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const int32_t *field_num)
{
  const char** fl;
  DIRFILE* D;
  unsigned int nfields;

  dtrace("%p, %p, %i, %i", name, name_l, *dirfile, *field_num);
  
  D = _GDF_GetDirfile(*dirfile);
  nfields = gd_nfields(D);

  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_field_list(D);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  dreturn("%i", *name_l);
}

/* gd_mfield_list wrapper -- this only returns one field name */
void F77_FUNC(gdmfdn, GDMFDN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *field_num)
{
  const char** fl;
  unsigned int nfields;
  DIRFILE* D = _GDF_GetDirfile(*dirfile);
  char *pa;

  dtrace("%p, %p, %i, %p, %i, %i", name, name_l, *dirfile, parent, *parent_l,
      *field_num);

  _GDF_CString(&pa, parent, *parent_l);
  nfields = gd_nmfields(D, pa);

  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_mfield_list(D, pa);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);

  dreturn("%i", *name_l);
}

/* gd_nfields wrapper */
void F77_FUNC(gdnfld, GDNFLD) (int32_t *nfields, const int32_t *dirfile)
{
  *nfields = gd_nfields(_GDF_GetDirfile(*dirfile));
}

/* gd_bof wrapper */
void F77_FUNC(gdgbof, GDGBOF) (int32_t *bof, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;
  dtrace("%p, %i, %p, %i", bof, *dirfile, field_code, *field_code_l);

  *bof = gd_bof(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));

  free(fc);
  dreturn("%i", *bof);
}

/* gd_eof wrapper */
void F77_FUNC(gdgeof, GDGEOF) (int32_t *eof, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;
  dtrace("%p, %i, %p, %i", eof, *dirfile, field_code, *field_code_l);

  *eof = gd_eof(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));
  free(fc);

  dreturn("%i", *eof);
}

/* gd_nframes wrapper */
void F77_FUNC(gdnfrm, GDNFRM) (int32_t *nframes, const int32_t *dirfile)
{
  *nframes = gd_nframes(_GDF_GetDirfile(*dirfile));
}

/* gd_spf wrapper */
void F77_FUNC(gdgspf, GDGSPF) (int32_t *spf, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *out;
  dtrace("%p, %i, %p, %i", spf, *dirfile, field_code, *field_code_l);

  *spf = gd_spf(_GDF_GetDirfile(*dirfile),
      _GDF_CString(&out, field_code, *field_code_l));
  free(out);
  dreturn("%i", *spf);
}

/* gd_putdata wrapper */
void F77_FUNC(gdputd, GDPUTD) (int32_t *n_wrote, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample,
    const int32_t *num_frames, const int32_t *num_samples,
    const int32_t *data_type, const void *data_in)
{
  char *out;
  dtrace("%p, %i, %p, %i, %i, %i, %i, %i, 0x%X, %p", n_wrote, *dirfile,
      field_code, *field_code_l, *first_frame, *first_sample, *num_frames,
      *num_samples, *data_type, data_in);

  *n_wrote = gd_putdata(_GDF_GetDirfile(*dirfile), _GDF_CString(&out,
        field_code, *field_code_l), *first_frame, *first_sample, *num_frames,
      *num_samples, (gd_type_t)*data_type, data_in);
  free(out);

  dreturn("%i", *n_wrote);
}

/* return the error number */
void F77_FUNC(gderor, GDEROR) (int32_t *error, const int32_t *dirfile)
{
  dtrace("%p, %i", error, *dirfile);

  *error = gd_error(_GDF_GetDirfile(*dirfile));

  dreturn("%i", *error);
}

/* gd_error_count wrapper */
void F77_FUNC(gdecnt, GDECNT) (int32_t *error_count, const int32_t *dirfile)
{
  dtrace("%p, %i", error_count, *dirfile);

  *error_count = gd_error_count(_GDF_GetDirfile(*dirfile));

  dreturn("%i", *error_count);
}

/* gd_error_string wrapper */
void F77_FUNC(gdestr, GDESTR) (const int32_t *dirfile, char *buffer,
    const int32_t *len)
{
  int i;
  gd_error_string(_GDF_GetDirfile(*dirfile), buffer, *len);

  /* space pad */
  for (i = 0; i < *len && buffer[i]; ++i)
    ;

  for (; i < *len; ++i)
    buffer[i] = ' ';
}

/* gd_entry_type wrapper */
void F77_FUNC(gdenty, GDENTY) (int32_t *type, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;

  dtrace("%p, %i, %p, %i", type, *dirfile, field_code, *field_code_l);

  *type = (int)gd_entry_type(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc,
        field_code, *field_code_l));

  free(fc);

  dreturn("%i", *type);
}

/* gd_entry wrapper for RAW */
void F77_FUNC(gdgerw, GDGERW) (int32_t *spf, int32_t *dtype,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %p, %p, %i, %p, %i", spf, dtype, fragment_index, *dirfile,
      field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_RAW_ENTRY)
    *spf = 0;
  else {
    *spf = E.EN(raw,spf);
    *dtype = E.EN(raw,data_type);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);

  dreturnvoid();
}

/* gd_entry wrapper for LINCOM */
void F77_FUNC(gdgelc, GDGELC) (int32_t *nfields,
    char *infield1, int32_t *infield1_l, double *m1, double *b1,
    char *infield2, int32_t *infield2_l, double *m2, double *b2,
    char *infield3, int32_t *infield3_l, double *m3, double *b3,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %i, %p, %i",
      nfields, infield1, infield1_l, m1, b1, infield2, infield2_l, m2, b2,
      infield3, infield3_l, m3, b3, fragment_index, *dirfile, field_code,
      *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_LINCOM_ENTRY)
    *nfields = 0;
  else {
    *nfields = E.EN(lincom,n_fields);
    *fragment_index = E.fragment_index;

    _GDF_FString(infield1, infield1_l, E.in_fields[0]);
    *m1 = E.EN(lincom,m)[0];
    *b1 = E.EN(lincom,b)[0];

    if (E.EN(lincom,n_fields) > 1) {
      _GDF_FString(infield2, infield2_l, E.in_fields[1]);
      *m2 = E.EN(lincom,m)[1];
      *b2 = E.EN(lincom,b)[1];
    }

    if (E.EN(lincom,n_fields) > 2) {
      _GDF_FString(infield3, infield3_l, E.in_fields[2]);
      *m3 = E.EN(lincom,m)[2];
      *b3 = E.EN(lincom,b)[2];
    }

    gd_free_entry_strings(&E);
  }
  free(out);

  dreturnvoid();
}

void F77_FUNC(gdgecl, GDGECL) (int32_t *nfields,
    char *infield1, int32_t *infield1_l, GD_DCOMPLEXP(m1), GD_DCOMPLEXP(b1),
    char *infield2, int32_t *infield2_l, GD_DCOMPLEXP(m2), GD_DCOMPLEXP(b2),
    char *infield3, int32_t *infield3_l, GD_DCOMPLEXP(m3), GD_DCOMPLEXP(b3),
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *fc;
  gd_entry_t E;

  dtrace("%p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %p, %i, %p, %i",
      nfields, infield1, infield1_l, m1, b1, infield2, infield2_l, m2, b2,
      infield3, infield3_l, m3, b3, fragment_index, *dirfile, field_code,
      *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
          *field_code_l), &E))
    *nfields = 0;
  else if (E.field_type != GD_LINCOM_ENTRY) {
    *nfields = 0;
    gd_free_entry_strings(&E);
  } else {
    *nfields = E.EN(lincom,n_fields);
    *fragment_index = E.fragment_index;

    _GDF_FString(infield1, infield1_l, E.in_fields[0]);
    gd_cs2cp_(m1, E.EN(lincom,cm)[0]);
    gd_cs2cp_(b1, E.EN(lincom,cb)[0]);

    if (E.EN(lincom,n_fields) > 1) {
      _GDF_FString(infield2, infield2_l, E.in_fields[1]);
      gd_cs2cp_(m2, E.EN(lincom,cm)[1]);
      gd_cs2cp_(b2, E.EN(lincom,cb)[1]);
    }

    if (E.EN(lincom,n_fields) > 2) {
      _GDF_FString(infield3, infield3_l, E.in_fields[2]);
      gd_cs2cp_(m3, E.EN(lincom,cm)[2]);
      gd_cs2cp_(b3, E.EN(lincom,cb)[2]);
    }
    gd_free_entry_strings(&E);
  }

  free(fc);

  dreturnvoid();
}

/* gd_entry wrapper for POLYNOM */
void F77_FUNC(gdgepn, GDGEPN) (int32_t *poly_ord, char *infield,
    int32_t *infield_l, double *a0, double *a1, double *a2, double *a3,
    double *a4, double *a5, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %p, %i, %p, %p, %p, %p, %p, %p, %p, %i, %p, %i", poly_ord,
      infield, *infield_l, a0, a1, a2, a3, a4, a5, fragment_index, *dirfile,
      field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_POLYNOM_ENTRY)
    *poly_ord = 0;
  else {
    *poly_ord = E.EN(polynom,poly_ord);
    *fragment_index = E.fragment_index;

    _GDF_FString(infield, infield_l, E.in_fields[0]);

    switch (E.EN(polynom,poly_ord)) {
      case 5:
        *a5 = E.EN(polynom,a)[5];
      case 4:
        *a4 = E.EN(polynom,a)[4];
      case 3:
        *a3 = E.EN(polynom,a)[3];
      case 2:
        *a2 = E.EN(polynom,a)[2];
      case 1:
        *a1 = E.EN(polynom,a)[1];
        *a0 = E.EN(polynom,a)[0];
    }
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

void F77_FUNC(gdgecp, GDGECP) (int32_t *poly_ord, char *infield,
    int32_t *infield_l, GD_DCOMPLEXP(a0), GD_DCOMPLEXP(a1), GD_DCOMPLEXP(a2),
    GD_DCOMPLEXP(a3), GD_DCOMPLEXP(a4), GD_DCOMPLEXP(a5),
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %p, %i, %p, %p, %p, %p, %p, %p, %p, %i, %p, %i", poly_ord,
      infield, *infield_l, a0, a1, a2, a3, a4, a5, fragment_index, *dirfile,
      field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_POLYNOM_ENTRY)
    *poly_ord = 0;
  else {
    *poly_ord = E.EN(polynom,poly_ord);
    *fragment_index = E.fragment_index;

    _GDF_FString(infield, infield_l, E.in_fields[0]);

    switch (E.EN(polynom,poly_ord)) {
      case 5:
        gd_cs2cp_(a5, E.EN(polynom,ca)[5]);
      case 4:
        gd_cs2cp_(a4, E.EN(polynom,ca)[4]);
      case 3:
        gd_cs2cp_(a3, E.EN(polynom,ca)[3]);
      case 2:
        gd_cs2cp_(a2, E.EN(polynom,ca)[2]);
      case 1:
        gd_cs2cp_(a1, E.EN(polynom,ca)[1]);
        gd_cs2cp_(a0, E.EN(polynom,ca)[0]);
    }
    gd_free_entry_strings(&E);
  }

  free(out);

  dreturnvoid();
}

/* gd_entry wrapper for LINTERP */
void F77_FUNC(gdgelt, GDGELT) (char *in_field, int32_t *in_field_l, char *table,
    int32_t *table_l, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %i, %p, %i, %p, %i, %p, %i", in_field, *in_field_l, table,
      *table_l, fragment_index, *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_LINTERP_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(table, table_l, E.EN(linterp,table));
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

/* gd_entry wrapper for BIT */
void F77_FUNC(gdgebt, GDGEBT) (char *in_field, int32_t *in_field_l,
    int32_t *bitnum, int32_t *numbits, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %i, %p, %p, %p, %i, %p, %i", in_field, *in_field_l, bitnum,
      numbits, fragment_index, *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_BIT_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *bitnum = E.EN(bit,bitnum);
    *numbits = E.EN(bit,numbits);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

/* gd_entry wrapper for SBIT */
void F77_FUNC(gdgesb, GDGESB) (char *in_field, int32_t *in_field_l,
    int32_t *bitnum, int32_t *numbits, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %i, %p, %p, %p, %i, %p, %i", in_field, *in_field_l, bitnum,
      numbits, fragment_index, *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_SBIT_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *bitnum = E.EN(bit,bitnum);
    *numbits = E.EN(bit,numbits);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

static void _GDF_GetEntryYoke(char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    gd_entype_t type)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %i, %p, %i, %p, %i, %p, %i, 0x%X", in_field1, *in_field1_l,
      in_field2, *in_field2_l, fragment_index, *dirfile, field_code,
      *field_code_l, type);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != type)
    *in_field1_l = 0;
  else {
    _GDF_FString(in_field1, in_field1_l, E.in_fields[0]);
    _GDF_FString(in_field2, in_field2_l, E.in_fields[1]);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

/* gd_entry wrapper for MULTIPLY */
void F77_FUNC(gdgemt, GDGEMT) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  dtrace("%p, %i, %p, %i, %p, %i, %p, %i", in_field1, *in_field1_l,
      in_field2, *in_field2_l, fragment_index, *dirfile, field_code,
      *field_code_l);

  _GDF_GetEntryYoke(in_field1, in_field1_l, in_field2, in_field2_l,
      fragment_index, dirfile, field_code, field_code_l, GD_MULTIPLY_ENTRY);

  dreturnvoid();
}

/* gd_entry wrapper for DIVIDE */
void F77_FUNC(gdgedv, GDGEDV) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  dtrace("%p, %i, %p, %i, %p, %i, %p, %i", in_field1, *in_field1_l,
      in_field2, *in_field2_l, fragment_index, *dirfile, field_code,
      *field_code_l);

  _GDF_GetEntryYoke(in_field1, in_field1_l, in_field2, in_field2_l,
      fragment_index, dirfile, field_code, field_code_l, GD_DIVIDE_ENTRY);

  dreturnvoid();
}

/* gd_entry wrapper for INDIR */
void F77_FUNC(gdgeid, GDGEID) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  dtrace("%p, %i, %p, %i, %p, %i, %p, %i", in_field1, *in_field1_l,
      in_field2, *in_field2_l, fragment_index, *dirfile, field_code,
      *field_code_l);

  _GDF_GetEntryYoke(in_field1, in_field1_l, in_field2, in_field2_l,
      fragment_index, dirfile, field_code, field_code_l, GD_INDIR_ENTRY);

  dreturnvoid();
}

/* gd_entry wrapper for SINDIR */
void F77_FUNC(gdgesd, GDGESD) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  dtrace("%p, %i, %p, %i, %p, %i, %p, %i", in_field1, *in_field1_l,
      in_field2, *in_field2_l, fragment_index, *dirfile, field_code,
      *field_code_l);

  _GDF_GetEntryYoke(in_field1, in_field1_l, in_field2, in_field2_l,
      fragment_index, dirfile, field_code, field_code_l, GD_SINDIR_ENTRY);

  dreturnvoid();
}

/* gd_entry wrapper for WINDOW */
void F77_FUNC(gdgewd, GDGEWD) (char *in_field, int32_t *in_field_l,
    char *check_field, int32_t *check_field_l, int32_t *windop,
    int32_t *ithreshold, double *rthreshold, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *fc;
  gd_entry_t E;

  dtrace("%p, %i, %p, %i, %p, %p, %p, %p, %i, %p, %i", in_field, *in_field_l,
      check_field, *check_field_l, windop, ithreshold, rthreshold,
      fragment_index, *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
          *field_code_l), &E) || E.field_type != GD_WINDOW_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    _GDF_FString(check_field, check_field_l, E.in_fields[1]);
    *windop = E.EN(window,windop);
    switch (E.EN(window,windop)) {
      case GD_WINDOP_EQ:
      case GD_WINDOP_NE:
        *ithreshold = E.EN(window,threshold.i);
        break;
      case GD_WINDOP_SET:
      case GD_WINDOP_CLR:
        *ithreshold = E.EN(window,threshold.u);
        break;
      default:
        *rthreshold = E.EN(window,threshold.r);
        break;
    }
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(fc);

  dreturnvoid();
}

/* gd_entry wrapper for MPLEX */
void F77_FUNC(gdgemx, GDGEMX) (char *in_field, int32_t *in_field_l,
    char *count_field, int32_t *count_field_l, int32_t *val, int32_t *period,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *fc;
  gd_entry_t E;

  dtrace("%p, %i, %p, %i, %p, %p, %p, %i, %p, %i", in_field, *in_field_l,
      count_field, *count_field_l, val, period, fragment_index, *dirfile,
      field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
          *field_code_l), &E) || E.field_type != GD_MPLEX_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    _GDF_FString(count_field, count_field_l, E.in_fields[1]);
    *val = E.EN(mplex,count_val);
    *period = E.EN(mplex,period);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(fc);

  dreturnvoid();
}

/* gd_entry wrapper for RECIP */
void F77_FUNC(gdgerc, GDGERC) (char *in_field, int32_t *in_field_l,
    double *dividend, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %i, %p, %p, %i, %p, %i", in_field, *in_field_l, dividend,
      fragment_index, *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_RECIP_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *dividend = E.EN(recip,dividend);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);

  dreturnvoid();
}

void F77_FUNC(gdgecr, GDGECR) (char *in_field, int32_t *in_field_l,
    GD_DCOMPLEXP(cdividend), int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %i, %p, %p, %i, %p, %i", in_field, *in_field_l, cdividend,
      fragment_index, *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_RECIP_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    gd_cs2cp_(cdividend, E.EN(recip,cdividend));
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);

  dreturnvoid();
}

/* gd_entry wrapper for PHASE */
void F77_FUNC(gdgeph, GDGEPH) (char *in_field, int32_t *in_field_l,
    int32_t *shift, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %i, %p, %p, %i, %p, %i", in_field, *in_field_l, shift,
      fragment_index, *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_PHASE_ENTRY)
    *in_field_l = 0;
  else {
    _GDF_FString(in_field, in_field_l, E.in_fields[0]);
    *shift = E.EN(phase,shift);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

/* gd_entry wrapper for CONST */
void F77_FUNC(gdgeco, GDGECO) (int32_t *data_type, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %p, %i, %p, %i", data_type, fragment_index, *dirfile, field_code,
      *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_CONST_ENTRY)
    *data_type = 0;
  else {
    *data_type = E.EN(scalar,const_type);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

/* gd_entry wrapper for CARRAY */
void F77_FUNC(gdgeca, GDGECA) (int32_t *data_type, int32_t *array_len,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %p, %p, %i, %p, %i", data_type, array_len, fragment_index,
      *dirfile, field_code, *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_CARRAY_ENTRY)
    *data_type = 0;
  else {
    *data_type = E.EN(scalar,const_type);
    *array_len = E.EN(scalar,array_len);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

/* gd_entry wrapper for SARRAY */
void F77_FUNC(gdgesa, GDGESA) (int32_t *array_len, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *out;
  gd_entry_t E;

  dtrace("%p, %p, %i, %p, %i", array_len, fragment_index, *dirfile, field_code,
      *field_code_l);

  if (gd_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l), &E) || E.field_type != GD_SARRAY_ENTRY)
    *array_len = -1;
  else {
    *array_len = E.EN(scalar,array_len);
    *fragment_index = E.fragment_index;
    gd_free_entry_strings(&E);
  }

  free(out);
  dreturnvoid();
}

/* gd_fragment_index wrapper */
void F77_FUNC(gdfrgi, GDFRGI) (int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;
  dtrace("%p, %i, %p, %i", fragment_index, *dirfile, field_code,
      *field_code_l);

  *fragment_index = gd_fragment_index(_GDF_GetDirfile(*dirfile),
      _GDF_CString(&fc, field_code, *field_code_l));

  free(fc);

  dreturn("%i", *fragment_index);
}

/* gd_add_raw wrapper */
void F77_FUNC(gdadrw, GDADRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const int32_t *fragment_index)
{
  char *out;

  dtrace("%i, %p, %i, %i, %i, %i", *dirfile, field_code, *field_code_l,
      *data_type, *spf, *fragment_index);

  gd_add_raw(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
        *field_code_l), (gd_type_t)(*data_type), (unsigned int)*spf,
      *fragment_index);
  free(out);
  dreturnvoid();
}

/* gd_add_lincom wrapper */
void F77_FUNC(gdadlc, GDADLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const double *b1,
    const char *in_field2, const int32_t *in_field2_l, const double *m2,
    const double *b2, const char *in_field3, const int32_t *in_field3_l,
    const double *m3, const double *b3, const int32_t *fragment_index)
{
  char *fc;
  char *in_fields[3] = {NULL, NULL, NULL};
  double m[3] = {0, 0, 0};
  double b[3] = {0, 0, 0};
  const int nf = *n_fields;

  dtrace("%i, %p, %i, %i, %p, %i, %p, %p, %p, %i, %p, %p, %p, %i, %p, %p, %i",
      *dirfile, field_code, *field_code_l, *n_fields, in_field1, *in_field1_l,
      m1, b1, in_field2, *in_field2_l, m2, b2, in_field3, *in_field3_l, m3, b3,
      *fragment_index);

  if (nf > 0) {
    _GDF_CString(in_fields, in_field1, *in_field1_l);
    m[0] = *m1;
    b[0] = *b1;
  }

  if (nf > 1) {
    _GDF_CString(in_fields + 1, in_field2, *in_field2_l);
    m[1] = *m2;
    b[1] = *b2;
  }

  if (nf > 2) {
    _GDF_CString(in_fields + 2, in_field3, *in_field3_l);
    m[2] = *m3;
    b[2] = *b3;
  }

  gd_add_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), nf, (const char**)in_fields, m, b, *fragment_index);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
  dreturnvoid();
}

void F77_FUNC(gdadcl, GDADCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char *in_field2, const int32_t *in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char *in_field3, const int32_t *in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3),
    const int32_t *fragment_index)
{
  char *fc;
  char *in_fields[3] = {NULL, NULL, NULL};
#ifdef GD_NO_C99_API
  double cm[6] = {0, 0, 0, 0, 0, 0};
  double cb[6] = {0, 0, 0, 0, 0, 0};
#else
  double complex cm[3] = {0, 0, 0};
  double complex cb[3] = {0, 0, 0};
#endif
  const int nf = *n_fields;

  dtrace("%i, %p, %i, %i, %p, %i, %p, %p, %p, %i, %p, %p, %p, %i, %p, %p, %i",
      *dirfile, field_code, *field_code_l, *n_fields, in_field1, *in_field1_l,
      m1, b1, in_field2, *in_field2_l, m2, b2, in_field3, *in_field3_l, m3, b3,
      *fragment_index);

  if (nf > 0) {
    _GDF_CString(in_fields, in_field1, *in_field1_l);
    gd_cp2ca_(cm, 0, m1);
    gd_cp2ca_(cb, 0, b1);
  }

  if (nf > 1) {
    _GDF_CString(in_fields + 1, in_field2, *in_field2_l);
    gd_cp2ca_(cm, 1, m2);
    gd_cp2ca_(cb, 1, b2);
  }

  if (nf > 2) {
    _GDF_CString(in_fields + 2, in_field3, *in_field3_l);
    gd_cp2ca_(cm, 2, m3);
    gd_cp2ca_(cb, 2, b3);
  }

  gd_add_clincom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), nf, (const char**)in_fields, cm, cb, *fragment_index);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
  dreturnvoid();
}

/* gd_add_polynom wrapper */
void F77_FUNC(gdadpn, GDADPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const double *a1,
    const double *a2, const double *a3, const double *a4, const double *a5,
    const int32_t *fragment_index)
{
  char *fc, *inf;
  double a[6] = {0, 0, 0, 0, 0, 0};
  const int po = (*poly_ord > 5) ? 5 : *poly_ord;

  dtrace("%i, %p, %i, %i, %p, %i, %p, %p, %p, %p, %p, %p", *dirfile, field_code,
      *field_code_l, *poly_ord, in_field, *in_field_l, a0, a1, a2, a3, a4, a5);

  _GDF_CString(&inf, in_field, *in_field_l);

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

  gd_add_polynom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), po, inf, a, *fragment_index);
  free(fc);
  free(inf);
  dreturnvoid();
}

void F77_FUNC(gdadcp, GDADCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5),
    const int32_t *fragment_index)
{
  char *fc, *inf;
#ifdef GD_NO_C99_API
  double ca[12] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
#else
  double complex ca[6] = {0, 0, 0, 0, 0, 0};
#endif
  const int po = (*poly_ord > 5) ? 5 : *poly_ord;

  dtrace("%i, %p, %i, %i, %p, %i, %p, %p, %p, %p, %p, %p", *dirfile, field_code,
      *field_code_l, *poly_ord, in_field, *in_field_l, a0, a1, a2, a3, a4, a5);

  _GDF_CString(&inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      gd_cp2ca_(ca, 5, a5);
    case 4:
      gd_cp2ca_(ca, 4, a4);
    case 3:
      gd_cp2ca_(ca, 3, a3);
    case 2:
      gd_cp2ca_(ca, 2, a2);
    case 1:
      gd_cp2ca_(ca, 1, a1);
      gd_cp2ca_(ca, 0, a0);
  }

  gd_add_cpolynom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), po, inf, ca, *fragment_index);
  free(fc);
  free(inf);
  dreturnvoid();
}

/* gd_add_linterp wrapper */
void F77_FUNC(gdadlt, GDADLT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *table, const int32_t *table_l,
    const int32_t *fragment_index)
{
  char *fc, *in, *tab;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, table, *table_l, *fragment_index);

  gd_add_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      _GDF_CString(&tab, table, *table_l), *fragment_index);
  free(fc);
  free(in);
  free(tab);
  dreturnvoid();
}

/* gd_add_bit wrapper */
void F77_FUNC(gdadbt, GDADBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits,
    const int32_t *fragment_index)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %i, %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, *bitnum, *numbits, *fragment_index);

  gd_add_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      *bitnum, *numbits, *fragment_index);
  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_add_sbit wrapper */
void F77_FUNC(gdadsb, GDADSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits,
    const int32_t *fragment_index)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %i, %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, *bitnum, *numbits, *fragment_index);

  gd_add_sbit(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      *bitnum, *numbits, *fragment_index);
  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_add_multiply wrapper */
void F77_FUNC(gdadmt, GDADMT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l, *fragment_index);

  gd_add_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l), *fragment_index);

  free(fc);
  free(in1);
  free(in2);
  dreturnvoid();
}

/* gd_add_divide wrapper */
void F77_FUNC(gdaddv, GDADDV) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l, *fragment_index);

  gd_add_divide(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l), *fragment_index);

  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_add_indir wrapper */
void F77_FUNC(gdadid, GDADID) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l, *fragment_index);

  gd_add_indir(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l), *fragment_index);

  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_add_sindir wrapper */
void F77_FUNC(gdadsd, GDADSD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l, *fragment_index);

  gd_add_sindir(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l), *fragment_index);

  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_add_recip wrapper */
void F77_FUNC(gdadrc, GDADRC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const int32_t *fragment_index)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %g, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, *dividend, *fragment_index);

  gd_add_recip(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l), *dividend,
      *fragment_index);

  free(fc);
  free(in);

  dreturnvoid();
}

void F77_FUNC(gdadcr, GDADCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *cdividend,
    const int32_t *fragment_index)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %g;%g, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, cdividend[0], cdividend[1], *fragment_index);

  gd_add_crecip89(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l), cdividend,
      *fragment_index);

  free(fc);
  free(in);

  dreturnvoid();
}

/* gd_add_phase wrapper */
void F77_FUNC(gdadph, GDADPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift,
    const int32_t *fragment_index)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, *shift, *fragment_index);

  gd_add_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l), *shift,
      *fragment_index);

  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_fragmentname wrapper */
void F77_FUNC(gdfrgn, GDFRGN) (char *filename, int32_t *filename_l,
    const int32_t *dirfile, const int32_t *index)
{
  dtrace("%p, %i, %i, %i", filename, *filename_l, *dirfile, *index);
  _GDF_FString(filename, filename_l, gd_fragmentname(_GDF_GetDirfile(*dirfile),
        *index));
  dreturnvoid();
}

/* gd_nfragments wrapper */
void F77_FUNC(gdnfrg, GDNFRG) (int32_t *nformats, const int32_t *dirfile)
{
  *nformats = gd_nfragments(_GDF_GetDirfile(*dirfile));
}

/* gd_metaflush wrapper */
void F77_FUNC(gdmfls, GDMFLS) (const int32_t *dirfile)
{
  dtrace("%i", *dirfile);

  gd_metaflush(_GDF_GetDirfile(*dirfile));

  dreturnvoid();
}

/* gd_rewrite_fragment wrapper */
void F77_FUNC(gdrfrg, GDRFRG) (const int32_t *dirfile, const int32_t *fragment)
{
  dtrace("%i, %i", *dirfile, *fragment);

  gd_rewrite_fragment(_GDF_GetDirfile(*dirfile), *fragment);

  dreturnvoid();
}

/* gd_include wrapper */
void F77_FUNC(gdincl, GDINCL) (const int32_t *dirfile, const char *file,
    const int32_t *file_l, const int32_t *fragment_index, const int32_t *flags)
{
  char *fi;

  dtrace("%i, %p, %i, %i, 0x%X", *dirfile, file, *file_l, *fragment_index,
      *flags);

  gd_include(_GDF_GetDirfile(*dirfile), _GDF_CString(&fi, file, *file_l),
      *fragment_index, *flags);

  free(fi);
  dreturnvoid();
}

/* gd_nfield_by_type wrapper */
void F77_FUNC(gdnfdt, GDNFDT) (int32_t *nfields, const int32_t *dirfile,
    const int32_t *type)
{
  dtrace("%p, %i, 0x%X", nfields, *dirfile, *type);

  *nfields = gd_nfields_by_type(_GDF_GetDirfile(*dirfile), (gd_entype_t)*type);

  dreturn("%i", *nfields);
}

/* gd_nvectors wrapper */
void F77_FUNC(gdnvec, GDNVEC) (int32_t *nvectors, const int32_t *dirfile)
{
  *nvectors = gd_nvectors(_GDF_GetDirfile(*dirfile));
}

/* gd_field_list_by_type wrapper -- this only returns one field name */
void F77_FUNC(gdfdnt, GDFDNT) (char *name, int32_t *name_l,
    const int32_t *dirfile, const int32_t *type, const int32_t *field_num)
{
  const char** fl;
  DIRFILE* D;
  unsigned int nfields;

  dtrace("%p, %p, %i, 0x%X, %i", name, name_l, *dirfile, *type, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  nfields = gd_nfields_by_type(D, (gd_entype_t)*type);

  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_field_list_by_type(D, (gd_entype_t)*type);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  dreturn("%i", *name_l);
}

/* gd_vector_list wrapper -- this only returns one field name */
void F77_FUNC(gdvecn, GDVECN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const int32_t *field_num)
{
  const char** fl;
  DIRFILE *D;
  unsigned int nfields;

  dtrace("%p, %p, %i, %i", name, name_l, *dirfile, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  nfields = gd_nvectors(D);

  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_vector_list(D);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  dreturn("%i", *name_l);
}

/* gd_mfield_list_by_type wrapper -- this only returns one field name */
void F77_FUNC(gdmfdt, GDMFDT) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *type, const int32_t *field_num)
{
  const char **fl;
  unsigned int nfields;
  DIRFILE *D;
  char *pa;

  dtrace("%p, %i, %i, %p, %i, 0x%X, %i", name, *name_l, *dirfile, parent,
      *parent_l, *type, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&pa, parent, *parent_l);

  nfields = gd_nmfields_by_type(D, pa, (gd_entype_t)*type);
  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_mfield_list_by_type(D, pa, (gd_entype_t)*type);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
  dreturnvoid();
}

/* gd_mvector_list wrapper -- this only returns one field name */
void F77_FUNC(gdmven, GDMVEN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *field_num)
{
  const char** fl;
  unsigned int nfields;
  DIRFILE* D;
  char *pa;

  dtrace("%p, %p, %i, %i", name, name_l, *dirfile, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&pa, parent, *parent_l);

  nfields = gd_nmvectors(D, pa);
  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nfields) {
    fl = gd_mvector_list(D, pa);
    _GDF_FString(name, name_l, fl[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
  dreturn("%i", *name_l);
}

/* gd_madd_lincom wrapper */
void F77_FUNC(gdmdlc, GDMDLC) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const double *b1,
    const char *in_field2, const int32_t *in_field2_l, const double *m2,
    const double *b2, const char *in_field3, const int32_t *in_field3_l,
    const double *m3, const double *b3)
{
  char *pa, *fc;
  char *in_fields[3] = {NULL, NULL, NULL};
  double m[3] = {0, 0, 0};
  double b[3] = {0, 0, 0};
  const int nf = *n_fields;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %p, %p, %p, %i, %p, %p, %p, %i, %p, "
      "%p", *dirfile, parent, *parent_l, field_code, *field_code_l, *n_fields,
      in_field1, *in_field1_l, m1, b1, in_field2, *in_field2_l, m2, b2,
      in_field3, *in_field3_l, m3, b3);

  if (nf > 0) {
    _GDF_CString(in_fields, in_field1, *in_field1_l);
    m[0] = *m1;
    b[0] = *b1;
  }

  if (nf > 1) {
    _GDF_CString(in_fields + 1, in_field2, *in_field2_l);
    m[1] = *m2;
    b[1] = *b2;
  }

  if (nf > 2) {
    _GDF_CString(in_fields + 2, in_field3, *in_field3_l);
    m[2] = *m3;
    b[2] = *b3;
  }

  gd_madd_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l), nf,
      (const char**)in_fields, m, b);
  free(pa);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
  dreturnvoid();
}

void F77_FUNC(gdmdcl, GDMDCL) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char *in_field2, const int32_t *in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char *in_field3, const int32_t *in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3))
{
  char *pa, *fc;
  char *in_fields[3] = {NULL, NULL, NULL};
#ifdef GD_NO_C99_API
  double cm[6] = {0, 0, 0, 0, 0, 0};
  double cb[6] = {0, 0, 0, 0, 0, 0};
#else
  double complex cm[3] = {0, 0, 0};
  double complex cb[3] = {0, 0, 0};
#endif
  const int nf = *n_fields;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %p, %p, %p, %i, %p, %p, %p, %i, %p, "
      "%p", *dirfile, parent, *parent_l, field_code, *field_code_l, *n_fields,
      in_field1, *in_field1_l, m1, b1, in_field2, *in_field2_l, m2, b2,
      in_field3, *in_field3_l, m3, b3);

  if (nf > 0) {
    _GDF_CString(in_fields, in_field1, *in_field1_l);
    gd_cp2ca_(cm, 0, m1);
    gd_cp2ca_(cb, 0, b1);
  }

  if (nf > 1) {
    _GDF_CString(in_fields + 1, in_field2, *in_field2_l);
    gd_cp2ca_(cm, 1, m2);
    gd_cp2ca_(cb, 1, b2);
  }

  if (nf > 2) {
    _GDF_CString(in_fields + 2, in_field3, *in_field3_l);
    gd_cp2ca_(cm, 2, m3);
    gd_cp2ca_(cb, 2, b3);
  }

  gd_madd_clincom(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l), nf,
      (const char**)in_fields, cm, cb);
  free(pa);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
  dreturnvoid();
}

/* gd_madd_polynom wrapper */
void F77_FUNC(gdmdpn, GDMDPN) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const double *a1,
    const double *a2, const double *a3, const double *a4, const double *a5)
{
  char *pa, *fc, *inf;
  double a[6] = {0, 0, 0, 0, 0, 0};
  const int po = (*poly_ord > 5) ? 5 : *poly_ord;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %p, %p, %p, %p, %p, %p", *dirfile,
      parent, *parent_l, field_code, *field_code_l, *poly_ord, in_field,
      *in_field_l, a0, a1, a2, a3, a4, a5);

  _GDF_CString(&inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      a[5] = *a5;
    case 4:
      a[4] = *a4;
    case 3:
      a[3] = *a3;
    case 2:
      a[2] = *a2;
    default:
      a[1] = *a1;
      a[0] = *a0;
  }

  gd_madd_polynom(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l), po, inf, a);
  free(pa);
  free(fc);
  free(inf);
  dreturnvoid();
}

void F77_FUNC(gdmdcp, GDMDCP) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5))
{
  char *pa, *fc, *inf;
#ifdef GD_NO_C99_API
  double ca[12] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
#else
  double complex ca[6] = {0, 0, 0, 0, 0, 0};
#endif
  int po = *poly_ord;
  if (po > 5)
    po = 5;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %p, %p, %p, %p, %p, %p", *dirfile,
      parent, *parent_l, field_code, *field_code_l, *poly_ord, in_field,
      *in_field_l, a0, a1, a2, a3, a4, a5);

  _GDF_CString(&inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      gd_cp2ca_(ca, 5, a5);
    case 4:
      gd_cp2ca_(ca, 4, a4);
    case 3:
      gd_cp2ca_(ca, 3, a3);
    case 2:
      gd_cp2ca_(ca, 2, a2);
    case 1:
      gd_cp2ca_(ca, 1, a1);
      gd_cp2ca_(ca, 0, a0);
  }

  gd_madd_cpolynom(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l), po, inf, ca);
  free(pa);
  free(fc);
  free(inf);
  dreturnvoid();
}

/* gd_madd_linterp wrapper */
void F77_FUNC(gdmdlt, GDMDLT) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *table, const int32_t *table_l)
{
  char *pa, *fc, *in, *tab;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %p, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field, *in_field_l, table, *table_l);

  gd_madd_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in, in_field, *in_field_l), _GDF_CString(&tab, table,
        *table_l));
  free(pa);
  free(fc);
  free(in);
  free(tab);
  dreturnvoid();
}

/* gd_madd_bit wrapper */
void F77_FUNC(gdmdbt, GDMDBT) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits)
{
  char *pa, *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field, *in_field_l, *bitnum, *numbits);

  gd_madd_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in, in_field, *in_field_l), *bitnum, *numbits);
  free(pa);
  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_madd_sbit wrapper */
void F77_FUNC(gdmdsb, GDMDSB) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits)
{
  char *pa, *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field, *in_field_l, *bitnum, *numbits);

  gd_madd_sbit(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in, in_field, *in_field_l), *bitnum, *numbits);
  free(pa);
  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_madd_multiply wrapper */
void F77_FUNC(gdmdmt, GDMDMT) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *pa, *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %p, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field1, *in_field1_l, in_field2,
      *in_field2_l);

  gd_madd_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in1, in_field1, *in_field1_l), _GDF_CString(&in2, in_field2,
        *in_field2_l));

  free(pa);
  free(fc);
  free(in1);
  free(in2);
  dreturnvoid();
}

/* gd_madd_divide wrapper */
void F77_FUNC(gdmddv, GDMDDV) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *pa, *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %p, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field1, *in_field1_l, in_field2,
      *in_field2_l);

  gd_madd_divide(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in1, in_field1, *in_field1_l), _GDF_CString(&in2, in_field2,
        *in_field2_l));

  free(pa);
  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_madd_indir wrapper */
void F77_FUNC(gdmdid, GDMDID) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *pa, *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %p, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field1, *in_field1_l, in_field2,
      *in_field2_l);

  gd_madd_indir(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in1, in_field1, *in_field1_l), _GDF_CString(&in2, in_field2,
        *in_field2_l));

  free(pa);
  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_madd_sindir wrapper */
void F77_FUNC(gdmdsd, GDMDSD) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *pa, *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %p, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field1, *in_field1_l, in_field2,
      *in_field2_l);

  gd_madd_sindir(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in1, in_field1, *in_field1_l), _GDF_CString(&in2, in_field2,
        *in_field2_l));

  free(pa);
  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_madd_recip wrapper */
void F77_FUNC(gdmdrc, GDMDRC) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend)
{
  char *pa, *fc, *in1;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %g", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field, *in_field_l, *dividend);

  gd_madd_recip(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in1, in_field, *in_field_l), *dividend);

  free(pa);
  free(fc);
  free(in1);

  dreturnvoid();
}

void F77_FUNC(gdmdcr, GDMDCR) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(cdividend))
{
  char *pa, *fc, *in1;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %g;%g", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field, *in_field_l, crealp(cdividend),
      cimagp(cdividend));

#ifdef GD_NO_C99_API
  gd_madd_crecip89(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in1, in_field, *in_field_l), cdividend);
#else
  gd_madd_crecip(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in1, in_field, *in_field_l), *cdividend);
#endif

  free(pa);
  free(fc);
  free(in1);

  dreturnvoid();
}

/* gd_madd_phase wrapper */
void F77_FUNC(gdmdph, GDMDPH) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift)
{
  char *pa, *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i", *dirfile, parent, *parent_l,
      field_code, *field_code_l, in_field, *in_field_l, *shift);

  gd_madd_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in, in_field, *in_field_l), *shift);

  free(pa);
  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_add_const wrapper */
void F77_FUNC(gdadco, GDADCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *data_type, const void *value, const int32_t *fragment_index)
{
  char *fc;

  dtrace("%i, %p, %i, %i, %i, %p, %i", *dirfile, field_code, *field_code_l,
      *const_type, *data_type, value, *fragment_index);

  gd_add_const(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*const_type, (gd_type_t)*data_type, value,
      *fragment_index);

  free(fc);
  dreturnvoid();
}

/* gd_madd_const wrapper */
void F77_FUNC(gdmdco, GDMDCO) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *data_type, const void *value)
{
  char *pa, *fc;

  dtrace("%i, %p, %i, %p, %i, %i, %i, %p", *dirfile, parent, *parent_l,
      field_code, *field_code_l, *const_type, *data_type, value);

  gd_madd_const(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      (gd_type_t)*const_type, (gd_type_t)*data_type, value);

  free(pa);
  free(fc);
  dreturnvoid();
}

/* gd_add_carray wrapper */
void F77_FUNC(gdadca, GDADCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type, int32_t *array_len,
    const int32_t *data_type, const void *value, const int32_t *fragment_index)
{
  char *fc;

  dtrace("%i, %p, %i, 0x%X, %i, 0x%X, %p, %i", *dirfile, field_code,
      *field_code_l, *const_type, *array_len, *data_type, value,
      *fragment_index);

  gd_add_carray(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*const_type, *array_len,
      (gd_type_t)*data_type, value, *fragment_index);

  free(fc);
  dreturnvoid();
}

/* gd_add_sarray wrapper -- values are intialised to nil */
void F77_FUNC(gdadsa, GDADSA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, int32_t *array_len,
    const int32_t *fragment_index)
{
  char *fc;
  const char **nils;
  int32_t i;

  dtrace("%i, %p, %i, %i, %i", *dirfile, field_code, *field_code_l, *array_len,
      *fragment_index);

  nils = malloc(*array_len * sizeof(*nils));
  if (nils == NULL) { /* XXX error reporting? */
    dreturnvoid();
    return;
  }
  for (i = 0; i < *array_len; ++i)
    nils[i] = "";

  gd_add_sarray(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), *array_len, nils, *fragment_index);

  free(fc);
  free(nils);
  dreturnvoid();
}

/* gd_madd_carray wrapper */
void F77_FUNC(gdmdca, GDMDCA) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *array_len, const int32_t *data_type, const void *value)
{
  char *pa, *fc;

  dtrace("%i, %p, %i, %p, %i, 0x%X, %i, 0x%X, %p", *dirfile, parent, *parent_l,
      field_code, *field_code_l, *const_type, *array_len, *data_type, value);

  gd_madd_carray(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      (gd_type_t)*const_type, *array_len, (gd_type_t)*data_type, value);

  free(pa);
  free(fc);
  dreturnvoid();
}

/* gd_madd_sarray wrapper -- values are intialised to nil */
void F77_FUNC(gdmdsa, GDMDSA) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *array_len)
{
  char *pa, *fc;
  const char **nils;
  int32_t i;

  dtrace("%i, %p, %i, %p, %i, %i", *dirfile, parent, *parent_l, field_code, 
      *field_code_l, *array_len);

  nils = malloc(*array_len * sizeof(*nils));
  if (nils == NULL) { /* XXX error reporting? */
    dreturnvoid();
    return;
  }
  for (i = 0; i < *array_len; ++i)
    nils[i] = "";

  gd_madd_sarray(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l), *array_len,
      nils);

  free(pa);
  free(fc);
  free(nils);
  dreturnvoid();
}

/* gd_add_string wrapper */
void F77_FUNC(gdadst, GDADST) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *value, const int32_t *value_l,
    const int32_t *fragment_index)
{
  char *fc, *va;

  dtrace("%i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l, value,
      *value_l, *fragment_index);

  gd_add_string(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&va, value, *value_l), *fragment_index);

  free(fc);
  free(va);
  dreturnvoid();
}

/* gd_madd_string wrapper */
void F77_FUNC(gdmdst, GDMDST) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *value, const int32_t *value_l)
{
  char *pa, *fc, *va;

  dtrace("%i, %p, %i, %p, %i, %p, %i", *dirfile, parent, *parent_l, field_code,
      *field_code_l, value, *value_l);

  gd_madd_string(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&va, value, *value_l));

  free(pa);
  free(fc);
  free(va);
  dreturnvoid();
}

/* gd_add_spec wrapper */
void F77_FUNC(gdadsp, GDADSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const int32_t *fragment_index)
{
  char *sp;

  dtrace("%i, %p, %i, %i", *dirfile, spec, *spec_l, *fragment_index);

  gd_add_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(&sp, spec, *spec_l),
      *fragment_index);

  free(sp);
  dreturnvoid();
}

/* gd_madd_spec wrapper */
void F77_FUNC(gdmdsp, GDMDSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const char *parent, const int32_t *parent_l)
{
  char *pa, *sp;

  dtrace("%i, %p, %i, %p, %i", *dirfile, spec, *spec_l, parent, *parent_l);

  gd_madd_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(&sp, spec,
        *spec_l), _GDF_CString(&pa, parent, *parent_l));

  free(pa);
  free(sp);

  dreturnvoid();
}

/* gd_get_constant wrapper */
void F77_FUNC(gdgtco, GDGTCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *return_type, void *data_out)
{
  char *fc;

  dtrace("%i, %p, %i, %i, %p", *dirfile, field_code, *field_code_l,
      *return_type, data_out);

  gd_get_constant(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*return_type, data_out);
  free(fc);

  dreturnvoid();
}

/* gd_get_carray wrapper */
void F77_FUNC(gdgtca, GDGTCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *return_type, void *data_out)
{
  char *fc;

  dtrace("%i, %p, %i, 0x%X, %p", *dirfile, field_code, *field_code_l,
      *return_type, data_out);

  gd_get_carray(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*return_type, data_out);
  free(fc);

  dreturnvoid();
}

/* gd_get_carray_slice wrapper */
void F77_FUNC(gdgcas, GDGCAS) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *start, const int32_t *n,
    const int32_t *return_type, void *data_out)
{
  char *fc;

  dtrace("%i, %p, %i, %i, %i, 0x%X, %p", *dirfile, field_code, *field_code_l,
      *start, *n, *return_type, data_out);

  gd_get_carray_slice(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), *start - 1, (size_t)*n, (gd_type_t)*return_type,
      data_out);
  free(fc);

  dreturnvoid();
}

/* gd_carray_len wrapper -- deprecated */
void F77_FUNC(gdcaln, GDCALN) (int32_t *len, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;

  dtrace("%p, %i, %p, %i", len, *dirfile, field_code, *field_code_l);

  *len = gd_array_len(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));
  free(fc);

  dreturn("%i", *len);
}

/* gd_get_string wrapper */
void F77_FUNC(gdgtst, GDGTST) (int32_t *size, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l, const int32_t *len,
    char *data_out)
{
  char *fc, *out;
  int l = *len;

  dtrace("%p, %i, %p, %i, %i, %p", size, *dirfile, field_code, *field_code_l,
      *len, data_out);

  out = (char*)malloc(l + 1);

  *size = gd_get_string(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (size_t)*len, out) - 1;

  _GDF_FString(data_out, &l, out);
  free(fc);
  free(out);

  dreturn("%i", *size);
}

/* gd_put_constant wrapper */
void F77_FUNC(gdptco, GDPTCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const void *data_in)
{
  char *fc;

  dtrace("%i, %p, %i, 0x%X, %p", *dirfile, field_code, *field_code_l,
      *data_type, data_in);

  gd_put_constant(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*data_type, data_in);
  free(fc);

  dreturnvoid();
}

/* gd_put_carray wrapper */
void F77_FUNC(gdptca, GDPTCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const void *data_in)
{
  char *fc;

  dtrace("%i, %p, %i, 0x%X, %p", *dirfile, field_code, *field_code_l,
      *data_type, data_in);

  gd_put_carray(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*data_type, data_in);
  free(fc);

  dreturnvoid();
}

/* gd_put_carray_slice wrapper */
void F77_FUNC(gdpcas, GDPCAS) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *start, const int32_t *n,
    const int32_t *data_type, const void *data_in)
{
  char *fc;

  dtrace("%i, %p, %i, %i, %i, 0x%X, %p", *dirfile, field_code, *field_code_l,
      *start, *n, *data_type, data_in);

  gd_put_carray_slice(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), *start - 1, (size_t)*n, (gd_type_t)*data_type, data_in);
  free(fc);

  dreturnvoid();
}

/* gd_put_string wrapper */
void F77_FUNC(gdptst, GDPTST) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *len, const char *data_in)
{
  char *fc, *in;
  dtrace("%i, %p, %i, %i, %p", *dirfile, field_code, *field_code_l, *len,
      data_in);

  gd_put_string(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, data_in, *len));
  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_nmfields wrapper */
void F77_FUNC(gdnmfd, GDNMFD) (int32_t *nfields, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l)
{
  char *pa;
  dtrace("%p, %i, %p, %i", nfields, *dirfile, parent, *parent_l);

  *nfields = gd_nmfields(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l));
  free(pa);
  dreturn("%i", *nfields);
}

/* gd_nmfields_by_type wrapper */
void F77_FUNC(gdnmft, GDNMFT) (int32_t *nfields, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *type)
{
  char *pa;
  dtrace("%p, %i, %p, %i, 0x%X", nfields, *dirfile, parent, *parent_l, *type);

  *nfields = gd_nmfields_by_type(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa,
        parent, *parent_l), (gd_entype_t)*type);
  free(pa);

  dreturn("%i", *nfields);
}

/* gd_nmvectors wrapper */
void F77_FUNC(gdnmve, GDNMVE) (int32_t *nvectors, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l)
{
  char *pa;

  dtrace("%p, %i, %p, %i", nvectors, *dirfile, parent, *parent_l);

  *nvectors = gd_nmvectors(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l));
  free(pa);

  dreturn("%i", *nvectors);
}

/* gd_discard wrapper */
void F77_FUNC(gddscd, GDDSCD) (const int32_t *dirfile)
{
  dtrace("%i", *dirfile);

  if (*dirfile != 0) {
    gd_discard(_GDF_GetDirfile(*dirfile));

    _GDF_ClearDirfile(*dirfile);
  }

  dreturnvoid();
}

/* gd_cbopen wrapper */
void F77_FUNC(gdcopn, GDCOPN) (int32_t *dirfile, const char *dirfilename,
    const int32_t *dirfilename_l, const int32_t *flags,
    const _GDF_callback_t callback)
{
  const struct _GDF_callback_container temp = { callback };
  DIRFILE *D;
  char *out;

  dtrace("%p, %p, %i, 0x%X, %p", dirfile, dirfilename, *dirfilename_l, *flags,
      callback);

  _GDF_CString(&out, dirfilename, *dirfilename_l);

  D = gd_cbopen(out, *flags, _GDF_Callback, (void*)&temp);
  *dirfile = _GDF_SetDirfile(D, 1);

  if (*dirfile >= 0) {
    /* save the callback */
    f77callbacks[*dirfile].func = callback;
    /* and tell getdata its new location */
    gd_parser_callback(D, _GDF_Callback, f77callbacks + *dirfile);
  }

  free(out);
  dreturn("%i", *dirfile);
}

/* gd_parser_callback wrapper */
void F77_FUNC(gdclbk, GDCLBK) (const int32_t *dirfile,
    const _GDF_callback_t callback)
{
  dtrace("%i, %p", *dirfile, callback);

  /* ensure *dirfile is sane */
  if (*dirfile < 0 || *dirfile >= GDF_N_DIRFILES) {
    dreturnvoid();
    return;
  }

  /* we only have to modify GetData's callback pointer if f77callbacks is
   * NULL for this dirfile (inidicating no previous callback); otherwise, just
   * update the saved callback pointer */
  if (f77callbacks[*dirfile].func == NULL) {
    f77callbacks[*dirfile].func = callback;
    gd_parser_callback(_GDF_GetDirfile(*dirfile), _GDF_Callback,
        f77callbacks + *dirfile);
  } else
    f77callbacks[*dirfile].func = callback;

  dreturnvoid();
}

/* deregister a callback function (ie. gd_parser_callback(..., NULL) */
void F77_FUNC(gdnocb, GDNOCB) (const int32_t *dirfile)
{
  dtrace("%i", *dirfile);

  /* ensure *dirfile is sane */
  if (*dirfile >= 0 && *dirfile < GDF_N_DIRFILES) {
    f77callbacks[*dirfile].func = NULL;
    gd_parser_callback(_GDF_GetDirfile(*dirfile), NULL, NULL);
  }

  dreturnvoid();
}

/* gd_alter_bit wrapper */
void F77_FUNC(gdalbt, GDALBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, *bitnum, *numbits);

  gd_alter_bit(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l), *bitnum,
      *numbits);

  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_alter_sbit wrapper */
void F77_FUNC(gdalsb, GDALSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, *bitnum, *numbits);

  gd_alter_sbit(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l), *bitnum,
      *numbits);

  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_alter_const wrapper */
void F77_FUNC(gdalco, GDALCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type)
{
  char *fc;

  dtrace("%i, %p, %i, %i", *dirfile, field_code, *field_code_l, *const_type);

  gd_alter_const(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*const_type);

  free(fc);
  dreturnvoid();
}

/* gd_alter_carray wrapper */
void F77_FUNC(gdalca, GDALCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *array_len)
{
  char *fc;

  dtrace("%i, %p, %i, 0x%X, %i", *dirfile, field_code, *field_code_l,
      *const_type, *array_len);

  gd_alter_carray(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (gd_type_t)*const_type, (size_t)*array_len);

  free(fc);
  dreturnvoid();
}

/* gd_alter_lincom wrapper */
void F77_FUNC(gdallc, GDALLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const double *b1,
    const char *in_field2, const int32_t *in_field2_l, const double *m2,
    const double *b2, const char *in_field3, const int32_t *in_field3_l,
    const double *m3, const double *b3)
{
  char *fc = NULL;
  char *in_fields[3] = {NULL, NULL, NULL};
  double m[3] = {0, 0, 0};
  double b[3] = {0, 0, 0};
  const int nf = *n_fields;

  dtrace("%i, %p, %i, %i, %p, %i, %g, %g, %p, %i, %g, %g, %p, %i, %g, %g",
      *dirfile, field_code, *field_code_l, *n_fields, in_field1, *in_field1_l,
      *m1, *b1, in_field2, *in_field2_l, *m2, *b2, in_field3, *in_field3_l, *m3,
      *b3);

  if (nf > 0) {
    _GDF_CString(in_fields, in_field1, *in_field1_l);
    m[0] = *m1;
    b[0] = *b1;
  }

  if (nf > 1) {
    _GDF_CString(in_fields + 1, in_field2, *in_field2_l);
    m[1] = *m2;
    b[1] = *b2;
  }

  if (nf > 2) {
    _GDF_CString(in_fields + 2, in_field3, *in_field3_l);
    m[2] = *m3;
    b[2] = *b3;
  }

  gd_alter_lincom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), nf, (const char**)in_fields, m, b);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
  dreturnvoid();
}

/* gd_alter_clincom wrapper */
void F77_FUNC(gdalcl, GDALCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char *in_field2, const int32_t *in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char *in_field3, const int32_t *in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3))
{
  char *fc;
  char *in_fields[3] = {NULL, NULL, NULL};
#ifdef GD_NO_C99_API
  double cm[6] = {0, 0, 0, 0, 0, 0};
  double cb[6] = {0, 0, 0, 0, 0, 0};
#else
  double complex cm[3] = {0, 0, 0};
  double complex cb[3] = {0, 0, 0};
#endif
  const int nf = *n_fields;

  dtrace("%i, %p, %i, %i, %p, %i, %p, %p, %p, %i, %p, %p, %p, %i, %p, %p",
      *dirfile, field_code, *field_code_l, *n_fields, in_field1, *in_field1_l,
      m1, b1, in_field2, *in_field2_l, m2, b2, in_field3, *in_field3_l, m3, b3);

  if (nf > 0) {
    _GDF_CString(in_fields, in_field1, *in_field1_l);
    gd_cp2ca_(cm, 0, m1);
    gd_cp2ca_(cb, 0, b1);
  }

  if (nf > 1) {
    _GDF_CString(in_fields + 1, in_field2, *in_field2_l);
    gd_cp2ca_(cm, 1, m2);
    gd_cp2ca_(cb, 1, b2);
  }

  if (nf > 2) {
    _GDF_CString(in_fields + 2, in_field3, *in_field3_l);
    gd_cp2ca_(cm, 2, m3);
    gd_cp2ca_(cb, 2, b3);
  }

  gd_alter_clincom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), nf, (const char**)in_fields, cm, cb);
  free(fc);
  free(in_fields[0]);
  free(in_fields[1]);
  free(in_fields[2]);
  dreturnvoid();
}

/* gd_alter_polynom wrapper */
void F77_FUNC(gdalpn, GDALPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const double *a1,
    const double *a2, const double *a3, const double *a4, const double *a5)
{
  char *fc, *inf;
  double a[6] = {0, 0, 0, 0, 0, 0};
  const int po = (*poly_ord > 5) ? 5 : *poly_ord;

  dtrace("%i, %p, %i, %i, %p, %i, %g, %g, %p, %p, %p, %p", *dirfile, field_code,
      *field_code_l, *poly_ord, in_field, *in_field_l, *a0, *a1, a2, a3, a4,
      a5);

  _GDF_CString(&inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      a[5] = *a5;
    case 4:
      a[4] = *a4;
    case 3:
      a[3] = *a3;
    case 2:
      a[2] = *a2;
    default:
      a[1] = *a1;
      a[0] = *a0;
  }

  gd_alter_polynom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), po, inf, a);
  free(fc);
  free(inf);
  dreturnvoid();
}

/* gd_alter_cpolynom wrapper */
void F77_FUNC(gdalcp, GDALCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5))
{
  char *fc, *inf;
#ifdef GD_NO_C99_API
  double ca[12] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
#else
  double complex ca[6] = {0, 0, 0, 0, 0, 0};
#endif
  const int po = (*poly_ord > 5) ? 5 : *poly_ord;

  dtrace("%i, %p, %i, %i, %p, %i, %p, %p, %p, %p, %p, %p", *dirfile, field_code,
      *field_code_l, *poly_ord, in_field, *in_field_l, a0, a1, a2, a3, a4, a5);

  _GDF_CString(&inf, in_field, *in_field_l);

  switch (po) {
    case 5:
      gd_cp2ca_(ca, 5, a5);
    case 4:
      gd_cp2ca_(ca, 4, a4);
    case 3:
      gd_cp2ca_(ca, 3, a3);
    case 2:
      gd_cp2ca_(ca, 2, a2);
    default:
      gd_cp2ca_(ca, 1, a1);
      gd_cp2ca_(ca, 0, a0);
  }

  gd_alter_cpolynom(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), po, inf, ca);
  free(fc);
  free(inf);
  dreturnvoid();
}

/* gd_alter_multiply wrapper */
void F77_FUNC(gdalmt, GDALMT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l);

  gd_alter_multiply(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l));

  free(fc);
  free(in1);
  free(in2);
  dreturnvoid();
}

/* gd_alter_divide wrapper */
void F77_FUNC(gdaldv, GDALDV) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l);

  gd_alter_divide(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l));

  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_alter_indir wrapper */
void F77_FUNC(gdalid, GDALID) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l);

  gd_alter_indir(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l));

  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_alter_sindir wrapper */
void F77_FUNC(gdalsd, GDALSD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l)
{
  char *fc, *in1, *in2;

  dtrace("%i, %p, %i, %p, %i, %p, %i", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, in_field2, *in_field2_l);

  gd_alter_sindir(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      _GDF_CString(&in2, in_field2, *in_field2_l));

  free(fc);
  free(in1);
  free(in2);

  dreturnvoid();
}

/* gd_alter_recip wrapper */
void F77_FUNC(gdalrc, GDALRC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const double *dividend)
{
  char *fc, *in1;

  dtrace("%i, %p, %i, %p, %i, %g", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, *dividend);

  gd_alter_recip(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      *dividend);

  free(fc);
  free(in1);

  dreturnvoid();
}

void F77_FUNC(gdalcr, GDALCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(cdividend))
{
  char *fc, *in1;

  dtrace("%i, %p, %i, %p, %i, %g;%g", *dirfile, field_code, *field_code_l,
      in_field1, *in_field1_l, crealp(cdividend), cimagp(cdividend));

#ifdef GD_NO_C99_API
  gd_alter_crecip89(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      cdividend);
#else
  gd_alter_crecip(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in1, in_field1, *in_field1_l),
      *cdividend);
#endif

  free(fc);
  free(in1);

  dreturnvoid();
}

/* gd_alter_phase wrapper */
void F77_FUNC(gdalph, GDALPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift)
{
  char *fc, *in;

  dtrace("%i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, *shift);

  gd_alter_phase(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l), *shift);

  free(fc);
  free(in);
  dreturnvoid();
}

/* gd_encoding wrapper */
void F77_FUNC(gdgenc, GDGENC) (int32_t *encoding, const int32_t *dirfile,
    const int32_t *fragment)
{
  *encoding = gd_encoding(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_endianness wrapper */
void F77_FUNC(gdgend, GDGEND) (int32_t *endianness, const int32_t *dirfile,
    const int32_t *fragment)
{
  *endianness = gd_endianness(_GDF_GetDirfile(*dirfile), *fragment);
}

/* dirfilename wrapper */
void F77_FUNC(gdname, GDNAME) (char *name, int32_t *name_l,
    const int32_t *dirfile)
{
  const char *dn = gd_dirfilename(_GDF_GetDirfile(*dirfile));
  _GDF_FString(name, name_l, dn);
}

/* gd_parent_fragment wrapper */
void F77_FUNC(gdpfrg, GDPFRG) (int32_t *parent, const int32_t *dirfile,
    const int32_t *fragment)
{
  *parent = gd_parent_fragment(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_alter_protection wrapper */
void F77_FUNC(gdaprt, GDAPRT) (const int32_t *dirfile,
    const int32_t *protection_level, const int32_t *fragment)
{
  gd_alter_protection(_GDF_GetDirfile(*dirfile), *protection_level, *fragment);
}

/* gd_protection wrapper */
void F77_FUNC(gdgprt, GDGPRT) (int32_t *protection_level,
    const int32_t *dirfile, const int32_t *fragment)
{
  *protection_level = gd_protection(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_raw_filename wrapper */
void F77_FUNC(gdrwfn, GDRWFN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *fc, *fn;

  dtrace("%p, %i, %i, %p, %i", name, *name_l, *dirfile, field_code,
      *field_code_l);

  fn = gd_raw_filename(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));

  _GDF_FString(name, name_l, fn);

  free(fc);
  free(fn);

  dreturn("%i", *name_l);
}

/* gd_reference wrapper */
void F77_FUNC(gdrefe, GDREFE) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *fc;
  const char *ref;

  dtrace("%p, %p, %i, %p, %i", name, name_l, *dirfile, field_code,
      *field_code_l);

  _GDF_CString(&fc, field_code, *field_code_l);

  /* F95 bindings pass an empty string instead of NULL */
  if (*field_code_l == 0) {
    free(fc);
    fc = NULL;
  }

  ref = gd_reference(_GDF_GetDirfile(*dirfile), fc);

  _GDF_FString(name, name_l, ref);

  free(fc);
  dreturn("%i", *name_l);
}

/* gd_alter_encoding wrapper */
void F77_FUNC(gdaenc, GDAENC) (const int32_t *dirfile, const int32_t *encoding,
    const int32_t *fragment, const int32_t *recode)
{
  gd_alter_encoding(_GDF_GetDirfile(*dirfile), *encoding, *fragment,
      *recode);
}

/* gd_alter_endianness wrapper */
void F77_FUNC(gdaend, GDAEND) (const int32_t *dirfile,
    const int32_t *endianness, const int32_t *fragment, const int32_t *recode)
{
  gd_alter_endianness(_GDF_GetDirfile(*dirfile), *endianness, *fragment,
      *recode);
}

/* gd_alter_linterp wrapper */
void F77_FUNC(gdallt, GDALLT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *table, const int32_t *table_l,
    const int32_t *recode)
{
  char *fc, *in, *tab;

  dtrace("%i, %p, %i, %p, %i, %p %i, %i", *dirfile, field_code, *field_code_l,
      in_field, *in_field_l, table, *table_l, *recode);

  gd_alter_linterp(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      _GDF_CString(&tab, table, *table_l), *recode);
  free(fc);
  free(in);
  free(tab);
  dreturnvoid();
}

/* gd_alter_raw wrapper */
void F77_FUNC(gdalrw, GDALRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const int32_t *recode)
{
  char *out;

  dtrace("%i, %p, %i, %i, %i, %i", *dirfile, field_code, *field_code_l,
      *data_type, *spf, *recode);

  gd_alter_raw(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
        *field_code_l), (gd_type_t)(*data_type), (unsigned int)*spf, *recode);
  free(out);
  dreturnvoid();
}

/* gd_alter_spec wrapper */
void F77_FUNC(gdalsp, GDALSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const int32_t *move)
{
  char *sp;

  dtrace("%i, %p, %i, %i", *dirfile, spec, *spec_l, *move);

  gd_alter_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(&sp, spec, *spec_l),
      *move);

  free(sp);
  dreturnvoid();
}

/* gd_delete wrapper */
void F77_FUNC(gddele, GDDELE) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *flags)
{
  char *fc;

  dtrace("%i, %p, %i, %i", *dirfile, field_code, *field_code_l, *flags);

  gd_delete(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), *flags);

  free(fc);
  dreturnvoid();
}

/* gd_malter_spec wrapper */
void F77_FUNC(gdmlsp, GDMLSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const char *parent, const int32_t *parent_l,
    const int32_t *move)
{
  char *sp, *pa;

  dtrace("%i, %p, %i, %p, %i, %i", *dirfile, spec, *spec_l, parent, *parent_l,
      *move);

  gd_malter_spec(_GDF_GetDirfile(*dirfile), _GDF_CString(&sp, spec,
        *spec_l), _GDF_CString(&pa, parent, *parent_l), *move);

  free(pa);
  free(sp);
  dreturnvoid();
}

/* gd_move wrapper */
void F77_FUNC(gdmove, GDMOVE) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *new_fragment,
    const int32_t *flags)
{
  char *fc;

  dtrace("%i, %p, %i, %i, %i", *dirfile, field_code, *field_code_l,
      *new_fragment, *flags);

  gd_move(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), *new_fragment, *flags);

  free(fc);
  dreturnvoid();
}

/* gd_rename wrapper */
void F77_FUNC(gdrenm, GDRENM) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *new_name,
    const int32_t *new_name_l, const int32_t *flags)
{
  char *fc, *nn;

  dtrace("%i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      new_name, *new_name_l, *flags);

  gd_rename(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&nn, new_name, *new_name_l), *flags);

  free(nn);
  free(fc);
  dreturnvoid();
}

/* gd_uninclude wrapper */
void F77_FUNC(gduinc, GDUINC) (const int32_t *dirfile, const int32_t *fragment,
    const int32_t *del)
{
  gd_uninclude(_GDF_GetDirfile(*dirfile), *fragment, *del);
}

/* gd_alter_frameoffset wrapper */
void F77_FUNC(gdafof, GDAFOF) (const int32_t *dirfile, const int32_t *offset,
    const int32_t *fragment, const int32_t *recode)
{
  gd_alter_frameoffset(_GDF_GetDirfile(*dirfile), *offset, *fragment,
      *recode);
}

/* gd_frameoffset wrapper */
void F77_FUNC(gdgfof, GDGFOF) (int32_t *offset, const int32_t *dirfile,
    const int32_t *fragment)
{
  *offset = gd_frameoffset(_GDF_GetDirfile(*dirfile), *fragment);
}

/* gd_native_type wrapper */
void F77_FUNC(gdntyp, GDNTYP) (int32_t *type, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;

  dtrace("%p, %i, %p, %i", type, *dirfile, field_code, *field_code_l);

  *type = gd_native_type(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc,
        field_code, *field_code_l));

  free(fc);
  dreturn("%i", *type);
}

/* returns the entry flags */
void F77_FUNC(gdenfl, GDENFL) (int32_t *flags, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;
  gd_entry_t E;
  DIRFILE *D;

  dtrace("%p, %i, %p, %i", flags, *dirfile, field_code, *field_code_l);

  D = _GDF_GetDirfile(*dirfile);

  if (gd_entry(D, _GDF_CString(&fc, field_code, *field_code_l), &E) == 0) {
    *flags = E.flags;

    gd_free_entry_strings(&E);
  } else
    *flags = -1;
  free(fc);

  dreturn("%i", *flags);
}

/* gd_validate wrapper */
void F77_FUNC(gdvldt, GDVLDT) (int32_t *valid, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;

  dtrace("%p, %i, %p, %i", valid, *dirfile, field_code, *field_code_l);

  *valid = gd_validate(_GDF_GetDirfile(*dirfile),
      _GDF_CString(&fc, field_code, *field_code_l));

  free(fc);
  dreturn("%i", *valid);
}

/* gd_framenum wrapper */
void F77_FUNC(gdfnum, GDFNUM) (double *framenum, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l, const double *value)
{
  char *fc;

  dtrace("%p, %i, %p. %i, %g", framenum, *dirfile, field_code, *field_code_l,
      *value);

  *framenum = gd_framenum(_GDF_GetDirfile(*dirfile),
      _GDF_CString(&fc, field_code, *field_code_l), *value);

  free(fc);
  dreturn("%g", *value);
}

/* gd_framenum_subset wrapper */
void F77_FUNC(gdfnss, GDFNSS) (double *framenum, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l, const double *value,
    const int32_t *start, const int32_t *end)
{
  char *fc;

  dtrace("%p, %i, %p, %i, %g, %i, %i", framenum, *dirfile, field_code,
      *field_code_l, *value, *start, *end);

  *framenum = gd_framenum_subset(_GDF_GetDirfile(*dirfile),
      _GDF_CString(&fc, field_code, *field_code_l), *value, *start, *end);

  free(fc);

  dreturn("%g", *framenum);
}

/* retrieve a scalar parameter */
void F77_FUNC(gdgsca, GDGSCA) (char *scalar, int32_t *scalar_l,
    int32_t *scalar_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *index)
{
  char *fc;
  int ok = 0;
  gd_entry_t E;
  DIRFILE *D;

  dtrace("%p, %p, %p, %i, %p, %i, %i", scalar, scalar_l, scalar_index,
      *dirfile, field_code, *field_code_l, *index);

  D = _GDF_GetDirfile(*dirfile);

  gd_entry(D, _GDF_CString(&fc, field_code, *field_code_l), &E);

  free(fc);

  if (!gd_error(D) && *index > 0) {
    ok = 1;
    switch (E.field_type) {
      case GD_NO_ENTRY:
      case GD_ALIAS_ENTRY:
      case GD_LINTERP_ENTRY:
      case GD_MULTIPLY_ENTRY:
      case GD_DIVIDE_ENTRY:
      case GD_INDIR_ENTRY:
      case GD_SINDIR_ENTRY:
      case GD_INDEX_ENTRY:
      case GD_CONST_ENTRY:
      case GD_CARRAY_ENTRY:
      case GD_SARRAY_ENTRY:
      case GD_STRING_ENTRY:
        ok = 0;
        break;
      case GD_LINCOM_ENTRY:
        if (*index > GD_MAX_LINCOM + E.EN(lincom,n_fields) ||
            (*index > E.EN(lincom,n_fields) && *index <= GD_MAX_LINCOM))
        {
          ok = 0;
        }
        break;
      case GD_POLYNOM_ENTRY:
        if (*index > E.EN(polynom,poly_ord) + 1)
          ok = 0;
        break;
      case GD_BIT_ENTRY:
      case GD_SBIT_ENTRY:
      case GD_MPLEX_ENTRY:
        if (*index > 2)
          ok = 0;
        break;
      case GD_RECIP_ENTRY:
      case GD_RAW_ENTRY:
      case GD_PHASE_ENTRY:
      case GD_WINDOW_ENTRY:
        if (*index > 1)
          ok = 0;
        break;
    }
  }

  if (ok && E.scalar[*index - 1] == NULL)
    ok = 0;

  _GDF_FString(scalar, scalar_l, (ok) ? E.scalar[*index - 1] : "");
  GDF_SCIND_C2F(*scalar_index, E.scalar_ind[*index - 1]);

  gd_free_entry_strings(&E);

  dreturnvoid();
}

/* set a scalar parameter */
void F77_FUNC(gdasca, GDASCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *index, const char *scalar,
    const int32_t *scalar_l, int32_t *scalar_index, int32_t *recode)
{
  int ok = 1;
  char *fc;
  gd_entry_t E;
  DIRFILE *D;

  dtrace("%i, %p, %i, %i, %p, %i, %i, %i", *dirfile, field_code, *field_code_l,
      *index, scalar, *scalar_l, *scalar_index, *recode);

  D = _GDF_GetDirfile(*dirfile);

  if (*index < 1) {
    dreturnvoid();
    return;
  }

  if (gd_entry(D, _GDF_CString(&fc, field_code, *field_code_l), &E) == -1) {
    dreturnvoid();
    return;
  }

  switch (E.field_type) {
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_SARRAY_ENTRY:
      ok = 0;
      break;
    case GD_LINCOM_ENTRY:
      if (*index > GD_MAX_LINCOM + E.EN(lincom,n_fields) ||
          (*index > E.EN(lincom,n_fields) && *index <= GD_MAX_LINCOM))
      {
        ok = 0;
      }
      break;
    case GD_POLYNOM_ENTRY:
      if (*index > E.EN(polynom,poly_ord) + 1)
        ok = 0;
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_MPLEX_ENTRY:
      if (*index > 2)
        ok = 0;
      break;
    case GD_RAW_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_WINDOW_ENTRY:
      if (*index > 1)
        ok = 0;
      break;
  }

  if (!ok || gd_error(D)) {
    dreturnvoid();
    return;
  }

  free(E.scalar[*index - 1]);
  _GDF_CString(E.scalar + *index - 1, scalar, *scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[*index - 1], *scalar_index);

  gd_alter_entry(D, fc, &E, *recode);

  gd_free_entry_strings(&E);
  free(fc);
  dreturnvoid();
}

/* gd_invalid_dirfile wrapper */
void F77_FUNC(gdinvd, GDINVD) (int32_t *dirfile)
{
  dtrace("%p", dirfile);

  *dirfile = _GDF_SetDirfile(gd_invalid_dirfile(), 1);

  dreturn("%i", *dirfile);
}

/* gd_dirfile_standards wrapper */
void F77_FUNC(gdstdv, GDSTDV) (int32_t *vers, const int32_t *dirfile)
{
  dtrace("%p, %i", vers, *dirfile);

  *vers = gd_dirfile_standards(_GDF_GetDirfile(*dirfile), *vers);

  dreturn("%i", *vers);
}

/* gd_seek wrapper */
void F77_FUNC(gdseek, GDSEEK) (int32_t *pos, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l,
    const int32_t *frame_num, const int32_t *sample_num, const int32_t *flags)
{
  char *fc;
  dtrace("%p, %i, %p, %i, %i, %i, 0x%X", pos, *dirfile, field_code,
      *field_code_l, *frame_num, *sample_num, *flags);

  *pos = (int)gd_seek(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), *frame_num, *sample_num, *flags);

  free(fc);
  dreturn("%i", *pos);
}

/* gd_tell wrapper */
void F77_FUNC(gdtell, GDTELL) (int32_t *pos, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;
  dtrace("%p, %i, %p, %i", pos, *dirfile, field_code, *field_code_l);

  *pos = (int)gd_tell(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));

  free(fc);
  dreturn("%i", *pos);
}

/* gd_constants wrapper -- this only returns one value */
void F77_FUNC(gdcons, GDCONS) (void *value, const int32_t *dirfile,
    const int32_t *return_type, const int32_t *field_num)
{
  const void *v;
  DIRFILE *D;
  unsigned int nfields;

  dtrace("%p, %i, 0x%X, %i", value, *dirfile, *return_type, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  nfields = gd_nfields_by_type(D, GD_CONST_ENTRY);

  if (!gd_error(D) && (*field_num > 0) && (*field_num <= (int)nfields)) {
    v = gd_constants(D, (gd_type_t)*return_type);
    if (!gd_error(D))
      memcpy(value, (char*)v + (*field_num - 1) * GD_SIZE(*return_type),
          GD_SIZE(*return_type));
  }
  dreturnvoid();
}

/* gd_mconstants wrapper -- this only returns one value */
void F77_FUNC(gdmcos, GDMCOS) (void *value, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *return_type,
    const int32_t *field_num)
{
  const void *v;
  DIRFILE *D;
  char *pa;
  unsigned int nfields;

  dtrace("%p, %i, %p, %i, 0x%X, %i", value, *dirfile, parent, *parent_l,
      *return_type, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&pa, parent, *parent_l);
  nfields = gd_nmfields_by_type(D, pa, GD_CONST_ENTRY);

  if (!gd_error(D) && (*field_num > 0) && (*field_num <= (int)nfields)) {
    v = gd_mconstants(D, pa, (gd_type_t)*return_type);
    if (!gd_error(D))
      memcpy(value, (char*)v + (*field_num - 1) * GD_SIZE(*return_type),
          GD_SIZE(*return_type));
  }

  free(pa);
  dreturnvoid();
}

/* gd_strings wrapper -- this only returns one value */
void F77_FUNC(gdstrs, GDSTRS) (char *value, int32_t *value_l,
    const int32_t *dirfile, const int32_t *field_num)
{
  const char **v;
  DIRFILE *D;
  unsigned int nfields;

  dtrace("%p, %i, %i, %i", value, *value_l, *dirfile, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  nfields = gd_nfields_by_type(D, GD_STRING_ENTRY);

  if (!gd_error(D) && (*field_num > 0) && (*field_num <= (int)nfields)) {
    v = gd_strings(D);
    _GDF_FString(value, value_l, gd_error(D) ? "" : v[*field_num - 1]);
  } else
    *value_l = 0;

  dreturn("%i", *value_l);
}

/* gd_mstrings wrapper -- this only returns one value */
void F77_FUNC(gdmsts, GDMSTS) (void *value, int32_t *value_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *field_num)
{
  const char **v;
  char *pa;
  DIRFILE *D;
  unsigned int nfields;

  dtrace("%p, %i, %i, %p, %i, %i", value, *value_l, *dirfile, parent,
      *parent_l, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&pa, parent, *parent_l);
  nfields = gd_nmfields_by_type(D, pa, GD_STRING_ENTRY);

  if (!gd_error(D) && (*field_num > 0) && (*field_num <= (int)nfields)) {
    v = gd_mstrings(D, pa);
    _GDF_FString((char*)value, value_l,
        gd_error(D) ? "" : ((char**)v)[*field_num - 1]);
  } else
    *value_l = 0;

  free(pa);
  dreturnvoid();
}

/* Return the maximum string value length */
void F77_FUNC(gdstrx, GDSTRX) (int32_t *max, const int32_t *dirfile)
{
  const char **v;
  size_t len = 0;
  DIRFILE *D;
  unsigned int i, nfields;

  dtrace("%p, %i", max, *dirfile);

  D = _GDF_GetDirfile(*dirfile);
  nfields = gd_nfields_by_type(D, GD_STRING_ENTRY);

  if (!gd_error(D)) {
    v = gd_strings(D);

    for (i = 0; i < nfields; ++i)
      if (strlen(v[i]) > len)
        len = strlen(v[i]);
  }

  *max = (int)len;

  dreturn("%i", *max);
}

/* Return the maximum meta string value length */
void F77_FUNC(gdmstx, GDMSTX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l)
{
  const char **v;
  size_t len = 0;
  DIRFILE *D;
  char *pa;
  unsigned int i, nfields;

  dtrace("%p, %i, %p, %i", max, *dirfile, parent, *parent_l);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&pa, parent, *parent_l);
  nfields = gd_nmfields_by_type(D, pa, GD_STRING_ENTRY);

  if (!gd_error(D)) {
    v = gd_mstrings(D, pa);

    for (i = 0; i < nfields; ++i)
      if (strlen(v[i]) > len)
        len = strlen(v[i]);
  }

  *max = (int)len;

  free(pa);

  dreturn("%i", *max);
}

/* gd_add_alias wrapper */
void F77_FUNC(gdadal, GDADAL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *target, const int32_t *target_l,
    const int32_t *fragment_index)
{
  char *tg, *fc;

  dtrace("%i, %p, %i, %p, %i, %i", *dirfile, field_code, *field_code_l,
      target, *target_l, *fragment_index);

  gd_add_alias(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&tg, target, *target_l), *fragment_index);

  free(fc);
  free(tg);

  dreturnvoid();
}

/* gd_add_window wrapper */
void F77_FUNC(gdadwd, GDADWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold,
    const int32_t *fragment_index)
{
  char *in, *cf, *fc;
  gd_triplet_t t;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %p, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, check_field, *check_field_l,
      *windop, threshold, *fragment_index);

  t = _GDF_SetTriplet((gd_windop_t)*windop, threshold);

  gd_add_window(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      _GDF_CString(&cf, check_field, *check_field_l), (gd_windop_t)*windop, t,
      *fragment_index);

  free(fc);
  free(cf);
  free(in);

  dreturnvoid();
}

/* gd_madd_window wrapper */
void F77_FUNC(gdmdwd, GDMDWD) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold)
{
  char *in, *cf, *fc, *pa;
  gd_triplet_t t;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %p, %i, %i, %p", *dirfile, parent,
      *parent_l, field_code, *field_code_l, in_field, *in_field_l, check_field,
      *check_field_l, *windop, threshold);

  t = _GDF_SetTriplet((gd_windop_t)*windop, threshold);

  gd_madd_window(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), _GDF_CString(&fc, field_code, *field_code_l),
      _GDF_CString(&in, in_field, *in_field_l), _GDF_CString(&cf, check_field,
        *check_field_l), (gd_windop_t)*windop, t);

  free(pa);
  free(fc);
  free(cf);
  free(in);

  dreturnvoid();
}

/* gd_add_mplex wrapper */
void F77_FUNC(gdadmx, GDADMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const int32_t *period,
    const int32_t *fragment_index)
{
  char *in, *cf, *fc;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, count_field, *count_field_l,
      *val, *period, *fragment_index);

  gd_add_mplex(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      _GDF_CString(&cf, count_field, *count_field_l), *val, *period,
      *fragment_index);

  free(fc);
  free(cf);
  free(in);

  dreturnvoid();
}

/* gd_madd_mplex wrapper */
void F77_FUNC(gdmdmx, GDMDMX) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const int32_t *period)
{
  char *in, *cf, *fc, *pa;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %p, %i, %i, %i", *dirfile, parent,
      *parent_l, field_code, *field_code_l, in_field, *in_field_l, count_field,
      *count_field_l, *val, *period);

  gd_madd_mplex(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent, *parent_l),
      _GDF_CString(&fc, field_code, *field_code_l), _GDF_CString(&in, in_field,
        *in_field_l), _GDF_CString(&cf, count_field, *count_field_l), *val,
      *period);

  free(pa);
  free(fc);
  free(cf);
  free(in);

  dreturnvoid();
}

/* gd_alias_target */
void F77_FUNC(gdatrg, GDATRG) (char *target, int32_t *target_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  const char *targ;
  char *fc;

  dtrace("%p, %i, %i, %p, %i", target, *target_l, *dirfile, field_code,
      *field_code_l);

  targ = gd_alias_target(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc,
        field_code, *field_code_l));

  _GDF_FString(target, target_l, targ);

  free(fc);

  dreturn("%i", *target_l);
}

/* Return the maximum alias length */
void F77_FUNC(gdalsx, GDALSX) (int32_t *max, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  const char **al;
  size_t len = 0;
  char *fc;
  unsigned int i, nalias;
  DIRFILE *D;

  dtrace("%p, %i, %p, %i", max, *dirfile, field_code, *field_code_l);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&fc, field_code, *field_code_l);

  nalias = gd_naliases(D, fc);

  if (!gd_error(D)) {
    al = gd_aliases(D, fc);

    for (i = 0; i < nalias; ++i)
      if (strlen(al[i]) > len)
        len = strlen(al[i]);
  }

  *max = len;
  free(fc);
  dreturn("%zu", len);
}

/* gd_naliases */
void F77_FUNC(gdnals, GDNALS) (int32_t *nalias, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;

  dtrace("%p, %i, %p, %i", nalias, *dirfile, field_code, *field_code_l);

  *nalias = gd_naliases(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));

  free(fc);
  dreturn("%i", *nalias);
}


/* gd_aliases -- this only returns one alias */
void F77_FUNC(gdalss, GDALSS) (char *alias, int32_t *alias_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    const int32_t *num)
{
  const char **al;
  char *fc;
  unsigned int nalias;
  DIRFILE *D;

  dtrace("%p, %i, %i, %p, %i, %i", alias, *alias_l, *dirfile, field_code,
      *field_code_l, *num);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&fc, field_code, *field_code_l);

  nalias = gd_naliases(D, fc);

  if (!gd_error(D) && *num > 0 && *num <= (int)nalias) {
    al = gd_aliases(D, fc);
    _GDF_FString(alias, alias_l, al[*num - 1]);
  } else 
    *alias_l = 0;

  free(fc);
  dreturn("%i", *alias_l);
}

/* gd_alter_affixes */
void F77_FUNC(gdaafx, GDAAFX) (const int32_t *dirfile, const int32_t *index,
    const char *prefix, const int32_t *prefix_l, const char *suffix,
    const int32_t *suffix_l)
{
  char *px, *sx;

  dtrace("%i, %i, %p, %i, %p, %i", *dirfile, *index, prefix, *prefix_l,
      suffix, *suffix_l);

  gd_alter_affixes(_GDF_GetDirfile(*dirfile), *index, _GDF_CString(&px, prefix,
        *prefix_l), _GDF_CString(&sx, suffix, *suffix_l));

  free(sx);
  free(px);

  dreturnvoid();
}

/* gd_alter_window */
void F77_FUNC(gdalwd, GDALWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold)
{
  char *fc, *in, *cf;
  gd_triplet_t t;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %p", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, check_field, *check_field_l,
      *windop, threshold);

  t = _GDF_SetTriplet((gd_windop_t)*windop, threshold);

  gd_alter_window(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      _GDF_CString(&cf, check_field, *check_field_l), (gd_windop_t)*windop, t);

  free(cf);
  free(in);
  free(fc);
  dreturnvoid();
}

/* gd_alter_mplex */
void F77_FUNC(gdalmx, GDALMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const int32_t *period)
{
  char *fc, *in, *cf;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, count_field, *count_field_l,
      *val, *period);

  gd_alter_mplex(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), _GDF_CString(&in, in_field, *in_field_l),
      _GDF_CString(&cf, count_field, *count_field_l), *val, *period);

  free(cf);
  free(in);
  free(fc);
  dreturnvoid();
}

/* gd_fragment_affixes */
void F77_FUNC(gdfraf, GDFRAF) (char *prefix, int32_t *prefix_l, char *suffix,
    int32_t *suffix_l, const int32_t *dirfile, const int32_t *index)
{
  char *px, *sx;

  dtrace("%p, %i, %p, %i, %i, %i", prefix, *prefix_l, suffix, *suffix_l,
      *dirfile, *index);

  if (!gd_fragment_affixes(_GDF_GetDirfile(*dirfile), *index, &px, &sx)) {
    _GDF_FString(prefix, prefix_l, px);
    _GDF_FString(suffix, suffix_l, sx);
    free(px);
    free(sx);
  } else
    *prefix_l = *suffix_l = 0;

  dreturnvoid();
}

/* gd_fragment_namespace */
void F77_FUNC(gdfrns, GDFRNS) (char *nsout, int32_t *nsout_l,
    const int32_t *dirfile, const int32_t *index, const char *nsin,
    const int32_t *nsin_l)
{
  const char *out; 
  char *in;

  dtrace("%p, %i, %i, %i, %p, %i", nsout, *nsout_l, *dirfile, *index, nsin,
      *nsin_l);

  out = gd_fragment_namespace(_GDF_GetDirfile(*dirfile), *index,
      _GDF_CString(&in, nsin, *nsin_l));
  free(in);

  if (out)
    _GDF_FString(nsout, nsout_l, out);
  else
    *nsout_l = 0;

  dreturnvoid();
}

/* gd_hidden */
void F77_FUNC(gdhidn, GDHIDN) (int32_t *result, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;

  dtrace("%p, %i, %p, %i", result, *dirfile, field_code, *field_code_l);

  *result = gd_hidden(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));

  free(fc);
  dreturn("%i", *result);
}

/* gd_hide */
void F77_FUNC(gdhide, GDHIDE) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *fc;

  dtrace("%i, %p, %i", *dirfile, field_code, *field_code_l);

  gd_hide(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));

  free(fc);
  dreturnvoid();
}

/* gd_unhide */
void F77_FUNC(gduhid, GDUHID) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  char *fc;

  dtrace("%i, %p, %i", *dirfile, field_code, *field_code_l);

  gd_unhide(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));

  free(fc);
  dreturnvoid();
}

/* gd_madd_alias wrapper */
void F77_FUNC(gdmdal, GDMDAL) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *target, const int32_t *target_l)
{
  char *pa, *tg, *fc;

  dtrace("%i, %p, %i, %p, %i, %p, %i", *dirfile, parent, *parent_l, field_code,
      *field_code_l, target, *target_l);

  gd_madd_alias(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent, *parent_l),
      _GDF_CString(&fc, field_code, *field_code_l), _GDF_CString(&tg, target,
        *target_l));

  free(fc);
  free(tg);
  free(pa);

  dreturnvoid();
}

/* gd_include_affix wrapper */
void F77_FUNC(gdinca, GDINCA) (const int32_t *dirfile, const char *file,
    const int32_t *file_l, const int32_t *fragment_index, const char *prefix,
    const int32_t *prefix_l, const char *suffix, const int32_t *suffix_l,
    const int32_t *flags)
{
  char *fi, *px, *sx;

  dtrace("%i, %p, %i, %i, %p, %i, %p, %i, %i", *dirfile, file, *file_l,
      *fragment_index, prefix, *prefix_l, suffix, *suffix_l, *flags);

  gd_include_affix(_GDF_GetDirfile(*dirfile), _GDF_CString(&fi, file, *file_l),
      *fragment_index, _GDF_CString(&px, prefix, *prefix_l), _GDF_CString(&sx,
        suffix, *suffix_l), *flags);

  free(sx);
  free(px);
  free(fi);
  dreturnvoid();
}

/* gd_include_ns wrapper */
void F77_FUNC(gdincn, GDINCN) (const int32_t *dirfile, const char *file,
    const int32_t *file_l, const int32_t *fragment_index, const char *nsin,
    const int32_t *ns_l, const int32_t *flags)
{
  char *fi, *ns;

  dtrace("%i, %p, %i, %i, %p, %i, %i", *dirfile, file, *file_l,
      *fragment_index, nsin, *ns_l, *flags);

  gd_include_ns(_GDF_GetDirfile(*dirfile), _GDF_CString(&fi, file, *file_l),
      *fragment_index, _GDF_CString(&ns, nsin, *ns_l), *flags);

  free(ns);
  free(fi);
  dreturnvoid();
}

/* gd_sync wrapper */
void F77_FUNC(gdsync, GDSYNC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  dtrace("%i, %p, %i", *dirfile, field_code, *field_code_l);

  if (*field_code_l == 0)
    gd_sync(_GDF_GetDirfile(*dirfile), NULL);
  else {
    char *out;
    gd_sync(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l));
    free(out);
  }

  dreturnvoid();
}

/* gd_raw_close wrapper */
void F77_FUNC(gdrclo, GDRCLO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l)
{
  dtrace("%i, %p, %i", *dirfile, field_code, *field_code_l);

  if (*field_code_l == 0)
    gd_sync(_GDF_GetDirfile(*dirfile), NULL);
  else {
    char *out;
    gd_raw_close(_GDF_GetDirfile(*dirfile), _GDF_CString(&out, field_code,
          *field_code_l));
    free(out);
  }

  dreturnvoid();
}

/* gd_strtok wrapper */
void F77_FUNC(gdtoke, GDTOKE) (char *toke, int32_t *toke_l,
    const int32_t *dirfile, const char *string, const int32_t *string_l)
{
  char *token, *st = NULL;
  DIRFILE *D;

  dtrace("%p, %p, %i, %p, %i", toke, toke_l, *dirfile, string, *string_l);

  D = _GDF_GetDirfile(*dirfile);
  if (*string_l > 0)
    _GDF_CString(&st, string, *string_l);

  token = gd_strtok(D, st);
  free(st);

  _GDF_FString(toke, toke_l, token);
  free(token);

  dreturn("%i", *toke_l);
}

/* gd_desync wrapper */
void F77_FUNC(gddsyn, GDDSYN) (int32_t *desync, const int32_t *dirfile,
    const int32_t *flags)
{
  dtrace("%p, %i, %i", desync, *dirfile, *flags);

  *desync = gd_desync(_GDF_GetDirfile(*dirfile), *flags);

  dreturn("%i", *desync);
}

/* gd_flags wrapper */
void F77_FUNC(gdflag, GDFLAG) (int32_t *flags, const int32_t *dirfile,
    const int32_t *set,
    const int32_t *reset)
{
  dtrace("%p, %i, 0x%X, 0x%X", flags, *dirfile, *set, *reset);

  *flags = gd_flags(_GDF_GetDirfile(*dirfile), *set, *reset);

  dreturn("%i", *flags);
}

/* gd_verbose_prefix wrapper */
void F77_FUNC(gdvbpx, GDVBPX) (const int32_t *dirfile, const char *prefix,
    const int32_t *prefix_l)
{
  char *px;

  dtrace("%i, %p, %i", *dirfile, prefix, *prefix_l);

  gd_verbose_prefix(_GDF_GetDirfile(*dirfile), _GDF_CString(&px, prefix,
        *prefix_l));
  free(px);

  dreturnvoid();
}

/* gd_mplex_lookback wrapper */
void F77_FUNC(gdmxlb, GDMXLB) (const int32_t *dirfile, const int32_t *lookback)
{
  dtrace("%i, %i", *dirfile, *lookback);

  gd_mplex_lookback(_GDF_GetDirfile(*dirfile), *lookback);

  dreturnvoid();
}

/* gd_nentries wrapper */
void F77_FUNC(gdnent, GDNENT) (int32_t *nentries, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *type,
    const int32_t *flags)
{
  char *pa;

  dtrace("%p, %i, %p, %i, 0x%X, 0x%X", nentries, *dirfile, parent, *parent_l,
      *type, *flags);

  *nentries = gd_nentries(_GDF_GetDirfile(*dirfile), _GDF_CString(&pa, parent,
        *parent_l), (unsigned int)*type, (unsigned int)*flags);

  free(pa);
  dreturn("%i", *nentries);
}

/* Return the maximum field name length */
void F77_FUNC(gdentx, GDENTX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *type,
    const int32_t *flags)
{
  const char **el;
  char *pa;
  size_t len = 0;
  DIRFILE* D;
  unsigned int i, nentries;
  const unsigned int utype = (unsigned int)*type;
  const unsigned int uflags = (unsigned int)*flags;

  dtrace("%p, %i, %p, %i, 0x%X, 0x%X", max, *dirfile, parent, *parent_l, utype,
      uflags);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&pa, parent, *parent_l);
  nentries = gd_nentries(D, pa, utype, uflags);

  if (!gd_error(D)) {
    el = gd_entry_list(D, pa, utype, uflags);

    for (i = 0; i < nentries; ++i)
      if (strlen(el[i]) > len)
        len = strlen(el[i]);
  }

  *max = len;
  free(pa);
  dreturn("%i", *max);
}

/* gd_entry_list wrapper -- this only returns one entry name */
void F77_FUNC(gdentn, GDENTN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *type, const int32_t *flags, const int32_t *field_num)
{
  const char** el;
  char *pa;
  DIRFILE* D;
  unsigned int nentries;
  const unsigned int utype = (unsigned int)*type;
  const unsigned int uflags = (unsigned int)*flags;

  dtrace("%p, %p, %i, %p, %i, 0x%X, 0x%X, %i", name, name_l, *dirfile,
      parent, *parent_l, utype, uflags, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&pa, parent, *parent_l);
  nentries = gd_nentries(D, pa, utype, uflags);

  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nentries) {
    el = gd_entry_list(D, pa, utype, uflags);
    _GDF_FString(name, name_l, el[*field_num - 1]);
  } else
    *name_l = 0;

  free(pa);
  dreturn("%i", *name_l);
}

/* gd_linterp_tablename wrapper */
void F77_FUNC(gdlttn, GDLTTN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l)
{
  char *fc, *fn;

  dtrace("%p, %i, %i, %p, %i", name, *name_l, *dirfile, field_code,
      *field_code_l);

  fn = gd_linterp_tablename(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc,
        field_code, *field_code_l));

  _GDF_FString(name, name_l, fn);

  free(fc);
  free(fn);

  dreturn("%i", *name_l);
}

/* gd_add_raw with scalar parameters */
void F77_FUNC(gdasrw, GDASRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const char *spf_scalar, const int32_t *spf_scalar_l,
    const int32_t *spf_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;

  dtrace("%i, %p, %i, %i, %i, %p, %i, %i, %i", *dirfile, field_code,
      *field_code_l, *data_type, *spf, spf_scalar, *spf_scalar_l,
      *spf_scalar_ind, *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = *fragment_index;
  E.EN(raw,data_type) = *data_type;
  E.EN(raw,spf) = *spf;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(E.scalar, spf_scalar, *spf_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *spf_scalar_ind);

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.scalar[0]);

  dreturnvoid();
}

void F77_FUNC(gdaslc, GDASLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind, const double *b1,
    const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const double *m2, const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind, const double *b2,
    const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const double *m3, const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind, const double *b3,
    const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;
  int i, n;

  dtrace("%i, %p, %i, %i, %p, %i, %g, %p, %i, %i, %g, %p, %i, %i, %p, %i, "
      "%g, %p, %i, %i, %g, %p, %i, %i, %p, %i, %g, %p, %i, %i, %g, %p, %i, "
      "%i, %i", *dirfile, field_code, *field_code_l, *n_fields, in_field1,
      *in_field1_l, *m1, m1_scalar, *m1_scalar_l, *m1_scalar_ind, *b1,
      b1_scalar, *b1_scalar_l, *b1_scalar_ind, in_field2, *in_field2_l, *m2,
      m2_scalar, *m2_scalar_l, *m2_scalar_ind, *b2, b2_scalar, *b2_scalar_l,
      *b2_scalar_ind, in_field3, *in_field3_l, *m3, m3_scalar, *m3_scalar_l,
      *m3_scalar_ind, *b3, b3_scalar, *b3_scalar_l, *b3_scalar_ind,
      *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_LINCOM_ENTRY;
  E.fragment_index = *fragment_index;
  n = E.EN(lincom,n_fields) = *n_fields;
  _GDF_CString(&E.field, field_code, *field_code_l);

  _GDF_CString(E.in_fields + 0, in_field1, *in_field1_l);
  _GDF_CString(E.scalar + 0, m1_scalar, *m1_scalar_l);
  _GDF_CString(E.scalar + 0 + GD_MAX_LINCOM, b1_scalar, *b1_scalar_l);

  GDF_SCIND_F2C(E.scalar_ind[0], *m1_scalar_ind);
  GDF_SCIND_F2C(E.scalar_ind[0 + GD_MAX_LINCOM], *b1_scalar_ind);
  E.EN(lincom,m[0]) = *m1;
  E.EN(lincom,b[0]) = *b1;

  if (n > 1) {
    _GDF_CString(E.in_fields + 1, in_field2, *in_field2_l);
    _GDF_CString(E.scalar + 1, m2_scalar, *m2_scalar_l);
    _GDF_CString(E.scalar + 1 + GD_MAX_LINCOM, b2_scalar, *b2_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[1], *m2_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[1 + GD_MAX_LINCOM], *b2_scalar_ind);
    E.EN(lincom,m[1]) = *m2;
    E.EN(lincom,b[1]) = *b2;
  }

  if (n > 2) {
    _GDF_CString(E.in_fields + 2, in_field3, *in_field3_l);
    _GDF_CString(E.scalar + 2, m3_scalar, *m3_scalar_l);
    _GDF_CString(E.scalar + 2 + GD_MAX_LINCOM, b3_scalar, *b3_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[2], *m3_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[2 + GD_MAX_LINCOM], *b3_scalar_ind);
    E.EN(lincom,m[2]) = *m3;
    E.EN(lincom,b[2]) = *b3;
  }

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  for (i = 0; i < n; ++i) {
    free(E.in_fields[i]);
    free(E.scalar[i]);
    free(E.scalar[i + GD_MAX_LINCOM]);
  }

  dreturnvoid();
}

void F77_FUNC(gdascl, GDASCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind,
    const GD_DCOMPLEXP(b1), const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const GD_DCOMPLEXP(m2), const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind,
    const GD_DCOMPLEXP(b2), const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const GD_DCOMPLEXP(m3), const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind,
    const GD_DCOMPLEXP(b3), const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;
  int i, n;

  dtrace("%i, %p, %i, %i, %p, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %p, "
      "%i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %p, %i, %g;%g, %p, %i, %i, "
      "%g;%g, %p, %i, %i, %i", *dirfile, field_code, *field_code_l, *n_fields,
      in_field1, *in_field1_l, crealp(m1), cimagp(m1), m1_scalar, *m1_scalar_l,
      *m1_scalar_ind, crealp(b1), cimagp(b1), b1_scalar, *b1_scalar_l,
      *b1_scalar_ind, in_field2, *in_field2_l, crealp(m2), cimagp(m2),
      m2_scalar, *m2_scalar_l, *m2_scalar_ind, crealp(b2), cimagp(b2),
      b2_scalar, *b2_scalar_l, *b2_scalar_ind, in_field3, *in_field3_l,
      crealp(m3), cimagp(m3), m3_scalar, *m3_scalar_l, *m3_scalar_ind,
      crealp(b3), cimagp(b3), b3_scalar, *b3_scalar_l, *b3_scalar_ind,
      *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_LINCOM_ENTRY;
  E.fragment_index = *fragment_index;
  n = E.EN(lincom,n_fields) = *n_fields;
  _GDF_CString(&E.field, field_code, *field_code_l);
  E.flags = GD_EN_COMPSCAL;

  _GDF_CString(E.in_fields + 0, in_field1, *in_field1_l);
  _GDF_CString(E.scalar + 0, m1_scalar, *m1_scalar_l);
  _GDF_CString(E.scalar + 0 + GD_MAX_LINCOM, b1_scalar, *b1_scalar_l);

  GDF_SCIND_F2C(E.scalar_ind[0], *m1_scalar_ind);
  GDF_SCIND_F2C(E.scalar_ind[0 + GD_MAX_LINCOM], *b1_scalar_ind);
  gd_cp2cs_(E.EN(lincom,cm)[0], m1);
  gd_cp2cs_(E.EN(lincom,cb)[0], b1);

  if (n > 1) {
    _GDF_CString(E.in_fields + 1, in_field2, *in_field2_l);
    _GDF_CString(E.scalar + 1, m2_scalar, *m2_scalar_l);
    _GDF_CString(E.scalar + 1 + GD_MAX_LINCOM, b2_scalar, *b2_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[1], *m2_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[1 + GD_MAX_LINCOM], *b2_scalar_ind);
    gd_cp2cs_(E.EN(lincom,cm)[1], m2);
    gd_cp2cs_(E.EN(lincom,cb)[1], b2);
  }

  if (n > 2) {
    _GDF_CString(E.in_fields + 2, in_field3, *in_field3_l);
    _GDF_CString(E.scalar + 2, m3_scalar, *m3_scalar_l);
    _GDF_CString(E.scalar + 2 + GD_MAX_LINCOM, b3_scalar, *b3_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[2], *m3_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[2 + GD_MAX_LINCOM], *b3_scalar_ind);
    gd_cp2cs_(E.EN(lincom,cm)[2], m3);
    gd_cp2cs_(E.EN(lincom,cb)[2], b3);
  }

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  for (i = 0; i < n; ++i) {
    free(E.in_fields[i]);
    free(E.scalar[i]);
    free(E.scalar[i + GD_MAX_LINCOM]);
  }

  dreturnvoid();
}

void F77_FUNC(gdaspn, GDASPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind, const double *a1,
    const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const double *a2, const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind, const double *a3,
    const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const double *a4, const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind, const double *a5,
    const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;
  int i, n;

  dtrace("%i, %p, %i, %i, %p, %i, %g, %p, %i, %i, %g, %p, %i, %i, %g, %p, %i, "
      "%i, %g, %p, %i, %i, %g, %p, %i, %i, %g, %p, %i, %i, %i",
      *dirfile, field_code, *field_code_l, *poly_ord, in_field, *in_field_l,
      *a0, a0_scalar, *a0_scalar_l, *a0_scalar_ind, *a1, a1_scalar,
      *a1_scalar_l, *a1_scalar_ind, *a2, a2_scalar, *a2_scalar_l,
      *a2_scalar_ind, *a3, a3_scalar, *a3_scalar_l, *a3_scalar_ind, *a4,
      a4_scalar, *a4_scalar_l, *a4_scalar_ind, *a5, a5_scalar, *a5_scalar_l,
      *a5_scalar_ind, *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_POLYNOM_ENTRY;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  E.fragment_index = *fragment_index;
  n = E.EN(polynom,poly_ord) = *poly_ord;

  if (n > 5)
    n = 5;
  else if (n < 1)
    n = 1;

  switch (n) {
    case 5:
      _GDF_CString(E.scalar + 5, a5_scalar, *a5_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[5], *a5_scalar_ind);
      E.EN(polynom,a[5]) = *a5;
      /* fallthrough */
    case 4:
      _GDF_CString(E.scalar + 4, a4_scalar, *a4_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[4], *a4_scalar_ind);
      E.EN(polynom,a[4]) = *a4;
      /* fallthrough */
    case 3:
      _GDF_CString(E.scalar + 3, a3_scalar, *a3_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[3], *a3_scalar_ind);
      E.EN(polynom,a[3]) = *a3;
      /* fallthrough */
    case 2:
      _GDF_CString(E.scalar + 2, a2_scalar, *a2_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[2], *a2_scalar_ind);
      E.EN(polynom,a[2]) = *a2;
      /* fallthrough */
    default:
      _GDF_CString(E.scalar + 1, a1_scalar, *a1_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[1], *a1_scalar_ind);
      E.EN(polynom,a[1]) = *a1;
      _GDF_CString(E.scalar + 0, a0_scalar, *a0_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[0], *a0_scalar_ind);
      E.EN(polynom,a[0]) = *a0;
  }

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  for (i = 0; i <= n; ++i)
    free(E.scalar[i]);

  dreturnvoid();
}

void F77_FUNC(gdascp, GDASCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind,
    const GD_DCOMPLEXP(a1), const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const GD_DCOMPLEXP(a2), const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind,
    const GD_DCOMPLEXP(a3), const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const GD_DCOMPLEXP(a4), const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind,
    const GD_DCOMPLEXP(a5), const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;
  int i, n;

  dtrace("%i, %p, %i, %i, %p, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %g;%g, "
      "%p, %i, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %i",
      *dirfile, field_code, *field_code_l, *poly_ord, in_field, *in_field_l,
      crealp(a0), cimagp(a0), a0_scalar, *a0_scalar_l, *a0_scalar_ind,
      crealp(a1), cimagp(a1), a1_scalar, *a1_scalar_l, *a1_scalar_ind,
      crealp(a2), cimagp(a2), a2_scalar, *a2_scalar_l, *a2_scalar_ind,
      crealp(a3), cimagp(a3), a3_scalar, *a3_scalar_l, *a3_scalar_ind,
      crealp(a4), cimagp(a4), a4_scalar, *a4_scalar_l, *a4_scalar_ind,
      crealp(a5), cimagp(a5), a5_scalar, *a5_scalar_l, *a5_scalar_ind,
      *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_POLYNOM_ENTRY;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  E.fragment_index = *fragment_index;
  n = E.EN(polynom,poly_ord) = *poly_ord;
  E.flags = GD_EN_COMPSCAL;

  if (n > 5)
    n = 5;
  else if (n < 1)
    n = 1;

  switch (n) {
    case 5:
      _GDF_CString(E.scalar + 5, a5_scalar, *a5_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[5], *a5_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[5], a5);
      /* fallthrough */
    case 4:
      _GDF_CString(E.scalar + 4, a4_scalar, *a4_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[4], *a4_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[4], a4);
      /* fallthrough */
    case 3:
      _GDF_CString(E.scalar + 3, a3_scalar, *a3_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[3], *a3_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[3], a3);
      /* fallthrough */
    case 2:
      _GDF_CString(E.scalar + 2, a2_scalar, *a2_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[2], *a2_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[2], a2);
      /* fallthrough */
    default:
      _GDF_CString(E.scalar + 1, a1_scalar, *a1_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[1], *a1_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[1], a1);
      _GDF_CString(E.scalar + 0, a0_scalar, *a0_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[0], *a0_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[0], a0);
  }

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  for (i = 0; i <= n; ++i)
    free(E.scalar[i]);

  dreturnvoid();
}

void F77_FUNC(gdasph, GDASPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift,
    const char *shift_scalar, const int32_t *shift_scalar_l,
    const int32_t *shift_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, *shift, shift_scalar,
      *shift_scalar_l, *shift_scalar_ind, *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_PHASE_ENTRY;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  E.fragment_index = *fragment_index;
  E.EN(phase,shift) = *shift;
  _GDF_CString(E.scalar + 0, shift_scalar, *shift_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *shift_scalar_ind);
  
  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  free(E.scalar[0]);

  dreturnvoid();
}

void F77_FUNC(gdasrc, GDASRC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;

  dtrace("%i, %p, %i, %p, %i, %g, %p, %i, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, *dividend, dividend_scalar,
      *dividend_scalar_l, *dividend_scalar_ind, *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RECIP_ENTRY;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  E.fragment_index = *fragment_index;
  E.EN(recip,dividend) = *dividend;
  _GDF_CString(E.scalar + 0, dividend_scalar, *dividend_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *dividend_scalar_ind);

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  free(E.scalar[0]);

  dreturnvoid();
}

void F77_FUNC(gdascr, GDASCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;

  dtrace("%i, %p, %i, %p, %i, %g;%g, %p, %i, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, dividend[0], dividend[1],
      dividend_scalar, *dividend_scalar_l, *dividend_scalar_ind,
      *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RECIP_ENTRY;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  E.fragment_index = *fragment_index;
  gd_li2cs_(E.EN(recip,cdividend), dividend[0], dividend[1]);
  _GDF_CString(E.scalar + 0, dividend_scalar, *dividend_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *dividend_scalar_ind);

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  free(E.scalar[0]);

  dreturnvoid();
}

void F77_FUNC(gdaswd, GDASWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold,
    const char *threshold_scalar, const int32_t *threshold_scalar_l,
    const int32_t *threshold_scalar_ind, const int32_t *fragment_index)
{
  gd_entry_t E;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %p, %p, %i, %i, %i", *dirfile,
      field_code, *field_code_l, in_field, *in_field_l, check_field,
      *check_field_l, *windop, threshold, threshold_scalar, *threshold_scalar_l,
      *threshold_scalar_ind, *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_WINDOW_ENTRY;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  _GDF_CString(&E.in_fields[1], check_field, *check_field_l);
  E.fragment_index = *fragment_index;
  E.EN(window,windop) = *windop;
  E.EN(window,threshold) = _GDF_SetTriplet(E.EN(window,windop), threshold);
  _GDF_CString(E.scalar + 0, threshold_scalar, *threshold_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *threshold_scalar_ind);

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  free(E.in_fields[1]);
  free(E.scalar[0]);

  dreturnvoid();
}

void F77_FUNC(gdasmx, GDASMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const char *val_scalar,
    const int32_t *val_scalar_l, const int32_t *val_scalar_ind,
    const int32_t *period, const char *period_scalar,
    const int32_t *period_scalar_l, const int32_t *period_scalar_ind,
    const int32_t *fragment_index)
{
  gd_entry_t E;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i, %i",
      *dirfile, field_code, *field_code_l, in_field, *in_field_l, count_field,
      *count_field_l, *val, val_scalar, *val_scalar_l, *val_scalar_ind,
      *period, period_scalar, *period_scalar_l, *period_scalar_ind,
      *fragment_index);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_MPLEX_ENTRY;
  _GDF_CString(&E.field, field_code, *field_code_l);
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  _GDF_CString(&E.in_fields[1], count_field, *count_field_l);
  E.fragment_index = *fragment_index;
  E.EN(mplex,count_val) = *val;
  E.EN(mplex,period) = *period;
  _GDF_CString(E.scalar + 0, val_scalar, *val_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *val_scalar_ind);
  _GDF_CString(E.scalar + 1, period_scalar, *period_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[1], *period_scalar_ind);

  gd_add(_GDF_GetDirfile(*dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  free(E.in_fields[1]);
  free(E.scalar[0]);
  free(E.scalar[1]);

  dreturnvoid();
}

void F77_FUNC(gdlsph, GDLSPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift,
    const char *shift_scalar, const int32_t *shift_scalar_l,
    const int32_t *shift_scalar_ind)
{
  gd_entry_t E;
  char *fc;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, *shift, shift_scalar,
      *shift_scalar_l, *shift_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_PHASE_ENTRY;
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  E.EN(phase,shift) = *shift;
  _GDF_CString(E.scalar + 0, shift_scalar, *shift_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *shift_scalar_ind);

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  free(E.scalar[0]);
  dreturnvoid();
}

void F77_FUNC(gdlspn, GDLSPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind,
    const double *a1, const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const double *a2, const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind,
    const double *a3, const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const double *a4, const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind,
    const double *a5, const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind)
{
  char *fc;
  gd_entry_t E;
  int i, n;

  dtrace("%i, %p, %i, %i, %p, %i, %g, %p, %i, %i, %g, %p, %i, %i, %g, "
      "%p, %i, %i, %g, %p, %i, %i, %g, %p, %i, %i, %g, %p, %i, %i",
      *dirfile, field_code, *field_code_l, *poly_ord, in_field, *in_field_l,
      *a0, a0_scalar, *a0_scalar_l, *a0_scalar_ind,
      *a1, a1_scalar, *a1_scalar_l, *a1_scalar_ind,
      *a2, a2_scalar, *a2_scalar_l, *a2_scalar_ind,
      *a3, a3_scalar, *a3_scalar_l, *a3_scalar_ind,
      *a4, a4_scalar, *a4_scalar_l, *a4_scalar_ind,
      *a5, a5_scalar, *a5_scalar_l, *a5_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_POLYNOM_ENTRY;
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  n = E.EN(polynom,poly_ord) = *poly_ord;

  if (n > 5)
    n = 5;
  else if (n < 1)
    n = 1;

  switch (n) {
    case 5:
      _GDF_CString(E.scalar + 5, a5_scalar, *a5_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[5], *a5_scalar_ind);
      E.EN(polynom,a)[5] = *a5;
      /* fallthrough */
    case 4:
      _GDF_CString(E.scalar + 4, a4_scalar, *a4_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[4], *a4_scalar_ind);
      E.EN(polynom,a)[4] = *a4;
      /* fallthrough */
    case 3:
      _GDF_CString(E.scalar + 3, a3_scalar, *a3_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[3], *a3_scalar_ind);
      E.EN(polynom,a)[3] = *a3;
      /* fallthrough */
    case 2:
      _GDF_CString(E.scalar + 2, a2_scalar, *a2_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[2], *a2_scalar_ind);
      E.EN(polynom,a)[2] = *a2;
      /* fallthrough */
    default:
      _GDF_CString(E.scalar + 1, a1_scalar, *a1_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[1], *a1_scalar_ind);
      E.EN(polynom,a)[1] = *a1;
      _GDF_CString(E.scalar + 0, a0_scalar, *a0_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[0], *a0_scalar_ind);
      E.EN(polynom,a)[0] = *a0;
  }

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  for (i = 0; i <= n; ++i)
    free(E.scalar[i]);

  dreturnvoid();
}

void F77_FUNC(gdlscp, GDLSCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind,
    const GD_DCOMPLEXP(a1), const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const GD_DCOMPLEXP(a2), const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind,
    const GD_DCOMPLEXP(a3), const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const GD_DCOMPLEXP(a4), const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind,
    const GD_DCOMPLEXP(a5), const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind)
{
  char *fc;
  gd_entry_t E;
  int i, n;

  dtrace("%i, %p, %i, %i, %p, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %g;%g, "
      "%p, %i, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i",
      *dirfile, field_code, *field_code_l, *poly_ord, in_field, *in_field_l,
      crealp(a0), cimagp(a0), a0_scalar, *a0_scalar_l, *a0_scalar_ind,
      crealp(a1), cimagp(a1), a1_scalar, *a1_scalar_l, *a1_scalar_ind,
      crealp(a2), cimagp(a2), a2_scalar, *a2_scalar_l, *a2_scalar_ind,
      crealp(a3), cimagp(a3), a3_scalar, *a3_scalar_l, *a3_scalar_ind,
      crealp(a4), cimagp(a4), a4_scalar, *a4_scalar_l, *a4_scalar_ind,
      crealp(a5), cimagp(a5), a5_scalar, *a5_scalar_l, *a5_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_POLYNOM_ENTRY;
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  n = E.EN(polynom,poly_ord) = *poly_ord;
  E.flags = GD_EN_COMPSCAL;

  if (n > 5)
    n = 5;
  else if (n < 1)
    n = 1;

  switch (n) {
    case 5:
      _GDF_CString(E.scalar + 5, a5_scalar, *a5_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[5], *a5_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[5], a5);
      /* fallthrough */
    case 4:
      _GDF_CString(E.scalar + 4, a4_scalar, *a4_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[4], *a4_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[4], a4);
      /* fallthrough */
    case 3:
      _GDF_CString(E.scalar + 3, a3_scalar, *a3_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[3], *a3_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[3], a3);
      /* fallthrough */
    case 2:
      _GDF_CString(E.scalar + 2, a2_scalar, *a2_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[2], *a2_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[2], a2);
      /* fallthrough */
    default:
      _GDF_CString(E.scalar + 1, a1_scalar, *a1_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[1], *a1_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[1], a1);
      _GDF_CString(E.scalar + 0, a0_scalar, *a0_scalar_l);
      GDF_SCIND_F2C(E.scalar_ind[0], *a0_scalar_ind);
      gd_cp2cs_(E.EN(polynom,ca)[0], a0);
  }

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  for (i = 0; i <= n; ++i)
    free(E.scalar[i]);

  dreturnvoid();
}

void F77_FUNC(gdlswd, GDLSWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold,
    const char *threshold_scalar, const int32_t *threshold_scalar_l,
    const int32_t *threshold_scalar_ind)
{
  gd_entry_t E;
  char *fc;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %p, %p, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, check_field, *check_field_l,
      *windop, threshold, threshold_scalar, *threshold_scalar_l,
      *threshold_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_WINDOW_ENTRY;
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  _GDF_CString(&E.in_fields[1], check_field, *check_field_l);
  E.EN(window,windop) = *windop;
  E.EN(window,threshold) = _GDF_SetTriplet(E.EN(window,windop), threshold);
  _GDF_CString(E.scalar + 0, threshold_scalar, *threshold_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *threshold_scalar_ind);

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  free(E.in_fields[1]);
  free(E.scalar[0]);

  dreturnvoid();
}

/* common code for gdasbt and gdassb */
static void _GDF_AddBitSBit(int32_t dirfile, const char *field_code,
    int32_t field_code_l, const char *in_field, int32_t in_field_l,
    int32_t bitnum, const char *bitnum_scalar, int32_t bitnum_scalar_l,
    int32_t bitnum_scalar_ind, int32_t numbits, const char *numbits_scalar,
    int32_t numbits_scalar_l, int32_t numbits_scalar_ind,
    int32_t fragment_index, int sbit)
{
  gd_entry_t E;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i, %i, %i", dirfile,
      field_code, field_code_l, in_field, in_field_l, bitnum, bitnum_scalar,
      bitnum_scalar_l, bitnum_scalar_ind, numbits, numbits_scalar,
      numbits_scalar_l, numbits_scalar_ind, fragment_index, sbit);

  memset(&E, 0, sizeof(E));
  E.field_type = sbit ? GD_SBIT_ENTRY : GD_BIT_ENTRY;
  E.fragment_index = fragment_index;
  E.EN(bit,bitnum) = bitnum;
  E.EN(bit,numbits) = numbits;
  _GDF_CString(&E.field, field_code, field_code_l);
  _GDF_CString(E.in_fields, in_field, in_field_l);
  _GDF_CString(E.scalar, bitnum_scalar, bitnum_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], bitnum_scalar_ind);
  _GDF_CString(E.scalar + 1, numbits_scalar, numbits_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[1], numbits_scalar_ind);

  gd_add(_GDF_GetDirfile(dirfile), &E);

  free(E.field);
  free(E.in_fields[0]);
  free(E.scalar[0]);
  free(E.scalar[1]);

  dreturnvoid();
}

/* gd_add_bit with scalar parameters */
void F77_FUNC(gdasbt, GDASBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind,
    const int32_t *fragment_index)
{
  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i, %i", *dirfile,
      field_code, *field_code_l, in_field, *in_field_l, *bitnum, bitnum_scalar,
      *bitnum_scalar_l, *bitnum_scalar_ind, *numbits, numbits_scalar,
      *numbits_scalar_l, *numbits_scalar_ind, *fragment_index);

  _GDF_AddBitSBit(*dirfile, field_code, *field_code_l, in_field, *in_field_l,
      *bitnum, bitnum_scalar, *bitnum_scalar_l, *bitnum_scalar_ind, *numbits,
      numbits_scalar, *numbits_scalar_l, *numbits_scalar_ind, *fragment_index,
      0);

  dreturnvoid();
}

/* gd_add_sbit with scalar parameters */
void F77_FUNC(gdassb, GDASSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind,
    const int32_t *fragment_index)
{
  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i, %i", *dirfile,
      field_code, *field_code_l, in_field, *in_field_l, *bitnum, bitnum_scalar,
      *bitnum_scalar_l, *bitnum_scalar_ind, *numbits, numbits_scalar,
      *numbits_scalar_l, *numbits_scalar_ind, *fragment_index);


  _GDF_AddBitSBit(*dirfile, field_code, *field_code_l, in_field, *in_field_l,
      *bitnum, bitnum_scalar, *bitnum_scalar_l, *bitnum_scalar_ind, *numbits,
      numbits_scalar, *numbits_scalar_l, *numbits_scalar_ind, *fragment_index,
      1);

  dreturnvoid();
}

/* common code for gdlsbt and gdlssb */
static void _GDF_AlterBitSBit(int32_t dirfile, const char *field_code,
    int32_t field_code_l, const char *in_field, int32_t in_field_l,
    int32_t bitnum, const char *bitnum_scalar, int32_t bitnum_scalar_l,
    int32_t bitnum_scalar_ind, int32_t numbits, const char *numbits_scalar,
    int32_t numbits_scalar_l, int32_t numbits_scalar_ind, int sbit)
{
  gd_entry_t E;
  char *fc;

  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i, %i", dirfile,
      field_code, field_code_l, in_field, in_field_l, bitnum, bitnum_scalar,
      bitnum_scalar_l, bitnum_scalar_ind, numbits, numbits_scalar,
      numbits_scalar_l, numbits_scalar_ind, sbit);

  memset(&E, 0, sizeof(E));
  E.field_type = sbit ? GD_SBIT_ENTRY : GD_BIT_ENTRY;
  E.EN(bit,bitnum) = bitnum;
  E.EN(bit,numbits) = numbits;
  _GDF_CString(E.in_fields, in_field, in_field_l);
  _GDF_CString(E.scalar, bitnum_scalar, bitnum_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], bitnum_scalar_ind);
  _GDF_CString(E.scalar + 1, numbits_scalar, numbits_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[1], numbits_scalar_ind);

  gd_alter_entry(_GDF_GetDirfile(dirfile), _GDF_CString(&fc, field_code,
        field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  free(E.scalar[0]);
  free(E.scalar[1]);

  dreturnvoid();
}

/* gd_alter_bit with scalar parameters */
void F77_FUNC(gdlsbt, GDLSBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind)
{
  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i", *dirfile,
      field_code, *field_code_l, in_field, *in_field_l, *bitnum, bitnum_scalar,
      *bitnum_scalar_l, *bitnum_scalar_ind, *numbits, numbits_scalar,
      *numbits_scalar_l, *numbits_scalar_ind);

  _GDF_AlterBitSBit(*dirfile, field_code, *field_code_l, in_field, *in_field_l,
      *bitnum, bitnum_scalar, *bitnum_scalar_l, *bitnum_scalar_ind, *numbits,
      numbits_scalar, *numbits_scalar_l, *numbits_scalar_ind, 0);

  dreturnvoid();
}

/* gd_alter_sbit with scalar parameters */
void F77_FUNC(gdlssb, GDLSSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind)
{
  dtrace("%i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i", *dirfile,
      field_code, *field_code_l, in_field, *in_field_l, *bitnum, bitnum_scalar,
      *bitnum_scalar_l, *bitnum_scalar_ind, *numbits, numbits_scalar,
      *numbits_scalar_l, *numbits_scalar_ind);

  _GDF_AlterBitSBit(*dirfile, field_code, *field_code_l, in_field, *in_field_l,
      *bitnum, bitnum_scalar, *bitnum_scalar_l, *bitnum_scalar_ind, *numbits,
      numbits_scalar, *numbits_scalar_l, *numbits_scalar_ind, 1);

  dreturnvoid();
}

/* gd_alter_lincom with scalar parameters */
void F77_FUNC(gdlslc, GDLSLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind, const double *b1,
    const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const double *m2, const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind, const double *b2,
    const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const double *m3, const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind, const double *b3,
    const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind)
{
  gd_entry_t E;
  int i, n;
  char *fc;

  dtrace("%i, %p, %i, %i, %p, %i, %g, %p, %i, %i, %g, %p, %i, %i, %p, %i, "
      "%g, %p, %i, %i, %g, %p, %i, %i, %p, %i, %g, %p, %i, %i, %g, %p, %i, "
      "%i", *dirfile, field_code, *field_code_l, *n_fields, in_field1,
      *in_field1_l, *m1, m1_scalar, *m1_scalar_l, *m1_scalar_ind, *b1,
      b1_scalar, *b1_scalar_l, *b1_scalar_ind, in_field2, *in_field2_l, *m2,
      m2_scalar, *m2_scalar_l, *m2_scalar_ind, *b2, b2_scalar, *b2_scalar_l,
      *b2_scalar_ind, in_field3, *in_field3_l, *m3, m3_scalar, *m3_scalar_l,
      *m3_scalar_ind, *b3, b3_scalar, *b3_scalar_l, *b3_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_LINCOM_ENTRY;
  n = E.EN(lincom,n_fields) = *n_fields;

  _GDF_CString(E.in_fields + 0, in_field1, *in_field1_l);
  _GDF_CString(E.scalar + 0, m1_scalar, *m1_scalar_l);
  _GDF_CString(E.scalar + 0 + GD_MAX_LINCOM, b1_scalar, *b1_scalar_l);

  GDF_SCIND_F2C(E.scalar_ind[0], *m1_scalar_ind);
  GDF_SCIND_F2C(E.scalar_ind[0 + GD_MAX_LINCOM], *b1_scalar_ind);
  E.EN(lincom,m[0]) = *m1;
  E.EN(lincom,b[0]) = *b1;

  if (n > 1) {
    _GDF_CString(E.in_fields + 1, in_field2, *in_field2_l);
    _GDF_CString(E.scalar + 1, m2_scalar, *m2_scalar_l);
    _GDF_CString(E.scalar + 1 + GD_MAX_LINCOM, b2_scalar, *b2_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[1], *m2_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[1 + GD_MAX_LINCOM], *b2_scalar_ind);
    E.EN(lincom,m[1]) = *m2;
    E.EN(lincom,b[1]) = *b2;
  }

  if (n > 2) {
    _GDF_CString(E.in_fields + 2, in_field3, *in_field3_l);
    _GDF_CString(E.scalar + 2, m3_scalar, *m3_scalar_l);
    _GDF_CString(E.scalar + 2 + GD_MAX_LINCOM, b3_scalar, *b3_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[2], *m3_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[2 + GD_MAX_LINCOM], *b3_scalar_ind);
    E.EN(lincom,m[2]) = *m3;
    E.EN(lincom,b[2]) = *b3;
  }

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  for (i = 0; i < n; ++i) {
    free(E.in_fields[i]);
    free(E.scalar[i]);
    free(E.scalar[i + GD_MAX_LINCOM]);
  }

  dreturnvoid();
}

/* gd_alter_clincom with scalar parameters */
void F77_FUNC(gdlscl, GDLSCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind,
    const GD_DCOMPLEXP(b1), const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const GD_DCOMPLEXP(m2), const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind,
    const GD_DCOMPLEXP(b2), const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const GD_DCOMPLEXP(m3), const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind,
    const GD_DCOMPLEXP(b3), const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind)
{
  gd_entry_t E;
  int i, n;
  char *fc;

  dtrace("%i, %p, %i, %i, %p, %i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %p, "
      "%i, %g;%g, %p, %i, %i, %g;%g, %p, %i, %i, %p, %i, %g;%g, %p, %i, %i, "
      "%g;%g, %p, %i, %i", *dirfile, field_code, *field_code_l, *n_fields,
      in_field1, *in_field1_l, crealp(m1), cimagp(m1), m1_scalar, *m1_scalar_l,
      *m1_scalar_ind, crealp(b1), cimagp(b1), b1_scalar, *b1_scalar_l,
      *b1_scalar_ind, in_field2, *in_field2_l, crealp(m2), cimagp(m2),
      m2_scalar, *m2_scalar_l, *m2_scalar_ind, crealp(b2), cimagp(b2),
      b2_scalar, *b2_scalar_l, *b2_scalar_ind, in_field3, *in_field3_l,
      crealp(m3), cimagp(m3), m3_scalar, *m3_scalar_l, *m3_scalar_ind,
      crealp(b3), cimagp(b3), b3_scalar, *b3_scalar_l, *b3_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_LINCOM_ENTRY;
  n = E.EN(lincom,n_fields) = *n_fields;
  E.flags = GD_EN_COMPSCAL;

  _GDF_CString(E.in_fields + 0, in_field1, *in_field1_l);
  _GDF_CString(E.scalar + 0, m1_scalar, *m1_scalar_l);
  _GDF_CString(E.scalar + 0 + GD_MAX_LINCOM, b1_scalar, *b1_scalar_l);

  GDF_SCIND_F2C(E.scalar_ind[0], *m1_scalar_ind);
  GDF_SCIND_F2C(E.scalar_ind[0 + GD_MAX_LINCOM], *b1_scalar_ind);
  gd_cp2cs_(E.EN(lincom,cm)[0], m1);
  gd_cp2cs_(E.EN(lincom,cb)[0], b1);

  if (n > 1) {
    _GDF_CString(E.in_fields + 1, in_field2, *in_field2_l);
    _GDF_CString(E.scalar + 1, m2_scalar, *m2_scalar_l);
    _GDF_CString(E.scalar + 1 + GD_MAX_LINCOM, b2_scalar, *b2_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[1], *m2_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[1 + GD_MAX_LINCOM], *b2_scalar_ind);
    gd_cp2cs_(E.EN(lincom,cm)[1], m2);
    gd_cp2cs_(E.EN(lincom,cb)[1], b2);
  }

  if (n > 2) {
    _GDF_CString(E.in_fields + 2, in_field3, *in_field3_l);
    _GDF_CString(E.scalar + 2, m3_scalar, *m3_scalar_l);
    _GDF_CString(E.scalar + 2 + GD_MAX_LINCOM, b3_scalar, *b3_scalar_l);

    GDF_SCIND_F2C(E.scalar_ind[2], *m3_scalar_ind);
    GDF_SCIND_F2C(E.scalar_ind[2 + GD_MAX_LINCOM], *b3_scalar_ind);
    gd_cp2cs_(E.EN(lincom,cm)[2], m3);
    gd_cp2cs_(E.EN(lincom,cb)[2], b3);
  }

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  for (i = 0; i < n; ++i) {
    free(E.in_fields[i]);
    free(E.scalar[i]);
    free(E.scalar[i + GD_MAX_LINCOM]);
  }

  dreturnvoid();
}

/* gd_alter_recip with scalar parameters */
void F77_FUNC(gdlsrc, GDLSRC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind)
{
  gd_entry_t E;
  char *fc;

  dtrace("%i, %p, %i, %p, %i, %g, %p, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, *dividend, dividend_scalar,
      *dividend_scalar_l, *dividend_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RECIP_ENTRY;
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  E.EN(recip,dividend) = *dividend;
  _GDF_CString(E.scalar + 0, dividend_scalar, *dividend_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *dividend_scalar_ind);

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  free(E.scalar[0]);

  dreturnvoid();
}

/* gd_alter_crecip with scalar parameters */
void F77_FUNC(gdlscr, GDLSCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind)
{
  gd_entry_t E;
  char *fc;

  dtrace("%i, %p, %i, %p, %i, %g;%g, %p, %i, %i", *dirfile, field_code,
      *field_code_l, in_field, *in_field_l, dividend[0], dividend[1],
      dividend_scalar, *dividend_scalar_l, *dividend_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RECIP_ENTRY;
  E.flags = GD_EN_COMPSCAL;
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  gd_li2cs_(E.EN(recip,cdividend), dividend[0], dividend[1]);
  _GDF_CString(E.scalar + 0, dividend_scalar, *dividend_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *dividend_scalar_ind);

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  free(E.scalar[0]);

  dreturnvoid();
}

/* gd_alter_raw with scalar parameters */
void F77_FUNC(gdlsrw, GDLSRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const char *spf_scalar, const int32_t *spf_scalar_l,
    const int32_t *spf_scalar_ind, const int32_t *recode)
{
  gd_entry_t E;
  char *fc;

  dtrace("%i, %p, %i, %i, %i, %p, %i, %i, %i", *dirfile, field_code,
      *field_code_l, *data_type, *spf, spf_scalar, *spf_scalar_l,
      *spf_scalar_ind, *recode);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RAW_ENTRY;
  E.EN(raw,data_type) = *data_type;
  E.EN(raw,spf) = *spf;
  _GDF_CString(E.scalar, spf_scalar, *spf_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *spf_scalar_ind);

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, *recode);

  free(fc);
  free(E.scalar[0]);

  dreturnvoid();
}

/* gd_alter_mplex with scalar parameters */
void F77_FUNC(gdlsmx, GDLSMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const char *val_scalar,
    const int32_t *val_scalar_l, const int32_t *val_scalar_ind,
    const int32_t *period, const char *period_scalar,
    const int32_t *period_scalar_l, const int32_t *period_scalar_ind)
{
  gd_entry_t E;
  char *fc;

  dtrace("%i, %p, %i, %p, %i, %p, %i, %i, %p, %i, %i, %i, %p, %i, %i",
      *dirfile, field_code, *field_code_l, in_field, *in_field_l, count_field,
      *count_field_l, *val, val_scalar, *val_scalar_l, *val_scalar_ind,
      *period, period_scalar, *period_scalar_l, *period_scalar_ind);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_MPLEX_ENTRY;
  _GDF_CString(&E.in_fields[0], in_field, *in_field_l);
  _GDF_CString(&E.in_fields[1], count_field, *count_field_l);
  E.EN(mplex,count_val) = *val;
  E.EN(mplex,period) = *period;
  _GDF_CString(E.scalar + 0, val_scalar, *val_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[0], *val_scalar_ind);
  _GDF_CString(E.scalar + 1, period_scalar, *period_scalar_l);
  GDF_SCIND_F2C(E.scalar_ind[1], *period_scalar_ind);

  gd_alter_entry(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), &E, 0);

  free(fc);
  free(E.in_fields[0]);
  free(E.in_fields[1]);
  free(E.scalar[0]);
  free(E.scalar[1]);

  dreturnvoid();
}

/* gd_encoding_support */
void F77_FUNC(gdencs,GDENCS) (int32_t *ret, const int32_t *encoding)
{
  dtrace("%p, 0x%X", ret, *encoding);

  *ret = gd_encoding_support((unsigned long)*encoding);

  dreturn("%i", *ret);
}

/* Return the maximum sarray value length */
void F77_FUNC(gdsarx, GDSARX) (int32_t *max, const int32_t *dirfile)
{
  const char ***pptr;
  const char **ptr;
  size_t len = 0;
  DIRFILE *D;

  dtrace("%p, %i", max, *dirfile);

  D = _GDF_GetDirfile(*dirfile);

  pptr = gd_sarrays(D);
  if (gd_error(D)) {
    dreturnvoid();
    return;
  }

  for (; *pptr; ++pptr)
    for (ptr = *pptr; *ptr; ++ptr) {
      size_t newlen = strlen(*ptr);
      if (newlen > len) {
        dprintf("\"%s\" > %zu", *ptr, len);
        len = newlen;
      }
    }

  *max = (int32_t)len;

  dreturn("%i", *max);
}

/* Return the maximum meta sarray value length */
void F77_FUNC(gdmsax, GDMSAX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l)
{
  const char ***pptr;
  const char **ptr;
  size_t len = 0;
  DIRFILE *D;
  char *pa;

  dtrace("%p, %i, %p, %i", max, *dirfile, parent, *parent_l);

  D = _GDF_GetDirfile(*dirfile);

  pptr = gd_msarrays(D, _GDF_CString(&pa, parent, *parent_l));
  free(pa);

  if (gd_error(D)) {
    dreturnvoid();
    return;
  }

  for (; *pptr; ++pptr)
    for (ptr = *pptr; *ptr; ++ptr) {
      size_t newlen = strlen(*ptr);
      if (newlen > len)
        len = newlen;
    }

  *max = (int32_t)len;

  dreturn("%i", *max);
}

/* gd_array_len wrapper */
void F77_FUNC(gdarln, GDARLN) (int32_t *len, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l)
{
  char *fc;

  dtrace("%p, %i, %p, %i", len, *dirfile, field_code, *field_code_l);

  *len = gd_array_len(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l));
  free(fc);

  dreturn("%i", *len);
}

/* gd_get_sarray_slice -- this only returns one value */
void F77_FUNC(gdgtsa, GDGTSA) (char *value, int32_t *value_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    const int32_t *index)
{
  const char* v[1];
  char *fc;
  int r;

  dtrace("%p, %p, %i, %p, %i, %i", value, value_l, *dirfile, field_code,
      *field_code_l, *index);
  
  r = gd_get_sarray_slice(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc,
        field_code, *field_code_l), *index - 1, 1, v);
  free(fc);

  if (r)
    *value_l = 0;
  else
    _GDF_FString(value, value_l, *v);

  dreturn("%i", *value_l);
}

/* gd_put_sarray_slice -- this only stores one value */
void F77_FUNC(gdptsa, GDPTSA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *index, const char *value,
    const int32_t *value_l)
{
  char *fc, *v;

  dtrace("%i, %p, %i, %i, %p, %i", *dirfile, field_code, *field_code_l,
      *index, value, *value_l);

  _GDF_CString(&v, value, *value_l);

  gd_put_sarray_slice(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc,
        field_code, *field_code_l), *index - 1, 1, (const char **)&v);

  free(v);
  free(fc);

  dreturnvoid();
}

/* gd_alter_sarray wrapper */
void F77_FUNC(gdalsa, GDALSA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *array_len)
{
  char *fc;

  dtrace("%i, %p, %i, %i", *dirfile, field_code, *field_code_l, *array_len);

  gd_alter_sarray(_GDF_GetDirfile(*dirfile), _GDF_CString(&fc, field_code,
        *field_code_l), (size_t)*array_len);

  free(fc);
  dreturnvoid();
}

/* gd_getdata wrapper for SINDIRs -- this only returns one sample */
void F77_FUNC(gdgstd, GDGSTD) (char *value, int32_t *value_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample)
{
  const char *datum[1];
  char *fc;
  size_t n_read;

  dtrace("%p, %p, %i, %p, %i, %i, %i", value, value_l, *dirfile, field_code,
      *field_code_l, *first_frame, *first_sample);

  n_read = gd_getdata(_GDF_GetDirfile(*dirfile),
      _GDF_CString(&fc, field_code, *field_code_l), *first_frame,
      *first_sample, 0, 1, GD_STRING, datum);
  free(fc);
  if (n_read == 0)
    *value_l = 0;
  else
    _GDF_FString(value, value_l, *datum);

  dreturn("%i", *value_l);
}

/* gd_getdata wrapper for SINDIRs-- this returns an opaquely packed array of
 * strings; see also GDXSTP and GDDSTP */
void F77_FUNC(gdgstp, GDGSTP) (int32_t *n_read, char *data,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample,
    const int32_t *num_frames, const int32_t *num_samples)
{
  const char **buffer;
  char *fc;
  unsigned spf;
  size_t ns;
  DIRFILE *D;
  
  dtrace("%p, %p, %i, %p, %i, %i, %i, %i, %i", n_read, data, *dirfile,
      field_code, *field_code_l, *first_frame, *first_sample, *num_frames,
      *num_samples);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&fc, field_code, *field_code_l);

  /* convert to num_samples */
  if (*num_frames > 0) {
    spf = gd_spf(D, fc);
    if (gd_error(D)) {
      free(fc);
      *n_read = 0;
      *((void**)data) = NULL;
      dreturn("%i", 0);
      return;
    }
    ns = *num_samples + *num_frames * spf;
  } else
    ns = *num_samples;

  /* output buffer */
  buffer = malloc(sizeof(*buffer) * ns);
  if (buffer == NULL) {
    free(fc);
    *n_read = 0;
    *((void**)data) = NULL;
    dreturn("%i", 0);
    return;
  }

  *n_read = gd_getdata(D, fc, *first_frame, *first_sample, 0, ns, GD_STRING,
      buffer);
  free(fc);

  if (*n_read == 0) {
    free(buffer);
    *((void**)data) = NULL;
  } else
    *((const char***)data) = buffer;

  dreturn("%i/%p", *n_read, *(const char***)data);
}

void F77_FUNC(gdxstp, GDXSTP) (char *value, int32_t *value_l, const char *data,
    const int32_t *index)
{
  const char **list;

  dtrace("%p, %p, %p, %i", value, value_l, data, *index);

  list = *((const char***)data);

  if (list == NULL) 
    *value_l = 0;
  else
    _GDF_FString(value, value_l, list[*index - 1]);

  dreturn("%i", *value_l);
}

void F77_FUNC(gddstp, GDDSTP) (char *data)
{
  dtrace("%p", data);

  free(*(void**)data);
  *((void**)data) = NULL;
  
  dreturnvoid();
}

/* Return the maximum field name length of a gd_match_entries call */
void F77_FUNC(gdmatx, GDMATX) (int32_t *max, const int32_t *dirfile,
    const char *regex, const int32_t *regex_l, const int32_t *fragment,
    const int32_t *type, const int32_t *flags)
{
  const char **el;
  char *rx;
  size_t len = 0;
  DIRFILE* D;
  unsigned int i, nentries;
  const unsigned int utype = (unsigned int)*type;
  const unsigned int uflags = (unsigned int)*flags;

  dtrace("%p, %i, %p, %i, %i, 0x%X, 0x%X", max, *dirfile, regex, *regex_l,
      *fragment, utype, uflags);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&rx, regex, *regex_l);
  nentries = gd_match_entries(D, rx, *fragment, utype, uflags, &el);

  if (!gd_error(D)) {
    for (i = 0; i < nentries; ++i)
      if (strlen(el[i]) > len)
        len = strlen(el[i]);
  }

  *max = len;
  free(rx);
  dreturn("%i", *max);
}

/* gd_match_entries wrapper */
void F77_FUNC(gdmatn, GDMATN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *regex, const int32_t *regex_l,
    const int32_t *fragment, const int32_t *type, const int32_t *flags,
    const int32_t *field_num)
{
  const char **el;
  char *rx;
  DIRFILE* D;
  unsigned int nentries;
  const unsigned int utype = (unsigned int)*type;
  const unsigned int uflags = (unsigned int)*flags;

  dtrace("%p, %i, %i, %p, %i, %i, 0x%X, 0x%X, %i", name, *name_l, *dirfile,
      regex, *regex_l, *fragment, utype, uflags, *field_num);

  D = _GDF_GetDirfile(*dirfile);
  _GDF_CString(&rx, regex, *regex_l);
  nentries = gd_match_entries(D, rx, *fragment, utype, uflags, &el);

  if (!gd_error(D) && *field_num > 0 && *field_num <= (int)nentries) 
    _GDF_FString(name, name_l, el[*field_num - 1]);
  else
    *name_l = 0;

  free(rx);
  dreturn("%i", *name_l);
}

/* gd_match_entries wrapper */
void F77_FUNC(gdnmat, GDNMAT) (int32_t *nentries, const int32_t *dirfile,
    const char *regex, const int32_t *regex_l, const int32_t *fragment,
    const int32_t *type, const int32_t *flags)
{
  char *rx;

  dtrace("%p, %i, %p, %i, %i, 0x%X, 0x%X", nentries, *dirfile, regex, *regex_l,
      *fragment, *type, *flags);

  *nentries = gd_match_entries(_GDF_GetDirfile(*dirfile), _GDF_CString(&rx,
        regex, *regex_l), *fragment, (unsigned int)*type, (unsigned int)*flags,
      NULL);

  free(rx);
  dreturn("%i", *nentries);
}
