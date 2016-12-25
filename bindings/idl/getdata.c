/* Copyright (C) 2009-2016 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GetData; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */
#define _LARGEFILE64_SOURCE 1
#include <stdio.h>
#include <idl_export.h>
#include <stdlib.h>
#define NO_GETDATA_LEGACY_API
#undef _BSD_SOURCE
#undef _POSIX_SOURCE
#undef _POSIX_C_SOURCE
#undef _SVID_SOURCE

#ifdef HAVE_CONFIG_H
# include "gd_config.h"
#endif

#ifdef GD_EXTERNAL
# include <complex.h>
# include <getdata.h>
# define dtracevoid()
# define dtrace(...)
# define dprintf(...)
# define dreturnvoid()
# define dreturn(...)
# define dwatch(...)
# define gd_colclear()
# define GD_INT_TYPE ((gd_type_t)(SIZEOF_INT | GD_SIGNED))
# define GD_UINT_TYPE ((gd_type_t)(SIZEOF_UNSIGNED_INT))
# define gd_static_inline_ static inline
#else
# include "../../src/internal.h"
#endif

#define GDIDL_N_DIRFILES 1024
static DIRFILE* idldirfiles[GDIDL_N_DIRFILES];
static int idldirfiles_initialised = 0;

static IDL_StructDefPtr gdidl_entry_def = NULL;
IDL_StructDefPtr gdidl_const_def = NULL;

#define GDIDL_ABORT(s) do { \
  gd_colclear(); \
  IDL_Message(IDL_M_GENERIC, IDL_MSG_LONGJMP, s); \
} while(0)
#define GDIDL_ABORT2(fmt, ...) do { \
  char gdidl_abort_buffer[1024]; \
  sprintf(gdidl_abort_buffer, fmt, __VA_ARGS__); \
  GDIDL_ABORT(gdidl_abort_buffer); \
} while(0)
#define GDIDL_KW_ABORT(s) \
  do { IDL_KW_FREE; GDIDL_ABORT(s); } while(0)
#define dtraceidl() dtrace("%i, %p, %p", argc, argv, argk)

/* Error reporting stuff */
#define GDIDL_KW_PAR_ERROR { "ERROR", 0, 0xffff, IDL_KW_OUT, 0, \
  IDL_KW_OFFSETOF(error) }
#define GDIDL_KW_PAR_ESTRING { "ESTRING", 0, 0xffff, IDL_KW_OUT, 0, \
  IDL_KW_OFFSETOF(estr) }
#define GDIDL_KW_RESULT_ERROR IDL_VPTR error, estr
#define GDIDL_KW_INIT_ERROR kw.error = kw.estr = NULL;
#define GDIDL_SET_ERROR(D) \
  do { \
    if (kw.error != NULL) { \
      IDL_ALLTYPES a; \
      a.i = gd_error(D); \
      IDL_StoreScalar(kw.error, IDL_TYP_INT, &a); \
    } \
    if (kw.estr != NULL) { \
      IDL_StoreScalarZero(kw.estr, IDL_TYP_INT); \
      char buffer[GD_MAX_LINE_LENGTH]; \
      kw.estr->type = IDL_TYP_STRING; \
      IDL_StrStore((IDL_STRING*)&kw.estr->value.s, gd_error_string(D, buffer, \
            GD_MAX_LINE_LENGTH)); \
    } \
  } while(0)

#define GDIDL_KW_ONLY_ERROR \
  typedef struct { \
    IDL_KW_RESULT_FIRST_FIELD; \
    GDIDL_KW_RESULT_ERROR; \
  } KW_RESULT; \
KW_RESULT kw; \
GDIDL_KW_INIT_ERROR; \
static IDL_KW_PAR kw_pars[] = { \
  GDIDL_KW_PAR_ERROR, \
  GDIDL_KW_PAR_ESTRING, \
  { NULL } }; \
argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

/* initialise the idldirfiles array */
static void gdidl_init_dirfile(void)
{
  dtracevoid();
  int i;

  for (i = 1; i < GDIDL_N_DIRFILES; ++i)
    idldirfiles[i] = NULL;

  /* we keep entry zero as a generic, invalid dirfile to return if
   * dirfile lookup fails */
  idldirfiles[0] = gd_invalid_dirfile();

  idldirfiles_initialised = 1;
  dreturnvoid();
}

/* convert a new DIRFILE* into an int */
static long gdidl_set_dirfile(DIRFILE* D)
{
  long i;

  dtrace("%p", D);

  if (!idldirfiles_initialised)
    gdidl_init_dirfile();

  for (i = 1; i < GDIDL_N_DIRFILES; ++i)
    if (idldirfiles[i] == NULL) {
      idldirfiles[i] = D;
      dreturn("%li", i);
      return i;
    }

  /* out of idldirfiles space: complain and abort */
  GDIDL_ABORT("DIRFILE space exhausted.");
  return 0; /* can't get here */
}

/* convert an int to a DIRFILE* */
DIRFILE* gdidl_get_dirfile(IDL_LONG d)
{
  dtrace("%i", (int)d);

  if (!idldirfiles_initialised)
    gdidl_init_dirfile();

  if (idldirfiles[d] == NULL) {
    dreturn("%p [0]", idldirfiles[0]);
    return idldirfiles[0];
  }

  dreturn("%p", idldirfiles[d]);
  return idldirfiles[d];
}

/* delete the supplied dirfile */
static void gdidl_clear_dirfile(IDL_LONG d)
{
  dtrace("%i", (int)d);

  if (d != 0)
    idldirfiles[d] = NULL;

  dreturnvoid();
}

/* convert a GetData type code to an IDL type code */
gd_static_inline_ UCHAR gdidl_idl_type(gd_type_t t)
{
  UCHAR it;
  dtrace("0x%X", t);

  switch (t) {
    case GD_UINT8:
      it = IDL_TYP_BYTE;
      break;
    case GD_UINT16:
      it = IDL_TYP_UINT;
      break;
    case GD_INT8: /* there is no signed 8-bit type in IDL
                     - we type promote to INT */
    case GD_INT16:
      it = IDL_TYP_INT;
      break;
    case GD_UINT32:
      it = IDL_TYP_ULONG;
      break;
    case GD_INT32:
      it = IDL_TYP_LONG;
      break;
    case GD_UINT64:
      it = IDL_TYP_ULONG64;
      break;
    case GD_INT64:
      it = IDL_TYP_LONG64;
      break;
    case GD_FLOAT32:
      it = IDL_TYP_FLOAT;
      break;
    case GD_FLOAT64:
      it = IDL_TYP_DOUBLE;
      break;
    case GD_COMPLEX64:
      it = IDL_TYP_COMPLEX;
      break;
    case GD_COMPLEX128:
      it = IDL_TYP_DCOMPLEX;
      break;
    default:
      it = IDL_TYP_UNDEF;
  }

  dreturn("%u", it);
  return it;
}

/* convert an IDL type code to a GetData type code */
gd_static_inline_ gd_type_t gdidl_gd_type(int t) {
  switch (t) {
    case IDL_TYP_BYTE:
      return GD_UINT8;
    case IDL_TYP_UINT:
      return GD_UINT16;
    case IDL_TYP_INT:
      return GD_INT16;
    case IDL_TYP_ULONG:
      return GD_UINT32;
    case IDL_TYP_LONG:
      return GD_INT32;
    case IDL_TYP_ULONG64:
      return GD_UINT64;
    case IDL_TYP_LONG64:
      return GD_INT64;
    case IDL_TYP_FLOAT:
      return GD_FLOAT32;
    case IDL_TYP_DOUBLE:
      return GD_FLOAT64;
    case IDL_TYP_COMPLEX:
      return GD_COMPLEX64;
    case IDL_TYP_DCOMPLEX:
      return GD_COMPLEX128;
  }

  return GD_UNKNOWN;
}

/* convert a datum (from a void*) to an IDL_ALLTYPES union */
gd_static_inline_ IDL_ALLTYPES gdidl_to_alltypes(gd_type_t t, void* d)
{
  dtrace("%x, %p", t, d);

  float complex fc;
  double complex dc;
  IDL_ALLTYPES v;
  v.c = 0;

  switch (t) {
    case GD_UINT8:
      v.c = *(uint8_t*)d;
      break;
    case GD_INT8: /* there is no signed 8-bit type in IDL --
                     we type promote to INT */
      v.i = *(int8_t*)d;
      break;
    case GD_UINT16:
      v.ui = *(uint16_t*)d;
      break;
    case GD_INT16:
      v.i = *(int16_t*)d;
      break;
    case GD_UINT32:
      v.ul = *(uint32_t*)d;
      break;
    case GD_INT32:
      v.l = *(int32_t*)d;
      break;
    case GD_UINT64:
      v.ul64 = *(uint64_t*)d;
      break;
    case GD_INT64:
      v.l64 = *(int64_t*)d;
      break;
    case GD_FLOAT32:
      v.f = *(float*)d;
      break;
    case GD_FLOAT64:
      v.d = *(double*)d;
      break;
    case GD_COMPLEX64:
      fc = *(float complex*)d;
      v.cmp.r = crealf(fc);
      v.cmp.i = cimagf(fc);
      break;
    case GD_COMPLEX128:
      dc = *(double complex*)d;
      v.cmp.r = creal(dc);
      v.cmp.i = cimag(dc);
      break;
    case GD_NULL:
    case GD_STRING:
    case GD_UNKNOWN:
      ;
  }

  dreturnvoid();
  return v;
}

/* convert an ALLTYPES to a value suitable for GetData -- all we do is 
 * reference the appropriate member */
gd_static_inline_ const void* gdidl_from_alltypes(UCHAR t, IDL_ALLTYPES* v)
{
  static float complex fc;
  static double complex dc;

  switch(t)
  {
    case IDL_TYP_BYTE:
      return &(v->c);
    case IDL_TYP_UINT:
      return &(v->ui);
    case IDL_TYP_INT:
      return &(v->i);
    case IDL_TYP_ULONG:
      return &(v->ul);
    case IDL_TYP_LONG:
      return &(v->l);
    case IDL_TYP_ULONG64:
      return &(v->ul64);
    case IDL_TYP_LONG64:
      return &(v->l64);
    case IDL_TYP_FLOAT:
      return &(v->f);
    case IDL_TYP_DOUBLE:
      return &(v->d);
    case IDL_TYP_COMPLEX:
      fc = v->cmp.r + _Complex_I * v->cmp.i;
      return &fc;
    case IDL_TYP_DCOMPLEX:
      dc = v->dcmp.r + _Complex_I * v->dcmp.i;
      return &dc;
  }

  return NULL;
}

/* copy (and convert) an array of complex values */
gd_static_inline_ void gdidl_cmp_to_c99(double complex* dest, IDL_COMPLEX* src,
    size_t n)
{
  dtrace("%p, %p, %zi", dest, src, n);

  size_t i;

  for (i = 0; i < n; ++i)
    dest[i] = src[i].r + _Complex_I * src[i].i;

  dreturnvoid();
}

/* copy (and convert) an array of complex values */
gd_static_inline_ void gdidl_dcmp_to_c99(double complex* dest,
    IDL_DCOMPLEX* src, size_t n)
{
  dtrace("%p, %p, %zi", dest, src, n);

  size_t i;

  for (i = 0; i < n; ++i)
    dest[i] = src[i].r + _Complex_I * src[i].i;

  dreturnvoid();
}

/* copy (and convert) an array of complex values */
gd_static_inline_ void gdidl_c99_to_dcmp(IDL_DCOMPLEX* dest,
    const double complex* src, size_t n)
{
  dtrace("%p, %p, %zi", dest, src, n);
  size_t i;

  for (i = 0; i < n; ++i) {
    dest[i].r = creal(src[i]);
    dest[i].i = cimag(src[i]);
  }

  dreturnvoid();
}

static double complex gdidl_dcomplexScalar(IDL_VPTR obj)
{
  double r = 0, i = 0;

  dtrace("%p", obj);

  /* accept either a scalar or a single element array */
  if (obj->flags & IDL_V_ARR) {
    if (obj->value.arr->n_dim != 1 || obj->value.arr->dim[0] != 1)
      GDIDL_ABORT("Scalar or single element array expected where multiple "
          "element array found");

    if (obj->type == IDL_TYP_DCOMPLEX) {
      r = ((IDL_DCOMPLEX *)obj->value.arr->data)[0].r;
      i = ((IDL_DCOMPLEX *)obj->value.arr->data)[0].i;
    } else if (obj->type == IDL_TYP_COMPLEX) {
      r = ((IDL_COMPLEX *)obj->value.arr->data)[0].r;
      i = ((IDL_COMPLEX *)obj->value.arr->data)[0].i;
    } else
      GDIDL_ABORT("complex value expected");
  } else {
    IDL_ENSURE_SCALAR(obj);

    if (obj->type == IDL_TYP_DCOMPLEX) {
      r = obj->value.dcmp.r;
      i = obj->value.dcmp.i;
    } else if (obj->type == IDL_TYP_COMPLEX) {
      r = obj->value.cmp.r;
      i = obj->value.cmp.i;
    } else
      GDIDL_ABORT("complex value expected");
  }

  dreturn("%g; %g", r, i);
  return r + _Complex_I * i;
}

/* convert a gd_entry_t to an IDL GD_ENTRY struct in a temporary variable */
IDL_VPTR gdidl_make_idl_entry(const gd_entry_t* E)
{
  dtrace("%p", E);

  int i;
  IDL_MEMINT dims[] = { 1 };
  IDL_VPTR r;
  void* data = IDL_MakeTempStruct(gdidl_entry_def, 1, dims, &r, IDL_TRUE);

  /* Here we make labourious calls to StructTagInfoByName becuase we don't
   * want to assume anything about the structure packing details of the IDL */

  IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "FIELD", IDL_MSG_LONGJMP, NULL)), E->field);
  *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "FIELD_TYPE",
        IDL_MSG_LONGJMP, NULL)) = E->field_type;
  *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "FRAGMENT",
        IDL_MSG_LONGJMP, NULL)) = E->fragment_index;

  /* the common IN_FIELDS case */
  if (E->field_type == GD_BIT_ENTRY || E->field_type == GD_LINTERP_ENTRY
      || E->field_type == GD_MULTIPLY_ENTRY || E->field_type == GD_PHASE_ENTRY
      || E->field_type == GD_SBIT_ENTRY || E->field_type == GD_POLYNOM_ENTRY
      || E->field_type == GD_DIVIDE_ENTRY || E->field_type == GD_RECIP_ENTRY
      || E->field_type == GD_WINDOW_ENTRY || E->field_type == GD_MPLEX_ENTRY
      || E->field_type == GD_INDIR_ENTRY || E->field_type == GD_SINDIR_ENTRY)
  {
    IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "IN_FIELDS", IDL_MSG_LONGJMP, NULL)), E->in_fields[0]);
  }

  *(IDL_UINT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
        "FLAGS", IDL_MSG_LONGJMP, NULL)) = E->flags;

  switch (E->field_type)
  {
    case GD_RAW_ENTRY:
      *(IDL_UINT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "SPF",
            IDL_MSG_LONGJMP, NULL)) = E->spf;
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "DATA_TYPE",
            IDL_MSG_LONGJMP, NULL)) = E->data_type;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      *(int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "SCALAR_IND",
            IDL_MSG_LONGJMP, NULL)) = (int16_t)E->scalar_ind[0];
      break;
    case GD_LINCOM_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "N_FIELDS", IDL_MSG_LONGJMP, NULL)) = E->n_fields;
      for (i = 0; i < E->n_fields; ++i) {
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "IN_FIELDS",
                IDL_MSG_LONGJMP, NULL)) + i, E->in_fields[i]);
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "SCALAR",
                IDL_MSG_LONGJMP, NULL)) + i, E->scalar[i]);
        ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[i] =
          (int16_t)E->scalar_ind[i];
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "SCALAR",
                IDL_MSG_LONGJMP, NULL)) + i + GD_MAX_LINCOM,
            E->scalar[i + GD_MAX_LINCOM]);
        ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[i + GD_MAX_LINCOM] =
          (int16_t)E->scalar_ind[i + GD_MAX_LINCOM];
      }
      gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
            IDL_StructTagInfoByName(gdidl_entry_def, "CM", IDL_MSG_LONGJMP,
              NULL)), E->cm, E->n_fields);
      gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
            IDL_StructTagInfoByName(gdidl_entry_def, "CB", IDL_MSG_LONGJMP,
              NULL)), E->cb, E->n_fields);
      memcpy(data + IDL_StructTagInfoByName(gdidl_entry_def, "M",
            IDL_MSG_LONGJMP, NULL), E->m, E->n_fields *
          sizeof(double));
      memcpy(data + IDL_StructTagInfoByName(gdidl_entry_def, "B",
            IDL_MSG_LONGJMP, NULL), E->b, E->n_fields *
          sizeof(double));
      break;
    case GD_LINTERP_ENTRY:
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "TABLE", IDL_MSG_LONGJMP, NULL)), E->table);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "BITNUM",
            IDL_MSG_LONGJMP, NULL)) = E->bitnum;
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "NUMBITS",
            IDL_MSG_LONGJMP, NULL)) = E->numbits;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[0] = (int16_t)E->scalar_ind[0];
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)) + 1, E->scalar[1]);
      ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[1] = (int16_t)E->scalar_ind[1];
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "IN_FIELDS", IDL_MSG_LONGJMP, NULL)) + 1, E->in_fields[1]);
      break;
    case GD_RECIP_ENTRY:
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[0] = (int16_t)E->scalar_ind[0];

      gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
            IDL_StructTagInfoByName(gdidl_entry_def, "CDIVIDEND",
              IDL_MSG_LONGJMP, NULL)), &E->cdividend, 1);
      *(double*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "DIVIDEND",
            IDL_MSG_LONGJMP, NULL)) = E->dividend;
      break;
    case GD_PHASE_ENTRY:
      *(IDL_LONG*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "SHIFT",
            IDL_MSG_LONGJMP, NULL)) = E->shift;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[0] = (int16_t)E->scalar_ind[0];
      break;
    case GD_POLYNOM_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "POLY_ORD", IDL_MSG_LONGJMP, NULL)) = E->poly_ord;

      for (i = 0; i <= E->poly_ord; ++i) {
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "SCALAR",
                IDL_MSG_LONGJMP, NULL)) + i, E->scalar[i]);
        ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[i] =
          (int16_t)E->scalar_ind[i];
      }

      gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
            IDL_StructTagInfoByName(gdidl_entry_def, "CA", IDL_MSG_LONGJMP,
              NULL)), E->ca, E->poly_ord + 1);
      memcpy(data + IDL_StructTagInfoByName(gdidl_entry_def, "A",
            IDL_MSG_LONGJMP, NULL), E->a, (E->poly_ord + 1) * sizeof(double));
      break;
    case GD_WINDOW_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "WINDOP", IDL_MSG_LONGJMP, NULL)) = E->windop;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "IN_FIELDS", IDL_MSG_LONGJMP, NULL)) + 1, E->in_fields[1]);

      switch (E->windop) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          *(IDL_LONG64*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
                "ITHRESHOLD", IDL_MSG_LONGJMP, NULL)) = E->threshold.i;
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          *(IDL_ULONG*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
                "UTHRESHOLD", IDL_MSG_LONGJMP, NULL)) = E->threshold.u;
          break;
        default:
          *(double*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
                "RTHRESHOLD", IDL_MSG_LONGJMP, NULL)) = E->threshold.r;
          break;
      }

      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[0] = (int16_t)E->scalar_ind[0];

      break;
    case GD_MPLEX_ENTRY:
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "IN_FIELDS", IDL_MSG_LONGJMP, NULL)) + 1, E->in_fields[1]);

      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "COUNT_VAL", IDL_MSG_LONGJMP, NULL)) = E->count_val;
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "PERIOD", IDL_MSG_LONGJMP, NULL)) = E->period;

      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[0] = (int16_t)E->scalar_ind[0];
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)) + 1, E->scalar[1]);
      ((int16_t*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
          "SCALAR_IND", IDL_MSG_LONGJMP, NULL)))[1] = (int16_t)E->scalar_ind[1];
      break;
    case GD_CARRAY_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "DATA_TYPE",
            IDL_MSG_LONGJMP, NULL)) = E->const_type;
      /* Fallthrough */
    case GD_SARRAY_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "ARRAY_LEN",
            IDL_MSG_LONGJMP, NULL)) = E->array_len;
      break;
    case GD_CONST_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "DATA_TYPE",
            IDL_MSG_LONGJMP, NULL)) = E->const_type;
      break;
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      break;
  }

  dreturn("%p", r);
  return r;
}

#define GDIDL_GET_TAG(v,tag,miss_ok,d) \
  IDL_StructTagInfoByName((v)->value.s.sdef, tag, \
      (miss_ok) ? IDL_MSG_RET | IDL_MSG_ATTR_NOPRINT : IDL_MSG_LONGJMP, &(d))

#define GDIDL_CCONVERT_ENTRY_ELEMENT(dstp,src,key,offset,type) \
  do { \
    switch (type) { \
      case IDL_TYP_BYTE:     (dstp)[0] = ((  int8_t*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_UINT:     (dstp)[0] = ((uint16_t*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_INT:      (dstp)[0] = (( int16_t*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_ULONG:    (dstp)[0] = ((uint32_t*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_LONG:     (dstp)[0] = (( int32_t*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_ULONG64:  (dstp)[0] = ((uint64_t*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_LONG64:   (dstp)[0] = (( int64_t*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_FLOAT:    (dstp)[0] = ((   float*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_DOUBLE:   (dstp)[0] = ((  double*)(src))[offset]; \
                             (dstp)[1] = 0; break; \
      case IDL_TYP_COMPLEX:  (dstp)[0] = (( IDL_COMPLEX*)(src))[offset].r; \
                             (dstp)[1] = (( IDL_COMPLEX*)(src))[offset].i; \
      break; \
      case IDL_TYP_DCOMPLEX: (dstp)[0] = ((IDL_DCOMPLEX*)(src))[offset].r; \
                             (dstp)[1] = ((IDL_DCOMPLEX*)(src))[offset].i; \
      break; \
      default: GDIDL_ABORT2("GD_ENTRY element %s must be of numeric type", \
                   key); \
    } \
  } while(0)

#define GDIDL_CONVERT_ENTRY_ELEMENT(dst,src,key,offset,type) \
  do { \
    switch (type) { \
      case IDL_TYP_BYTE:     (dst) = ((  int8_t*)(src))[offset]; break; \
      case IDL_TYP_UINT:     (dst) = ((uint16_t*)(src))[offset]; break; \
      case IDL_TYP_INT:      (dst) = (( int16_t*)(src))[offset]; break; \
      case IDL_TYP_ULONG:    (dst) = ((uint32_t*)(src))[offset]; break; \
      case IDL_TYP_LONG:     (dst) = (( int32_t*)(src))[offset]; break; \
      case IDL_TYP_ULONG64:  (dst) = ((uint64_t*)(src))[offset]; break; \
      case IDL_TYP_LONG64:   (dst) = (( int64_t*)(src))[offset]; break; \
      case IDL_TYP_FLOAT:    (dst) = ((   float*)(src))[offset]; break; \
      case IDL_TYP_DOUBLE:   (dst) = ((  double*)(src))[offset]; break; \
      case IDL_TYP_COMPLEX:  (dst) = (( IDL_COMPLEX*)(src))[offset].r; break; \
      case IDL_TYP_DCOMPLEX: (dst) = ((IDL_DCOMPLEX*)(src))[offset].r; break; \
      default: GDIDL_ABORT2("GD_ENTRY element %s must be of numeric type",key);\
    } \
  } while(0)

/* convert structure data */
static void gdidl_convert_entry_num(void *dst, gd_type_t dtype, const char *key,
    const unsigned char *src, int offset, int stype)
{
  dtrace("%p, 0x%X, \"%s\", %p, %i, %i", dst, dtype, key, src, offset, stype);

  switch (dtype) {
    case GD_NULL:
    case GD_UNKNOWN:
    case GD_STRING:
      break;
    case GD_UINT8:
      GDIDL_CONVERT_ENTRY_ELEMENT(*(( uint8_t*)dst), src, key, offset, stype);
      break;
    case GD_INT8:
      GDIDL_CONVERT_ENTRY_ELEMENT(*((  int8_t*)dst), src, key, offset, stype);
      break;
    case GD_UINT16:
      GDIDL_CONVERT_ENTRY_ELEMENT(*((uint16_t*)dst), src, key, offset, stype);
      break;
    case GD_INT16:
      GDIDL_CONVERT_ENTRY_ELEMENT(*(( int16_t*)dst), src, key, offset, stype);
      break;
    case GD_UINT32:
      GDIDL_CONVERT_ENTRY_ELEMENT(*((uint32_t*)dst), src, key, offset, stype);
      break;
    case GD_INT32:
      GDIDL_CONVERT_ENTRY_ELEMENT(*(( int32_t*)dst), src, key, offset, stype);
      break;
    case GD_UINT64:
      GDIDL_CONVERT_ENTRY_ELEMENT(*((uint64_t*)dst), src, key, offset, stype);
      break;
    case GD_INT64:
      GDIDL_CONVERT_ENTRY_ELEMENT(*(( int64_t*)dst), src, key, offset, stype);
      break;
    case GD_FLOAT32:
      GDIDL_CONVERT_ENTRY_ELEMENT(*((   float*)dst), src, key, offset, stype);
      break;
    case GD_FLOAT64:
      GDIDL_CONVERT_ENTRY_ELEMENT(*((  double*)dst), src, key, offset, stype);
      break;
    case GD_COMPLEX64:
      GDIDL_CCONVERT_ENTRY_ELEMENT(     (float*)dst, src, key, offset, stype);
      break;
    case GD_COMPLEX128:
      GDIDL_CCONVERT_ENTRY_ELEMENT(    (double*)dst, src, key, offset, stype);
      break;
  }

  dreturnvoid();
}

/* get a scalar */
static int gdidl_get_entry_num(void *dst, gd_type_t type, IDL_VPTR v, char *key,
    int miss_ok)
{
  IDL_VPTR d;

  dtrace("%p, 0x%X, %p, \"%s\", %i", dst, type, v, key, miss_ok);

  const unsigned char* data = v->value.s.arr->data;
  IDL_MEMINT o = GDIDL_GET_TAG(v, key, miss_ok, d);

  if (o == -1) {
    dreturn("%i", 0);
    return 0;
  }

  /* filter out bad things */
  if (d->flags & IDL_V_ARR) {
    IDL_ARRAY* arr = (IDL_ARRAY*)(data + o);
    if (arr->n_elts > 1)
      GDIDL_ABORT2("GD_ENTRY element %s must be of scalar type", key);
  }

  gdidl_convert_entry_num(dst, type, key, data + o, 0, d->type);

  dreturn("%i", 1);
  return 1;
}

/* convert the entry scalar and scalar_ind arrays */
static unsigned gdidl_get_entry_scalars(gd_entry_t *E, unsigned mask,
    IDL_VPTR v)
{
  int i, n_scalar, n_scalar_ind;
  unsigned mask_out = 0;
  IDL_MEMINT o_scalar, o_scalar_ind;
  IDL_VPTR d_scalar, d_scalar_ind;

  unsigned char* data = v->value.s.arr->data;

  dtrace("%p, 0x%X, %p", E, mask, v);

  /* find the data */
  o_scalar = GDIDL_GET_TAG(v, "SCALAR", 1, d_scalar);
  if (o_scalar == -1) {
    dreturn("%i", 0);
    return 0;
  } else if (d_scalar->type != IDL_TYP_STRING)
    GDIDL_ABORT("GD_ENTRY element SCALAR must be an array of strings");
 
  if (d_scalar->flags & IDL_V_ARR) {
    n_scalar = d_scalar->value.arr->n_elts;

    if (n_scalar > GD_MAX_POLYORD + 1)
      n_scalar = GD_MAX_POLYORD + 1;
  } else
    n_scalar = 1;

  o_scalar_ind = GDIDL_GET_TAG(v, "SCALAR_IND", 1, d_scalar_ind);

  if (o_scalar_ind != -1) {
    if (d_scalar_ind->flags & IDL_V_ARR)
      n_scalar_ind = d_scalar_ind->value.arr->n_elts;
    else
      n_scalar_ind = 1;
  } else
    n_scalar_ind = 0;

  for (i = 0; i < n_scalar; ++i) {
    if (!(mask & (1 << i)))
      continue;

    E->scalar[i] = IDL_STRING_STR((IDL_STRING*)(data + o_scalar) + i);

    if (E->scalar[i] && E->scalar[i][0] == '\0') {
      E->scalar[i] = NULL;
      continue;
    }

    if (i < n_scalar_ind)
      gdidl_convert_entry_num(E->scalar_ind + i, GD_INT_TYPE, "SCALAR_IND",
          data + o_scalar_ind, i, d_scalar_ind->type);

    mask_out |= (1 << i);
  }

  dreturn("0x%X", mask_out);
  return mask_out;
}

static int gdidl_get_in_fields(char **in_fields, IDL_VPTR v, int min, int max,
    int miss_ok)
{
  int i, n;
  IDL_MEMINT o;
  IDL_VPTR d;
  unsigned char* data = v->value.s.arr->data;

  dtrace("%p, %p, %i, %i, %i", in_fields, v, min, max, miss_ok);

  o = GDIDL_GET_TAG(v, "IN_FIELDS", miss_ok, d);

  if (o == -1) {
    dreturn("%i", 0);
    return 0;
  }

  IDL_ENSURE_STRING(d);

  if (d->flags & IDL_V_ARR)
    n = d->value.arr->n_elts;
  else
    n = 1;

  if (n < min || n > max)
    GDIDL_ABORT2("Bad length (%i) for IN_FIELDS in GD_ENTRY", n);

  for (i = 0; i < n; ++i)
    in_fields[i] = IDL_STRING_STR((IDL_STRING*)(data + o) + i);

  dreturn("%i", n);
  return n;
}

static int gdidl_get_entry_cmparr(double complex *c, IDL_VPTR v, char key,
    int min, int max, unsigned mask, int miss_ok)
{
  int n, i;
  IDL_MEMINT o;
  IDL_VPTR d;
  unsigned char* data = v->value.s.arr->data;
  char ckey[3] = { 'C', key, 0 };

  dtrace("%p, %p, '%c', %i, %i, 0x%X, %i", c, v, key, min, max, mask, miss_ok);

  /* try key "<c>" before key "C<c>" */
  o = GDIDL_GET_TAG(v, ckey + 1, 1, d);
  if (o == -1) {
    o = GDIDL_GET_TAG(v, ckey, miss_ok, d);

    if (o == -1) {
      dreturn("%i", 0);
      return 0;
    }
  }

  if (d->flags & IDL_V_ARR)
    n = d->value.arr->n_elts;
  else
    n = 1;

  for (i = 0; i < n; ++i)
    gdidl_convert_entry_num(c + i, GD_COMPLEX128, ckey + 1, data + o, i,
        d->type);

  /* add trailing named scalars */
  for (; n < GD_MAX_POLYORD + 1; ++n)
    if (!(mask & (1 << n)))
      break;

  if (n < min || n > max)
    GDIDL_ABORT2("Bad length (%i) for %c in GD_ENTRY", n, key);

  dreturn("%i", n);
  return n;
}

/* convert an IDL structure into an gd_entry_t */
static void gdidl_read_idl_entry(gd_entry_t *E, IDL_VPTR v, int no_fragment,
    const gd_entry_t *old_E)
{
  /* this function is fairly agnostic about the structure it's given: so
   * long as it gets a structure with the fields it wants (of the right type)
   * it's happy */

  dtrace("%p, %p, %i, %p", E, v, no_fragment, old_E);

  IDL_VPTR d;
  IDL_MEMINT o;
  int n = 0, min, max;
  const int miss_ok = (old_E != NULL);
  unsigned mask, tmask;

  if (old_E)
    memcpy(E, old_E, sizeof(gd_entry_t));
  else
    memset(E, 0, sizeof(gd_entry_t));

  unsigned char* data = v->value.s.arr->data;

  if (!miss_ok) {
    /* field */
    o = GDIDL_GET_TAG(v, "FIELD", 0, d);
    IDL_ENSURE_STRING(d);
    E->field = IDL_STRING_STR((IDL_STRING*)(data + o));
  }

  if (!no_fragment) /* fragment_index */
    gdidl_get_entry_num(&E->fragment_index, GD_INT_TYPE, v, "FRAGMENT", 0);

  /* field_type */
  gdidl_get_entry_num(&E->field_type, GD_INT_TYPE, v, "FIELD_TYPE", 0);

  switch (E->field_type)
  {
    case GD_RAW_ENTRY:
      gdidl_get_entry_num(&E->data_type, GD_INT_TYPE, v, "DATA_TYPE", miss_ok);

      mask = gdidl_get_entry_scalars(E, 1, v);

      if (!(mask & 1))
        gdidl_get_entry_num(&E->spf, GD_UINT_TYPE, v, "SPF", miss_ok);

      break;
    case GD_LINCOM_ENTRY:
      if (gdidl_get_entry_num(&n, GD_INT_TYPE, v, "N_FIELDS", 1))
        min = max = n;
      else {
        min = 1;
        max = GD_MAX_LINCOM;
      }

      E->n_fields = gdidl_get_in_fields((char**)E->in_fields, v, min, max,
          miss_ok);

      if (E->n_fields > 0)
        min = max = E->n_fields;

      E->flags |= GD_EN_COMPSCAL;

      tmask = (1 << max) - 1;
      mask = gdidl_get_entry_scalars(E, 9 * tmask, v);

      if ((mask & tmask) != tmask) {
        E->n_fields = gdidl_get_entry_cmparr(E->cm, v, 'M', min, max,
            mask & tmask, miss_ok);

        if (E->n_fields > 0)
          min = max = E->n_fields;
      }

      if (((mask >> GD_MAX_LINCOM) & tmask) != tmask)
        E->n_fields = gdidl_get_entry_cmparr(E->cb, v, 'B', min, max,
            (mask >> GD_MAX_LINCOM) & tmask, miss_ok);
      else
        E->n_fields = max;

      break;
    case GD_LINTERP_ENTRY:
      gdidl_get_in_fields((char**)E->in_fields, v, 1, 1, miss_ok);
      o = GDIDL_GET_TAG(v, "TABLE", miss_ok, d);
      if (o != -1) {
        IDL_ENSURE_STRING(d);
        E->table = IDL_STRING_STR((IDL_STRING*)(data + o));
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      gdidl_get_in_fields((char**)E->in_fields, v, 1, 1, miss_ok);

      mask = gdidl_get_entry_scalars(E, 0x3, v);

      if (!(mask & 0x1))
        gdidl_get_entry_num(&E->bitnum, GD_INT_TYPE, v, "BITNUM", miss_ok);

      if (!(mask & 0x3)) {
        if (!gdidl_get_entry_num(&E->numbits, GD_INT_TYPE, v, "NUMBITS", 1))
          if (!miss_ok)
            E->numbits = 1;
      }

      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      gdidl_get_in_fields((char**)E->in_fields, v, 2, 2, miss_ok);
      break;
    case GD_RECIP_ENTRY:
      gdidl_get_in_fields((char**)E->in_fields, v, 1, 1, miss_ok);
      E->flags |= GD_EN_COMPSCAL;

      mask = gdidl_get_entry_scalars(E, 0x1, v);

      if (!(mask & 1))
        if (gdidl_get_entry_num(&E->cdividend, GD_COMPLEX128, v, "CDIVIDEND",
              1) == 0)
        {
          gdidl_get_entry_num(&E->cdividend, GD_COMPLEX128, v, "DIVIDEND",
              miss_ok);
        }
      break;
    case GD_PHASE_ENTRY:
      gdidl_get_in_fields((char**)E->in_fields, v, 1, 1, miss_ok);

      mask = gdidl_get_entry_scalars(E, 0x1, v);

      if (!(mask & 1))
        gdidl_get_entry_num(&E->shift, GD_INT64, v, "SHIFT", miss_ok);
      break;
    case GD_POLYNOM_ENTRY:
      if (gdidl_get_entry_num(&n, GD_INT_TYPE, v, "POLY_ORD", 1))
        min = max = n + 1;
      else {
        min = 2;
        max = GD_MAX_POLYORD + 1;
      }

      gdidl_get_in_fields((char**)E->in_fields, v, 1, 1, miss_ok);

      tmask = (1 << max) - 1;
      mask = gdidl_get_entry_scalars(E, tmask, v);

      E->flags |= GD_EN_COMPSCAL;

      if ((mask & tmask) != tmask)
        E->poly_ord = gdidl_get_entry_cmparr(E->ca, v, 'A', min, max, mask,
            miss_ok) - 1;
      else
        E->poly_ord = max - 1;
      break;
    case GD_WINDOW_ENTRY:
      gdidl_get_in_fields((char**)E->in_fields, v, 2, 2, miss_ok);
      gdidl_get_entry_num(&E->windop, GD_INT_TYPE, v, "WINDOP", miss_ok);

      mask = gdidl_get_entry_scalars(E, 0x1, v);

      if (!(mask & 1))
        switch (E->windop) {
          case GD_WINDOP_EQ:
          case GD_WINDOP_NE:
            if (gdidl_get_entry_num(&E->threshold.i, GD_INT64, v, "ITHRESHOLD",
                  1) == 0)
            {
              gdidl_get_entry_num(&E->threshold.i, GD_INT64, v, "THRESHOLD",
                  miss_ok);
            }
            break;
          case GD_WINDOP_SET:
          case GD_WINDOP_CLR:
            if (gdidl_get_entry_num(&E->threshold.u, GD_UINT64, v, "UTHRESHOLD",
                  1) == 0)
            {
              gdidl_get_entry_num(&E->threshold.u, GD_UINT64, v, "THRESHOLD",
                  miss_ok);
            }
            break;
          default:
            if (gdidl_get_entry_num(&E->threshold.r, GD_FLOAT64, v,
                  "RTHRESHOLD", 1) == 0)
            {
              gdidl_get_entry_num(&E->threshold.r, GD_FLOAT64, v, "THRESHOLD",
                  miss_ok);
            }
            break;
        }
      break;
    case GD_MPLEX_ENTRY:
      gdidl_get_in_fields((char**)E->in_fields, v, 2, 2, miss_ok);

      mask = gdidl_get_entry_scalars(E, 0x3, v);

      if (!(mask & 1))
        gdidl_get_entry_num(&E->count_val, GD_INT_TYPE, v, "COUNT_VAL",
            miss_ok);

      if (!(mask & 3))
        gdidl_get_entry_num(&E->period, GD_INT_TYPE, v, "PERIOD", 1);
      break;
    case GD_CARRAY_ENTRY:
      gdidl_get_entry_num(&E->const_type, GD_INT_TYPE, v, "DATA_TYPE", miss_ok);
      /* FALLTHROUGH */
    case GD_SARRAY_ENTRY:
      gdidl_get_entry_num(&E->array_len, GD_INT_TYPE, v, "ARRAY_LEN", miss_ok);
      break;
    case GD_CONST_ENTRY:
      gdidl_get_entry_num(&E->const_type, GD_INT_TYPE, v, "DATA_TYPE", miss_ok);
      break;
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      break;
  }

  dreturnvoid();
}

/* convert an IDL string or numerical encoding key to a GetData flag */
unsigned long gdidl_convert_encoding(IDL_VPTR idl_enc)
{
  dtrace("%p", idl_enc);

  unsigned long encoding = 0;

  IDL_ENSURE_SIMPLE(idl_enc);
  if (idl_enc->type == IDL_TYP_STRING) {
    const char* enc = IDL_VarGetString(idl_enc);
    if (strcasecmp(enc, "BZIP2"))
      encoding = GD_BZIP2_ENCODED;
    else if (strcasecmp(enc, "GZIP"))
      encoding = GD_GZIP_ENCODED;
    else if (strcasecmp(enc, "FLAC"))
      encoding = GD_FLAC_ENCODED;
    else if (strcasecmp(enc, "LZMA"))
      encoding = GD_LZMA_ENCODED;
    else if (strcasecmp(enc, "SLIM"))
      encoding = GD_SLIM_ENCODED;
    else if (strcasecmp(enc, "SIE"))
      encoding = GD_SIE_ENCODED;
    else if (strcasecmp(enc, "TEXT"))
      encoding = GD_TEXT_ENCODED;
    else if (strcasecmp(enc, "ZZIP"))
      encoding = GD_ZZIP_ENCODED;
    else if (strcasecmp(enc, "ZZSLIM"))
      encoding = GD_ZZSLIM_ENCODED;
    else if (strcasecmp(enc, "NONE"))
      encoding = GD_UNENCODED;
    else if (strcasecmp(enc, "RAW"))
      encoding = GD_UNENCODED;
    else if (strcasecmp(enc, "UNENCODED"))
      encoding = GD_UNENCODED;
    else if (strcasecmp(enc, "AUTO"))
      encoding = GD_AUTO_ENCODED;
    else
      GDIDL_ABORT("Unknown encoding type.");
  } else
    encoding = IDL_LongScalar(idl_enc);

  dreturn("%lx", encoding);
  return encoding;
}


/* The public subroutines begin here.  The `DLM' lines are magical. */

/* @@DLM: F gdidl_dirfilename GD_DIRFILENAME 1 1 KEYWORDS */
IDL_VPTR gdidl_dirfilename(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  const char* name = gd_dirfilename(D);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING((char*)name);
  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_add GD_ADD 2 2 KEYWORDS */
void gdidl_add(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gd_entry_t E;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  IDL_ENSURE_STRUCTURE(argv[1]);
  gdidl_read_idl_entry(&E, argv[1], kw.parent_x, NULL);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd(D, &E, parent);
  } else
    gd_add(D, &E);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_bit GD_ADD_BIT 3 3 KEYWORDS */
void gdidl_add_bit(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int bitnum;
    int numbits;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.bitnum = kw.fragment_index = 0;
  kw.parent_x = 0;
  kw.numbits = 1;

  static IDL_KW_PAR kw_pars[] = {
    { "BITNUM", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(bitnum) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "NUMBITS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(numbits) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field = IDL_VarGetString(argv[2]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_bit(D, parent, field_code, in_field, kw.bitnum, kw.numbits);
  } else
    gd_add_bit(D, field_code, in_field, kw.bitnum, kw.numbits,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_const GD_ADD_CONST 2 2 KEYWORDS */
void gdidl_add_const(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  double zero = 0;
  gd_type_t data_type = GD_FLOAT64;
  const void *data = &zero;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int const_type;
    IDL_VPTR value;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.value = NULL;
  kw.fragment_index = 0;
  kw.parent_x = 0;
  kw.const_type = GD_FLOAT64;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(const_type) },
    { "VALUE", 0, 1, IDL_KW_VIN, 0, IDL_KW_OFFSETOF(value) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  if (kw.value) {
    data = gdidl_from_alltypes(kw.value->type, &kw.value->value);
    data_type = gdidl_gd_type(kw.value->type);
  }

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_const(D, parent, field_code, kw.const_type, data_type, data);
  } else
    gd_add_const(D, field_code, kw.const_type, data_type, data,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_carray GD_ADD_CARRAY 2 2 KEYWORDS */
void gdidl_add_carray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gd_type_t data_type = GD_INT8;
  void *data = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int const_type;
    int n;
    IDL_VPTR value;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.value = NULL;
  kw.fragment_index = kw.n = 0;
  kw.parent_x = 0;
  kw.const_type = GD_FLOAT64;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "LENGTH", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(n) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(const_type) },
    { "VALUE", 0, 1, IDL_KW_VIN, 0, IDL_KW_OFFSETOF(value) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  if (kw.value) {
    IDL_ENSURE_ARRAY(kw.value);
    if (kw.value->value.arr->n_dim != 1)
      GDIDL_KW_ABORT("VALUE must be a vector, not a multidimensional array");
    data = (void *)kw.value->value.arr->data;
    data_type = gdidl_gd_type(kw.value->type);
    kw.n = kw.value->value.arr->n_elts;
  } else if (kw.n) {
    data = malloc(kw.n);
    memset(data, 0, kw.n);
  } else
    GDIDL_KW_ABORT("either LENGTH or VALUE must be specified");

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_carray(D, parent, field_code, kw.const_type, kw.n, data_type, data);
  } else
    gd_add_carray(D, field_code, kw.const_type, kw.n, data_type, data,
        kw.fragment_index);

  if (!kw.value)
    free(data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_lincom GD_ADD_LINCOM 5 11 KEYWORDS */
void gdidl_add_lincom(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  int i, comp_scal = 0;
  const char* in_field[3];
  double m[3];
  double b[3];
  double complex cm[3];
  double complex cb[3];

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]); 
  int n_fields = (argc - 2) / 3; /* IDL's runtime check on # of args should
                                    ensure this is 1, 2, or 3. */

  for (i = 0; i < n_fields; ++i) {
    in_field[i] = IDL_VarGetString(argv[2 + i * 3]);
    if (argv[3 + i * 3]->type == IDL_TYP_DCOMPLEX ||
        argv[3 + i * 3]->type == IDL_TYP_COMPLEX)
    {
      comp_scal = 1;
      m[i] = cm[i] = gdidl_dcomplexScalar(argv[3 + i * 3]);
    } else
      cm[i] = m[i] = IDL_DoubleScalar(argv[3 + i * 3]);

    if (argv[4 + i * 3]->type == IDL_TYP_DCOMPLEX ||
        argv[4 + i * 3]->type == IDL_TYP_COMPLEX)
    {
      comp_scal = 1;
      b[i] = cb[i] = gdidl_dcomplexScalar(argv[4 + i * 3]);
    } else
      cb[i] = b[i] = IDL_DoubleScalar(argv[4 + i * 3]);
  }

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    if (comp_scal) 
      gd_madd_clincom(D, parent, field_code, n_fields, in_field, cm, cb);
    else
      gd_madd_lincom(D, parent, field_code, n_fields, in_field, m, b);
  } else if (comp_scal) 
    gd_add_clincom(D, field_code, n_fields, in_field, cm, cb,
        kw.fragment_index);
  else
    gd_add_lincom(D, field_code, n_fields, in_field, m, b,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_linterp GD_ADD_LINTERP 4 4 KEYWORDS */
void gdidl_add_linterp(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field = IDL_VarGetString(argv[2]);
  const char* table = IDL_VarGetString(argv[3]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_linterp(D, parent, field_code, in_field, table);
  } else
    gd_add_linterp(D, field_code, in_field, table, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

static void gdidl_add_yoke_(gd_type_t type, int argc, IDL_VPTR argv[],
    char *argk)
{
  dtrace("0x%X, %i, %p, %p", type, argc, argv, argk);

  gd_entry_t E;
  memset(&E, 0, sizeof(E));

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  E.field = IDL_VarGetString(argv[1]);
  E.field_type = type;
  E.in_fields[0] = IDL_VarGetString(argv[2]);
  E.in_fields[1] = IDL_VarGetString(argv[3]);
  E.fragment_index = kw.fragment_index;

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd(D, &E, parent);
  } else
    gd_add(D, &E);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_multiply GD_ADD_MULTIPLY 4 4 KEYWORDS */
void gdidl_add_multiply(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_add_yoke_(GD_MULTIPLY_ENTRY, argc, argv, argk);

  dreturnvoid();
}

/* @@DLM: P gdidl_add_divide GD_ADD_DIVIDE 4 4 KEYWORDS */
void gdidl_add_divide(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_add_yoke_(GD_DIVIDE_ENTRY, argc, argv, argk);

  dreturnvoid();
}

/* @@DLM: P gdidl_add_recip GD_ADD_RECIP 3 3 KEYWORDS */
void gdidl_add_recip(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int comp_scal = 0;
  double complex cdividend = 0;
  double dividend = 1;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_VPTR dividend;
    int dividend_x;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.parent_x = kw.dividend_x = 0;
  kw.dividend = NULL;

  static IDL_KW_PAR kw_pars[] = {
    { "DIVIDEND", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(dividend_x),
      IDL_KW_OFFSETOF(dividend) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field1 = IDL_VarGetString(argv[2]);

  if (kw.dividend_x) {
    if (kw.dividend->type == IDL_TYP_DCOMPLEX ||
        kw.dividend->type == IDL_TYP_COMPLEX)
    {
      comp_scal = 1;
      cdividend = gdidl_dcomplexScalar(kw.dividend);
    } else
      dividend = IDL_DoubleScalar(kw.dividend);
  }

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    if (comp_scal)
      gd_madd_crecip(D, parent, field_code, in_field1, cdividend);
    else
      gd_madd_recip(D, parent, field_code, in_field1, dividend);
  } else if (comp_scal)
    gd_add_crecip(D, field_code, in_field1, cdividend, kw.fragment_index);
  else
    gd_add_recip(D, field_code, in_field1, dividend, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_phase GD_ADD_PHASE 4 4 KEYWORDS */
void gdidl_add_phase(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  kw.fragment_index = 0;
  kw.parent_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field = IDL_VarGetString(argv[2]);
  long shift = IDL_LongScalar(argv[3]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_phase(D, parent, field_code, in_field, shift);
  } else
    gd_add_phase(D, field_code, in_field, shift, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_polynom GD_ADD_POLYNOM 4 9 KEYWORDS */
void gdidl_add_polynom(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  int i, comp_scal = 0;
  double a[GD_MAX_POLYORD + 1];
  double complex ca[GD_MAX_POLYORD + 1];

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field = IDL_VarGetString(argv[2]);

  int poly_ord;
  if (argv[3]->flags & IDL_V_ARR) {
    if (argv[3]->value.arr->n_dim != 1)
      GDIDL_KW_ABORT("The array of coeffecients may only have a single "
          "dimension");

    poly_ord = argv[3]->value.arr->dim[0] - 1;
    if (poly_ord < 1)
      GDIDL_KW_ABORT("The array of coeffecients must have at least two "
          "elements");
    if (poly_ord > GD_MAX_POLYORD)
      poly_ord = GD_MAX_POLYORD;

    for (i = 0; i <= poly_ord; ++i) {
      switch(argv[3]->type)
      {
        case IDL_TYP_BYTE:
          ca[i] = a[i] = ((UCHAR*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_INT:
          ca[i] = a[i] = ((IDL_INT*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_LONG:
          ca[i] = a[i] = ((IDL_LONG*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_FLOAT:
          ca[i] = a[i] = ((float*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_DOUBLE:
          ca[i] = a[i] = ((double*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_UINT:
          ca[i] = a[i] = ((IDL_UINT*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_ULONG:
          ca[i] = a[i] = ((IDL_ULONG*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_LONG64:
          ca[i] = a[i] = ((IDL_LONG64*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_ULONG64:
          ca[i] = a[i] = ((IDL_ULONG64*)(argv[3]->value.arr->data))[i];
          break;
        case IDL_TYP_COMPLEX:
          comp_scal = 1;
          ca[i] = ((IDL_COMPLEX*)(argv[3]->value.arr->data))[i].r
            + _Complex_I * ((IDL_COMPLEX*)(argv[3]->value.arr->data))[i].i;
          break;
        case IDL_TYP_DCOMPLEX:
          comp_scal = 1;
          ca[i] = ((IDL_DCOMPLEX*)(argv[3]->value.arr->data))[i].r
            + _Complex_I * ((IDL_DCOMPLEX*)(argv[3]->value.arr->data))[i].i;
          break;
        default:
          GDIDL_KW_ABORT("The coeffecients must be of scalar type");
      }
    }
  } else {
    poly_ord = argc - 4;

    for (i = 0; i <= poly_ord; ++i)
      if (argv[i + 3]->type == IDL_TYP_COMPLEX) {
        comp_scal = 1;
        ca[i] = argv[i + 3]->value.cmp.r +
          _Complex_I * argv[i + 3]->value.cmp.i;
      } else if (argv[i + 3]->type == IDL_TYP_DCOMPLEX) {
        comp_scal = 1;
        ca[i] = argv[i + 3]->value.dcmp.r +
          _Complex_I * argv[i + 3]->value.dcmp.i;
      } else
        ca[i] = a[i] = IDL_DoubleScalar(argv[i + 3]);
  }

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    if (comp_scal)
      gd_madd_cpolynom(D, parent, field_code, poly_ord, in_field, ca);
    else
      gd_madd_polynom(D, parent, field_code, poly_ord, in_field, a);
  } else if (comp_scal)
    gd_add_cpolynom(D, field_code, poly_ord, in_field, ca,
        kw.fragment_index);
  else
    gd_add_polynom(D, field_code, poly_ord, in_field, a,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_raw GD_ADD_RAW 3 3 KEYWORDS */
void gdidl_add_raw(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    unsigned int spf;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.spf = 1;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "SPF", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(spf) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_add_raw(D, field_code, IDL_LongScalar(argv[2]), kw.spf,
      kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}
/* @@DLM: P gdidl_add_sbit GD_ADD_SBIT 3 3 KEYWORDS */
void gdidl_add_sbit(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int bitnum;
    int numbits;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.bitnum = kw.fragment_index = 0;
  kw.parent_x = 0;
  kw.numbits = 1;

  static IDL_KW_PAR kw_pars[] = {
    { "BITNUM", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(bitnum) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "NUMBITS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(numbits) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field = IDL_VarGetString(argv[2]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_sbit(D, parent, field_code, in_field, kw.bitnum, kw.numbits);
  } else
    gd_add_sbit(D, field_code, in_field, kw.bitnum, kw.numbits,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_spec GD_ADD_SPEC 2 2 KEYWORDS */
void gdidl_add_spec(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* line = IDL_VarGetString(argv[1]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_spec(D, line, parent);
  } else
    gd_add_spec(D, line, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_string GD_ADD_STRING 2 2 KEYWORDS */
void gdidl_add_string(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* str = "";

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING value;
    int value_x;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.value_x = 0;
  kw.parent_x = 0;
  kw.fragment_index = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "VALUE", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(value_x),
      IDL_KW_OFFSETOF(value) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.value_x) 
    str = IDL_STRING_STR(&kw.value);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_string(D, parent, field_code, str);
  } else
    gd_add_string(D, field_code, str, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_bit GD_ALTER_BIT 2 2 KEYWORDS */
void gdidl_alter_bit(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int bitnum;
    int bitnum_x;
    int numbits;
    IDL_STRING in_field;
    int in_field_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.bitnum = 0;
  kw.bitnum_x = 0;
  kw.numbits = 0;
  kw.in_field_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "BITNUM", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(bitnum_x),
      IDL_KW_OFFSETOF(bitnum) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { "NUMBITS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(numbits) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (!kw.bitnum_x)
    kw.bitnum = -1;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.in_field_x)
    in_field = IDL_STRING_STR(&kw.in_field);

  gd_alter_bit(D, field_code, in_field, kw.bitnum, kw.numbits);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_const GD_ALTER_CONST 2 2 KEYWORDS */
void gdidl_alter_const(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int const_type;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.const_type = GD_NULL;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(const_type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_alter_const(D, field_code, kw.const_type);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_carray GD_ALTER_CARRAY 2 2 KEYWORDS */
void gdidl_alter_carray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int len;
    int const_type;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.const_type = GD_NULL;
  kw.len = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "LENGTH", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(len) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(const_type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_alter_carray(D, field_code, kw.const_type, (size_t)kw.len);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_encoding GD_ALTER_ENCODING 2 2 KEYWORDS */
void gdidl_alter_encoding(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    int fragment_index_x;
    int recode;
  } KW_RESULT;
  KW_RESULT kw;

  kw.recode = 0;
  kw.fragment_index = 0;
  kw.fragment_index_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(fragment_index_x),
      IDL_KW_OFFSETOF(fragment_index) },
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (!kw.fragment_index_x)
    kw.fragment_index = GD_ALL_FRAGMENTS;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_alter_encoding(D, gdidl_convert_encoding(argv[1]), kw.fragment_index,
      kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_endianness GD_ALTER_ENDIANNESS 1 1 KEYWORDS */
void gdidl_alter_endianness(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int arm_end;
    int big_end;
    int fragment_index;
    int fragment_index_x;
    int little_end;
    int not_arm_end;
    int recode;
  } KW_RESULT;
  KW_RESULT kw;

  kw.recode = 0;
  kw.fragment_index = 0;
  kw.fragment_index_x = 0;
  kw.arm_end = kw.big_end = kw.little_end = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "ARM_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(arm_end) },
    { "BIG_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(big_end) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(fragment_index_x),
      IDL_KW_OFFSETOF(fragment_index) },
    { "LITTLE_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(little_end) },
    { "NOT_ARM_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(not_arm_end) },
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (!kw.fragment_index_x)
    kw.fragment_index = GD_ALL_FRAGMENTS;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_alter_endianness(D, (kw.big_end ? GD_BIG_ENDIAN : 0) | 
      (kw.little_end ? GD_LITTLE_ENDIAN : 0) | (kw.arm_end ? GD_ARM_ENDIAN : 0)
      | (kw.not_arm_end ? GD_NOT_ARM_ENDIAN : 0), kw.fragment_index, kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_entry GD_ALTER_ENTRY 3 3 KEYWORDS */
void gdidl_alter_entry(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int recode;
  } KW_RESULT;
  KW_RESULT kw;

  gd_entry_t E, old_E;

  kw.recode = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  IDL_ENSURE_STRUCTURE(argv[2]);

  gd_entry(D, field_code, &old_E);
  gd_free_entry_strings(&old_E);

  GDIDL_SET_ERROR(D);

  gdidl_read_idl_entry(&E, argv[2], 1, &old_E);

  gd_alter_entry(D, field_code, &E, kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_frameoffset GD_ALTER_FRAMEOFFSET 2 2 KEYWORDS */
void gdidl_alter_frameoffset(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    int fragment_index_x;
    int recode;
  } KW_RESULT;
  KW_RESULT kw;

  kw.recode = 0;
  kw.fragment_index = 0;
  kw.fragment_index_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(fragment_index_x),
      IDL_KW_OFFSETOF(fragment_index) },
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (!kw.fragment_index_x)
    kw.fragment_index = GD_ALL_FRAGMENTS;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_alter_frameoffset64(D, IDL_Long64Scalar(argv[1]), kw.fragment_index,
      kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_lincom GD_ALTER_LINCOM 2 2 KEYWORDS */
void gdidl_alter_lincom(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_VPTR in_field;
    int in_field_x;
    IDL_VPTR m;
    int m_x;
    IDL_VPTR b;
    int b_x;
    int n_fields;
  } KW_RESULT;
  KW_RESULT kw;

  int i;
  const char* local_in_field[3];
  double* m = NULL;
  double* b = NULL;
  double complex* cm = NULL;
  double complex* cb = NULL;
  const char** in_field = NULL;
  IDL_VPTR tmp_m = NULL;
  IDL_VPTR tmp_b = NULL;
  int comp_scal = 0;

  GDIDL_KW_INIT_ERROR;
  kw.in_field = kw.m = kw.b = NULL;
  kw.in_field_x = kw.m_x = kw.b_x = kw.n_fields = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "B", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(b_x), IDL_KW_OFFSETOF(b) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELDS", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { "M", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(m_x), IDL_KW_OFFSETOF(m) },
    { "N_FIELDS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(n_fields) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  /* check keywords */
  if (kw.in_field_x) {
    IDL_ENSURE_ARRAY(kw.in_field);
    IDL_ENSURE_STRING(kw.in_field);
    if (kw.in_field->value.arr->n_dim != 1)
      GDIDL_KW_ABORT("IN_FIELDS must be a vector");

    if (kw.n_fields == 0) {
      kw.n_fields = kw.in_field->value.arr->dim[0];
      if (kw.n_fields > GD_MAX_LINCOM)
        kw.n_fields = GD_MAX_LINCOM;
    } else if (kw.in_field->value.arr->dim[0] < kw.n_fields) 
      GDIDL_KW_ABORT("Insufficient number of elements in IN_FIELDS");

    for (i = 0; i < kw.n_fields; ++i)
      local_in_field[i] =
        IDL_STRING_STR((IDL_STRING*)(kw.in_field->value.arr->data) + i);
    in_field = local_in_field;
  }

  if (kw.m_x) {
    IDL_ENSURE_ARRAY(kw.m);
    if (kw.n_fields == 0) {
      kw.n_fields = kw.in_field->value.arr->dim[0];
      if (kw.n_fields > GD_MAX_LINCOM)
        kw.n_fields = GD_MAX_LINCOM;
    } else if (kw.m->value.arr->dim[0] < kw.n_fields) 
      GDIDL_KW_ABORT("Insufficient number of elements in M");

    if (kw.m->type == IDL_TYP_COMPLEX || kw.m->type == IDL_TYP_DCOMPLEX) {
      comp_scal = 1;
      cm = malloc(sizeof(double complex) * kw.n_fields);
      if (kw.m->type == IDL_TYP_DCOMPLEX)
        gdidl_dcmp_to_c99(cm, (IDL_DCOMPLEX*)kw.m->value.arr->data,
            kw.n_fields);
      else
        gdidl_cmp_to_c99(cm, (IDL_COMPLEX*)kw.m->value.arr->data, kw.n_fields);
    } else {
      tmp_m = IDL_CvtDbl(1, &kw.m);
      m = (double*)tmp_m->value.arr->data;
    }
  }

  if (kw.b_x) {
    IDL_ENSURE_ARRAY(kw.b);
    if (kw.n_fields == 0) {
      kw.n_fields = kw.in_field->value.arr->dim[0];
      if (kw.n_fields > GD_MAX_LINCOM)
        kw.n_fields = GD_MAX_LINCOM;
    } else if (kw.b->value.arr->dim[0] < kw.n_fields) 
      GDIDL_KW_ABORT("Insufficient number of elements in B");

    if (kw.b->type == IDL_TYP_COMPLEX || kw.b->type == IDL_TYP_DCOMPLEX) {
      comp_scal = 1;
      cb = malloc(sizeof(double complex) * kw.n_fields);
      if (kw.b->type == IDL_TYP_DCOMPLEX)
        gdidl_dcmp_to_c99(cb, (IDL_DCOMPLEX*)kw.b->value.arr->data,
            kw.n_fields);
      else
        gdidl_cmp_to_c99(cb, (IDL_COMPLEX*)kw.b->value.arr->data, kw.n_fields);
    } else {
      tmp_b = IDL_CvtDbl(1, &kw.b);
      b = (double*)tmp_b->value.arr->data;
    }
  }

  if (comp_scal)
    gd_alter_clincom(D, field_code, kw.n_fields, in_field, cm, cb);
  else
    gd_alter_lincom(D, field_code, kw.n_fields, in_field, m, b);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  if (tmp_m != NULL && kw.m->type != IDL_TYP_DOUBLE)
    IDL_Deltmp(tmp_m);
  if (tmp_b != NULL && kw.b->type != IDL_TYP_DOUBLE)
    IDL_Deltmp(tmp_b);

  free(cm);
  free(cb);

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_linterp GD_ALTER_LINTERP 2 2 KEYWORDS */
void gdidl_alter_linterp(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field = NULL;
  const char* table = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING table;
    int table_x;
    IDL_STRING in_field;
    int in_field_x;
    int rename;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.table_x = 0;
  kw.rename = 0;
  kw.in_field_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { "RENAME_TABLE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(rename) },
    { "TABLE", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(table_x),
      IDL_KW_OFFSETOF(table) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.in_field_x)
    in_field = IDL_STRING_STR(&kw.in_field);
  if (kw.table_x)
    table = IDL_STRING_STR(&kw.table);

  gd_alter_linterp(D, field_code, in_field, table, kw.rename);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

void gdidl_alter_yoke_(gd_type_t type, int argc, IDL_VPTR argv[], char *argk)
{
  dtrace("0x%X, %i, %p, %p", type, argc, argv, argk);

  gd_entry_t E;
  E.in_fields[0] = E.in_fields[1] = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING in_field1;
    int in_field1_x;
    IDL_STRING in_field2;
    int in_field2_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.in_field1_x = 0;
  kw.in_field2_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD1", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field1_x),
      IDL_KW_OFFSETOF(in_field1) },
    { "IN_FIELD2", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field2_x),
      IDL_KW_OFFSETOF(in_field2) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  E.field = IDL_VarGetString(argv[1]);
  E.field_type = type;

  if (kw.in_field1_x)
    E.in_fields[0] = IDL_STRING_STR(&kw.in_field1);
  if (kw.in_field2_x)
    E.in_fields[1] = IDL_STRING_STR(&kw.in_field2);

  gd_alter_entry(D, E.field, &E, 0);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_multiply GD_ALTER_MULTIPLY 2 2 KEYWORDS */
void gdidl_alter_multiply(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_alter_yoke_(GD_MULTIPLY_ENTRY, argc, argv, argk);

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_divide GD_ALTER_DIVIDE 2 2 KEYWORDS */
void gdidl_alter_divide(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_alter_yoke_(GD_DIVIDE_ENTRY, argc, argv, argk);

  dreturnvoid();
}
/* @@DLM: P gdidl_alter_recip GD_ALTER_RECIP 2 2 KEYWORDS */
void gdidl_alter_recip(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field = NULL;
  int comp_scal = 0;
  double dividend = 0;
  complex double cdividend = 0;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_VPTR dividend;
    int dividend_x;
    IDL_STRING in_field;
    int in_field_x;
  } KW_RESULT;
  KW_RESULT kw;

  kw.dividend_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "DIVIDEND", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(dividend_x),
      IDL_KW_OFFSETOF(dividend) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.in_field_x)
    in_field = IDL_STRING_STR(&kw.in_field);

  if (kw.dividend_x) {
    if (kw.dividend->type == IDL_TYP_DCOMPLEX ||
        kw.dividend->type == IDL_TYP_COMPLEX)
    {
      comp_scal = 1;
      cdividend = gdidl_dcomplexScalar(kw.dividend);
    } else
      dividend = IDL_DoubleScalar(kw.dividend);
  }

  if (comp_scal)
    gd_alter_crecip(D, field_code, in_field, cdividend);
  else
    gd_alter_recip(D, field_code, in_field, dividend);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_phase GD_ALTER_PHASE 2 2 KEYWORDS */
void gdidl_alter_phase(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int shift;
    IDL_STRING in_field;
    int in_field_x;
  } KW_RESULT;
  KW_RESULT kw;

  kw.shift = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { "SHIFT", IDL_TYP_LONG, 1, 0, 0, IDL_KW_OFFSETOF(shift) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.in_field_x)
    in_field = IDL_STRING_STR(&kw.in_field);

  gd_alter_phase(D, field_code, in_field, kw.shift);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_polynom GD_ALTER_POLYNOM 2 2 KEYWORDS */
void gdidl_alter_polynom(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_VPTR in_field;
    int in_field_x;
    IDL_VPTR a;
    int a_x;
    int poly_ord;
  } KW_RESULT;
  KW_RESULT kw;

  int comp_scal = 0;
  double* a = NULL;
  double complex* ca = NULL;
  const char* in_field = NULL;
  IDL_VPTR tmp_a = NULL;

  GDIDL_KW_INIT_ERROR;
  kw.in_field_x = kw.a_x = kw.poly_ord = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "A", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(a_x), IDL_KW_OFFSETOF(a) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { "POLY_ORD", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(poly_ord) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.a_x) {
    IDL_ENSURE_ARRAY(kw.a);
    if (kw.poly_ord == 0) {
      kw.poly_ord = kw.in_field->value.arr->dim[0] - 1;
      if (kw.poly_ord > GD_MAX_POLYORD)
        kw.poly_ord = GD_MAX_POLYORD;
    } else if (kw.a->value.arr->dim[0] < kw.poly_ord + 1) 
      GDIDL_KW_ABORT("Insufficient number of elements in A");

    if (kw.a->type == IDL_TYP_COMPLEX || kw.a->type == IDL_TYP_DCOMPLEX) {
      comp_scal = 1;
      ca = malloc(sizeof(double complex) * (kw.poly_ord + 1));
      if (kw.a->type == IDL_TYP_DCOMPLEX)
        gdidl_dcmp_to_c99(ca, (IDL_DCOMPLEX*)kw.a->value.arr->data,
            kw.poly_ord + 1);
      else
        gdidl_cmp_to_c99(ca, (IDL_COMPLEX*)kw.a->value.arr->data,
            kw.poly_ord + 1);
    } else {
      tmp_a = IDL_CvtDbl(1, &kw.a);
      a = (double*)tmp_a->value.arr->data;
    }
  }

  if (comp_scal)
    gd_alter_cpolynom(D, field_code, kw.poly_ord, in_field, ca);
  else
    gd_alter_polynom(D, field_code, kw.poly_ord, in_field, a);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  /* If no type conversion is needed IDL_CvtDbl returns its input,
   * so we must check the input type to determine whether we need to delete
   * the temporary variable */
  if (tmp_a != NULL && kw.a->type != IDL_TYP_DOUBLE)
    IDL_Deltmp(tmp_a);

  free(ca);

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_raw GD_ALTER_RAW 2 2 KEYWORDS */
void gdidl_alter_raw(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    unsigned int spf;
    gd_type_t type;
    int recode;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.recode = 0;
  kw.spf = 0;
  kw.type = GD_NULL;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
    { "SPF", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(spf) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_alter_raw(D, field_code, IDL_LongScalar(argv[2]), kw.spf,
      kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_sbit GD_ALTER_SBIT 2 2 KEYWORDS */
void gdidl_alter_sbit(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int bitnum;
    int bitnum_x;
    int numbits;
    IDL_STRING in_field;
    int in_field_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.bitnum = kw.bitnum_x = 0;
  kw.numbits = 0;
  kw.in_field_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "BITNUM", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(bitnum_x),
      IDL_KW_OFFSETOF(bitnum) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { "NUMBITS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(numbits) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (!kw.bitnum_x)
    kw.bitnum = -1;

  if (kw.in_field_x)
    in_field = IDL_STRING_STR(&kw.in_field);

  gd_alter_sbit(D, field_code, in_field, kw.bitnum, kw.numbits);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_spec GD_ALTER_SPEC 2 2 KEYWORDS */
void gdidl_alter_spec(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int recode;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.parent_x = 0;
  kw.recode = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* line = IDL_VarGetString(argv[1]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_malter_spec(D, line, parent, kw.recode);
  } else
    gd_alter_spec(D, line, kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_close GD_CLOSE 1 1 KEYWORDS */
void gdidl_close(int argc, IDL_VPTR argv[], char *argk)
{
  int ret = 0;
  DIRFILE* D = NULL;

  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int discard;
  } KW_RESULT;
  KW_RESULT kw;

  kw.discard = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "DISCARD", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(discard) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  IDL_LONG d = IDL_LongScalar(argv[0]);

  if (d != 0) {
    D = gdidl_get_dirfile(d);

    if (kw.discard)
      ret = gd_discard(D);
    else
      ret = gd_close(D);
  }

  if (ret)
    GDIDL_SET_ERROR(D);
  else {
    if (kw.error != NULL)
      IDL_StoreScalarZero(kw.error, IDL_TYP_INT);
    if (kw.estr != NULL) {
      IDL_StoreScalarZero(kw.estr, IDL_TYP_INT); /* free dynamic memory */
      kw.estr->type = IDL_TYP_STRING;
      IDL_StrStore((IDL_STRING*)&kw.estr->value.s, "Success");
    }

    if (d != 0)
      gdidl_clear_dirfile(d);
  }

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_delete GD_DELETE 2 2 KEYWORDS */
void gdidl_delete(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int data;
    int deref;
    int force;
    int meta;
  } KW_RESULT;
  KW_RESULT kw;

  kw.data = kw.deref = kw.force = kw.meta = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "DEL_DATA", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(data) },
    { "DEREF", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(deref) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FORCE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force) },
    { "META", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(meta) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_delete(D, field_code, (kw.data ? GD_DEL_DATA : 0) |
      (kw.deref ? GD_DEL_DEREF : 0) | (kw.force ? GD_DEL_FORCE : 0) |
      (kw.meta ? GD_DEL_META : 0));

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_flush GD_FLUSH 1 1 KEYWORDS */
void gdidl_flush(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING field_code;
    int field_code_x;
    int noclose;
    int nosync;
  } KW_RESULT;
  KW_RESULT kw;

  const char* field_code = NULL;

  GDIDL_KW_INIT_ERROR;
  kw.field_code_x = 0;
  kw.noclose = kw.nosync = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FIELD_CODE", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(field_code_x),
      IDL_KW_OFFSETOF(field_code) },
    { "NOCLOSE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(noclose) },
    { "NOSYNC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(nosync) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.field_code_x)
    field_code = IDL_STRING_STR(&kw.field_code);

  if (kw.noclose && kw.nosync)
    GDIDL_KW_ABORT("nothing to do");
  else if (kw.noclose)
    gd_sync(D, field_code);
  else if (kw.nosync)
    gd_raw_close(D, field_code);
  else
    gd_flush(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_include GD_INCLUDE 2 2 KEYWORDS */
void gdidl_include(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int free_prefix = 0;
  char *prefix = NULL;
  char *suffix = NULL;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int arm_end;
    int big_end;
    int creat;
    int excl;
    int force_enc;
    int force_end;
    int ignore_dups;
    int ignore_refs;
    int little_end;
    int not_arm_end;
    int pedantic;
    int permissive;
    int trunc;
    int enc_x;
    IDL_VPTR enc;
    IDL_VPTR index;
    int fragment_index;
    int index_x;
    IDL_STRING prefix;
    int prefix_x;
    IDL_STRING suffix;
    int suffix_x;
    IDL_STRING ns;
    int ns_x;
  } KW_RESULT;
  KW_RESULT kw;
  kw.big_end = kw.creat = kw.excl = kw.force_enc = kw.force_end =
    kw.ignore_dups = kw.ignore_refs = kw.little_end = kw.pedantic = kw.trunc =
    kw.enc_x = kw.index_x = kw.fragment_index = kw.arm_end = kw.not_arm_end =
    kw.prefix_x = kw.suffix_x = kw.ns_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    IDL_KW_FAST_SCAN,
    { "ARM_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(arm_end) },
    { "BIG_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(big_end) },
    { "CREAT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(creat) },
    { "ENCODING", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(enc_x),
      IDL_KW_OFFSETOF(enc) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "EXCL", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(excl) },
    { "FORCE_ENCODING", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force_enc) },
    { "FORCE_ENDIANNESS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force_end) },
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "INDEX", 0, 1, IDL_KW_OUT, IDL_KW_OFFSETOF(index_x),
      IDL_KW_OFFSETOF(index) },
    { "IGNORE_DUPS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ignore_dups) },
    { "IGNORE_REFS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ignore_dups) },
    { "LITTLE_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(little_end) },
    { "NAMESPACE", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(ns_x),
      IDL_KW_OFFSETOF(ns) },
    { "NOT_ARM_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(not_arm_end) },
    { "PEDANTIC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pedantic) },
    { "PERMISSIVE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(permissive) },
    { "PREFIX", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(prefix_x),
      IDL_KW_OFFSETOF(prefix) },
    { "SUFFIX", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(suffix_x),
      IDL_KW_OFFSETOF(suffix) },
    { "TRUNC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(trunc) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  /* check writability before doing anything */
  if (kw.index_x)
    IDL_StoreScalarZero(kw.index, IDL_TYP_INT);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char *file = IDL_VarGetString(argv[1]);

  unsigned long flags = (kw.arm_end ? GD_ARM_ENDIAN : 0)
    | (kw.big_end ? GD_BIG_ENDIAN : 0) | (kw.creat ? GD_CREAT : 0)
    | (kw.excl ? GD_EXCL : 0) | (kw.force_enc ? GD_FORCE_ENCODING : 0)
    | (kw.force_end ? GD_FORCE_ENDIAN : 0)
    | (kw.ignore_dups ? GD_IGNORE_DUPS : 0)
    | (kw.ignore_refs ? GD_IGNORE_REFS : 0)
    | (kw.little_end ? GD_LITTLE_ENDIAN : 0)
    | (kw.not_arm_end ? GD_NOT_ARM_ENDIAN : 0)
    | (kw.pedantic ? GD_PEDANTIC : 0) | (kw.permissive ? GD_PERMISSIVE : 0)
    | (kw.trunc ? GD_TRUNC : 0);

  if (kw.enc_x)
    flags |= gdidl_convert_encoding(kw.enc);

  /* Handle combinations of prefix and namespace */
  if (kw.ns_x && kw.prefix_x) {
    const char *fmt;
    char *ns = IDL_STRING_STR(&kw.ns);
    char *prefix0 = IDL_STRING_STR(&kw.prefix);

    prefix = malloc(strlen(prefix0) + strlen(ns) + 2);
    free_prefix = 1;

    if (ns[strlen(ns) - 1] == '.')
      fmt = "%s%s";
    else
      fmt = "%s.%s";
    
    sprintf(prefix, fmt, ns, prefix0);
  } else if (kw.ns_x) {
    char *ns = IDL_STRING_STR(&kw.ns);
    if (ns[strlen(ns) - 1] == '.')
      prefix = ns;
    else {
      prefix = malloc(strlen(ns) + 2);
      free_prefix = 1;
      sprintf(prefix, "%s.", ns);
    }
  } else if (kw.prefix_x)
    prefix = IDL_STRING_STR(&kw.prefix);

  if (kw.suffix_x)
    suffix = IDL_STRING_STR(&kw.suffix);

  int index = (int16_t)gd_include_affix(D, file, kw.fragment_index, prefix,
      suffix, flags);

  if (free_prefix)
    free(prefix);

  if (kw.index_x) {
    IDL_ALLTYPES v;
    v.i = index;
    IDL_StoreScalar(kw.index, IDL_TYP_INT, &v);
  }

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_metaflush GD_METAFLUSH 1 1 KEYWORDS */
void gdidl_metaflush(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_metaflush(D);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_rewrite_fragment GD_REWRITE_FRAGMENT 1 1 KEYWORDS */
void gdidl_rewrite_fragment(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    int fragment_index_x;
  } KW_RESULT;
  KW_RESULT kw;

  kw.fragment_index = 0;
  kw.fragment_index_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(fragment_index_x),
      IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (!kw.fragment_index_x)
    kw.fragment_index = GD_ALL_FRAGMENTS;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_rewrite_fragment(D, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_move GD_MOVE 3 3 KEYWORDS */
void gdidl_move(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int dangle, force, move_data, updatedb;
  } KW_RESULT;
  KW_RESULT kw;

  kw.dangle = kw.force = kw.move_data = kw.updatedb = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "DANGLE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(dangle) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FORCE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force) },
    { "MOVE_DATA", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(move_data) },
    { "UPDATEDB", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(updatedb) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_move(D, field_code, IDL_LongScalar(argv[2]),
      (kw.dangle ? GD_REN_DANGLE : 0) |
      (kw.force ? GD_REN_FORCE : 0) |
      (kw.move_data ? GD_REN_DATA : 0) |
      (kw.updatedb ? GD_REN_UPDB : 0));

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_open GD_OPEN 1 1 KEYWORDS */
IDL_VPTR gdidl_open(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  char* name = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int rdwr;
    int arm_end;
    int big_end;
    int creat;
    int excl;
    int force_enc;
    int force_end;
    int ignore_dups;
    int ignore_refs;
    int little_end;
    int not_arm_end;
    int pedantic;
    int permissive;
    int pretty_print;
    int trunc;
    int truncsub;
    int verbose;
    int enc_x;
    IDL_VPTR enc;
  } KW_RESULT;
  KW_RESULT kw;
  kw.rdwr = kw.big_end = kw.creat = kw.excl = kw.force_enc = kw.force_end =
    kw.ignore_dups = kw.little_end = kw.pedantic = kw.trunc = kw.verbose =
    kw.enc_x = kw.arm_end = kw.not_arm_end = kw.pretty_print = kw.truncsub =
    kw.ignore_refs = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    IDL_KW_FAST_SCAN,
    { "ARM_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(arm_end) },
    { "BIG_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(big_end) },
    { "CREAT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(creat) },
    { "ENCODING", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(enc_x),
      IDL_KW_OFFSETOF(enc) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "EXCL", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(excl) },
    { "FORCE_ENCODING", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force_enc) },
    { "FORCE_ENDIANNESS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force_end) },
    { "IGNORE_DUPS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ignore_dups) },
    { "IGNORE_REFS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ignore_refs) },
    { "LITTLE_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(little_end) },
    { "NOT_ARM_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(not_arm_end) },
    { "PEDANTIC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pedantic) },
    { "PERMISSIVE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(permissive) },
    { "PRETTY_PRINT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pretty_print) },
    { "RDWR", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(rdwr) },
    { "TRUNC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(trunc) },
    { "TRUNCSUB", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(truncsub) },
    { "VERBOSE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(verbose) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  name = IDL_VarGetString(argv[0]);

  unsigned long flags = (kw.rdwr ? GD_RDWR : GD_RDONLY)
    | (kw.arm_end ? GD_ARM_ENDIAN : 0) | (kw.big_end ? GD_BIG_ENDIAN : 0)
    | (kw.creat ? GD_CREAT : 0) | (kw.excl ? GD_EXCL : 0)
    | (kw.force_enc ? GD_FORCE_ENCODING : 0)
    | (kw.force_end ? GD_FORCE_ENDIAN : 0)
    | (kw.ignore_dups ? GD_IGNORE_DUPS : 0)
    | (kw.ignore_refs ? GD_IGNORE_REFS : 0)
    | (kw.little_end ? GD_LITTLE_ENDIAN : 0)
    | (kw.not_arm_end ? GD_NOT_ARM_ENDIAN : 0)
    | (kw.pedantic ? GD_PEDANTIC : 0) | (kw.permissive ? GD_PERMISSIVE : 0)
    | (kw.pretty_print ? GD_PRETTY_PRINT : 0) | (kw.trunc ? GD_TRUNC : 0)
    | (kw.truncsub ? GD_TRUNCSUB : 0) | (kw.verbose ? GD_VERBOSE : 0);

  if (kw.enc_x)
    flags |= gdidl_convert_encoding(kw.enc);

  DIRFILE* D = gd_open(name, flags);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(gdidl_set_dirfile(D));
  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_alter_protection GD_ALTER_PROTECTION 2 2 KEYWORDS */
void gdidl_alter_protection(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    int fragment_index_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.fragment_index_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(fragment_index_x),
      IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (!kw.fragment_index_x)
    kw.fragment_index = GD_ALL_FRAGMENTS;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_alter_protection(D, IDL_LongScalar(argv[1]), kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_reference GD_REFERENCE 1 2 KEYWORDS */
IDL_VPTR gdidl_reference(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char *field_code = NULL;
  IDL_VPTR r;

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  if (argc > 1)
    field_code = IDL_VarGetString(argv[1]);

  field_code = gd_reference(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  if (field_code)
    r = IDL_StrToSTRING((char*)field_code);
  else
    r = IDL_StrToSTRING("");

  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_rename GD_RENAME 3 3 KEYWORDS */
void gdidl_rename(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int dangle, force, move_data, updatedb;
  } KW_RESULT;
  KW_RESULT kw;

  kw.dangle = kw.force = kw.move_data = kw.updatedb = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "DANGLE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(dangle) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FORCE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force) },
    { "MOVE_DATA", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(move_data) },
    { "UPDATEDB", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(updatedb) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* new_code = IDL_VarGetString(argv[2]);

  gd_rename(D, field_code, new_code,
      (kw.dangle ? GD_REN_DANGLE : 0) |
      (kw.force ? GD_REN_FORCE : 0) |
      (kw.move_data ? GD_REN_DATA : 0) |
      (kw.updatedb ? GD_REN_UPDB : 0));

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_uninclude GD_UNINCLUDE 2 2 KEYWORDS */
void gdidl_uninclude(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int del;
  } KW_RESULT;
  KW_RESULT kw;

  kw.del = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "DELETE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(del) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_uninclude(D, IDL_LongScalar(argv[1]), kw.del);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_validate GD_VALIDATE 2 2 KEYWORDS */
IDL_VPTR gdidl_validate(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  int v = gd_validate(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(v);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_getdata GD_GETDATA 2 2 KEYWORDS */
IDL_VPTR gdidl_getdata(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_LONG64 first_frame;
    IDL_LONG64 first_sample;
    IDL_LONG n_frames;
    IDL_LONG n_samples;
    int first_frame_x, first_sample_x;
    gd_type_t return_type;
  } KW_RESULT;
  KW_RESULT kw;

  size_t n;
  IDL_VPTR r;

  kw.first_frame = kw.first_sample = kw.n_frames = kw.n_samples = 0;
  kw.first_frame_x = kw.first_sample_x = 0;
  kw.return_type = GD_FLOAT64;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FIRST_FRAME", IDL_TYP_LONG64, 1, 0, IDL_KW_OFFSETOF(first_frame_x),
      IDL_KW_OFFSETOF(first_frame) },
    { "FIRST_SAMPLE", IDL_TYP_LONG64, 1, 0, IDL_KW_OFFSETOF(first_sample_x),
      IDL_KW_OFFSETOF(first_sample) },
    { "NUM_FRAMES", IDL_TYP_LONG, 1, 0, 0, IDL_KW_OFFSETOF(n_frames) },
    { "NUM_SAMPLES", IDL_TYP_LONG, 1, 0, 0, IDL_KW_OFFSETOF(n_samples) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(return_type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  /* check for metadata */
  unsigned int spf = 1;
  gd_entype_t t = gd_entry_type(D, field_code);
  if (!gd_error(D) && kw.n_frames > 0)
    spf = gd_spf(D, field_code);

  if (gd_error(D) || (kw.n_frames == 0 && kw.n_samples == 0)) {
    r = IDL_GettmpInt(0);
  } else {
    if (kw.first_frame_x == 0 && kw.first_sample_x == 0) {
      kw.first_frame = GD_HERE;
      kw.first_sample = 0;
    }
    kw.n_samples += kw.n_frames * spf;

    if (t == GD_SINDIR_ENTRY) {
      const char **data = malloc(kw.n_samples * sizeof(*data));

      n = gd_getdata64(D, field_code, kw.first_frame, kw.first_sample, 0,
          kw.n_samples, GD_STRING, data);

      if (n == 0)
        r = IDL_GettmpInt(0);
      else {
        int i;
        IDL_STRING *idata = (IDL_STRING*)IDL_MakeTempVector(IDL_TYP_STRING, n,
            IDL_ARR_INI_ZERO, &r);
        for (i = 0; i < n; ++i)
          IDL_StrStore(idata + i, (char*)data[i]);
      }
      free(data);
    } else if (kw.return_type == GD_INT8) /* no signed 8-bit type in IDL */
      GDIDL_KW_ABORT("Cannot return data as a signed 8-bit integer.");
		else if (kw.return_type == GD_NULL) {
      n = gd_getdata64(D, field_code, kw.first_frame, kw.first_sample, 0,
          kw.n_samples, GD_NULL, NULL);

      r = IDL_GettmpInt(n);
    } else {
      void* data = malloc(kw.n_samples * GD_SIZE(kw.return_type));

      n = gd_getdata64(D, field_code, kw.first_frame, kw.first_sample, 0,
          kw.n_samples, kw.return_type, data);

      if (n == 0) {
        free(data);
        r = IDL_GettmpInt(0);
      } else {
        IDL_MEMINT dim[] = { n };
        r = IDL_ImportArray(1, dim, gdidl_idl_type(kw.return_type), data,
            (IDL_ARRAY_FREE_CB)free, NULL);
      }
    }
  }

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_bof GD_BOF 2 2 KEYWORDS */
IDL_VPTR gdidl_get_bof(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char *field_code = IDL_VarGetString(argv[1]);

  gd_off64_t bof = gd_bof64(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)bof;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_constant GD_GET_CONSTANT 2 2 KEYWORDS */
IDL_VPTR gdidl_get_constant(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int const_type;
  } KW_RESULT;
  KW_RESULT kw;
  IDL_VPTR r;

  GDIDL_KW_INIT_ERROR;
  kw.const_type = GD_FLOAT64;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(const_type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.const_type == GD_NULL) {
    int ret = gd_get_constant(D, field_code, GD_NULL, NULL);

    r = IDL_GettmpInt(ret);
  } else {
    void* data = malloc(16);

    int ret = gd_get_constant(D, field_code, kw.const_type, data);

    if (!ret) {
      r = IDL_Gettmp();
      r->value = gdidl_to_alltypes(kw.const_type, data);
      r->type = gdidl_idl_type(kw.const_type);
    } else {
      GDIDL_SET_ERROR(D);
      r = IDL_GettmpInt(0);
    }
    free(data);
  }

  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_carray GD_GET_CARRAY 2 2 KEYWORDS */
IDL_VPTR gdidl_get_carray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();
  int ret = 1;
  void *data = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    unsigned int start;
    unsigned int n;
    int const_type;
  } KW_RESULT;
  KW_RESULT kw;
  IDL_VPTR r;

  GDIDL_KW_INIT_ERROR;
  kw.const_type = GD_FLOAT64;
  kw.start = kw.n = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "LENGTH", IDL_TYP_UINT, 1, 0, 0, IDL_KW_OFFSETOF(n) },
    { "START", IDL_TYP_UINT, 1, 0, 0, IDL_KW_OFFSETOF(start) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(const_type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.const_type == GD_NULL) {
    if (kw.n == 0)
      ret = gd_get_carray(D, field_code, GD_NULL, NULL);
    else 
      ret = gd_get_carray_slice(D, field_code, kw.start, kw.n, GD_NULL, NULL);

    GDIDL_SET_ERROR(D);
    r = IDL_GettmpInt(ret);
  } else {
    if (kw.n == 0) {
      kw.n = gd_array_len(D, field_code);
      if (kw.n > kw.start)
        kw.n -= kw.start;
      else
        kw.n = 0;
    }

    data = malloc(16 * kw.n);
    ret = gd_get_carray_slice(D, field_code, kw.start, kw.n, kw.const_type,
        data);

    if (!ret) {
      IDL_MEMINT dim[] = { kw.n };
      r = IDL_ImportArray(1, dim, gdidl_idl_type(kw.const_type), data,
          (IDL_ARRAY_FREE_CB)free, NULL);
    } else {
      free(data);
      GDIDL_SET_ERROR(D);
      r = IDL_GettmpInt(0);
    }
  }

  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_constants GD_CONSTANTS 1 1 KEYWORDS */
IDL_VPTR gdidl_get_constants(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int const_type;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  unsigned int nconst;
  const void* consts;

  GDIDL_KW_INIT_ERROR;
  kw.parent_x = 0;
  kw.const_type = GD_FLOAT64;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(const_type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  /* no signed 8-bit type in IDL */
  if (kw.const_type == GD_INT8)
    GDIDL_KW_ABORT("Cannot return data as a signed 8-bit integer.");

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    nconst = gd_nmfields_by_type(D, parent, GD_CONST_ENTRY);
    consts = gd_mconstants(D, parent, kw.const_type);
  } else {
    nconst = gd_nfields_by_type(D, GD_CONST_ENTRY);
    consts = gd_constants(D, kw.const_type);
  }

  IDL_VPTR r;
  if (consts != NULL) {
    void* data = malloc(GD_SIZE(kw.const_type) * nconst);
    memcpy(data, consts, GD_SIZE(kw.const_type) * nconst);
    IDL_MEMINT dim[IDL_MAX_ARRAY_DIM] = { nconst };

    r = IDL_ImportArray(1, dim, gdidl_idl_type(kw.const_type), data,
        (IDL_ARRAY_FREE_CB)free, NULL);
  } else
    IDL_MakeTempVector(IDL_TYP_INT, 0, IDL_ARR_INI_ZERO, &r);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_encoding GD_ENCODING 1 1 KEYWORDS */
IDL_VPTR gdidl_get_encoding(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  unsigned long enc = gd_encoding(D, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(enc);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_endianness GD_ENDIANNESS 1 1 KEYWORDS */
IDL_VPTR gdidl_get_endianness(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  unsigned long end = gd_endianness(D, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(end);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_entry GD_ENTRY 2 2 KEYWORDS */
IDL_VPTR gdidl_get_entry(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_entry_t E;
  int ret = gd_entry(D, field_code, &E);

  IDL_VPTR r = NULL;
  if (ret) {
    GDIDL_SET_ERROR(D);
    r = IDL_GettmpInt(0);
  } else {
    r = gdidl_make_idl_entry(&E);
    gd_free_entry_strings(&E);
  }

  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_entry_type GD_ENTRY_TYPE 2 2 KEYWORDS */
IDL_VPTR gdidl_get_entry_type(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_entype_t type = gd_entry_type(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(type);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_error GD_ERROR 1 1 */
IDL_VPTR gdidl_error(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int err = gd_error(gdidl_get_dirfile(IDL_LongScalar(argv[0])));

  IDL_VPTR r = IDL_GettmpInt(err);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_error_count GD_ERROR_COUNT 1 1 */
IDL_VPTR gdidl_error_count(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int err = gd_error_count(gdidl_get_dirfile(IDL_LongScalar(argv[0])));

  IDL_VPTR r = IDL_GettmpInt(err);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_error_string GD_ERROR_STRING 1 1 */
IDL_VPTR gdidl_error_string(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  char buffer[GD_MAX_LINE_LENGTH];
  gd_error_string(gdidl_get_dirfile(IDL_LongScalar(argv[0])), buffer,
      GD_MAX_LINE_LENGTH);

  IDL_VPTR r = IDL_StrToSTRING(buffer);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_eof GD_EOF 2 2 KEYWORDS */
IDL_VPTR gdidl_get_eof(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char *field_code = IDL_VarGetString(argv[1]);

  gd_off64_t eof = gd_eof64(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)eof;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_field_list GD_MATCH_ENTRIES 1 1 KEYWORDS */
/* the following aliases are needed for backwards compatibility */
/* @@DLM: F gdidl_get_field_list GD_ENTRY_LIST 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_field_list GD_FIELD_LIST 1 1 KEYWORDS */
IDL_VPTR gdidl_get_field_list(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  unsigned int i, nentries, flags = 0;
  const char **list;
  const char *parent = NULL;
  const char *regex = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int type;
    IDL_STRING parent;
    IDL_STRING regex;
    int parent_x, regex_x, fragment_x;
    int hidden, noalias, scalars, vectors, aliases, fragment;
    int extended, pcre, javascript, icase, unicode;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.type = 0;
  kw.fragment = 0;
  kw.hidden = kw.noalias = kw.scalars = kw.vectors = kw.aliases = 0;
  kw.extended = kw.pcre = kw.javascript = kw.icase = kw.unicode = 0;
  kw.parent_x = kw.regex_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "ALIASES", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(aliases) },
    { "CASELESS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(icase) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "EXTENDED", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(extended) },
    { "FRAGMENT", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(fragment_x),
      IDL_KW_OFFSETOF(fragment) },
    { "HIDDEN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(hidden) },
    { "ICASE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(icase) },
    { "JAVASCRIPT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(javascript) },
    { "NOALIAS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(noalias) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "PCRE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pcre) },
    { "REGEX", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(regex_x),
      IDL_KW_OFFSETOF(regex) },
    { "SCALARS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(scalars) },
    { "TYPE", IDL_TYP_UINT, 1, 0, 0, IDL_KW_OFFSETOF(type) },
    { "UNICODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(unicode) },
    { "VECTORS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(vectors) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.parent_x && kw.regex_x)
    GDIDL_KW_ABORT("parent and regex may not both be specified");
  if (kw.parent_x && kw.fragment_x)
    GDIDL_KW_ABORT("parent and fragment may not both be specified");

  if (!kw.fragment_x)
    kw.fragment = GD_ALL_FRAGMENTS;

  if (kw.parent_x)
    parent = IDL_STRING_STR(&kw.parent);

  if (kw.regex_x) {
    regex = IDL_STRING_STR(&kw.regex);
    if (kw.extended)
      flags |= GD_REGEX_EXTENDED;
    if (kw.icase)
      flags |= GD_REGEX_ICASE;
    if (kw.javascript)
      flags |= GD_REGEX_JAVASCRIPT;
    if (kw.pcre)
      flags |= GD_REGEX_PCRE;
    if (kw.unicode)
      flags |= GD_REGEX_UNICODE;
  }

  if (kw.hidden)
    flags |= GD_ENTRIES_HIDDEN;
  if (kw.noalias)
    flags |= GD_ENTRIES_NOALIAS;

  if (kw.type == 0) {
    if (kw.vectors)
      kw.type = GD_VECTOR_ENTRIES;
    else if (kw.scalars)
      kw.type = GD_SCALAR_ENTRIES;
    else if (kw.aliases)
      kw.type = GD_ALIAS_ENTRIES;
  }

  if (kw.parent_x) {
    nentries = gd_nentries(D, parent, kw.type, flags);
    list = gd_entry_list(D, parent, kw.type, flags);
  } else
    nentries = gd_match_entries(D, regex, kw.fragment, kw.type, flags, &list);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r;

  IDL_STRING *data = (IDL_STRING*)IDL_MakeTempVector(IDL_TYP_STRING, nentries,
      IDL_ARR_INI_ZERO, &r);
  for (i = 0; i < nentries; ++i)
    IDL_StrStore(data + i, (char*)list[i]);

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_fragment_index GD_FRAGMENT_INDEX 2 2 KEYWORDS */
IDL_VPTR gdidl_get_fragment_index(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  char* field_code = IDL_VarGetString(argv[1]);

  int index = gd_fragment_index(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(index);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_fragmentname GD_FRAGMENTNAME 2 2 KEYWORDS */
IDL_VPTR gdidl_get_fragmentname(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  int index = (int)IDL_LongScalar(argv[1]);

  const char* name = gd_fragmentname(D, index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING((char*)name);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_framenum GD_FRAMENUM 3 3 KEYWORDS */
IDL_VPTR gdidl_get_framenum(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_LONG64 frame_start;
    IDL_LONG64 frame_end;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.frame_start = 0;
  kw.frame_end = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FIELD_START", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(frame_start) },
    { "FIELD_END", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(frame_end) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  double value = IDL_DoubleScalar(argv[2]);

  double frame = gd_framenum_subset64(D, field_code, value, kw.frame_start,
      kw.frame_end);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_DOUBLE;
  r->value.d = frame;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_frameoffset GD_FRAMEOFFSET 1 1 KEYWORDS */
IDL_VPTR gdidl_get_frameoffset(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_off64_t foffs = gd_frameoffset64(D, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)foffs;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_native_type GD_NATIVE_TYPE 2 2 KEYWORDS */
IDL_VPTR gdidl_get_native_type(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_type_t t = gd_native_type(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(t);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_nfields GD_NENTRIES 1 1 KEYWORDS */
/* the following alias is needed for backwards compatibility */
/* @@DLM: F gdidl_get_nfields GD_NFIELDS 1 1 KEYWORDS */
IDL_VPTR gdidl_get_nfields(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  unsigned int nentries, flags = 0;
  const char *parent = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int type;
    IDL_STRING parent;
    int parent_x;
    int aliases, hidden, noalias, scalars, vectors;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.type = 0;
  kw.hidden = kw.noalias = kw.scalars = kw.vectors = kw.aliases = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "ALIASES", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(aliases) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "HIDDEN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(hidden) },
    { "NOALIAS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(noalias) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "SCALARS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(scalars) },
    { "TYPE", IDL_TYP_UINT, 1, 0, 0, IDL_KW_OFFSETOF(type) },
    { "VECTORS", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(vectors) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.parent_x)
    parent = IDL_STRING_STR(&kw.parent);

  if (kw.hidden)
    flags |= GD_ENTRIES_HIDDEN;
  if (kw.noalias)
    flags |= GD_ENTRIES_NOALIAS;

  if (kw.type == 0) {
    if (kw.vectors)
      kw.type = GD_VECTOR_ENTRIES;
    else if (kw.scalars)
      kw.type = GD_SCALAR_ENTRIES;
    else if (kw.aliases)
      kw.type = GD_ALIAS_ENTRIES;
  }

  nentries = gd_nentries(D, parent, kw.type, flags);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(nentries);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_nfragments GD_NFRAGMENTS 1 1 KEYWORDS */
IDL_VPTR gdidl_get_nfragments(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  unsigned int nfrags;

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  nfrags = gd_nfragments(D);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(nfrags);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_nframes GD_NFRAMES 1 1 KEYWORDS */
IDL_VPTR gdidl_get_nframes(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_off64_t nframes = gd_nframes64(D);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)nframes;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_nvectors GD_NVECTORS 1 1 KEYWORDS */
IDL_VPTR gdidl_get_nvectors(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  unsigned int nfields;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    nfields = gd_nmvectors(D, parent);
  } else 
    nfields = gd_nvectors(D);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(nfields);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_parent_fragment GD_PARENT_FRAGMENT 1 1 KEYWORDS */
IDL_VPTR gdidl_get_parent_fragment(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  int parent = gd_parent_fragment(D, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(parent);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_protection GD_PROTECTION 1 1 KEYWORDS */
IDL_VPTR gdidl_get_protection(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  int prot = gd_protection(D, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(prot);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_raw_filename GD_RAW_FILENAME 2 2 KEYWORDS */
IDL_VPTR gdidl_get_raw_filename(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  DIRFILE *D;
  char *name;
  const char *field_code;
  GDIDL_KW_ONLY_ERROR;

  D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  field_code = IDL_VarGetString(argv[1]);

  name = gd_raw_filename(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING(name);
  free(name);

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_spf GD_SPF 2 2 KEYWORDS */
IDL_VPTR gdidl_get_spf(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  unsigned int spf = gd_spf(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpUInt(spf);
  dreturn("%p", r);
  return r;
}

/* GD_CARRAY_LEN is a deprecated alias */
/* @@DLM: F gdidl_array_len GD_CARRAY_LEN 2 2 KEYWORDS */
/* @@DLM: F gdidl_array_len GD_ARRAY_LEN 2 2 KEYWORDS */
IDL_VPTR gdidl_array_len(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  size_t len = gd_array_len(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpUInt(len);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_string GD_GET_STRING 2 2 KEYWORDS */
IDL_VPTR gdidl_get_string(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  char buffer[GD_MAX_LINE_LENGTH];

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_get_string(D, field_code, GD_MAX_LINE_LENGTH, buffer);

  IDL_VPTR r;
  if (gd_error(D)) {
    GDIDL_SET_ERROR(D);
    r = IDL_StrToSTRING("");
  } else 
    r = IDL_StrToSTRING(buffer);

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_strings GD_STRINGS 1 1 KEYWORDS */
IDL_VPTR gdidl_get_strings(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  unsigned int nstring;
  const char** strings;

  GDIDL_KW_INIT_ERROR;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    nstring = gd_nmfields_by_type(D, parent, GD_STRING_ENTRY);
    strings = gd_mstrings(D, parent);
  } else {
    nstring = gd_nfields_by_type(D, GD_STRING_ENTRY);
    strings = gd_strings(D);
  }

  IDL_VPTR r;
  if (nstring > 0) {
    int i;
    IDL_STRING *data = (IDL_STRING*)IDL_MakeTempVector(IDL_TYP_STRING, nstring,
        IDL_ARR_INI_ZERO, &r);
    for (i = 0; i < nstring; ++i)
      IDL_StrStore(data + i, (char*)strings[i]);
  } else
    IDL_MakeTempVector(IDL_TYP_STRING, 0, IDL_ARR_INI_ZERO, &r);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_vector_list GD_VECTOR_LIST 1 1 KEYWORDS */
IDL_VPTR gdidl_get_vector_list(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int i;
  unsigned int nfields;
  const char** list;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    gd_type_t type;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    nfields = gd_nmvectors(D, parent);
    list = gd_mvector_list(D, parent);
  } else {
    nfields = gd_nvectors(D);
    list = gd_vector_list(D);
  }

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r;

  IDL_STRING *data = (IDL_STRING*)IDL_MakeTempVector(IDL_TYP_STRING, nfields,
      IDL_ARR_INI_ZERO, &r);
  for (i = 0; i < nfields; ++i)
    IDL_StrStore(data + i, (char*)list[i]);

  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_putdata GD_PUTDATA 3 3 KEYWORDS */
void gdidl_putdata(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_LONG64 first_frame;
    IDL_LONG64 first_sample;
    int first_frame_x, first_sample_x;
  } KW_RESULT;
  KW_RESULT kw;

  kw.first_frame = kw.first_sample = kw.first_frame_x = kw.first_sample_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FIRST_FRAME", IDL_TYP_LONG64, 1, 0, IDL_KW_OFFSETOF(first_sample_x),
      IDL_KW_OFFSETOF(first_frame) },
    { "FIRST_SAMPLE", IDL_TYP_LONG64, 1, 0, IDL_KW_OFFSETOF(first_sample_x),
      IDL_KW_OFFSETOF(first_sample) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.first_frame_x == 0 && kw.first_sample_x == 0) {
    kw.first_frame = GD_HERE;
    kw.first_sample = 0;
  }

  IDL_ENSURE_ARRAY(argv[2]);
  if (argv[2]->value.arr->n_dim != 1)
    GDIDL_KW_ABORT("data must be a vector, not a multidimensional array");

  gd_off64_t n_samples = argv[2]->value.arr->n_elts;

  gd_putdata64(D, field_code, kw.first_frame, kw.first_sample, 0, n_samples,
      gdidl_gd_type(argv[2]->type), argv[2]->value.arr->data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_put_constant GD_PUT_CONSTANT 3 3 KEYWORDS */
void gdidl_put_constant(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  const void* data = gdidl_from_alltypes(argv[2]->type, &argv[2]->value);
  gd_put_constant(D, field_code, gdidl_gd_type(argv[2]->type), data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_put_carray GD_PUT_CARRAY 3 3 KEYWORDS */
void gdidl_put_carray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int start;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.start = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "START", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(start) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  IDL_ENSURE_ARRAY(argv[2]);
  if (argv[2]->value.arr->n_dim != 1)
    GDIDL_KW_ABORT("data must be a vector, not a multidimensional array");

  int length = argv[2]->value.arr->n_elts;

  gd_put_carray_slice(D, field_code, kw.start, length,
      gdidl_gd_type(argv[2]->type), argv[2]->value.arr->data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_put_string GD_PUT_STRING 3 3 KEYWORDS */
void gdidl_put_string(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* data = IDL_VarGetString(argv[2]);

  gd_put_string(D, field_code, data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_invalid_dirfile GD_INVALID_DIRFILE 0 0 */
IDL_VPTR gdidl_invalid_dirfile(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  IDL_VPTR r = IDL_GettmpInt(gdidl_set_dirfile(gd_invalid_dirfile()));
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_dirfile_standards GD_DIRFILE_STANDARDS 1 2 KEYWORDS */
IDL_VPTR gdidl_dirfile_standards(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int earliest;
    int current;
    int latest;
  } KW_RESULT;
  KW_RESULT kw;
  int vers = 16384;

  kw.earliest = kw.current = kw.latest = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "CURRENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(current) },
    { "EARLIEST", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(earliest) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "LATEST", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(latest) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (argc > 1)
    vers = IDL_LongScalar(argv[1]);

  vers = gd_dirfile_standards(D, (vers != 16384) ? vers :
      kw.current ? GD_VERSION_CURRENT : kw.latest ? GD_VERSION_LATEST :
      kw.earliest ? GD_VERSION_EARLIEST : 16384);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(vers);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_seek GD_SEEK 2 2 KEYWORDS */
IDL_VPTR gdidl_seek(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    gd_off64_t frame_num;
    gd_off64_t sample_num;
    int whence;
    int write;
  } KW_RESULT;
  KW_RESULT kw;

  kw.whence = GD_SEEK_SET;
  kw.frame_num = kw.sample_num = 0;
  kw.write = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAME_NUM", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(frame_num) },
    { "SAMPLE_NUM", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(sample_num) },
    { "WHENCE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(whence) },
    { "WRITE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(write) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_off64_t pos = gd_seek64(D, field_code, kw.frame_num, kw.sample_num,
      (kw.whence & (GD_SEEK_SET | GD_SEEK_CUR | GD_SEEK_END)) |
      (kw.write ? GD_SEEK_WRITE : 0));

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)pos;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_tell GD_TELL 2 2 KEYWORDS */
IDL_VPTR gdidl_tell(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_off64_t pos = gd_tell64(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)pos;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_hidden GD_HIDDEN 2 2 KEYWORDS */
IDL_VPTR gdidl_hidden(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  int hidden = gd_hidden(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(hidden);
  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_hide GD_HIDE 2 2 KEYWORDS */
void gdidl_hide(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_hide(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_unhide GD_UNHIDE 2 2 KEYWORDS */
void gdidl_unhide(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_unhide(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_window GD_ADD_WINDOW 5 5 KEYWORDS */
void gdidl_add_window(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gd_windop_t windop;
  gd_triplet_t threshold;
  int nop = 0;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING parent;
    int eq, ne, le, lt, gt, ge, set, clr;
    int parent_x;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.eq = kw.ne = kw.le = kw.lt = kw.gt = kw.ge = kw.set = kw.clr = 0;
  kw.fragment_index = kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "CLR", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(clr) },
    { "EQ", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(eq) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "GE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ge) },
    { "GT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(gt) },
    { "LE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(le) },
    { "LT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(lt) },
    { "NE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ne) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "SET", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(set) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field = IDL_VarGetString(argv[2]);
  const char* check = IDL_VarGetString(argv[3]);

  /* check operator */
  if (kw.eq) {
    windop = GD_WINDOP_EQ;
    nop++;
  }
  if (kw.ne) {
    windop = GD_WINDOP_NE;
    nop++;
  }
  if (kw.le) {
    windop = GD_WINDOP_LE;
    nop++;
  }
  if (kw.lt) {
    windop = GD_WINDOP_LT;
    nop++;
  }
  if (kw.ge) {
    windop = GD_WINDOP_GE;
    nop++;
  }
  if (kw.gt) {
    windop = GD_WINDOP_GT;
    nop++;
  }
  if (kw.set) {
    windop = GD_WINDOP_SET;
    nop++;
  }
  if (kw.clr) {
    windop = GD_WINDOP_CLR;
    nop++;
  }

  if (nop > 1)
    GDIDL_KW_ABORT("Multiple operations specified");
  else if (nop == 0)
    GDIDL_KW_ABORT("No operation specified");

  switch (windop) {
    case GD_WINDOP_EQ:
    case GD_WINDOP_NE:
      threshold.i = IDL_Long64Scalar(argv[4]);
      break;
    case GD_WINDOP_SET:
    case GD_WINDOP_CLR:
      threshold.u = IDL_ULong64Scalar(argv[4]);
      break;
    default:
      threshold.r = IDL_DoubleScalar(argv[4]);
      break;
  }

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_window(D, parent, field_code, in_field, check, windop, threshold);
  } else
    gd_add_window(D, field_code, in_field, check, windop, threshold,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_mplex GD_ADD_MPLEX 5 5 KEYWORDS */
void gdidl_add_mplex(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING parent;
    int parent_x;
    int fragment_index;
    int max;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = kw.parent_x = kw.max = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "MAX", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(max) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* in_field = IDL_VarGetString(argv[2]);
  const char* count = IDL_VarGetString(argv[3]);
  int val = IDL_LongScalar(argv[4]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_mplex(D, parent, field_code, in_field, count, val, kw.max);
  } else
    gd_add_mplex(D, field_code, in_field, count, val, kw.max,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_window GD_ALTER_WINDOW 2 2 KEYWORDS */
void gdidl_alter_window(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int nop = 0;
  const char* in_field = NULL;
  const char* check = NULL;
  gd_triplet_t threshold;
  gd_windop_t windop = GD_WINDOP_UNK;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_VPTR threshold;
    int eq, ne, le, lt, gt, ge, set, clr;
    int threshold_x;
    IDL_STRING in_field;
    int in_field_x;
    IDL_STRING check;
    int check_x;
  } KW_RESULT;
  KW_RESULT kw;

  threshold.r = 0;
  kw.check_x = kw.in_field_x = kw.threshold_x = 0;
  kw.eq = kw.ne = kw.le = kw.lt = kw.gt = kw.ge = kw.set = kw.clr = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "CHECK_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(check_x),
      IDL_KW_OFFSETOF(check) },
    { "CLR", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(clr) },
    { "EQ", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(eq) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "GE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ge) },
    { "GT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(gt) },
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field_x),
      IDL_KW_OFFSETOF(in_field) },
    { "LE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(le) },
    { "LT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(lt) },
    { "NE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(ne) },
    { "SET", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(set) },
    { "THRESHOLD", 0, 1, IDL_KW_VIN, IDL_KW_OFFSETOF(threshold_x),
      IDL_KW_OFFSETOF(threshold) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  /* count operators */
  if (kw.eq) {
    windop = GD_WINDOP_EQ;
    nop++;
  }
  if (kw.ne) {
    windop = GD_WINDOP_NE;
    nop++;
  }
  if (kw.le) {
    windop = GD_WINDOP_LE;
    nop++;
  }
  if (kw.lt) {
    windop = GD_WINDOP_LT;
    nop++;
  }
  if (kw.ge) {
    windop = GD_WINDOP_GE;
    nop++;
  }
  if (kw.gt) {
    windop = GD_WINDOP_GT;
    nop++;
  }
  if (kw.set) {
    windop = GD_WINDOP_SET;
    nop++;
  }
  if (kw.clr) {
    windop = GD_WINDOP_CLR;
    nop++;
  }

  if (nop > 1)
    GDIDL_KW_ABORT("Multiple operations specified");

  /* Need the current windop and/or threshold */
  if (nop == 0 || !kw.threshold_x) {
    gd_entry_t E;
    gd_entry(D, field_code, &E);
    gd_free_entry_strings(&E);
    if (gd_error(D)) {
      GDIDL_SET_ERROR(D);

      IDL_KW_FREE;

      dreturnvoid();
      return;
    }
    if (nop == 0)
      windop = E.windop;
    if (!kw.threshold_x)
      threshold = E.threshold;
  }

  if (kw.in_field_x)
    in_field = IDL_STRING_STR(&kw.in_field);

  if (kw.check_x)
    check = IDL_STRING_STR(&kw.check);

  if (kw.threshold_x) {
    switch (windop) {
      case GD_WINDOP_EQ:
      case GD_WINDOP_NE:
        threshold.i = IDL_Long64Scalar(kw.threshold);
        break;
      case GD_WINDOP_SET:
      case GD_WINDOP_CLR:
        threshold.u = IDL_ULong64Scalar(kw.threshold);
        break;
      default:
        threshold.r = IDL_DoubleScalar(kw.threshold);
        break;
    }
  }

  gd_alter_window(D, field_code, in_field, check, windop, threshold);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_mplex GD_ALTER_MPLEX 2 2 KEYWORDS */
void gdidl_alter_mplex(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field1 = NULL;
  const char* in_field2 = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int val, val_x, period;
    IDL_STRING in_field1;
    int in_field1_x;
    IDL_STRING in_field2;
    int in_field2_x;
  } KW_RESULT;
  KW_RESULT kw;

  kw.in_field1_x = kw.in_field2_x = kw.val_x = 0;
  kw.period = -1;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "COUNT_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field2_x),
      IDL_KW_OFFSETOF(in_field2) },
    { "COUNT_VAL", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(val_x),
      IDL_KW_OFFSETOF(val) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "IN_FIELD", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(in_field1_x),
      IDL_KW_OFFSETOF(in_field1) },
    { "PERIOD", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(period) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  /* no value specified, figure out the previous value */
  if (!kw.val_x) {
    gd_entry_t E;
    gd_entry(D, field_code, &E);
    kw.val = E.count_val;
  }

  if (!gd_error(D)) {
    if (kw.in_field1_x)
      in_field1 = IDL_STRING_STR(&kw.in_field1);

    if (kw.in_field2_x)
      in_field2 = IDL_STRING_STR(&kw.in_field2);

    gd_alter_mplex(D, field_code, in_field1, in_field2, kw.val, kw.period);
  }

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_naliases GD_NALIASES 2 2 KEYWORDS */
IDL_VPTR gdidl_naliases(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  unsigned int nalias;

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  nalias = gd_naliases(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpUInt(nalias);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_alias_target GD_ALIAS_TARGET 2 2 KEYWORDS */
IDL_VPTR gdidl_alias_target(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  const char* name = gd_alias_target(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING((char*)name);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_aliases GD_ALIASES 2 2 KEYWORDS */
IDL_VPTR gdidl_aliases(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int i;
  unsigned int nalias;
  const char** list;

  GDIDL_KW_ONLY_ERROR;

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  nalias = gd_naliases(D, field_code);
  list = gd_aliases(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r;

  IDL_STRING *data = (IDL_STRING*)IDL_MakeTempVector(IDL_TYP_STRING, nalias,
      IDL_ARR_INI_ZERO, &r);
  for (i = 0; i < nalias; ++i)
    IDL_StrStore(data + i, (char*)list[i]);

  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_add_alias GD_ADD_ALIAS 3 3 KEYWORDS */
void gdidl_add_alias(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* target = IDL_VarGetString(argv[2]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_alias(D, parent, field_code, target);
  } else
    gd_add_alias(D, field_code, target, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_affixes GD_ALTER_AFFIXES 1 1 KEYWORDS */
void gdidl_alter_affixes(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  char *prefix = NULL;
  char *suffix = NULL;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
    IDL_STRING prefix;
    int prefix_x;
    IDL_STRING suffix;
    int suffix_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = kw.prefix_x = kw.suffix_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "PREFIX", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(prefix_x),
      IDL_KW_OFFSETOF(prefix) },
    { "SUFFIX", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(suffix_x),
      IDL_KW_OFFSETOF(suffix) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.prefix_x)
    prefix = IDL_STRING_STR(&kw.prefix);

  if (kw.suffix_x)
    suffix = IDL_STRING_STR(&kw.suffix);

  gd_alter_affixes(D, kw.fragment_index, prefix, suffix);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_fragment_affixes GD_FRAGMENT_AFFIXES 1 1 KEYWORDS */
IDL_VPTR gdidl_fragment_affixes(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  char *prefix;
  char *suffix;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int fragment_index;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment_index = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_fragment_affixes(D, kw.fragment_index, &prefix, &suffix);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r;

  IDL_STRING *data = (IDL_STRING*)IDL_MakeTempVector(IDL_TYP_STRING, 2,
      IDL_ARR_INI_ZERO, &r);
  IDL_StrStore(data, prefix);
  IDL_StrStore(data + 1, suffix);

  free(prefix);
  free(suffix);

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_fragment_namespace GD_FRAGMENT_NAMESPACE 1 1 KEYWORDS */
IDL_VPTR gdidl_fragment_namespace(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  DIRFILE *D;
  char *nsin = NULL;
  const char *nsout;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING ns;
    int ns_x, fragment;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.fragment = kw.ns_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment) },
    { "NAMESPACE", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(ns_x),
      IDL_KW_OFFSETOF(ns) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.ns_x)
    nsin = IDL_STRING_STR(&kw.ns);

  nsout = gd_fragment_namespace(D, kw.fragment, nsin);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING((char*)nsout);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_strtok GD_STRTOK 1 1 KEYWORDS */
IDL_VPTR gdidl_strtok(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char *string = NULL;
  char *token;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING string;
    int string_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.string_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "STRING", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(string_x),
      IDL_KW_OFFSETOF(string) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.string_x)
    string = IDL_STRING_STR(&kw.string);

  token = gd_strtok(D, string);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING(token);
  free(token);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_desync GD_DESYNC 1 1 KEYWORDS */
IDL_VPTR gdidl_desync(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int ret;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int pathcheck;
    int reopen;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.pathcheck = kw.reopen = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PATHCHECK", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pathcheck) },
    { "REOPEN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(reopen) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  ret = gd_desync(D, (kw.pathcheck ? GD_DESYNC_PATHCHECK : 0) |
      (kw.reopen ? GD_DESYNC_REOPEN : 0));

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(ret);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_flags GD_FLAGS 1 1 KEYWORDS */
IDL_VPTR gdidl_flags(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  unsigned long flags, set = 0, reset = 0;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    short int pretty, verbose;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.pretty = kw.verbose = -1;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PRETTY_PRINT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pretty) },
    { "VERBOSE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(verbose) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (kw.pretty == 1)
    set |= GD_PRETTY_PRINT;
  else if (kw.pretty == 0)
    reset |= GD_PRETTY_PRINT;

  if (kw.verbose == 1)
    set |= GD_VERBOSE;
  else if (kw.verbose == 0)
    reset |= GD_VERBOSE;

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  flags = gd_flags(D, set, reset);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(flags);
  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_verbose_prefix GD_VERBOSE_PREFIX 1 1 KEYWORDS */
void gdidl_verbose_prefix(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  char *prefix = NULL;
  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING prefix;
    int prefix_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.prefix_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PREFIX", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(prefix_x),
      IDL_KW_OFFSETOF(prefix) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.prefix_x)
    prefix = IDL_STRING_STR(&kw.prefix);

  gd_verbose_prefix(D, prefix);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_mplex_lookback GD_MPLEX_LOOKBACK 1 2 KEYWORDS */
void gdidl_mplex_lookback(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int lookback = GD_DEFAULT_LOOKBACK;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int all;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.all = 0;

  static IDL_KW_PAR kw_pars[] = {
    { "ALL", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(all) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (kw.all)
    lookback = GD_LOOKBACK_ALL;
  else if (argc > 1)
    lookback = IDL_LongScalar(argv[1]);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_mplex_lookback(D, lookback);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_get_linterp_tablename GD_LINTERP_TABLENAME 2 2 KEYWORDS */
IDL_VPTR gdidl_get_linterp_tablename(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char *field_code = IDL_VarGetString(argv[1]);

  char *name = gd_linterp_tablename(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING(name);
  free(name);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_encoding_support GD_ENCODING_SUPPORT 1 1 */
IDL_VPTR gdidl_encoding_support(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  IDL_VPTR r = IDL_GettmpInt(gd_encoding_support(IDL_LongScalar(argv[0])));
  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_add_sarray GD_ADD_SARRAY 2 2 KEYWORDS */
void gdidl_add_sarray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char **data = NULL;
  int i;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int n;
    IDL_VPTR value;
    int fragment_index;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.value = NULL;
  kw.fragment_index = kw.n = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FRAGMENT", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(fragment_index) },
    { "LENGTH", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(n) },
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "VALUE", 0, 1, IDL_KW_VIN, 0, IDL_KW_OFFSETOF(value) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  if (kw.value) {
    IDL_ENSURE_ARRAY(kw.value);
    IDL_ENSURE_STRING(kw.value);
    if (kw.value->value.arr->n_dim != 1)
      GDIDL_KW_ABORT("VALUE must be a vector, not a multidimensional array");
    kw.n = kw.value->value.arr->n_elts;
    data = malloc(kw.n * sizeof(const char*));
    for (i = 0; i < kw.n; ++i)
      data[i] = IDL_STRING_STR((IDL_STRING*)(kw.value->value.arr->data) + i);
  } else if (kw.n) {
    data = malloc(kw.n * sizeof(const char*));
    for (i = 0; i < kw.n; ++i)
      data[i] = "";
  } else
    GDIDL_KW_ABORT("either LENGTH or VALUE must be specified");

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_sarray(D, parent, field_code, kw.n, data);
  } else
    gd_add_sarray(D, field_code, kw.n, data, kw.fragment_index);

  free(data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_get_sarray GD_GET_SARRAY 2 2 KEYWORDS */
IDL_VPTR gdidl_get_sarray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();
  int ret = 1;
  const char **data = NULL;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    unsigned int start;
    unsigned int n;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.start = kw.n = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "LENGTH", IDL_TYP_UINT, 1, 0, 0, IDL_KW_OFFSETOF(n) },
    { "START", IDL_TYP_UINT, 1, 0, 0, IDL_KW_OFFSETOF(start) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  if (kw.n == 0) {
    kw.n = gd_array_len(D, field_code);
    if (kw.n > kw.start)
      kw.n -= kw.start;
    else
      kw.n = 0;
  }

  data = malloc(sizeof(*data) * kw.n);
  ret = gd_get_sarray_slice(D, field_code, kw.start, kw.n, data);

  IDL_VPTR r;
  if (!ret) {
    int i;
    IDL_STRING *idata = (IDL_STRING*)IDL_MakeTempVector(IDL_TYP_STRING, kw.n,
        IDL_ARR_INI_ZERO, &r);
    for (i = 0; i < kw.n; ++i)
      IDL_StrStore(idata + i, (char*)data[i]);
  } else {
    GDIDL_SET_ERROR(D);
    r = IDL_GettmpInt(0);
  }

  free(data);
  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_alter_sarray GD_ALTER_SARRAY 2 2 KEYWORDS */
void gdidl_alter_sarray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int len;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.len = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "LENGTH", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(len) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_alter_sarray(D, field_code, (size_t)kw.len);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_put_sarray GD_PUT_SARRAY 3 3 KEYWORDS */
void gdidl_put_sarray(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char **data;
  int i;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int start;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.start = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "START", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(start) },
    { NULL }
  };

  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  IDL_ENSURE_ARRAY(argv[2]);
  IDL_ENSURE_STRING(argv[2]);
  if (argv[2]->value.arr->n_dim != 1)
    GDIDL_KW_ABORT("data must be a vector, not a multidimensional array");

  int length = argv[2]->value.arr->n_elts;
  data = malloc(sizeof(*data) * length);
  for (i = 0; i < length; ++i)
    data[i] = IDL_STRING_STR((IDL_STRING*)(argv[2]->value.arr->data) + i);

  gd_put_sarray_slice(D, field_code, kw.start, length, data);

  GDIDL_SET_ERROR(D);

  free(data);
  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_add_indir GD_ADD_INDIR 4 4 KEYWORDS */
void gdidl_add_indir(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_add_yoke_(GD_INDIR_ENTRY, argc, argv, argk);

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_indir GD_ALTER_INDIR 2 2 KEYWORDS */
void gdidl_alter_indir(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_alter_yoke_(GD_INDIR_ENTRY, argc, argv, argk);

  dreturnvoid();
}

/* @@DLM: P gdidl_add_sindir GD_ADD_SINDIR 4 4 KEYWORDS */
void gdidl_add_sindir(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_add_yoke_(GD_SINDIR_ENTRY, argc, argv, argk);

  dreturnvoid();
}

/* @@DLM: P gdidl_alter_sindir GD_ALTER_SINDIR 2 2 KEYWORDS */
void gdidl_alter_sindir(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  gdidl_alter_yoke_(GD_SINDIR_ENTRY, argc, argv, argk);

  dreturnvoid();
}

/**** Module initialisation ****/

/* These are defined in the automatically generated sublist.c */
extern IDL_SYSFUN_DEF2 gdidl_procs[];
extern int gdidl_n_procs;
extern IDL_SYSFUN_DEF2 gdidl_funcs[];
extern int gdidl_n_funcs;

/* These are defined in the automatically generated constants.c */
extern IDL_STRUCT_TAG_DEF gdidl_constants[];
/* @@DLM: F gdidl_generate_constants GETDATA_CONSTANTS 0 0 */
extern IDL_VPTR gdidl_generate_constants(int argc, IDL_VPTR argv[], char *argk);

/* GD_ENTRY structure form */
static IDL_MEMINT lincom_dims[] = { 1, GD_MAX_LINCOM };
static IDL_MEMINT polynom_dims[] = { 1, GD_MAX_POLYORD + 1 };
static IDL_STRUCT_TAG_DEF gdidl_entry[] = {
  { "FIELD",      0, (void*)IDL_TYP_STRING },
  { "FIELD_TYPE", 0, (void*)IDL_TYP_INT },
  { "FLAGS",      0, (void*)IDL_TYP_UINT },
  { "FRAGMENT",   0, (void*)IDL_TYP_INT },

  { "IN_FIELDS",  lincom_dims, (void*)IDL_TYP_STRING },
  { "A",          polynom_dims, (void*)IDL_TYP_DOUBLE }, /* POLYNOM */
  { "CA",         polynom_dims, (void*)IDL_TYP_DCOMPLEX }, /* POLYNOM */
  { "ARRAY_LEN",  0, (void*)IDL_TYP_INT }, /* CARRAY / SARRAY */
  { "B",          lincom_dims, (void*)IDL_TYP_DOUBLE }, /* LINCOM */
  { "CB",         lincom_dims, (void*)IDL_TYP_DCOMPLEX }, /* LINCOM */
  { "BITNUM",     0, (void*)IDL_TYP_INT }, /* (S)BIT */
  { "COUNT_VAL",  0, (void*)IDL_TYP_INT }, /* MPLEX */
  { "DATA_TYPE",  0, (void*)IDL_TYP_INT }, /* RAW / CONST / CARRAY */
  { "DIVIDEND",   0, (void*)IDL_TYP_DOUBLE }, /* RECIP */
  { "CDIVIDEND",  0, (void*)IDL_TYP_DCOMPLEX }, /* RECIP */
  { "M",          lincom_dims, (void*)IDL_TYP_DOUBLE }, /* LINCOM */
  { "CM",         lincom_dims, (void*)IDL_TYP_DCOMPLEX }, /* LINCOM */
  { "N_FIELDS",   0, (void*)IDL_TYP_INT },  /* LINCOM */
  { "NUMBITS",    0, (void*)IDL_TYP_INT }, /* (S)BIT */
  { "PERIOD",     0, (void*)IDL_TYP_INT }, /* MPLEX */
  { "POLY_ORD",   0, (void*)IDL_TYP_INT }, /* POLYNOM */
  { "SCALAR",     polynom_dims, (void*)IDL_TYP_STRING },
  { "SCALAR_IND", polynom_dims, (void*)IDL_TYP_INT },
  { "SHIFT",      0, (void*)IDL_TYP_LONG }, /* PHASE */
  { "SPF",        0, (void*)IDL_TYP_UINT }, /* RAW */
  { "TABLE",      0, (void*)IDL_TYP_STRING }, /* LINTERP */
  { "UTHRESHOLD", 0, (void*)IDL_TYP_ULONG }, /* WINDOW */
  { "ITHRESHOLD", 0, (void*)IDL_TYP_LONG }, /* WINDOW */
  { "RTHRESHOLD", 0, (void*)IDL_TYP_DOUBLE }, /* WINDOW */
  { "WINDOP",     0, (void*)IDL_TYP_INT }, /* WINDOW */
  { NULL }
};

int IDL_Load(void)
{
  dtracevoid();

  IDL_SysRtnAdd(gdidl_procs, IDL_FALSE, gdidl_n_procs);
  IDL_SysRtnAdd(gdidl_funcs, IDL_TRUE, gdidl_n_funcs);

  /* entry struct */
  gdidl_entry_def = IDL_MakeStruct("GD_ENTRY", gdidl_entry);
  gdidl_const_def = IDL_MakeStruct("GD_CONSTANTS", gdidl_constants);

  dreturn("%i", 1);
  return 1;
}
