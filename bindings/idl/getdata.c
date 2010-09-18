/* (C) 2009-2010 D. V. Wiebe
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
#include "../../src/internal.h"

#define GDIDL_N_DIRFILES 1024
static DIRFILE* idldirfiles[GDIDL_N_DIRFILES];
static int idldirfiles_initialised = 0;

static IDL_StructDefPtr gdidl_entry_def = NULL;
IDL_StructDefPtr gdidl_const_def = NULL;

/* Remember: there's a longjmp here -- in general this will play merry havoc
 * with our debugging messagecruft */
#define idl_abort(s) do { dreturnvoid(); \
  IDL_Message(IDL_M_GENERIC, IDL_MSG_LONGJMP, s); } while(0)
#define idl_kw_abort(s) do { IDL_KW_FREE; idl_abort(s); } while(0)
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
  idl_abort("DIRFILE space exhausted.");
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
static inline UCHAR gdidl_idl_type(gd_type_t t) {
  switch (t) {
    case GD_UINT8:
      return IDL_TYP_BYTE;
    case GD_UINT16:
      return IDL_TYP_UINT;
    case GD_INT8: /* there is no signed 8-bit type in IDL
                     - we type promote to INT */
    case GD_INT16:
      return IDL_TYP_INT;
    case GD_UINT32:
      return IDL_TYP_ULONG;
    case GD_INT32:
      return IDL_TYP_LONG;
    case GD_UINT64:
      return IDL_TYP_ULONG64;
    case GD_INT64:
      return IDL_TYP_LONG64;
    case GD_FLOAT32:
      return IDL_TYP_FLOAT;
    case GD_FLOAT64:
      return IDL_TYP_DOUBLE;
    case GD_COMPLEX64:
      return IDL_TYP_COMPLEX;
    case GD_COMPLEX128:
      return IDL_TYP_DCOMPLEX;
    case GD_NULL:
    case GD_UNKNOWN:
      ;
  }

  return IDL_TYP_UNDEF;
}

/* convert an IDL type code to a GetData type code */
static inline gd_type_t gdidl_gd_type(int t) {
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
static inline IDL_ALLTYPES gdidl_to_alltypes(gd_type_t t, void* d)
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
    case GD_UNKNOWN:
      ;
  }

  dreturnvoid();
  return v;
}

/* convert an ALLTYPES to a value suitable for GetData -- all we do is 
 * reference the appropriate member */
static inline const void* gdidl_from_alltypes(UCHAR t, IDL_ALLTYPES* v)
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
static inline void gdidl_cmp_to_c99(double complex* dest, IDL_COMPLEX* src,
    size_t n)
{
  dtrace("%p, %p, %zi", dest, src, n);

  size_t i;

  for (i = 0; i < n; ++i)
    dest[i] = src[i].r + _Complex_I * src[i].i;

  dreturnvoid();
}

/* copy (and convert) an array of complex values */
static inline void gdidl_dcmp_to_c99(double complex* dest, IDL_DCOMPLEX* src,
    size_t n)
{
  dtrace("%p, %p, %zi", dest, src, n);

  size_t i;

  for (i = 0; i < n; ++i)
    dest[i] = src[i].r + _Complex_I * src[i].i;

  dreturnvoid();
}

/* copy (and convert) an array of complex values */
static inline void gdidl_c99_to_dcmp(IDL_DCOMPLEX* dest,
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

  /* accept either a scalar or a single element array */
  if (obj->flags & IDL_V_ARR) {
    if (obj->value.arr->n_dim != 1 || obj->value.arr->dim[0] != 1)
      idl_abort("Scalar or single element array expected where multiple "
          "element array found");

    if (obj->type == IDL_TYP_DCOMPLEX) {
      r = ((IDL_DCOMPLEX *)obj->value.arr->data)[0].r;
      i = ((IDL_DCOMPLEX *)obj->value.arr->data)[0].i;
    } else if (obj->type == IDL_TYP_COMPLEX) {
      r = ((IDL_COMPLEX *)obj->value.arr->data)[0].r;
      i = ((IDL_COMPLEX *)obj->value.arr->data)[0].i;
    } else
      idl_abort("complex value expected");
  } else {
    IDL_ENSURE_SCALAR(obj);

    if (obj->type == IDL_TYP_DCOMPLEX) {
      r = obj->value.dcmp.r;
      i = obj->value.dcmp.i;
    } else if (obj->type == IDL_TYP_COMPLEX) {
      r = obj->value.cmp.r;
      i = obj->value.cmp.i;
    } else
      idl_abort("complex value expected");
  }

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
      || E->field_type == GD_DIVIDE_ENTRY || E->field_type == GD_RECIP_ENTRY)
  {
    IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "IN_FIELDS", IDL_MSG_LONGJMP, NULL)), E->in_fields[0]);
  }

  switch (E->field_type)
  {
    case GD_RAW_ENTRY:
      *(IDL_UINT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "SPF",
            IDL_MSG_LONGJMP, NULL)) = E->u.raw.spf;
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "DATA_TYPE",
            IDL_MSG_LONGJMP, NULL)) = E->u.raw.type;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      break;
    case GD_LINCOM_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "N_FIELDS", IDL_MSG_LONGJMP, NULL)) = E->u.lincom.n_fields;
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "COMP_SCAL", IDL_MSG_LONGJMP, NULL)) = E->comp_scal;
      for (i = 0; i < E->u.lincom.n_fields; ++i) {
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "IN_FIELDS",
                IDL_MSG_LONGJMP, NULL)) + i, E->in_fields[i]);
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "SCALAR",
                IDL_MSG_LONGJMP, NULL)) + i, E->scalar[i]);
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "SCALAR",
                IDL_MSG_LONGJMP, NULL)) + i + GD_MAX_LINCOM,
            E->scalar[i + GD_MAX_LINCOM]);
      }
      if (E->comp_scal) {
        gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
            IDL_StructTagInfoByName(gdidl_entry_def, "CM", IDL_MSG_LONGJMP,
              NULL)), E->u.lincom.cm, E->u.lincom.n_fields);
        gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
            IDL_StructTagInfoByName(gdidl_entry_def, "CB", IDL_MSG_LONGJMP,
              NULL)), E->u.lincom.cb, E->u.lincom.n_fields);
      } else {
        memcpy(data + IDL_StructTagInfoByName(gdidl_entry_def, "M",
              IDL_MSG_LONGJMP, NULL), E->u.lincom.m, E->u.lincom.n_fields *
            sizeof(double));
        memcpy(data + IDL_StructTagInfoByName(gdidl_entry_def, "B",
              IDL_MSG_LONGJMP, NULL), E->u.lincom.b, E->u.lincom.n_fields *
            sizeof(double));
      }
      break;
    case GD_LINTERP_ENTRY:
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "TABLE", IDL_MSG_LONGJMP, NULL)), E->u.linterp.table);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "BITNUM",
            IDL_MSG_LONGJMP, NULL)) = E->u.bit.bitnum;
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "NUMBITS",
            IDL_MSG_LONGJMP, NULL)) = E->u.bit.numbits;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)) + 1, E->scalar[1]);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "IN_FIELDS", IDL_MSG_LONGJMP, NULL)) + 1, E->in_fields[1]);
      break;
    case GD_RECIP_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "COMP_SCAL", IDL_MSG_LONGJMP, NULL)) = E->comp_scal;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);

      if (E->comp_scal)
        gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "CDIVIDEND",
              IDL_MSG_LONGJMP, NULL)), &E->u.recip.cdividend, 1);
      else
        *(double*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "DIVIDEND",
              IDL_MSG_LONGJMP, NULL)) = E->u.recip.dividend;
      break;
    case GD_PHASE_ENTRY:
      *(IDL_LONG*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "SHIFT",
            IDL_MSG_LONGJMP, NULL)) = E->u.phase.shift;
      IDL_StrStore((IDL_STRING*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
              "SCALAR", IDL_MSG_LONGJMP, NULL)), E->scalar[0]);
      break;
    case GD_POLYNOM_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "COMP_SCAL", IDL_MSG_LONGJMP, NULL)) = E->comp_scal;
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def,
            "POLY_ORD", IDL_MSG_LONGJMP, NULL)) = E->u.polynom.poly_ord;

      for (i = 0; i <= E->u.polynom.poly_ord; ++i)
        IDL_StrStore((IDL_STRING*)(data +
              IDL_StructTagInfoByName(gdidl_entry_def, "SCALAR",
                IDL_MSG_LONGJMP, NULL)) + i, E->scalar[i]);

      if (E->comp_scal)
        gdidl_c99_to_dcmp((IDL_DCOMPLEX*)(data +
            IDL_StructTagInfoByName(gdidl_entry_def, "CA", IDL_MSG_LONGJMP,
              NULL)), E->u.polynom.ca, E->u.polynom.poly_ord + 1);
      else
        memcpy(data + IDL_StructTagInfoByName(gdidl_entry_def, "A",
              IDL_MSG_LONGJMP, NULL), E->u.polynom.a,
            (E->u.polynom.poly_ord + 1) * sizeof(double));
      break;
    case GD_CONST_ENTRY:
      *(IDL_INT*)(data + IDL_StructTagInfoByName(gdidl_entry_def, "DATA_TYPE",
            IDL_MSG_LONGJMP, NULL)) = E->u.cons.type;
      break;
    case GD_NO_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      break;
  }

  dreturn("%p", r);
  return r;
}

/* convert an IDL structure into an gd_entry_t */
void gdidl_read_idl_entry(gd_entry_t *E, IDL_VPTR v, int alter)
{
  /* this function is fairly agnostic about the structure it's given: so
   * long as it gets a structure with the fields it wants (of the right type)
   * it's happy */

  dtrace("%p, %p, %i", E, v, alter);

  IDL_VPTR d;
  IDL_MEMINT o;
  int i, n = 0;
  int copy_scalar[GD_MAX_POLYORD + 1];
  int action = (alter) ? IDL_MSG_RET | IDL_MSG_ATTR_NOPRINT : IDL_MSG_LONGJMP;

  memset(copy_scalar, 0, sizeof(int) * (GD_MAX_POLYORD + 1));
  memset(E, 0, sizeof(gd_entry_t));

  unsigned char* data = v->value.s.arr->data;

  if (!alter) {
    /* field */
    o = IDL_StructTagInfoByName(v->value.s.sdef, "FIELD", IDL_MSG_LONGJMP, &d);
    IDL_ENSURE_STRING(d);
    E->field = IDL_STRING_STR((IDL_STRING*)(data + o));

    /* fragment_index */
    o = IDL_StructTagInfoByName(v->value.s.sdef, "FRAGMENT", IDL_MSG_LONGJMP,
        &d);
    if (d->type != IDL_TYP_INT)
      idl_abort("GD_ENTRY element FRAGMENT must be of type INT");
    E->fragment_index = *(int16_t*)(data + o);
  }

  /* field_type */
  o = IDL_StructTagInfoByName(v->value.s.sdef, "FIELD_TYPE", IDL_MSG_LONGJMP,
      &d);
  if (d->type != IDL_TYP_INT)
    idl_abort("GD_ENTRY element FIELD_TYPE must be of type INT");
  E->field_type = *(int16_t*)(data + o);

  /* the common case in_fields */
  if (E->field_type == GD_BIT_ENTRY || E->field_type == GD_LINTERP_ENTRY
      || E->field_type == GD_MULTIPLY_ENTRY || E->field_type == GD_PHASE_ENTRY
      || E->field_type == GD_SBIT_ENTRY || E->field_type == GD_POLYNOM_ENTRY
      || E->field_type == GD_DIVIDE_ENTRY || E->field_type == GD_RECIP_ENTRY)
  {
    o = IDL_StructTagInfoByName(v->value.s.sdef, "IN_FIELDS", action, &d);
    if (o != -1) {
      IDL_ENSURE_STRING(d);
      E->in_fields[0] = IDL_STRING_STR((IDL_STRING*)(data + o));
    }
  }

  switch (E->field_type)
  {
    case GD_RAW_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "SPF", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_UINT)
          idl_abort("GD_ENTRY element INDEX must be of type UINT");
        E->u.raw.spf = *(uint16_t*)(data + o);
      }

      o = IDL_StructTagInfoByName(v->value.s.sdef, "DATA_TYPE", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element DATA_TYPE must be of type INT");
        E->u.raw.type = *(int16_t*)(data + o);
      } else
        E->u.raw.type = GD_NULL;

      copy_scalar[0] = 1;

      break;
    case GD_LINCOM_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "N_FIELDS", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element N_FIELDS must be of type INT");
        n = E->u.lincom.n_fields = *(int16_t*)(data + o);
      }

      o = IDL_StructTagInfoByName(v->value.s.sdef, "COMP_SCAL", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element COMP_SCAL must be of type INT");
        E->comp_scal = *(int16_t*)(data + o);
      }

      o = IDL_StructTagInfoByName(v->value.s.sdef, "IN_FIELDS", action, &d);
      if (o != -1) {
        IDL_ENSURE_STRING(d);
        IDL_ENSURE_ARRAY(d);
        if (n == 0) {
          n = d->value.arr->dim[0];
          if (n > GD_MAX_LINCOM)
            n = GD_MAX_LINCOM;
        }
        for (i = 0; i < n; ++i)
          E->in_fields[i] = IDL_STRING_STR((IDL_STRING*)(data + o) + i);
      }

      for (i = 0 ; i < n; ++i) {
        copy_scalar[i] = 1;
        copy_scalar[i + GD_MAX_LINCOM] = 1;
      }

      if (E->comp_scal) {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "CM", action, &d);
        if (o != -1) {
          IDL_ENSURE_ARRAY(d);
          if (n == 0) {
            n = d->value.arr->dim[0];
            if (n > GD_MAX_LINCOM)
              n = GD_MAX_LINCOM;
          }
          if (d->type == IDL_TYP_DCOMPLEX)
            gdidl_dcmp_to_c99(E->u.lincom.cm, (IDL_DCOMPLEX*)data + o, n);
          else
            idl_abort("GD_ENTRY element CM must be of type DCOMPLEX");
        }
      } else {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "M", action, &d);
        if (o != -1) {
          IDL_ENSURE_ARRAY(d);
          if (n == 0) {
            n = d->value.arr->dim[0];
            if (n > GD_MAX_LINCOM)
              n = GD_MAX_LINCOM;
          }
          if (d->type == IDL_TYP_DOUBLE) {
            memcpy(E->u.lincom.m, data + o, n * sizeof(double));
            for (i = 0; i < n; ++i)
              E->u.lincom.cm[i] = E->u.lincom.m[i];
          } else
            idl_abort("GD_ENTRY element M must be of type DOUBLE");
        }
      }

      if (E->comp_scal) {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "CB", action, &d);
        if (o != -1) {
          IDL_ENSURE_ARRAY(d);
          if (n == 0) {
            n = d->value.arr->dim[0];
            if (n > GD_MAX_LINCOM)
              n = GD_MAX_LINCOM;
          }
          if (d->type == IDL_TYP_DCOMPLEX)
            gdidl_dcmp_to_c99(E->u.lincom.cb, (IDL_DCOMPLEX*)data + o,
                E->u.lincom.n_fields);
          else
            idl_abort("GD_ENTRY element CB must be of type DCOMPLEX");
        }
      } else {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "B", action, &d);
        if (o != -1) {
          IDL_ENSURE_ARRAY(d);
          if (n == 0) {
            n = d->value.arr->dim[0];
            if (n > GD_MAX_LINCOM)
              n = GD_MAX_LINCOM;
          }
          if (d->type == IDL_TYP_DOUBLE) {
            memcpy(E->u.lincom.b, data + o, E->u.lincom.n_fields *
                sizeof(double));
            for (i = 0; i < E->u.lincom.n_fields; ++i)
              E->u.lincom.cb[i] = E->u.lincom.b[i];
          } else
            idl_abort("GD_ENTRY element B must be of type DOUBLE");
        }
      }
      break;
    case GD_LINTERP_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "TABLE", action, &d);
      if (o != -1) {
        IDL_ENSURE_STRING(d);
        E->field = IDL_STRING_STR((IDL_STRING*)(data + o));
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "BITNUM", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element BITNUM must be of type INT");
        E->u.bit.bitnum = *(int16_t*)(data + o);
      } else
        E->u.bit.bitnum = -1;

      o = IDL_StructTagInfoByName(v->value.s.sdef, "NUMBITS", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element NUMBITS must be of type INT");
        E->u.bit.numbits = *(int16_t*)(data + o);
      }

      copy_scalar[0] = copy_scalar[1] = 1;
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "IN_FIELDS", action, &d);
      if (o != -1) {
        IDL_ENSURE_STRING(d);
        E->in_fields[1] = IDL_STRING_STR((IDL_STRING*)(data + o) + 1);
      }
      break;
    case GD_RECIP_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "COMP_SCAL", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element COMP_SCAL must be of type INT");
        E->comp_scal = *(int16_t*)(data + o);
      }

      if (E->comp_scal) {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "CDIVIDEND", action, &d);
        if (o != -1) {
          if (d->type == IDL_TYP_DCOMPLEX)
            gdidl_dcmp_to_c99(&E->u.recip.cdividend, (IDL_DCOMPLEX*)data + o,
                1);
          else
            idl_abort("GD_ENTRY element CDIVIDEND must be of type DCOMPLEX");
        }
      } else {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "DIVIDEND", action, &d);
        if (o != -1) {
          if (d->type == IDL_TYP_DOUBLE)
            E->u.recip.dividend = *(double*)(data + o);
          else
            idl_abort("GD_ENTRY element DIVIDEND must be of type DOUBLE");
        }
      }
      copy_scalar[0] = 1;
      break;
    case GD_PHASE_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "SHIFT", IDL_MSG_LONGJMP,
          &d);
      if (d->type != IDL_TYP_LONG)
        idl_abort("GD_ENTRY element SHIFT must be of type LONG");
      E->u.phase.shift = *(int16_t*)(data + o);
      copy_scalar[0] = 1;
      break;
    case GD_POLYNOM_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "POLY_ORD", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element POLY_ORD must be of type INT");
        E->u.polynom.poly_ord = *(int16_t*)(data + o);
        n = E->u.polynom.poly_ord + 1;
      }

      for (i = 0 ; i < n; ++i)
        copy_scalar[i] = 1;

      o = IDL_StructTagInfoByName(v->value.s.sdef, "COMP_SCAL", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element COMP_SCAL must be of type INT");
        E->comp_scal = *(int16_t*)(data + o);
      }

      if (E->comp_scal) {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "CA", action, &d);
        if (o != -1) {
          IDL_ENSURE_ARRAY(d);
          if (n == 0) {
            n = d->value.arr->dim[0];
            if (n > GD_MAX_POLYORD + 1)
              n = GD_MAX_POLYORD;
          }
          if (d->type == IDL_TYP_DCOMPLEX)
            gdidl_dcmp_to_c99(E->u.polynom.ca, (IDL_DCOMPLEX*)data + o, n);
          else
            idl_abort("GD_ENTRY element CA must be of type DCOMPLEX");
        }
      } else {
        o = IDL_StructTagInfoByName(v->value.s.sdef, "A", action, &d);
        if (o != -1) {
          IDL_ENSURE_ARRAY(d);
          if (n == 0) {
            n = d->value.arr->dim[0];
            if (n > GD_MAX_POLYORD + 1)
              n = GD_MAX_POLYORD;
          }
          if (d->type == IDL_TYP_DOUBLE)
            memcpy(E->u.polynom.a, data + o, n * sizeof(double));
          else
            idl_abort("GD_ENTRY element A must be of type DOUBLE");
        }
      }
      break;
    case GD_CONST_ENTRY:
      o = IDL_StructTagInfoByName(v->value.s.sdef, "DATA_TYPE", action, &d);
      if (o != -1) {
        if (d->type != IDL_TYP_INT)
          idl_abort("GD_ENTRY element DATA_TYPE must be of type INT");
        E->u.cons.type = *(int16_t*)(data + o);
      } else
        E->u.cons.type = GD_NULL;
      break;
    case GD_NO_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      break;
  }

  /* scalars */
  o = IDL_StructTagInfoByName(v->value.s.sdef, "SCALAR", action, &d);
  if (o != -1) {
    for (i = 0; i < GD_MAX_POLYORD + 1; ++i)
      if (copy_scalar[i]) {
        E->scalar[i] = IDL_STRING_STR((IDL_STRING*)(data + o) + i);
        if (E->scalar[i][0] == '\0')
          E->scalar[i] = NULL;
      }
  } else 
    for (i = 0; i < GD_MAX_POLYORD + 1; ++i)
      E->scalar[i] = NULL;

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
    else if (strcasecmp(enc, "LZMA"))
      encoding = GD_LZMA_ENCODED;
    else if (strcasecmp(enc, "SLIM"))
      encoding = GD_SLIM_ENCODED;
    else if (strcasecmp(enc, "TEXT"))
      encoding = GD_TEXT_ENCODED;
    else if (strcasecmp(enc, "NONE"))
      encoding = GD_UNENCODED;
    else if (strcasecmp(enc, "RAW"))
      encoding = GD_UNENCODED;
    else if (strcasecmp(enc, "UNENCODED"))
      encoding = GD_UNENCODED;
    else if (strcasecmp(enc, "AUTO"))
      encoding = GD_AUTO_ENCODED;
    else
      idl_abort("Unknown encoding type.");
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

/* @@DLM: P gdidl_gd_add GD_ADD 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_add GD_MADD 2 2 KEYWORDS */
void gdidl_gd_add(int argc, IDL_VPTR argv[], char *argk)
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
  gdidl_read_idl_entry(&E, argv[1], 0);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd(D, &E, parent);
  } else
    gd_add(D, &E);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_add_bit GD_ADD_BIT 3 3 KEYWORDS */
/* @@DLM: P gdidl_gd_add_bit GD_MADD_BIT 3 3 KEYWORDS */
void gdidl_gd_add_bit(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_add_const GD_ADD_CONST 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_add_const GD_MADD_CONST 2 2 KEYWORDS */
void gdidl_gd_add_const(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  double float64 = 0;
  uint64_t uint64;
  int64_t int64;

  void* ptr = &float64;
  gd_type_t data_type = GD_FLOAT64;

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

  /* Hey, guys: if the caller passes us something that's not a valid type
   * code, we still convert the value to something and then let the C library
   * complain about things. */
  if (kw.value) {
    if (kw.const_type & GD_IEEE754)
      float64 = IDL_DoubleScalar(kw.value);
    else if (kw.const_type & GD_SIGNED) {
      int64 = IDL_Long64Scalar(kw.value);
      ptr = &int64;
      data_type = GD_INT64;
    } else {
      uint64 = IDL_ULong64Scalar(kw.value);
      ptr = &uint64;
      data_type = GD_UINT64;
    }
  }

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_const(D, parent, field_code, kw.const_type, data_type, ptr);
  } else
    gd_add_const(D, field_code, kw.const_type, data_type, ptr,
        kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_add_lincom GD_ADD_LINCOM 5 11 KEYWORDS */
/* @@DLM: P gdidl_gd_add_lincom GD_ADD_CLINCOM 5 11 KEYWORDS */
/* @@DLM: P gdidl_gd_add_lincom GD_MADD_CLINCOM 5 11 KEYWORDS */
/* @@DLM: P gdidl_gd_add_lincom GD_MADD_LINCOM 5 11 KEYWORDS */
void gdidl_gd_add_lincom(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_add_linterp GD_ADD_LINTERP 4 4 KEYWORDS */
/* @@DLM: P gdidl_gd_add_linterp GD_MADD_LINTERP 4 4 KEYWORDS */
void gdidl_gd_add_linterp(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_add_multiply GD_ADD_MULTIPLY 4 4 KEYWORDS */
/* @@DLM: P gdidl_gd_add_multiply GD_MADD_MULTIPLY 4 4 KEYWORDS */
void gdidl_gd_add_multiply(int argc, IDL_VPTR argv[], char *argk)
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
  const char* in_field1 = IDL_VarGetString(argv[2]);
  const char* in_field2 = IDL_VarGetString(argv[3]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_multiply(D, parent, field_code, in_field1, in_field2);
  } else
    gd_add_multiply(D, field_code, in_field1, in_field2, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_add_divide GD_ADD_DIVIDE 4 4 KEYWORDS */
/* @@DLM: P gdidl_gd_add_divide GD_MADD_DIVIDE 4 4 KEYWORDS */
void gdidl_gd_add_divide(int argc, IDL_VPTR argv[], char *argk)
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
  const char* in_field1 = IDL_VarGetString(argv[2]);
  const char* in_field2 = IDL_VarGetString(argv[3]);

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    gd_madd_divide(D, parent, field_code, in_field1, in_field2);
  } else
    gd_add_divide(D, field_code, in_field1, in_field2, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_add_recip GD_ADD_RECIP 4 4 KEYWORDS */
/* @@DLM: P gdidl_gd_add_recip GD_ADD_CRECIP 4 4 KEYWORDS */
/* @@DLM: P gdidl_gd_add_recip GD_MADD_RECIP 4 4 KEYWORDS */
/* @@DLM: P gdidl_gd_add_recip GD_MADD_CRECIP 4 4 KEYWORDS */
void gdidl_gd_add_recip(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int comp_scal = 0;
  double complex cdividend = 0;
  double dividend = 0;

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
  const char* in_field1 = IDL_VarGetString(argv[2]);

  if (argv[3]->type == IDL_TYP_DCOMPLEX || argv[3]->type == IDL_TYP_COMPLEX) {
    comp_scal = 1;
    cdividend = gdidl_dcomplexScalar(argv[3]);
  } else
    dividend = IDL_DoubleScalar(argv[3]);

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

/* @@DLM: P gdidl_gd_add_phase GD_ADD_PHASE 4 4 KEYWORDS */
/* @@DLM: P gdidl_gd_add_phase GD_MADD_PHASE 4 4 KEYWORDS */
void gdidl_gd_add_phase(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_add_polynom GD_ADD_POLYNOM 4 9 KEYWORDS */
/* @@DLM: P gdidl_gd_add_polynom GD_ADD_CPOLYNOM 4 8 KEYWORDS */
/* @@DLM: P gdidl_gd_add_polynom GD_MADD_CPOLYNOM 4 9 KEYWORDS */
/* @@DLM: P gdidl_gd_add_polynom GD_MADD_POLYNOM 4 9 KEYWORDS */
void gdidl_gd_add_polynom(int argc, IDL_VPTR argv[], char *argk)
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
      idl_kw_abort("The array of coeffecients may only have a single "
          "dimension");

    poly_ord = argv[3]->value.arr->dim[0] - 1;
    if (poly_ord < 1)
      idl_kw_abort("The array of coeffecients must have at least two elements");
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
          idl_kw_abort("The coeffecients must be of scalar type");
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

/* @@DLM: P gdidl_gd_add_raw GD_ADD_RAW 3 3 KEYWORDS */
void gdidl_gd_add_raw(int argc, IDL_VPTR argv[], char *argk)
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
/* @@DLM: P gdidl_gd_add_sbit GD_ADD_SBIT 3 3 KEYWORDS */
/* @@DLM: P gdidl_gd_add_sbit GD_MADD_SBIT 3 3 KEYWORDS */
void gdidl_gd_add_sbit(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_add_spec GD_ADD_SPEC 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_add_spec GD_MADD_SPEC 2 2 KEYWORDS */
void gdidl_gd_add_spec(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_add_string GD_ADD_STRING 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_add_string GD_MADD_STRING 2 2 KEYWORDS */
void gdidl_gd_add_string(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_bit GD_ALTER_BIT 2 2 KEYWORDS */
void gdidl_gd_alter_bit(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_const GD_ALTER_CONST 2 2 KEYWORDS */
void gdidl_gd_alter_const(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_encoding GD_ALTER_ENCODING 2 2 KEYWORDS */
void gdidl_gd_alter_encoding(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_endianness GD_ALTER_ENDIANNESS 1 1 KEYWORDS */
void gdidl_gd_alter_endianness(int argc, IDL_VPTR argv[], char *argk)
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
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  if (!kw.fragment_index_x)
    kw.fragment_index = GD_ALL_FRAGMENTS;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_alter_endianness(D, (kw.big_end ? GD_BIG_ENDIAN : 0) | 
      (kw.little_end ? GD_LITTLE_ENDIAN : 0) | (kw.arm_end ? GD_ARM_ENDIAN : 0),
      kw.fragment_index, kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_alter_entry GD_ALTER_ENTRY 3 3 KEYWORDS */
void gdidl_gd_alter_entry(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int recode;
  } KW_RESULT;
  KW_RESULT kw;

  gd_entry_t E;

  kw.recode = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "RECODE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(recode) },
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  IDL_ENSURE_STRUCTURE(argv[2]);
  gdidl_read_idl_entry(&E, argv[2], 1);

  gd_alter_entry(D, field_code, &E, kw.recode);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_alter_frameoffset GD_ALTER_FRAMEOFFSET 2 2 KEYWORDS */
void gdidl_gd_alter_frameoffset(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_lincom GD_ALTER_LINCOM 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_alter_lincom GD_ALTER_CLINCOM 2 2 KEYWORDS */
void gdidl_gd_alter_lincom(int argc, IDL_VPTR argv[], char *argk)
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
  int comp_scal = 1;

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
      idl_kw_abort("IN_FIELDS must be a vector");

    if (kw.n_fields == 0) {
      kw.n_fields = kw.in_field->value.arr->dim[0];
      if (kw.n_fields > GD_MAX_LINCOM)
        kw.n_fields = GD_MAX_LINCOM;
    } else if (kw.in_field->value.arr->dim[0] < kw.n_fields) 
      idl_kw_abort("Insufficient number of elements in IN_FIELDS");

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
      idl_kw_abort("Insufficient number of elements in M");

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
      idl_kw_abort("Insufficient number of elements in B");

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

/* @@DLM: P gdidl_gd_alter_linterp GD_ALTER_LINTERP 2 2 KEYWORDS */
void gdidl_gd_alter_linterp(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_multiply GD_ALTER_MULTIPLY 2 2 KEYWORDS */
void gdidl_gd_alter_multiply(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field1 = NULL;
  const char* in_field2 = NULL;

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
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.in_field1_x)
    in_field1 = IDL_STRING_STR(&kw.in_field1);
  if (kw.in_field2_x)
    in_field2 = IDL_STRING_STR(&kw.in_field2);

  gd_alter_multiply(D, field_code, in_field1, in_field2);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_alter_divide GD_ALTER_DIVIDE 2 2 KEYWORDS */
void gdidl_gd_alter_divide(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  const char* in_field1 = NULL;
  const char* in_field2 = NULL;

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
  const char* field_code = IDL_VarGetString(argv[1]);

  if (kw.in_field1_x)
    in_field1 = IDL_STRING_STR(&kw.in_field1);
  if (kw.in_field2_x)
    in_field2 = IDL_STRING_STR(&kw.in_field2);

  gd_alter_divide(D, field_code, in_field1, in_field2);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_alter_recip GD_ALTER_RECIP 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_alter_recip GD_ALTER_CRECIP 2 2 KEYWORDS */
void gdidl_gd_alter_recip(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_phase GD_ALTER_PHASE 2 2 KEYWORDS */
void gdidl_gd_alter_phase(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_polynom GD_ALTER_POLYNOM 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_alter_polynom GD_ALTER_CPOLYNOM 2 2 KEYWORDS */
void gdidl_gd_alter_polynom(int argc, IDL_VPTR argv[], char *argk)
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

  int comp_scal = 1;
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
      idl_kw_abort("Insufficient number of elements in A");

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

/* @@DLM: P gdidl_gd_alter_raw GD_ALTER_RAW 2 2 KEYWORDS */
void gdidl_gd_alter_raw(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_sbit GD_ALTER_SBIT 2 2 KEYWORDS */
void gdidl_gd_alter_sbit(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_alter_spec GD_ALTER_SPEC 2 2 KEYWORDS */
/* @@DLM: P gdidl_gd_alter_spec GD_MALTER_SPEC 2 2 KEYWORDS */
void gdidl_gd_alter_spec(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_close GD_CLOSE 1 1 KEYWORDS */
void gdidl_gd_close(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_delete GD_DELETE 2 2 KEYWORDS */
void gdidl_gd_delete(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int data;
    int deref;
    int force;
  } KW_RESULT;
  KW_RESULT kw;

  kw.data = kw.deref = kw.force = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    { "DEL_DATA", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(data) },
    { "DEREF", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(deref) },
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FORCE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(force) }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_delete(D, field_code, (kw.data ? GD_DEL_DATA : 0) |
      (kw.deref ? GD_DEL_DEREF : 0) | (kw.force) ? GD_DEL_FORCE : 0);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_flush GD_FLUSH 1 1 KEYWORDS */
void gdidl_gd_flush(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    IDL_STRING field_code;
    int field_code_x;
  } KW_RESULT;
  KW_RESULT kw;

  const char* field_code = NULL;

  GDIDL_KW_INIT_ERROR;
  kw.field_code_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FIELD_CODE", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(field_code_x),
      IDL_KW_OFFSETOF(field_code) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.field_code_x)
    field_code = IDL_STRING_STR(&kw.field_code);

  gd_flush(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_include GD_INCLUDE 2 2 KEYWORDS */
void gdidl_gd_include(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

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
    int pedantic;
    int permissive;
    int trunc;
    int enc_x;
    IDL_VPTR enc;
    IDL_VPTR index;
    int fragment_index;
    int index_x;
  } KW_RESULT;
  KW_RESULT kw;
  kw.big_end = kw.creat = kw.excl = kw.force_enc = kw.force_end =
    kw.ignore_dups = kw.ignore_refs = kw.little_end = kw.pedantic = kw.trunc =
    kw.enc_x = kw.index_x = kw.fragment_index = 0;
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
    { "PEDANTIC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pedantic) },
    { "PERMISSIVE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(permissive) },
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
    | (kw.pedantic ? GD_PEDANTIC : 0) | (kw.permissive ? GD_PERMISSIVE : 0)
    | (kw.trunc ? GD_TRUNC : 0);

  if (kw.enc_x)
    flags |= gdidl_convert_encoding(kw.enc);

  int index = (int16_t)gd_include(D, file, kw.fragment_index, flags);

  if (kw.index_x) {
    IDL_ALLTYPES v;
    v.i = index;
    IDL_StoreScalar(kw.index, IDL_TYP_INT, &v);
  }

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_metaflush GD_METAFLUSH 1 1 KEYWORDS */
void gdidl_gd_metaflush(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_metaflush(D);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_rewrite_fragment GD_REWRITE_FRAGMENT 1 1 KEYWORDS */
void gdidl_gd_rewrite_fragment(int argc, IDL_VPTR argv[], char *argk)
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
      IDL_KW_OFFSETOF(fragment_index) }
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

/* @@DLM: P gdidl_gd_move GD_MOVE 3 3 KEYWORDS */
void gdidl_gd_move(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int move_data;
  } KW_RESULT;
  KW_RESULT kw;

  kw.move_data = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "MOVE_DATA", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(move_data) },
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_move(D, field_code, IDL_LongScalar(argv[2]), kw.move_data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_gd_open GD_OPEN 1 1 KEYWORDS */
IDL_VPTR gdidl_gd_open(int argc, IDL_VPTR argv[], char *argk)
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
    int little_end;
    int pedantic;
    int permissive;
    int trunc;
    int verbose;
    int enc_x;
    IDL_VPTR enc;
  } KW_RESULT;
  KW_RESULT kw;
  kw.rdwr = kw.big_end = kw.creat = kw.excl = kw.force_enc = kw.force_end =
    kw.ignore_dups = kw.little_end = kw.pedantic = kw.trunc = kw.verbose =
    kw.enc_x = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    IDL_KW_FAST_SCAN,
    { "ARM_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(big_end) },
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
    { "LITTLE_ENDIAN", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(little_end) },
    { "PEDANTIC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(pedantic) },
    { "RDWR", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(rdwr) },
    { "TRUNC", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(trunc) },
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
    | (kw.little_end ? GD_LITTLE_ENDIAN : 0)
    | (kw.pedantic ? GD_PEDANTIC : 0) | (kw.permissive ? GD_PERMISSIVE : 0)
    | (kw.trunc ? GD_TRUNC : 0) | (kw.verbose ? GD_VERBOSE : 0);

  if (kw.enc_x)
    flags |= gdidl_convert_encoding(kw.enc);

  DIRFILE* D = gd_open(name, flags);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpInt(gdidl_set_dirfile(D));
  dreturn("%p", r);
  return r;
}

/* @@DLM: P gdidl_gd_alter_protection GD_ALTER_PROTECTION 2 2 KEYWORDS */
void gdidl_gd_alter_protection(int argc, IDL_VPTR argv[], char *argk)
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

/* @@DLM: P gdidl_gd_reference GD_REFERENCE 2 2 KEYWORDS */
void gdidl_gd_reference(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  gd_reference(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_rename GD_RENAME 3 3 KEYWORDS */
void gdidl_gd_rename(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    int move_data;
  } KW_RESULT;
  KW_RESULT kw;

  kw.move_data = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "MOVE_DATA", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(move_data) },
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);
  const char* new_code = IDL_VarGetString(argv[2]);

  gd_rename(D, field_code, new_code, kw.move_data);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: P gdidl_gd_uninclude GD_UNINCLUDE 2 2 KEYWORDS */
void gdidl_gd_uninclude(int argc, IDL_VPTR argv[], char *argk)
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
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  gd_uninclude(D, IDL_LongScalar(argv[1]), kw.del);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  dreturnvoid();
}

/* @@DLM: F gdidl_gd_validate GD_VALIDATE 2 2 KEYWORDS */
IDL_VPTR gdidl_gd_validate(int argc, IDL_VPTR argv[], char *argk)
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
    gd_type_t return_type;
  } KW_RESULT;
  KW_RESULT kw;

  IDL_VPTR r;

  kw.first_frame = kw.first_sample = kw.n_frames = kw.n_samples = 0;
  kw.return_type = GD_FLOAT64;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FIRST_FRAME", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(first_frame) },
    { "FIRST_SAMPLE", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(first_sample) },
    { "NUM_FRAMES", IDL_TYP_LONG, 1, 0, 0, IDL_KW_OFFSETOF(n_frames) },
    { "NUM_SAMPLES", IDL_TYP_LONG, 1, 0, 0, IDL_KW_OFFSETOF(n_samples) },
    { "TYPE", IDL_TYP_INT, 1, 0, 0, IDL_KW_OFFSETOF(return_type) }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  /* no signed 8-bit type in IDL */
  if (kw.return_type == GD_INT8)
    idl_kw_abort("Cannot return data as a signed 8-bit integer.");

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  unsigned int spf = gd_spf(D, field_code);

  if (gd_error(D))
    r = IDL_GettmpInt(0);
  else {
    void* data = malloc((kw.n_frames * spf + kw.n_samples) *
        GD_SIZE(kw.return_type));

    gd_getdata64(D, field_code, kw.first_frame, kw.first_sample, kw.n_frames,
        kw.n_samples, kw.return_type, data);

    if (gd_error(D))
      r = IDL_GettmpInt(0);
    else {
      IDL_MEMINT dim[] = { kw.n_frames * spf + kw.n_samples };
      r = IDL_ImportArray(1, dim, gdidl_idl_type(kw.return_type), data,
          (IDL_ARRAY_FREE_CB)free, NULL);
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

  off64_t bof = gd_bof(D, field_code);

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

  void* data = malloc(8);

  int ret = gd_get_constant(D, field_code, kw.const_type, data);

  IDL_VPTR r;
  if (!ret) {
    r = IDL_Gettmp();
    r->value = gdidl_to_alltypes(kw.const_type, data);
    r->type = gdidl_idl_type(kw.const_type);
  } else {
    GDIDL_SET_ERROR(D);
    r = IDL_GettmpInt(0);
  }

  IDL_KW_FREE;

  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_constants GD_CONSTANTS 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_constants GD_MCONSTANTS 1 1 KEYWORDS */
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
    idl_kw_abort("Cannot return data as a signed 8-bit integer.");

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
    IDL_ALLTYPES v = gdidl_to_alltypes(kw.const_type, data);
    IDL_StoreScalar(r, gdidl_idl_type(kw.const_type), &v);
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

/* @@DLM: F gdidl_gd_error GD_ERROR 1 1 */
IDL_VPTR gdidl_gd_error(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int err = gd_error(gdidl_get_dirfile(IDL_LongScalar(argv[0])));

  IDL_VPTR r = IDL_GettmpInt(err);
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_gd_error_string GD_ERROR_STRING 1 1 */
IDL_VPTR gdidl_gd_error_string(int argc, IDL_VPTR argv[], char *argk)
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

  off64_t eof = gd_eof(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)eof;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_field_list GD_FIELD_LIST 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_field_list GD_FIELD_LIST_BY_TYPE 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_field_list GD_MFIELD_LIST 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_field_list GD_MFIELD_LIST_BY_TYPE 1 1 KEYWORDS */
IDL_VPTR gdidl_get_field_list(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  int i;
  unsigned int nfields;
  const char** list;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    gd_type_t type;
    int type_x;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.type = kw.type_x = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "TYPE", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(type_x),
      IDL_KW_OFFSETOF(type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    if (kw.type_x) {
      nfields = gd_nmfields_by_type(D, parent, kw.type);
      list = gd_mfield_list_by_type(D, parent, kw.type);
    } else {
      nfields = gd_nmfields(D, parent);
      list = gd_mfield_list(D, parent);
    }
  } else {
    if (kw.type_x) {
      nfields = gd_nfields_by_type(D, kw.type);
      list = gd_field_list_by_type(D, kw.type);
    } else {
      nfields = gd_nfields(D);
      list = gd_field_list(D);
    }
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
/* @@DLM: F gdidl_get_framenum GD_FRAMENUM_SUBSET 3 3 KEYWORDS */
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

  off64_t foffs = gd_frameoffset64(D, kw.fragment_index);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (long long)foffs;
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

/* @@DLM: F gdidl_get_nfields GD_NFIELDS 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_nfields GD_NFIELDS_BY_TYPE 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_nfields GD_NMFIELDS 1 1 KEYWORDS */
/* @@DLM: F gdidl_get_nfields GD_NMFIELDS_BY_TYPE 1 1 KEYWORDS */
IDL_VPTR gdidl_get_nfields(int argc, IDL_VPTR argv[], char *argk)
{
  dtraceidl();

  unsigned int nfields;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    GDIDL_KW_RESULT_ERROR;
    gd_type_t type;
    int type_x;
    IDL_STRING parent;
    int parent_x;
  } KW_RESULT;
  KW_RESULT kw;

  GDIDL_KW_INIT_ERROR;
  kw.type = kw.type_x = 0;
  kw.parent_x = 0;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "PARENT", IDL_TYP_STRING, 1, 0, IDL_KW_OFFSETOF(parent_x),
      IDL_KW_OFFSETOF(parent) },
    { "TYPE", IDL_TYP_INT, 1, 0, IDL_KW_OFFSETOF(type_x),
      IDL_KW_OFFSETOF(type) },
    { NULL }
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));

  if (kw.parent_x) {
    const char* parent = IDL_STRING_STR(&kw.parent);
    if (kw.type_x) 
      nfields = gd_nmfields_by_type(D, parent, kw.type);
    else 
      nfields = gd_nmfields(D, parent);
  } else {
    if (kw.type_x) 
      nfields = gd_nfields_by_type(D, kw.type);
    else 
      nfields = gd_nfields(D);
  }

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_GettmpLong(nfields);
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

  off64_t nframes = gd_nframes64(D);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_Gettmp();
  r->type = IDL_TYP_LONG64;
  r->value.l64 = (IDL_LONG64)nframes;
  dreturn("%p", r);
  return r;
}

/* @@DLM: F gdidl_get_nvectors GD_NMVECTORS 1 1 KEYWORDS */
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

  GDIDL_KW_ONLY_ERROR;

  DIRFILE* D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  const char* name = gd_raw_filename(D, field_code);

  GDIDL_SET_ERROR(D);

  IDL_KW_FREE;

  IDL_VPTR r = IDL_StrToSTRING((char*)name);
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
/* @@DLM: F gdidl_get_strings GD_MSTRINGS 1 1 KEYWORDS */
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
/* @@DLM: F gdidl_get_vector_list GD_MVECTOR_LIST 1 1 KEYWORDS */
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
  } KW_RESULT;
  KW_RESULT kw;

  kw.first_frame = kw.first_sample = 0;
  GDIDL_KW_INIT_ERROR;

  static IDL_KW_PAR kw_pars[] = {
    GDIDL_KW_PAR_ERROR,
    GDIDL_KW_PAR_ESTRING,
    { "FIRST_FRAME", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(first_frame) },
    { "FIRST_SAMPLE", IDL_TYP_LONG64, 1, 0, 0, IDL_KW_OFFSETOF(first_sample) },
  };

  IDL_KWProcessByOffset(argc, argv, argk, kw_pars, NULL, 1, &kw);

  DIRFILE *D = gdidl_get_dirfile(IDL_LongScalar(argv[0]));
  const char* field_code = IDL_VarGetString(argv[1]);

  IDL_ENSURE_ARRAY(argv[2]);
  if (argv[2]->value.arr->n_dim != 1)
    idl_kw_abort("data must be a vector, not a multidimensional array");

  off64_t n_samples = argv[2]->value.arr->n_elts;

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

/**** Module initialisation ****/

/* These are defined in the automatically generated sublist.c */
extern IDL_SYSFUN_DEF2 gdidl_procs[];
extern int gdidl_n_procs;
extern IDL_SYSFUN_DEF2 gdidl_funcs[];
extern int gdidl_n_funcs;

/* These are defined in the automatically gnerated constants.c */
extern IDL_STRUCT_TAG_DEF gdidl_constants[];
/* @@DLM: F gdidl_generate_constants GETDATA_CONSTANTS 0 0 */
extern IDL_VPTR gdidl_generate_constants(int argc, IDL_VPTR argv[], char *argk);

/* GD_ENTRY structure form */
static IDL_MEMINT lincom_dims[] = { 1, GD_MAX_LINCOM };
static IDL_MEMINT polynom_dims[] = { 1, GD_MAX_POLYORD + 1 };
static IDL_STRUCT_TAG_DEF gdidl_entry[] = {
  { "FIELD",      0, (void*)IDL_TYP_STRING },
  { "FIELD_TYPE", 0, (void*)IDL_TYP_INT },
  { "FRAGMENT",   0, (void*)IDL_TYP_INT },

  { "IN_FIELDS",  lincom_dims, (void*)IDL_TYP_STRING },
  { "A",          polynom_dims, (void*)IDL_TYP_DOUBLE }, /* POLYNOM */
  { "CA",         polynom_dims, (void*)IDL_TYP_DCOMPLEX }, /* POLYNOM */
  { "B",          lincom_dims, (void*)IDL_TYP_DOUBLE }, /* LINCOM */
  { "CB",         lincom_dims, (void*)IDL_TYP_DCOMPLEX }, /* LINCOM */
  { "BITNUM",     0, (void*)IDL_TYP_INT }, /* (S)BIT */
  { "COMP_SCAL",  0, (void*)IDL_TYP_INT }, /* LINCOM / POLYNOM */
  { "DATA_TYPE",  0, (void*)IDL_TYP_INT }, /* RAW / CONST */
  { "DIVIDEND",   0, (void*)IDL_TYP_DOUBLE }, /* RECIP */
  { "CDIVIDEND",  0, (void*)IDL_TYP_DCOMPLEX }, /* RECIP */
  { "M",          lincom_dims, (void*)IDL_TYP_DOUBLE }, /* LINCOM */
  { "CM",         lincom_dims, (void*)IDL_TYP_DCOMPLEX }, /* LINCOM */
  { "N_FIELDS",   0, (void*)IDL_TYP_INT },  /* LINCOM */
  { "NUMBITS",    0, (void*)IDL_TYP_INT }, /* (S)BIT */
  { "POLY_ORD",   0, (void*)IDL_TYP_INT }, /* POLYNOM */
  { "SCALAR",     polynom_dims, (void*)IDL_TYP_STRING },
  { "SHIFT",      0, (void*)IDL_TYP_LONG }, /* PHASE */
  { "SPF",        0, (void*)IDL_TYP_UINT }, /* RAW */
  { "TABLE",      0, (void*)IDL_TYP_STRING }, /* LINTERP */
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
