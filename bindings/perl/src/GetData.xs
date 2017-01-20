/* Copyright (C) 2011-2017 D. V. Wiebe
 *
 **************************************************************************
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
 *
 */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#undef _BSD_SOURCE
#undef _POSIX_SOURCE
#undef _SVID_SOURCE
#undef VERSION
#include "internal.h"

#ifdef GETDATA_DEBUG
#include "debug.c"
#endif

/* Perl 5.8.9 and earlier don't provide hv_fetchs */
#ifdef hv_fetchs
# define gdp_hv_fetchs hv_fetchs
#else
# define gdp_hv_fetchs(hv,key,lval) hv_fetch(hv, key, sizeof(key) - 1, lval)
#endif

#define undef &PL_sv_undef

/* fake data types to simplify our typemap */
typedef GD_DCOMPLEXP_t gdp_complex_in;
typedef GD_DCOMPLEXP_t gdp_complex_undef;
typedef GD_DCOMPLEXA(gdp_complex);
typedef int gdp_ffff_t;
typedef int gdp_numbits_t;
typedef gd_int64_t gdp_int64_t;
typedef unsigned int gdp_uint_t;
typedef gd_type_t gdp_type_t;
typedef int gdp_int;
typedef const char gdp_char;
typedef gd_entry_t gdp_pentry_t;

#define GDP_DIRFILE_ALIAS \
  const char *gdp_package = ix ? "GetData::Dirifle" : "GetData";

#define GDP_UNDEF_ON_ERROR(x) \
      if (gd_error(dirfile)) { x; dreturnvoid(); XSRETURN_UNDEF; }

#define GDP_PUSHpvn(s)       PUSHs(sv_2mortal(newSVpvn(s, sizeof(s) - 1)))
#define GDP_PUSHpvz(s)       PUSHs(sv_2mortal(newSVpv(s, 0)))
#define GDP_PUSHuv(s)        PUSHs(sv_2mortal(newSVuv(s)))
#define GDP_PUSHiv(s)        PUSHs(sv_2mortal(newSViv(s)))
#define GDP_PUSHnv(s)        PUSHs(sv_2mortal(newSVnv(s)))
#define GDP_PUSHrv(s)        PUSHs(sv_2mortal(newRV_noinc((SV*)s)))
#define GDP_PUSHcmp(r,i)     PUSHs(sv_2mortal(gdp_newSVcmp(r,i)))
#define GDP_PUSHrvavpv(s,n)  PUSHs(sv_2mortal(gdp_newRVavpv((const char**)s,n)))
#define GDP_PUSHrvavcmp(s,n) PUSHs(sv_2mortal(gdp_newRVavcmp(s,n)))
#define GDP_PUSHrvavnv(s,n)  PUSHs(sv_2mortal(gdp_newRVavnv(s,n)))

struct gdp_callback_stuff_t {
  SV *func;
  SV *data;
};

struct gdp_dirfile_t {
  DIRFILE *D;
  struct gdp_callback_stuff_t cbdata;
};

static DIRFILE *gdp_invalid = NULL;

/* Newx wrapper for the C library to use */
static void *gdp_malloc(size_t n)
{
  void *ptr;
  dtrace("%zu", n);

  Newx(ptr, n, char);

  dreturn("%p", ptr);
  return ptr;
}

/* sv might be NULL, indicating undef */
static gd_type_t gdp_get_type(SV **sv, const char *pkg, const char *func)
{
  dtrace("%p, \"%s\", \"%s\"", sv, pkg, func);

  if (sv == NULL || *sv == undef)
    croak("%s::%s() - Value may not be undef", pkg, func);

  if (sv_isa(*sv, "Math::Complex")) {
    dreturn("%03x", GD_COMPLEX128);
    return GD_COMPLEX128;
  } else if (SvTYPE(*sv) == SVt_IV) {
    dreturn("%03x", GD_INT64);
    return GD_INT64;
  }

  dreturn("%03x", GD_FLOAT64);
  return GD_FLOAT64;
}

/* convert a reference to a list of strings into a const char** */
static const char **gdp_convert_const_avpv(SV *src, size_t *len_out,
  const char *pkg, const char *func)
{
  const char **dst;

  dtrace("%p, %p, \"%s\", \"%s\"", src, len_out, pkg, func);

  if (src == undef) {
    dst = NULL;
    if (len_out)
      *len_out = 0;
  } else if (SvROK(src) && SvTYPE(SvRV(src)) == SVt_PVAV) {
    AV *av = (AV*)SvRV(src);
    I32 i, len = av_len(av);

    Newx(dst, 1 + len, const char*);
    for (i = 0; i <= len; ++i) {
      SV **sv = av_fetch(av, i, 0);
      if (sv == NULL || SvTYPE(*sv) != SVt_PV) {
        safefree(dst);
        croak("%s::%s() - Expected array of strings", pkg, func);
      }
      dst[i] = SvPV_nolen(*av_fetch(av, i, 0));
    }
    if (len_out)
      *len_out = (size_t)len + 1;
  } else
    croak("%s::%s - Expected array of strings", pkg, func);

  dreturn("%p", dst);
  return dst;
}

/* convert a string list or a reference to a list into a const char ** */
static const char **gdp_convert_strarr(size_t *len_out,  I32 items, I32 ax,
    int offs, const char *pkg, const char *func)
{
  const char **dst;

  dtrace("%p, %i, %i, %i, \"%s\", \"%s\"", len_out, (int)items, (int)ax,
      offs, pkg, func);

  /* if we have more than one data argument, or the first argument is a
   * string, assume it's not a reference */
  if (items - offs > 1 || SvTYPE(ST(offs)) == SVt_PVAV) {
    I32 i, len;
    len = items - offs;
    Newx(dst, items - offs, const char*);
    for (i = 0; i < len; ++i) {
      SV *sv = ST(offs + i);
      if (SvTYPE(sv) != SVt_PV) {
        safefree(dst);
        croak("%s::%s() - Expected array of strings", pkg, func);
      }
      dst[i] = SvPV_nolen(sv);
    }

    if (len_out)
      *len_out = len;
  } else
    dst = gdp_convert_const_avpv(ST(offs), len_out, pkg, func);

  dreturn("%p", dst);
  return dst;
}

/* convert a Perl object into a complex number */
static void gdp_convert_cmp(GD_DCOMPLEXP_t val, SV *src, int *ok,
    const char* pkg, const char *func)
{
  dtrace("%p, %p, %p, \"%s\", \"%s\"", val, src, ok, pkg, func);

  if (sv_isa(src, "Math::Complex")) {
    HV *hv = (HV *)SvRV(src);
    SV **c_dirty = gdp_hv_fetchs(hv, "c_dirty", 0);
    if (c_dirty == NULL)
      croak("%s::%s() - Malformed Math::Complex object", pkg, func);
    if (SvIV(*c_dirty)) {
      SV **sv = gdp_hv_fetchs(hv, "polar", 0);
      if (sv == NULL || !SvROK(*sv) || SvTYPE(SvRV(*sv)) != SVt_PVAV)
        croak("%s::%s() - Malformed Math::Complex object", pkg, func);

      AV *data = (AV*)SvRV(*sv);
      SV **m = av_fetch(data, 0, 0);
      SV **a = av_fetch(data, 1, 0);
      if (m == NULL || a == NULL)
        croak("%s::%s() - Malformed Math::Complex object", pkg, func);
      gd_po2cp_(val, SvNV(*m), SvNV(*a));
    } else {
      SV **sv = gdp_hv_fetchs(hv, "cartesian", 0);
      if (sv == NULL || !SvROK(*sv) || SvTYPE(SvRV(*sv)) != SVt_PVAV)
        croak("%s::%s() - Malformed Math::Complex object", pkg, func);

      AV *data = (AV*)SvRV(*sv);
      SV **r = av_fetch(data, 0, 0);
      SV **i = av_fetch(data, 1, 0);
      if (r == NULL || i == NULL)
        croak("%s::%s() - Malformed Math::Complex object", pkg, func);
      gd_li2cp_(val, SvNV(*r), SvNV(*i));
    }
  } else if (ok) {
    /* if ok is non-NULL, the caller is prepared to handle non-complex data */
    *ok = 0;
  } else {
    gd_rs2cp_(val, SvNV(src));
  }

  dreturn("(%g;%g)", crealp(val), cimagp(val));
}

#define GDP_EHASH_FETCH(part,key) \
  v = gdp_hv_fetchs((HV*)sv, key, 0); \
  if (!part && v == NULL) \
    croak("%s::%s() - Missing required key '" key "' in entry hash", pkg, func)

/* handle both "<foo>" and "c<foo>" names */
#define GDP_EHASH_FETCH_CMP(part,key,member) do { \
    GDP_EHASH_FETCH(1,"c" key); \
    if (v == NULL) GDP_EHASH_FETCH(part,key); \
    if (v) gdp_convert_cmp(gd_csp_(E->member), *v, NULL, pkg, func); \
  } while(0)

#define GDP_EHASH_FETCH_IV(part,key,variable,type) \
  do { GDP_EHASH_FETCH(part,key); if (v) variable = (type)SvIV(*v); } while(0)

#define GDP_EHASH_FETCH_NV(part,key,member) \
  do { GDP_EHASH_FETCH(part,key); if (v) E->member = SvNV(*v); } while(0)

#define GDP_EHASH_FETCH_UV(part,key,member,type) \
  do { GDP_EHASH_FETCH(part,key); if (v) E->member = (type)SvUV(*v); } while(0)

#define GDP_EHASH_FETCH_PV(part,key,member) \
 do { \
    GDP_EHASH_FETCH(part,key); \
    if (v) E->member = (!SvOK(*v)) ? NULL : SvPV_nolen(*v); \
  } while(0)

/* populate a complex double array in gd_entry_t */
static int gdp_fetch_cmp_list(GD_DCOMPLEXV(c), HV *hv, int partial, char key,
    int min, int max, unsigned mask, const char *pkg, const char *func)
{
  dtrace("%p, %p, %i, '%c', %i, %i, 0x%X, \"%s\", \"%s\"", c, hv, partial, key,
      min, max, mask, pkg, func);

  int i, n = 0;
  char ckey[3] = { 'c', key, 0 };
  int have[GD_MAX_POLYORD + 1];
  SV **v, *sv = NULL;

  /* try without the 'c' prefix */
  v = hv_fetch(hv, ckey + 1, 1, 0);

  /* try with the 'c' prefix */
  if (v == NULL)
    v = hv_fetch(hv, ckey, 2, 0);

  /* de-reference as needed */
  if (v) {
    sv = *v;
    while (SvROK(sv))
      sv = SvRV(sv);
  }

  if (sv == NULL || SvTYPE(sv) == SVt_NULL) {
    if (partial) {
      dreturn("%i", 0);
      return 0;
    }

    croak("%s::%s() - Missing required key '%c' in entry hash", pkg, func,
        key);
  }

  Zero(have, GD_MAX_POLYORD + 1, int);
  for (i = 0; i < max; ++i)
    if (mask & (1 << i))
      have[i] = 1;

  if (SvTYPE(sv) != SVt_PVAV)
    croak("%s::%s() - Key '%c' must be list in entry hash (%i)", pkg, func, key,
    SvTYPE(sv));

  for (i = 0; i < GD_MAX_LINCOM; ++i)
    if (!have[i]) {
      v = av_fetch((AV*)sv, i, 0);
      if (v) {
        if (i < max)
          gdp_convert_cmp(gd_cap_(c,i), *v, NULL, pkg, func);
        have[i] = 1;
      }
    }

  /* find n */
  for (i = 0; i < GD_MAX_POLYORD + 1; ++i)
    if (i >= n && have[n])
      n = i + 1;

  if (n < min || n > max)
    croak("%s::%s() - Bad array length (%i) for key '%c' in entry hash", pkg,
        func, n, key);

  dreturn("%i", n);
  return n;
}

/* populate in_fields in gd_entry_t */
static int gdp_fetch_in_fields(char **in_fields, SV *sv, int partial, int min,
    int max, const char *pkg, const char *func)
{
  dtrace("%p, %p, %i, %i, %i, \"%s\", \"%s\"", in_fields, sv, partial, min, max,
      pkg, func);

  int i, n = 0;
  SV **v, *vv;

  GDP_EHASH_FETCH(partial, "in_fields");
  if (partial && !v) {
    dreturn("%i", 0);
    return 0;
  }

  /* de-reference as needed */
  vv = *v;
  while (SvROK(vv))
    vv = SvRV(vv);

  if (SvTYPE(vv) == SVt_NULL) { /* undef */
    dreturn("%i", 0);
    return 0;
  }

  if (SvTYPE(vv) != SVt_PVAV) {
    if (SvOK(vv)) {
      n = 1;
      in_fields[0] = SvPV_nolen(vv);
    } else
      croak("%s::%s() - Key 'in_fields' must be list or string in entry hash",
          pkg, func);
  } else {
    int have[GD_MAX_LINCOM * 2];

    Zero(have, GD_MAX_LINCOM * 2, int);

    for (i = 0; i < GD_MAX_LINCOM; ++i) {
      v = av_fetch((AV*)vv, i, 0);
      if (v) {
        if (i < max)
          in_fields[i] = SvPV_nolen(*v);
        have[i] = 1;
      }
    }

    /* find n */
    for (i = 0; i < GD_MAX_LINCOM; ++i)
      if (i >= n && have[n])
        n = i + 1;
  }

  if (n < min || n > max) {
    croak("%s::%s() - Bad array length (%i) for key 'in_fields' in entry hash",
        pkg, func, n);
  }

  dreturn("%i", n);
  return n;
}

/* populate scalar elements of gd_entry_t */
static unsigned gdp_fetch_scalars(gd_entry_t *E, HV *hv, unsigned int mask,
  const char *pkg, const char* func)
{
  dtrace("%p, %p, %06x \"%s\", \"%s\"", E, hv, mask, pkg, func);
  int i;
  unsigned mask_out = 0;

  SV *scalar, *scalar_ind = NULL;
  SV **v = gdp_hv_fetchs(hv, "scalar", 0);

  /* there's no point in recording scalar indicies if we don't have scalars */
  if (v == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  scalar = *v;
  while (SvROK(scalar))
    scalar = SvRV(scalar);

  if (SvTYPE(scalar) == SVt_NULL) { /* drop undef */
    dreturn("%i", 0);
    return 0;
  }

  v = gdp_hv_fetchs(hv, "scalar_ind", 0);
  if (v) {
    scalar_ind = *v;
    while ((SvTYPE(scalar_ind) != SVt_NULL) && SvROK(scalar_ind))
      scalar_ind = SvRV(scalar_ind);

    if (SvTYPE(scalar_ind) == SVt_NULL)
      scalar_ind = NULL;
  }

  if (SvTYPE(scalar) != SVt_PVAV)
    croak("%s::%s() - Key 'scalar' must be list in entry hash", pkg, func);
  if (scalar_ind && SvTYPE(scalar_ind) != SVt_PVAV)
    croak("%s::%s() - Key 'scalar_ind' must be list in entry hash", pkg, func);

  for (i = 0; i <= GD_MAX_POLYORD; ++i)
    if (mask & (1 << i)) {
      v = av_fetch((AV*)scalar, i, 0);
      if (v == NULL || SvTYPE(*v) == SVt_NULL)
        E->scalar[i] = NULL; /* skip */
      else {
        E->scalar[i] = SvPV_nolen(*v);
        mask_out |= (1 << i);
        if (scalar_ind) {
          v = av_fetch((AV*)scalar_ind, i, 0);
          if (v && SvTYPE(*v) != SVt_NULL)
            E->scalar_ind[i] = SvIV(*v);
          else
            E->scalar_ind[i] = 0;
        } else
          E->scalar_ind[i] = 0;
      }
    }

  dreturn("%u", mask_out);
  return mask_out;
}

/* convert a Perl hash into a gd_entry_t */
static void gdp_to_entry(gd_entry_t *E, SV *sv, const gd_entry_t *old_E,
    const char *pkg, const char *func)
{
  dtrace("%p, %p, %p, \"%s\", \"%s\"", E, sv, old_E, pkg, func);
  SV **v;
  int n, min, max;
  unsigned mask, tmask;
  const int partial = (old_E != NULL);

  if (partial)
    StructCopy(old_E, E, gd_entry_t);
  else
    Zero(E, 1, gd_entry_t);

  /* de-reference as needed */
  while (SvROK(sv))
    sv = SvRV(sv);

  if (SvTYPE(sv) != SVt_PVHV)
    croak("%s::%s() - Entry must be hash", pkg, func);

  GDP_EHASH_FETCH_UV(0, "field_type", field_type, gd_entype_t);
  GDP_EHASH_FETCH_PV(partial, "field", field);
  GDP_EHASH_FETCH_UV(partial, "fragment_index", fragment_index, int);

  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 1, 1, pkg, func);

      mask = gdp_fetch_scalars(E, (HV*)sv, 0x3, pkg, func);

      if (!(mask & 1))
        GDP_EHASH_FETCH_UV(partial, "bitnum", EN(bit,bitnum), int);

      if (!(mask & 2)) {
        GDP_EHASH_FETCH_UV(1, "numbits", EN(bit,numbits), int);
        if (v == NULL)
          if (!partial)
            E->EN(bit,numbits) = 1;
      }

      break;
    case GD_SARRAY_ENTRY:
      GDP_EHASH_FETCH_UV(partial, "array_len", EN(scalar,array_len), size_t);
      break;
    case GD_CARRAY_ENTRY:
      GDP_EHASH_FETCH_UV(partial, "array_len", EN(scalar,array_len), size_t);
      /* fallthrough */
    case GD_CONST_ENTRY:
      GDP_EHASH_FETCH_UV(partial, "const_type", EN(scalar,const_type),
          gd_type_t);
      break;
    case GD_LINCOM_ENTRY:
      GDP_EHASH_FETCH_IV(1, "n_fields", n, int);
      if (v) {
        if (n > GD_MAX_LINCOM || n < 1)
          croak("%s::%s() - n_fields out of range", pkg, func);

        min = max = n;
      } else {
        min = 1;
        max = GD_MAX_LINCOM;
      }

      E->EN(lincom,n_fields) = gdp_fetch_in_fields(E->in_fields, sv, partial,
          min, max, pkg, func);

      if (E->EN(lincom,n_fields) != 0)
        min = max = E->EN(lincom,n_fields);

      E->flags |= GD_EN_COMPSCAL;
      tmask = (1 << max) - 1;

      mask = gdp_fetch_scalars(E, (HV*)sv, ((1 << max) - 1) * 9, pkg, func);

      if ((mask & tmask) != tmask) {
        E->EN(lincom,n_fields) = gdp_fetch_cmp_list(E->EN(lincom,cm), (HV*)sv,
            partial, 'm', min, max, mask, pkg, func);

        if (E->EN(lincom,n_fields) != 0)
          min = max = E->EN(lincom,n_fields);
      }

      if (((mask >> GD_MAX_LINCOM) & tmask) != tmask)
        E->EN(lincom,n_fields) = gdp_fetch_cmp_list(E->EN(lincom,cb), (HV*)sv,
            partial, 'b', min, max, mask >> GD_MAX_LINCOM, pkg, func);
      else
        E->EN(lincom,n_fields) = max;
      break;
    case GD_LINTERP_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 1, 1, pkg, func);
      GDP_EHASH_FETCH_PV(partial, "table", EN(linterp,table));
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 2, 2, pkg, func);
      break;
    case GD_PHASE_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 1, 1, pkg, func);

      mask = gdp_fetch_scalars(E, (HV*)sv, 1, pkg, func);

      if (!(mask & 1))
        GDP_EHASH_FETCH_IV(partial, "shift", E->EN(phase,shift), gd_int64_t);
      break;
    case GD_POLYNOM_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 1, 1, pkg, func);

      GDP_EHASH_FETCH_IV(1, "poly_ord", n, int);
      if (v) {
        if (n > GD_MAX_POLYORD || n < 1)
          croak("%s::%s() - poly_ord out of range", pkg, func);

        min = max = n + 1;
      } else {
        min = 2;
        max = GD_MAX_POLYORD + 1;
      }

      mask = gdp_fetch_scalars(E, (HV*)sv, (1 << (max + 1)) - 1, pkg, func);
      tmask = (1 << max) - 1;

      E->flags |= GD_EN_COMPSCAL;
      if ((mask & tmask) != tmask)
        E->EN(polynom,poly_ord) = gdp_fetch_cmp_list(E->EN(polynom,ca), (HV*)sv,
            partial, 'a', min, max, mask, pkg, func) - 1;
      else
        E->EN(polynom,poly_ord) = max - 1;
      break;
    case GD_RECIP_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 1, 1, pkg, func);
      mask = gdp_fetch_scalars(E, (HV*)sv, 1, pkg, func);

      E->flags |= GD_EN_COMPSCAL;
      if (!(mask & 1))
        GDP_EHASH_FETCH_CMP(partial, "dividend", EN(recip,cdividend));
      break;
    case GD_WINDOW_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 2, 2, pkg, func);
      GDP_EHASH_FETCH_IV(partial, "windop", E->EN(window,windop), gd_windop_t);

      mask = gdp_fetch_scalars(E, (HV*)sv, 1, pkg, func);

      if (!(mask & 1))
        switch(E->EN(window,windop)) {
          case GD_WINDOP_EQ:
          case GD_WINDOP_NE:
            GDP_EHASH_FETCH_IV(partial, "threshold", E->EN(window,threshold).i,
                int64_t);
            break;
          case GD_WINDOP_SET:
          case GD_WINDOP_CLR:
            GDP_EHASH_FETCH_UV(partial, "threshold", EN(window,threshold).u,
                uint64_t);
            break;
          default:
            GDP_EHASH_FETCH_NV(partial, "threshold", EN(window,threshold).r);
            break;
        }
      break;
    case GD_MPLEX_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, partial, 2, 2, pkg, func);

      mask = gdp_fetch_scalars(E, (HV*)sv, 0x3, pkg, func);

      if (!(mask & 1))
        GDP_EHASH_FETCH_UV(partial, "count_val", EN(mplex,count_val), int);

      if (!(mask & 2))
        GDP_EHASH_FETCH_UV(1, "period", EN(mplex,period), int);
      break;
    case GD_RAW_ENTRY:
      GDP_EHASH_FETCH_UV(partial, "data_type", EN(raw,data_type), gd_type_t);

      mask = gdp_fetch_scalars(E, (HV*)sv, 1, pkg, func);

      if (!(mask & 1))
        GDP_EHASH_FETCH_UV(partial, "spf", EN(raw,spf), unsigned int);
      break;
    case GD_NO_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      break;
    default:
      croak("%s::%s() - Invalid field type %x in entry hash", pkg, func,
        E->field_type);
  }

  dreturnvoid();
}

/* convert a single perl datum into the desired type */
static void gdp_convert_from_perl(void *dest, SV *src, gd_type_t type,
  const char *pkg, const char *func)
{
  dtrace("%p, %p, %03x, \"%s\", \"%s\"", dest, src, type, pkg, func);
  GD_DCOMPLEXA(c);
  int cmp = 1;

  /* undef results in randomness */
  if (src == NULL) {
    dreturnvoid();
    return;
  }

  /* check for and convert complex data */
  gdp_convert_cmp(gd_csp_(c), src, &cmp, pkg, func);

  switch (type) {
    case GD_UINT8:
      *(uint8_t*)dest = cmp ? (uint8_t)creal(c) : (uint8_t)SvUV(src);
      break;
    case GD_INT8:
      *(int8_t*)dest = cmp ? (int8_t)creal(c) : (int8_t)SvIV(src);
      break;
    case GD_UINT16:
      *(uint16_t*)dest = cmp ? (uint16_t)creal(c) : (uint16_t)SvUV(src);
      break;
    case GD_INT16:
      *(int16_t*)dest = cmp ? (int16_t)creal(c) : (int16_t)SvIV(src);
      break;
    case GD_UINT32:
      *(uint32_t*)dest = cmp ? (uint32_t)creal(c) : (uint32_t)SvUV(src);
      break;
    case GD_INT32:
      *(int32_t*)dest = cmp ? (int32_t)creal(c) : (int32_t)SvIV(src);
      break;
    case GD_UINT64:
      *(uint64_t*)dest = cmp ? (uint64_t)creal(c) : (uint64_t)SvUV(src);
      break;
    case GD_INT64:
      *(int64_t*)dest = cmp ? (int64_t)creal(c) : (int64_t)SvIV(src);
      break;
    case GD_FLOAT32:
      *(float*)dest = cmp ? (float)creal(c) : (float)SvNV(src);
      break;
    case GD_FLOAT64:
      *(double*)dest = cmp ? (double)creal(c) : (double)SvNV(src);
      break;
    case GD_COMPLEX64:
      if (cmp)
        gd_cs2ca_(dest,0,c,float);
      else
        gd_rs2ca_(dest,0,SvNV(src),float);
      break;
    case GD_COMPLEX128:
      if (cmp)
        gd_cs2ca_(dest,0,c,double);
      else
        gd_rs2ca_(dest,0,SvNV(src),double);
      break;
    case GD_NULL:
    case GD_UNKNOWN:
    case GD_STRING:
      ;
  }

  dreturnvoid();
}

struct gdp_din {
  void *data_in;
  gd_type_t type;
  int arg_type;
  size_t nsamp;
};

#define GDP_DATA_IN_LIST 0
#define GDP_DATA_IN_PACK 1
#define GDP_DATA_IN_REF  2
static struct gdp_din gdp_convert_data(SV *d, I32 items, I32 ax, size_t idx,
  const char *pkg, const char *func)
{
  struct gdp_din din = {NULL, GD_NULL, GDP_DATA_IN_PACK, 0};
  size_t i;
  AV *av = NULL;

  dtrace("%p, %i, %i, %" PRIuSIZE ", \"%s\", \"%s\"", d, (int)items, (int)ax,
      idx, pkg, func);

  /* argument grokking goes thus (in order!):
   * if d == undef, the remaining arguments are taken to be data
   * if d == reference to an array, the array is taken to be data, and
   *            remaining arguments ignored
   * if there is exactly one argument after d, d is taken as a type code
   *            and the following argument as either packed data or a
   *            reference to a list.
   * otherwise, d is taken to be the first datum in a list of data.
   */

  if (d == undef) {
    idx++;
    din.arg_type = GDP_DATA_IN_LIST;
  } else if (sv_isa(d, "Math::Complex"))
    din.arg_type = GDP_DATA_IN_LIST;
  else if (SvROK(d)) {
    av = (AV*)SvRV(d);
    din.arg_type = GDP_DATA_IN_REF;
  } else if (items != 6)
    din.arg_type = GDP_DATA_IN_LIST;
  else if (SvROK(ST(5))) {
    av = (AV*)SvRV(ST(5));
    din.arg_type = GDP_DATA_IN_REF;
  }

  if (din.arg_type == GDP_DATA_IN_LIST) {
    din.nsamp = items - idx;
    din.type = gdp_get_type(&ST(idx), pkg, func);
  } else if (din.arg_type == GDP_DATA_IN_REF) {
    if (SvTYPE((SV*)av) != SVt_PVAV)
      croak("%s::%s() - Expected array reference, but found some other "
          "type of object", pkg, func);
    din.nsamp = (size_t)av_len(av) + 1;
    din.type = gdp_get_type(av_fetch(av, 0, 0), pkg, func);
  } else {
    din.type = (gd_type_t)SvIV(d);
    if (GD_SIZE(din.type) == 0)
      croak("%s::%s() - Invalid type code", pkg, func);
    STRLEN n;
    din.data_in = SvPV(ST(5), n);
    din.nsamp = (size_t)n / GD_SIZE(din.type);
  }

  if (din.arg_type != GDP_DATA_IN_PACK)
    Newx(din.data_in, din.nsamp * GD_SIZE(din.type), char);

  if (din.arg_type == GDP_DATA_IN_LIST) {
    for (i = idx; i < items; ++i)
      gdp_convert_from_perl((char*)din.data_in + GD_SIZE(din.type) * (i - idx),
          ST(i), din.type, pkg, func);
  } else if (din.arg_type == GDP_DATA_IN_REF) {
    for (i = 0; i < din.nsamp; ++i) {
      SV **sv = av_fetch(av, i, 0);
      if (sv == NULL)
        croak("%s::%s() - Undefined datum encountered", pkg, func);
      gdp_convert_from_perl((char*)din.data_in + GD_SIZE(din.type) * i, *sv,
          din.type, pkg, func);
    }
  }

  dreturn("{ %p, %03x, %i, %" PRIuSIZE " }", din.data_in, din.type,
      din.arg_type, din.nsamp);
  return din;
}

/* convert perl datum (in some sort of format) into an appropriate
 * C datum, with type */
static gd_type_t gdp_to_voidp(void *dest, SV *src, gd_type_t hint,
  const char *pkg, const char *func)
{
  dtrace("%p, %p, %03x, \"%s\", \"%s\"", dest, src, hint, pkg, func);
  gd_type_t type = GD_UNKNOWN;

  /* treat undef as zero */
  if (src == undef) {
    Zero(dest, 1, char);
    type = GD_UINT8;
  } else {
    int cmp = 0;
    GD_DCOMPLEXA(c);
    gdp_convert_cmp(gd_csp_(c), src, &cmp, pkg, func);

    if (cmp) {
      gd_cs2ca_(dest, 0, c, double);
      type = GD_COMPLEX128;
    } else if (SvNOK(src)) {
      *(double*)dest = SvNV(src);
      type = GD_FLOAT64;
    } else if (SvUOK(src)) {
      *(uint64_t*)dest = (uint64_t)SvUV(src);
      type = GD_UINT64;
    } else if (SvIOK(src)) {
      *(int64_t*)dest = (int64_t)SvIV(src);
      type = GD_INT64;
    } else if (hint & GD_COMPLEX || hint & GD_IEEE754) {
      *(double*)dest = SvNV(src);
      type = GD_FLOAT64;
    } else if (hint & GD_SIGNED) {
      *(int64_t*)dest = (int64_t)SvIV(src);
      type = GD_INT64;
    } else {
      *(uint64_t*)dest = (uint64_t)SvUV(src);
      type = GD_UINT64;
    }
  }

  dreturn("%03x", type);
  return type;
}

static SV *gdp_newSVcmp(double r, double i)
{
  SV **dummy;
  SV *sv;
  AV *av;
  HV *hv, *stash;

  dtrace("%g; %g", r, i);

  /* build a list containing the data, and take it's reference */
  av = newAV();
  av_extend(av, 1);
  av_store(av, 0, newSVnv(r));
  av_store(av, 1, newSVnv(i));
  sv = newRV_noinc((SV*)av);

  /* create a Math::Complex object */
  hv = newHV();
  dummy = hv_store(hv, "p_dirty", 7, newSVuv(1), 0);
  dummy = hv_store(hv, "c_dirty", 7, newSVuv(0), 0);
  dummy = hv_store(hv, "cartesian", 9, sv, 0);
  stash = gv_stashpv("Math::Complex", GV_ADD);
  sv = sv_bless(newRV_noinc((SV*)hv), stash);

  dreturn("%p", sv);
  return sv;
}

/* convert a NULL-terminated char ** into a reference to a list of strings */
static SV *gdp_newRVavpv0(const char **l)
{
  dtrace("%p", l);
  SV *rv;
  int i;
  AV *av = newAV();

  for (i = 0; l[i]; ++i)
    av_store(av, i, newSVpv(l[i], 0));

  rv = newRV_noinc((SV*)av);
  dreturn("%p", rv);
  return rv;
}

/* convert a char ** with length into a reference to a list of strings */
static SV *gdp_newRVavpv(const char **l, size_t n)
{
  dtrace("%p, %" PRIuSIZE, l, n);
  SV *rv;
  int i;
  AV *av = newAV();
  av_extend(av, n - 1);

  for (i = 0; i < n; ++i)
    av_store(av, i, newSVpv(l[i], 0));

  rv = newRV_noinc((SV*)av);
  dreturn("%p", rv);
  return rv;
}

/* convert a complex double * into a reference to a list of complex data */
static SV *gdp_newRVavcmp(GD_DCOMPLEXV(l), size_t n)
{
  dtrace("%p, %" PRIuSIZE, l, n);
  SV *rv;
  int i;
  AV *av = newAV();
  av_extend(av, n - 1);

  for (i = 0; i < n; ++i)
    av_store(av, i, gdp_newSVcmp(creal(l[i]), cimag(l[i])));

  rv = newRV_noinc((SV*)av);
  dreturn("%p", rv);
  return rv;
}

/* store some scalar values in the entry hash on the stack from the gd_entry_t
*/
static SV **gdp_store_scalars(SV **sp, const gd_entry_t *E, unsigned int mask)
{
  dtrace("%p, %p, %06x", sp, E, mask);
  int i;

  if (!mask) {
    dreturn("%p", sp);
    return sp;
  }

  AV *scalar = newAV();
  AV *scalar_ind = newAV();
  av_extend(scalar, GD_MAX_POLYORD);
  av_extend(scalar_ind, GD_MAX_POLYORD);
  for (i = 0; i <= GD_MAX_POLYORD; ++i)
    if (mask & (1 << i)) {
      av_store(scalar, i, newSVpv(E->scalar[i], 0));
      if (E->scalar[i] == NULL)
        av_store(scalar_ind, i, undef);
      else
        av_store(scalar_ind, i, newSViv(E->scalar_ind[i]));
    }
  EXTEND(sp, 4);
  GDP_PUSHpvn("scalar");
  GDP_PUSHrv(scalar);
  GDP_PUSHpvn("scalar_ind");
  GDP_PUSHrv(scalar_ind);

  dreturn("%p", sp);
  return sp;
}

/* parser callback wrapper */
static int gdp_parser_callback(gd_parser_data_t *pdata, void *extra)
{
  SV *callback_func = ((struct gdp_callback_stuff_t *)extra)->func;
  SV *callback_data = ((struct gdp_callback_stuff_t *)extra)->data;
  SV *ret, **dummy;
  int n, sem = GD_SYNTAX_ABORT;
  AV *av;
  int len;

  /* local stack pointer */
  dSP;

  dtrace("%p, %p", pdata, extra);

  /* create pseudo-block */
  ENTER;
  SAVETMPS;

  /* create the parser data hash */
  HV *phash = newHV();
  dummy = hv_store(phash, "suberror", 8, newSVuv(pdata->suberror), 0);
  dummy = hv_store(phash, "linenum", 7, newSVuv(pdata->linenum), 0);
  dummy = hv_store(phash, "line", 4, newSVpv(pdata->line, 0), 0);
  dummy = hv_store(phash, "filename", 8, newSVpv(pdata->filename, 0), 0);

  /* create stack frame */
  PUSHMARK(SP);
  EXTEND(SP, 2);

  /* push args */
  PUSHs(sv_2mortal(newRV_noinc((SV *)phash)));
  PUSHs(callback_data);

  /* finalise stack frame */
  PUTBACK;

  /* call the object */
  n = call_sv(callback_func, G_SCALAR);

  /* refresh local stack pointer */
  SPAGAIN;

  /* delete the hash */
  hv_undef(phash);

  /* Make sure we got a scalar back; otherwise complain and abort */
  if (n != 1) {
    croak("GetData: expected scalar response from parser callback.");
    return GD_SYNTAX_ABORT; /* ca'n't get here */
  }

  /* pop our return value */
  ret = POPs;

  /* de-reference as needed */
  while (SvROK(ret)) {
    ret = SvRV(ret);
  }

  /* ferret out response */
  switch (SvTYPE(ret)) {
    case SVt_IV:
      sem = SvIV(ret);
      break;
    case SVt_PVAV:
      av = (AV *)ret;
      len = av_len(av);
      if (len < 0) {
        croak("GetData: parser callback returned empty array.");
        return GD_SYNTAX_ABORT; /* ca'n't get here */
      } else if (len > 1) {
        croak("GetData: too many elements in array returned by parser "
            "callback.");
        return GD_SYNTAX_ABORT; /* ca'n't get here */
      } else if (len == 0) {
        SV **val = av_fetch(av, 0, 0);
        if (val == NULL || SvROK(*val)) {
          croak("GetData: bad data type in array returned by parser callback.");
          return GD_SYNTAX_ABORT; /* ca'n't get here */
        }

        if (SvTYPE(*val) == SVt_IV) {
          sem = SvIV(*val);
        } else if (SvTYPE(*val) == SVt_PV) {
          pdata->line = savepv(SvPV_nolen(*val));
          sem = GD_SYNTAX_RESCAN;
        } else {
          croak("GetData: bad data type in array returned by parser callback.");
          return GD_SYNTAX_ABORT; /* ca'n't get here */
        }
      } else { /* len == 1 */
        SV **val0 = av_fetch(av, 0, 0);
        SV **val1 = av_fetch(av, 1, 0);

        if (val0 == NULL || SvROK(*val0) || val1 == NULL || SvROK(*val1)) {
          croak("GetData: bad data type in array returned by parser callback.");
          return GD_SYNTAX_ABORT; /* ca'n't get here */
        }

        if (SvTYPE(*val0) == SVt_IV && SvTYPE(*val1) == SVt_PV) {
          sem = SvIV(*val0);
          pdata->line = savepv(SvPV_nolen(*val1));
        } else {
          croak("GetData: bad data type in array returned by parser callback.");
          return GD_SYNTAX_ABORT; /* ca'n't get here */
        }
      }
      break;
    case SVt_PV:
    pdata->line = savepv(SvPV_nolen(ret));
    sem = GD_SYNTAX_RESCAN;
    break;
    default:
    croak("GetData: bad data type returned by parser callback.");
    return GD_SYNTAX_ABORT; /* ca'n't get here */
  }

  /* resync global stack pointer */
  PUTBACK;

  /* destroy pseudo-block */
  FREETMPS;
  LEAVE;

  dreturn("%i", sem);
  return sem;
}

#define GDP_UNPACKU(t) \
  if (sp) { \
    EXTEND(sp, n); \
    for (i = 0; i < n; ++i) PUSHs(sv_2mortal(newSVuv(((t*)data)[i]))); \
  } else for (i = 0; i < n; ++i) av_store(av, i,   newSVuv(((t*)data)[i]))

#define GDP_UNPACKI(t) \
  if (sp) { \
    EXTEND(sp, n); \
    for (i = 0; i < n; ++i) PUSHs(sv_2mortal(newSViv(((t*)data)[i]))); \
  } else for (i = 0; i < n; ++i) av_store(av, i,   newSViv(((t*)data)[i]))

#define GDP_UNPACKN(t) \
  if (sp) { \
    EXTEND(sp, n); \
    for (i = 0; i < n; ++i) PUSHs(sv_2mortal(newSVnv(((t*)data)[i]))); \
  } else for (i = 0; i < n; ++i) av_store(av, i,   newSVnv(((t*)data)[i]))

#define GDP_UNPACKC(t) \
  if (sp) { \
    EXTEND(sp, n); \
    for (i = 0; i < n; ++i) \
      PUSHs(sv_2mortal(gdp_newSVcmp(((t*)data)[2 * i], \
              ((t*)data)[2 * i + 1]))); \
  } else for (i = 0; i < n; ++i) av_store(av, i, \
      gdp_newSVcmp(((t*)data)[2 * i], ((t*)data)[2 * i + 1]))

/* unpack data. If sp is NULL, return an AV, otherwise push it onto the perl
 * stack; returns the updated stack pointer */
static void * gdp_unpack(SV **sp, const void *data, size_t n, gd_type_t type)
{
  dtrace("%p, %p, %" PRIuSIZE ", %03x", sp, data, n, type);
  size_t i;
  AV *av = NULL;

  if (!sp) {
    av = newAV();
    av_extend(av, n - 1);
  }

  if (n == 0) {
    dreturn("%p", sp);
    return sp ? (void *)sp : (void *)av;
  }

  switch (type) {
    case GD_UINT8:
      GDP_UNPACKU(uint8_t);
      break;
    case GD_INT8:
      GDP_UNPACKI(int8_t);
      break;
    case GD_UINT16:
      GDP_UNPACKU(uint16_t);
      break;
    case GD_INT16:
      GDP_UNPACKI(int16_t);
      break;
    case GD_UINT32:
      GDP_UNPACKU(uint32_t);
      break;
    case GD_INT32:
      GDP_UNPACKI(int32_t);
      break;
    case GD_UINT64:
      GDP_UNPACKU(uint64_t);
      break;
    case GD_INT64:
      GDP_UNPACKI(int64_t);
      break;
    case GD_FLOAT32:
      GDP_UNPACKN(float);
      break;
    case GD_FLOAT64:
      GDP_UNPACKN(double);
      break;
    case GD_COMPLEX64:
      GDP_UNPACKC(float);
      break;
    case GD_COMPLEX128:
      GDP_UNPACKC(double);
      break;
    case GD_UNKNOWN:
    case GD_NULL:
    case GD_STRING:
      break;
  }

  dreturn("%p", sp ? (void *)sp : (void *)av);
  return sp ? (void *)sp : (void *)av;
}

/* Module starts here --------------------------------------------------- */
MODULE = GetData  PACKAGE = GetData
PROTOTYPES: ENABLE

BOOT:
  gd_alloc_funcs(gdp_malloc, safefree);
  gdp_invalid = gd_invalid_dirfile();

void
DESTROY(gdp_dirfile)
  struct gdp_dirfile_t * gdp_dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::DESTROY = 1
  CODE:
    dtrace("%p", gdp_dirfile);
    if (gdp_dirfile->D != NULL)
      gd_discard(gdp_dirfile->D);
    safefree(gdp_dirfile);
  CLEANUP:
    dreturnvoid();

int
error(dirfile)
  DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::error = 1
  CODE:
    dtrace("%p", dirfile);

    RETVAL = gd_error(dirfile);
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);


struct gdp_dirfile_t *
open(dirfilename, flags, sehandler=undef, extra=undef)
  const char * dirfilename
  unsigned long flags
  SV * sehandler
  SV * extra
  CODE:
    dtrace("\"%s\", %lu, %p, %p", dirfilename, flags, sehandler, extra);
    Newx(RETVAL, 1, struct gdp_dirfile_t);
    if (sehandler == undef) {
      RETVAL->cbdata.func = NULL;
      RETVAL->cbdata.data = NULL;

      RETVAL->D = gd_cbopen(dirfilename, flags, NULL, NULL);
    } else {
      RETVAL->cbdata.func = sehandler;
      RETVAL->cbdata.data = extra;

      RETVAL->D = gd_cbopen(dirfilename, flags, gdp_parser_callback,
          &RETVAL->cbdata);
    }
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%p", RETVAL);

struct gdp_dirfile_t *
invalid_dirfile()
  CODE:
    dtracevoid();
    Newx(RETVAL, 1, struct gdp_dirfile_t);
    RETVAL->cbdata.func = NULL;
    RETVAL->cbdata.data = NULL;

    RETVAL->D = gd_invalid_dirfile();
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%p", RETVAL);

void
get_carray(dirfile, field_code, return_type)
  DIRFILE * dirfile
  const char * field_code
  gd_type_t return_type
  PREINIT:
    void *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::get_carray = 1
  PPCODE:
    dtrace("%p, \"%s\", %03x; %i", dirfile, field_code, return_type,
        (int)GIMME_V);

    if (return_type == GD_NULL) {
      gd_get_carray(dirfile, field_code, GD_NULL, NULL);

      GDP_UNDEF_ON_ERROR();
      
      if (GIMME_V != G_ARRAY) {
        dreturnvoid();
        XSRETURN_UNDEF;
      }
    } else {
      size_t len = gd_array_len(dirfile, field_code);
      Newx(data_out, GD_SIZE(return_type) * len, char);
      gd_get_carray(dirfile, field_code, return_type, data_out);

      GDP_UNDEF_ON_ERROR(safefree(data_out));

      if (GIMME_V == G_ARRAY)
        sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
      else
        XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

      safefree(data_out);
    }

    dreturnvoid();

void
get_carray_slice(dirfile, field_code, start, len, return_type)
  DIRFILE * dirfile
  const char * field_code
  gd_type_t return_type
  unsigned int start
  size_t len
  PREINIT:
    void *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::get_carray_slice = 1
  PPCODE:
    dtrace("%p, \"%s\", %u, %" PRIuSIZE ", %03x; %i", dirfile, field_code,
      start, len, return_type, (int)GIMME_V);

    if (return_type == GD_NULL) {
      gd_get_carray_slice(dirfile, field_code, start, len, GD_NULL, NULL);

      GDP_UNDEF_ON_ERROR();
      
      if (GIMME_V != G_ARRAY) {
        dreturnvoid();
        XSRETURN_UNDEF;
      }
    } else {
      Newx(data_out, GD_SIZE(return_type) * len, char);
      gd_get_carray_slice(dirfile, field_code, start, len, return_type,
        data_out);

      GDP_UNDEF_ON_ERROR(safefree(data_out));

      if (GIMME_V == G_ARRAY)
        sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
      else
        XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

      safefree(data_out);
    }
    dreturnvoid();

SV *
get_constant(dirfile, field_code, return_type)
  DIRFILE * dirfile
  const char * field_code
  gd_type_t return_type
  PREINIT:
    char data_out[16];
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::get_constant = 1
  CODE:
    gd_type_t type;
    dtrace("%p, \"%s\", %03x", dirfile, field_code, return_type);

    if (return_type == GD_NULL)
      type = GD_NULL;
    else if (return_type & GD_COMPLEX)
      type = GD_COMPLEX128;
    else if (return_type & GD_IEEE754)
      type = GD_FLOAT64;
    else if (return_type & GD_SIGNED)
      type = GD_INT64;
    else
      type = GD_UINT64;

    gd_get_constant(dirfile, field_code, type, data_out);

    GDP_UNDEF_ON_ERROR();

    if (type == GD_NULL) {
      dreturnvoid();
      XSRETURN_UNDEF;
    } else if (type == GD_COMPLEX128)
      RETVAL = gdp_newSVcmp(((double*)data_out)[0], ((double*)data_out)[1]);
    else if (type == GD_FLOAT64)
      RETVAL = newSVnv(*(double*)data_out);
    else if (type == GD_INT64)
      RETVAL = newSViv(*(int64_t*)data_out);
    else
      RETVAL = newSVuv(*(uint64_t*)data_out);
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%p", RETVAL);

void
constants(dirfile, return_type)
  DIRFILE * dirfile
  gd_type_t return_type
  PREINIT:
    const void *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::constants = 1
  PPCODE:
    dtrace("%p, %03x; %i", dirfile, return_type, (int)GIMME_V);
    int len = gd_nfields_by_type(dirfile, GD_CONST_ENTRY);
    data_out = gd_constants(dirfile, return_type);

    GDP_UNDEF_ON_ERROR();

    if (GIMME_V == G_ARRAY)
      sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
    else
      XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

    dreturnvoid();

void
carrays(dirfile, return_type)
  DIRFILE * dirfile
  gd_type_t return_type
  PREINIT:
    const gd_carray_t *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::carrays = 1
  PPCODE:
    dtrace("%p, %03x; %i", dirfile, return_type, (int)GIMME_V);
    I32 i, len = (I32)gd_nfields_by_type(dirfile, GD_CARRAY_ENTRY);
    data_out = gd_carrays(dirfile, return_type);

    GDP_UNDEF_ON_ERROR();

    /* in array context, return an array of arrays of unpacked data.
     * Otherwise, return a reference to an array of packed data. */
    if (GIMME_V == G_ARRAY) {
      EXTEND(sp, len);
      for (i = 0; i < len; ++i)
        PUSHs(sv_2mortal(newRV_noinc((SV *)gdp_unpack(NULL, data_out[i].d,
                  data_out[i].n, return_type))));
    } else {
      AV *av = newAV();
      for (i = 0; i < len; ++i)
        av_store(av, i, newSVpvn(data_out[i].d,
              data_out[i].n * GD_SIZE(return_type)));
      XPUSHs(sv_2mortal(newRV_noinc((SV*)av)));
    }

    dreturnvoid();

void
entry(dirfile, field_code)
  DIRFILE * dirfile
  const char * field_code
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::entry = 1
  PPCODE:
    dtrace("%p, \"%s\"; %i", dirfile, field_code, (int)GIMME_V);

    if (GIMME_V == G_ARRAY) {
      gd_entry_t E;
      gd_entry(dirfile, field_code, &E);

      GDP_UNDEF_ON_ERROR();

      /* push the hash onto the stack */
      EXTEND(sp, 6);
      GDP_PUSHpvn("field");
      GDP_PUSHpvz(E.field);
      GDP_PUSHpvn("field_type");
      GDP_PUSHuv(E.field_type);
      GDP_PUSHpvn("fragment_index");
      GDP_PUSHuv(E.fragment_index);
      switch (E.field_type) {
        case GD_BIT_ENTRY:
        case GD_SBIT_ENTRY:
          EXTEND(sp, 6);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("bitnum");
          GDP_PUSHuv(E.EN(bit,bitnum));
          GDP_PUSHpvn("numbits");
          GDP_PUSHuv(E.EN(bit,numbits));
          sp = gdp_store_scalars(sp, &E, 0x3);
          break;
        case GD_SARRAY_ENTRY:
          EXTEND(sp, 2);
          GDP_PUSHpvn("array_len");
          GDP_PUSHuv(E.EN(scalar,array_len));
          break;
        case GD_CARRAY_ENTRY:
          EXTEND(sp, 2);
          GDP_PUSHpvn("array_len");
          GDP_PUSHuv(E.EN(scalar,array_len));
          /* fallthrough */
        case GD_CONST_ENTRY:
          EXTEND(sp, 2);
          GDP_PUSHpvn("const_type");
          GDP_PUSHuv(E.EN(scalar,const_type));
          break;
        case GD_LINCOM_ENTRY:
          EXTEND(sp, 8);
          GDP_PUSHpvn("n_fields");
          GDP_PUSHiv(E.EN(lincom,n_fields));
          GDP_PUSHpvn("in_fields");
          GDP_PUSHrvavpv(E.in_fields, E.EN(lincom,n_fields));
          GDP_PUSHpvn("m");
          GDP_PUSHrvavcmp(E.EN(lincom,cm), E.EN(lincom,n_fields));
          GDP_PUSHpvn("b");
          GDP_PUSHrvavcmp(E.EN(lincom,cb), E.EN(lincom,n_fields));
          sp = gdp_store_scalars(sp, &E,
              ((1 << E.EN(lincom,n_fields)) - 1) * 9);
          break;
        case GD_LINTERP_ENTRY:
          EXTEND(sp, 4);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("table");
          GDP_PUSHpvz(E.EN(linterp,table));
          break;
        case GD_MULTIPLY_ENTRY:
        case GD_DIVIDE_ENTRY:
        case GD_INDIR_ENTRY:
        case GD_SINDIR_ENTRY:
          EXTEND(sp, 2);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHrvavpv(E.in_fields, 2);
          break;
        case GD_PHASE_ENTRY:
          EXTEND(sp, 4);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("shift");
          GDP_PUSHiv(E.EN(phase,shift));
          sp = gdp_store_scalars(sp, &E, 1);
          break;
        case GD_POLYNOM_ENTRY:
          EXTEND(sp, 6);
          GDP_PUSHpvn("poly_ord");
          GDP_PUSHiv(E.EN(polynom,poly_ord));
          GDP_PUSHpvn("in_fields");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("a");
          GDP_PUSHrvavcmp(E.EN(polynom,ca), E.EN(polynom,poly_ord) + 1);
          sp = gdp_store_scalars(sp, &E,
              (1 << (E.EN(polynom,poly_ord) + 1)) - 1);
          break;
        case GD_RECIP_ENTRY:
          EXTEND(sp, 4);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("dividend");
          GDP_PUSHcmp(creal(E.EN(recip,cdividend)),
              cimag(E.EN(recip,cdividend)));
          sp = gdp_store_scalars(sp, &E, 1);
          break;
        case GD_RAW_ENTRY:
          EXTEND(sp, 4);
          GDP_PUSHpvn("spf");
          GDP_PUSHuv(E.EN(raw,spf));
          GDP_PUSHpvn("data_type");
          GDP_PUSHuv(E.EN(raw,data_type));
          sp = gdp_store_scalars(sp, &E, 1);
          break;
        case GD_WINDOW_ENTRY:
          EXTEND(sp, 6);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHrvavpv(E.in_fields, 2);
          GDP_PUSHpvn("windop");
          GDP_PUSHiv(E.EN(window,windop));
          GDP_PUSHpvn("threshold");
          switch(E.EN(window,windop)) {
            case GD_WINDOP_EQ:
            case GD_WINDOP_NE:
              GDP_PUSHiv(E.EN(window,threshold).i);
              break;
            case GD_WINDOP_SET:
            case GD_WINDOP_CLR:
              GDP_PUSHuv(E.EN(window,threshold).u);
              break;
            default:
              GDP_PUSHnv(E.EN(window,threshold).r);
              break;
          }
          sp = gdp_store_scalars(sp, &E, 1);
          break;
        case GD_MPLEX_ENTRY:
          EXTEND(sp, 6);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHrvavpv(E.in_fields, 2);
          GDP_PUSHpvn("count_val");
          GDP_PUSHuv(E.EN(mplex,count_val));
          GDP_PUSHpvn("period");
          GDP_PUSHuv(E.EN(mplex,period));
          sp = gdp_store_scalars(sp, &E, 0x3);
          break;
        case GD_INDEX_ENTRY:
        case GD_STRING_ENTRY:
        case GD_ALIAS_ENTRY: /* avoid compiler warnings */
        case GD_NO_ENTRY:
          break;
      }

      gd_free_entry_strings(&E);
    } else {
      gd_entype_t t = gd_entry_type(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHiv(t);
    }
    dreturnvoid();

SV *
error_string(dirfile)
  DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::error_string = 1
  CODE:
    dtrace("%p", dirfile);
    char *s = gd_error_string(dirfile, NULL, 0);

    RETVAL = newSVpv(s, 0);
  OUTPUT:
    RETVAL
  CLEANUP:
    safefree(s);
    dreturn("%p", RETVAL);

AV *
mcarrays(dirfile, parent, return_type)
  DIRFILE * dirfile
  const char * parent;
  gd_type_t return_type
  PREINIT:
    const gd_carray_t *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::mcarrays = 1
  PPCODE:
    dtrace("%p, %03x; %i", dirfile, return_type, (int)GIMME_V);
    I32 i, len = (I32)gd_nmfields_by_type(dirfile, parent, GD_CARRAY_ENTRY);
    data_out = gd_mcarrays(dirfile, parent, return_type);

    GDP_UNDEF_ON_ERROR();

    /* in array context, return an array of arrays of unpacked data.
     * Otherwise, return a reference to an array of packed data. */
    if (GIMME_V == G_ARRAY) {
      EXTEND(sp, len);
      for (i = 0; i < len; ++i)
        PUSHs(sv_2mortal(newRV_noinc((SV *)gdp_unpack(NULL, data_out[i].d,
                  data_out[i].n, return_type))));
    } else {
      AV *av = newAV();
      for (i = 0; i < len; ++i)
        av_store(av, i, newSVpvn(data_out[i].d,
              data_out[i].n * GD_SIZE(return_type)));
      XPUSHs(sv_2mortal(newRV_noinc((SV*)av)));
    }

    dreturnvoid();

void
mconstants(dirfile, parent, return_type)
  DIRFILE * dirfile
  const char * parent
  gd_type_t return_type
  PREINIT:
    const void *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::mconstants = 1
  PPCODE:
    dtrace("%p, %03x; %i", dirfile, return_type, (int)GIMME_V);
    int len = gd_nmfields_by_type(dirfile, parent, GD_CONST_ENTRY);
    data_out = gd_mconstants(dirfile, parent, return_type);

    GDP_UNDEF_ON_ERROR();

    if (GIMME_V == G_ARRAY)
      sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
    else
      XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

    dreturnvoid();

void
parser_callback(gdp_dirfile, sehandler, extra=undef)
  struct gdp_dirfile_t * gdp_dirfile
  SV *sehandler
  SV *extra
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::parser_callback = 1
  CODE:
    dtrace("%p, %p, %p", gdp_dirfile, sehandler, extra);
    if (gdp_dirfile->D) {
      if (sehandler == undef) {
        gdp_dirfile->cbdata.func = NULL;
        gdp_dirfile->cbdata.data = NULL;

        gd_parser_callback(gdp_dirfile->D, NULL, NULL);
      } else {
        gdp_dirfile->cbdata.func = sehandler;
        gdp_dirfile->cbdata.data = extra;

        gd_parser_callback(gdp_dirfile->D, gdp_parser_callback,
            &gdp_dirfile->cbdata);
      }
    }
  CLEANUP:
    dreturnvoid();

char *
get_string(dirfile, field_code)
  DIRFILE * dirfile
  const char * field_code
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::get_string = 1
  CODE:
    dtrace("%p, \"%s\"", dirfile, field_code);

    /* get string length */
    size_t len = gd_get_string(dirfile, field_code, 0, NULL);
    Newx(RETVAL, len, char);

    /* get string */
    gd_get_string(dirfile, field_code, len, RETVAL);
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("\"%s\"", RETVAL);
    safefree(RETVAL);

int
close(gdp_dirfile)
  struct gdp_dirfile_t * gdp_dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::close = 1
  CODE:
    dtrace("%p", gdp_dirfile);

    if (gdp_dirfile->D != NULL) {
      RETVAL = gd_close(gdp_dirfile->D);

      if (!RETVAL)
        gdp_dirfile->D = NULL;
    } else
      RETVAL = 0;
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);

int
discard(gdp_dirfile)
  struct gdp_dirfile_t * gdp_dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::discard = 1
  CODE:
    dtrace("%p", gdp_dirfile);

    if (gdp_dirfile->D != NULL) {
      RETVAL = gd_discard(gdp_dirfile->D);

      if (!RETVAL)
        gdp_dirfile->D = NULL;
    } else
      RETVAL = 0;
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);

void
getdata(dirfile, field_code, first_frame, first_samp, num_frames, num_samp, \
  return_type=GD_UNKNOWN)
  DIRFILE * dirfile
  const char * field_code
  gd_off64_t first_frame
  gd_off64_t first_samp
  size_t num_frames
  size_t num_samp
  gd_type_t return_type
  PREINIT:
    unsigned int spf = 1;
    gd_entype_t t;
    GDP_DIRFILE_ALIAS;
    size_t i, len;
    void * data_out;
  ALIAS:
    GetData::Dirfile::getdata = 1
  PPCODE:
    dtrace("%p, \"%s\", %" PRId64 ", %" PRId64 ", %" PRIuSIZE ", %" PRIuSIZE
      ", %03x; %i", dirfile, field_code, (int64_t)first_frame,
      (int64_t)first_samp, num_frames, num_samp, return_type, (int)GIMME_V);

    t = gd_entry_type(dirfile, field_code);

    GDP_UNDEF_ON_ERROR();

    if (num_frames) {
      spf = gd_spf(dirfile, field_code);

      num_samp += spf * num_frames;

      GDP_UNDEF_ON_ERROR();
    }

    if (t == GD_SINDIR_ENTRY) {
      const char **data_out;
      Newx(data_out, num_samp, const char*);

      len = gd_getdata64(dirfile, field_code, first_frame, first_samp, 0,
          num_samp, GD_STRING, data_out);

      GDP_UNDEF_ON_ERROR(safefree(data_out));

      /* In array context return the array; in scalar context return a reference
       * to an array */
      if (GIMME_V == G_ARRAY)
        for (i = 0; i < len; ++i)
          GDP_PUSHpvz(data_out[i]);
      else
        XPUSHs(sv_2mortal(gdp_newRVavpv(data_out, len)));

      safefree(data_out);
    } else if (return_type == GD_UNKNOWN)
      croak("%s::getdata() - No return type specified", gdp_package);
    else if (return_type == GD_NULL) {
      len = gd_getdata64(dirfile, field_code, first_frame, first_samp, 0,
          num_samp, GD_NULL, NULL);

      GDP_UNDEF_ON_ERROR();
      
      /* For GD_NULL, we return len in scalar context and an empty array
       * in array context */
      if (GIMME_V == G_ARRAY)
        ; /* nothing to return */
      else {
        EXTEND(sp, 1);
        GDP_PUSHuv(len);
      }
    } else {
      Newx(data_out, GD_SIZE(return_type) * num_samp, char);

      len = gd_getdata64(dirfile, field_code, first_frame, first_samp, 0,
          num_samp, return_type, data_out);

      GDP_UNDEF_ON_ERROR(safefree(data_out));

      /* In array context, unpack the array and push it onto the stack,
       * otherwise just return the packed data */
      if (GIMME_V == G_ARRAY)
        sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
      else
        XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

      safefree(data_out);
    }

    dreturnvoid();

void
field_list(dirfile)
    DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::field_list = 1
  PPCODE:
    dtrace("%p; %i", dirfile, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_field_list(dirfile);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nfields(dirfile);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
field_list_by_type(dirfile, type)
    DIRFILE * dirfile
    gd_entype_t type
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::field_list_by_type = 1
  PPCODE:
    dtrace("%p, %i; %i", dirfile, type, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_field_list_by_type(dirfile, type);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nfields_by_type(dirfile, type);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
entry_list(dirfile, parent, type, flags)
    DIRFILE * dirfile
    gdp_char * parent
    gdp_int    type
    gdp_uint_t flags
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::entry_list = 1
  PPCODE:
    dtrace("%p, \"%s\", %i, %u; %i", dirfile, parent, type, flags,
        (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **el = gd_entry_list(dirfile, parent, type, flags);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; el[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(el[i]);
      }
    } else {
      unsigned int ne = gd_nentries(dirfile, parent, type, flags);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(ne);
    }

    dreturnvoid();

void
match_entries(dirfile, regex, fragment=-1, type=0, flags=0)
    DIRFILE * dirfile
    gdp_char * regex
    gdp_ffff_t fragment
    gdp_int    type
    gdp_uint_t flags
  PREINIT:
    GDP_DIRFILE_ALIAS;
    const char **el;
    unsigned int ne;
  ALIAS:
    GetData::Dirfile::match_entries = 1
  PPCODE:
    dtrace("%p, \"%s\", %i, %i, %u; %i", dirfile, regex, fragment, type, flags,
        (int)GIMME_V);

    ne = gd_match_entries(dirfile, regex, fragment, type, flags, &el);

    GDP_UNDEF_ON_ERROR();

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;

      for (i = 0; el[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(el[i]);
      }
    } else {
      EXTEND(sp, 1);
      GDP_PUSHuv(ne);
    }

    dreturnvoid();

void
vector_list(dirfile)
    DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::vector_list = 1
  PPCODE:
    dtrace("%p; %i", dirfile, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_vector_list(dirfile);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nvectors(dirfile);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
strings(dirfile)
    DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::strings = 1
  PPCODE:
    dtrace("%p; %i", dirfile, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_strings(dirfile);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nfields_by_type(dirfile, GD_STRING_ENTRY);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
mfield_list(dirfile, field_code)
  DIRFILE * dirfile
  const char * field_code
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::mfield_list = 1
  PPCODE:
    dtrace("%p, \"%s\"; %i", dirfile, field_code, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mfield_list(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nmfields(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
mfield_list_by_type(dirfile, parent, type)
    DIRFILE * dirfile
    const char * parent
    gd_entype_t type
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::mfield_list_by_type = 1
  PPCODE:
    dtrace("%p, \"%s\", %i; %i", dirfile, parent, type, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mfield_list_by_type(dirfile, parent, type);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nmfields_by_type(dirfile, parent, type);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
mvector_list(dirfile, parent)
    DIRFILE * dirfile
    const char * parent
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::mvector_list = 1
  PPCODE:
    dtrace("%p; %i", dirfile, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mvector_list(dirfile, parent);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nmvectors(dirfile, parent);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
mstrings(dirfile, field_code)
  DIRFILE * dirfile
  const char * field_code
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::mstrings = 1
  PPCODE:
    dtrace("%p, \"%s\"; %i", dirfile, field_code, (int)GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mstrings(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_nmfields_by_type(dirfile, field_code,
          GD_STRING_ENTRY);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

int
put_carray(dirfile, field_code, d, ...)
    DIRFILE * dirfile
    const char * field_code
    SV *d;
  PREINIT:
    GDP_DIRFILE_ALIAS;
    struct gdp_din din;
  ALIAS:
    GetData::Dirfile::put_carray = 1
  CODE:
    dtrace("%p, \"%s\", %p, ...[%li]", dirfile, field_code, d, (long)items - 3);

    din = gdp_convert_data(d, items, ax, 2, gdp_package, "put_carray");

    RETVAL = gd_put_carray(dirfile, field_code, din.type, din.data_in);

    GDP_UNDEF_ON_ERROR(if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in);
    dreturn("%i", RETVAL);

int
put_carray_slice(dirfile, field_code, start, d, ...)
    DIRFILE * dirfile
    const char * field_code
    gd_off64_t start
    SV *d;
  PREINIT:
    GDP_DIRFILE_ALIAS;
    struct gdp_din din;
  ALIAS:
    GetData::Dirfile::put_carray_slice = 1
  CODE:
    dtrace("%p, \"%s\", %" PRId64 ", %p, ...[%li]", dirfile, field_code,
        (int64_t)start, d, (long)items - 4);

    din = gdp_convert_data(d, items, ax, 3, gdp_package, "put_carray_slice");

    RETVAL = gd_put_carray_slice(dirfile, field_code, start, din.nsamp,
        din.type, din.data_in);

    GDP_UNDEF_ON_ERROR(if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in);
    dreturn("%i", RETVAL);

int
add_carray(dirfile, field_code, const_type, fragment_index, d, ...)
    DIRFILE * dirfile
    const char * field_code
    int fragment_index
    gd_type_t const_type
    SV *d
  PREINIT:
    GDP_DIRFILE_ALIAS;
    struct gdp_din din;
  ALIAS:
    GetData::Dirfile::add_carray = 1
  CODE:
    dtrace("%p, \"%s\", %03x, %i, %p, ...[%li]", dirfile, field_code,
        const_type, fragment_index, d, (long)items - 5);

    din = gdp_convert_data(d, items, ax, 4, gdp_package, "add_carray");

    RETVAL = gd_add_carray(dirfile, field_code, const_type, din.nsamp,
        din.type, din.data_in, fragment_index);

    GDP_UNDEF_ON_ERROR(if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in);
    dreturn("%i", RETVAL);

int
madd_carray(dirfile, parent, field_code, const_type, d, ...)
    DIRFILE * dirfile
    const char * parent
    const char * field_code
    gd_type_t const_type
    SV *d
  PREINIT:
    GDP_DIRFILE_ALIAS;
    struct gdp_din din;
  ALIAS:
    GetData::Dirfile::madd_carray = 1
  CODE:
    dtrace("%p, \"%s\", \"%s\", %03x, %p, ...[%li]", dirfile, parent,
        field_code, const_type, d, (long)items - 5);

    din = gdp_convert_data(d, items, ax, 4, gdp_package, "madd_carray");

    RETVAL = gd_madd_carray(dirfile, parent, field_code, const_type, din.nsamp,
        din.type, din.data_in);

    GDP_UNDEF_ON_ERROR(if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in);
    dreturn("%i", RETVAL);

size_t
putdata(dirfile, field_code, first_frame, first_sample, d, ...)
    DIRFILE * dirfile
    const char * field_code
    gd_off64_t first_frame
    gd_off64_t first_sample
    SV *d;
  PREINIT:
    GDP_DIRFILE_ALIAS;
    struct gdp_din din;
  ALIAS:
    GetData::Dirfile::putdata = 1
  CODE:
    dtrace("%p, \"%s\", %" PRId64 ", %" PRId64 ", %p, ...[%li]", dirfile,
      field_code, (int64_t)first_frame, (int64_t)first_sample, d,
      (long)items - 5);

    din = gdp_convert_data(d, items, ax, 4, gdp_package, "putdata");

    RETVAL = gd_putdata(dirfile, field_code, first_frame, first_sample, 0,
        din.nsamp, din.type, din.data_in);

    GDP_UNDEF_ON_ERROR(if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    if (din.arg_type != GDP_DATA_IN_PACK)
      safefree(din.data_in);
    dreturn("%" PRIuSIZE, RETVAL);

int
add_const(dirfile, field_code, const_type, value=undef, fragment_index=0)
  DIRFILE * dirfile
  const char * field_code
  gd_type_t const_type
  SV * value
  int fragment_index
  PREINIT:
    gd_type_t data_type;
    char data_in[16];
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::add_const = 1
  CODE:
    dtrace("%p, \"%s\", %03x, %p, %i", dirfile, field_code, const_type, value,
        fragment_index);

    data_type = gdp_to_voidp(data_in, value, const_type, gdp_package,
      "add_const");

    RETVAL = gd_add_const(dirfile, field_code, const_type, data_type, data_in,
        fragment_index);

    GDP_UNDEF_ON_ERROR();
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);

int
madd_const(dirfile, parent, field_code, const_type, value=undef)
  DIRFILE * dirfile
  const char * parent
  const char * field_code
  gd_type_t const_type
  SV * value
  PREINIT:
    gd_type_t data_type;
    char data_in[16];
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::madd_const = 1
  CODE:
    dtrace("%p, \"%s\", \"%s\", %03x, %p", dirfile, parent, field_code,
        const_type, value);

    data_type = gdp_to_voidp(data_in, value, const_type, gdp_package,
      "madd_const");

    RETVAL = gd_madd_const(dirfile, parent, field_code, const_type, data_type,
        data_in);

    GDP_UNDEF_ON_ERROR();
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);

int
put_constant(dirfile, field_code, value)
  DIRFILE * dirfile
  const char * field_code
  SV * value
  PREINIT:
    gd_type_t data_type;
    char data_in[16];
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::put_constant = 1
  CODE:
    dtrace("%p, \"%s\", %p", dirfile, field_code, value);

    data_type = gdp_to_voidp(data_in, value, GD_FLOAT64, gdp_package,
      "put_constant");

    RETVAL = gd_put_constant(dirfile, field_code, data_type, data_in);

    GDP_UNDEF_ON_ERROR();
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);

void
fragments(dirfile)
  DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
    int nf;
  ALIAS:
    GetData::Dirfile::fragments = 1
  PPCODE:
    dtrace("%p; %i", dirfile, (int)GIMME_V);

    nf = gd_nfragments(dirfile);

    if (GIMME_V == G_ARRAY) {
      int i;
      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, nf);
      for (i = 0; i < nf; ++i)
        GDP_PUSHpvz(gd_fragmentname(dirfile, i));
    } else {
      EXTEND(sp, 1);
      GDP_PUSHiv(nf);
    }

    dreturnvoid();

void
aliases(dirfile, field_code)
  DIRFILE * dirfile
  const char * field_code
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::aliases = 1
  PPCODE:
    dtrace("%p, \"%s\"; %i", dirfile, field_code, (int)GIMME_V);

    /* in array context, return the field list, otherwise return naliases */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_aliases(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i) {
        EXTEND(sp, 1);
        GDP_PUSHpvz(fl[i]);
      }
    } else {
      unsigned int nf = gd_naliases(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHuv(nf);
    }

    dreturnvoid();

void
fragment_affixes(dirfile, fragment_index)
  DIRFILE * dirfile
  int fragment_index
  PREINIT:
    char *prefix;
    char *suffix;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::fragment_affixes = 1
  PPCODE:
    dtrace("%p, %i", dirfile, fragment_index);

    gd_fragment_affixes(dirfile, fragment_index, &prefix, &suffix);

    GDP_UNDEF_ON_ERROR();

    EXTEND(sp, 2);
    GDP_PUSHpvz(prefix);
    GDP_PUSHpvz(suffix);

    dreturnvoid();

void
strtok(dirfile, string)
  DIRFILE * dirfile
  const char * string
  PREINIT:
    char *token;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::strtok = 1
  PPCODE:
    dtrace("%p, \"%s\"", dirfile, string);

    /* return an array of all the parsed tokens */
    for (token = gd_strtok(dirfile, string); token;
      token = gd_strtok(dirfile, NULL))
    {
      GDP_UNDEF_ON_ERROR();

      EXTEND(sp, 1);
      GDP_PUSHpvz(token);
      safefree(token);
    }

    dreturnvoid();

int
include(dirfile, file, fragment_index, namespace=NULL, flags=0)
	DIRFILE * dirfile
	const char * file
	int fragment_index
	unsigned long int flags
	gdp_char * namespace
	PREINIT:
		GDP_DIRFILE_ALIAS;
	ALIAS:
		GetData::Dirfile::include = 1
	CODE:
		dtrace("%p, \"%s\", %i, \"%s\", %lu", dirfile, file, fragment_index,
        namespace, flags);

		RETVAL = gd_include_ns(dirfile, file, fragment_index, namespace, flags);
		GDP_UNDEF_ON_ERROR();
	OUTPUT:
		RETVAL
	CLEANUP:
		dreturn("%i", RETVAL);

size_t
carray_len(dirfile, field_code)
	DIRFILE * dirfile
	const char * field_code
	PREINIT:
		GDP_DIRFILE_ALIAS;
	ALIAS:
		GetData::Dirfile::carray_len = 1
	CODE:
		dtrace("%p, \"%s\"", dirfile, field_code);
    warn("carray_len is deprecated.  Use array_len instead.");
		RETVAL = gd_array_len(dirfile, field_code);
		GDP_UNDEF_ON_ERROR();
	OUTPUT:
		RETVAL
	CLEANUP:
		dreturn("%" PRIuSIZE, RETVAL);

void
get_sarray(dirfile, field_code)
  DIRFILE * dirfile
  const char * field_code
  PREINIT:
    size_t i, len;
    const char **data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::get_sarray = 1
  PPCODE:
    dtrace("%p, \"%s\"; %i", dirfile, field_code, (int)GIMME_V);
    len = gd_array_len(dirfile, field_code);
    Newx(data_out, len, const char*);
    gd_get_sarray(dirfile, field_code, data_out);

    GDP_UNDEF_ON_ERROR(safefree(data_out));

    /* In array context return the array; in scalar context return a reference
     * to an array */
    if (GIMME_V == G_ARRAY)
      for (i = 0; i < len; ++i)
        GDP_PUSHpvz(data_out[i]);
    else
      XPUSHs(sv_2mortal(gdp_newRVavpv(data_out, len)));

    safefree(data_out);
    dreturnvoid();

void
get_sarray_slice(dirfile, field_code, start, len);
  DIRFILE * dirfile
  const char * field_code
  unsigned int start
  size_t len
  PREINIT:
    size_t i;
    const char **data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::get_sarray_slice = 1
  PPCODE:
    dtrace("%p, \"%s\", %u, %" PRIuSIZE "; %i", dirfile, field_code, start, len,
        (int)GIMME_V);
    Newx(data_out, len, const char*);
    gd_get_sarray_slice(dirfile, field_code, start, len, data_out);

    GDP_UNDEF_ON_ERROR(safefree(data_out));

    if (GIMME_V == G_ARRAY)
      for (i = 0; i < len; ++i)
        GDP_PUSHpvz(data_out[i]);
    else
      XPUSHs(sv_2mortal(gdp_newRVavpv(data_out, len)));

    safefree(data_out);
    dreturnvoid();

void
sarrays(dirfile)
  DIRFILE * dirfile
  PREINIT:
    int i;
    const char ***data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::sarrays = 1
  PPCODE:
    dtrace("%p; %i", dirfile, (int)GIMME_V);
    data_out = gd_sarrays(dirfile);

    GDP_UNDEF_ON_ERROR();

    /* in array context, return an array of arrays of strings.
     * Otherwise, return a reference to the same. */
    if (GIMME_V == G_ARRAY)
      for (i = 0; data_out[i]; ++i)
        XPUSHs(sv_2mortal(gdp_newRVavpv0(data_out[i])));
    else {
      AV *av = newAV();
      for (i = 0; data_out[i]; ++i)
        av_store(av, i, gdp_newRVavpv0(data_out[i]));
      XPUSHs(sv_2mortal(newRV_noinc((SV*)av)));
    }

    dreturnvoid();

int
put_sarray(dirfile, field_code, sv_in, ...)
    DIRFILE * dirfile
    const char * field_code
    SV *sv_in
  PREINIT:
    const char **data_in;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::put_sarray = 1
  CODE:
    dtrace("%p, \"%s\", %p ...[%li]", dirfile, field_code, sv_in,
        (long)items - 3);

    data_in = gdp_convert_strarr(NULL, items, ax, 2, gdp_package, "put_sarray");

    RETVAL = gd_put_sarray(dirfile, field_code, data_in);

    GDP_UNDEF_ON_ERROR(safefree(data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    safefree(data_in);
    dreturn("%i", RETVAL);

int
put_sarray_slice(dirfile, field_code, start, sv_in, ...)
    DIRFILE * dirfile
    const char * field_code
    gd_off64_t start
    SV *sv_in
  PREINIT:
    GDP_DIRFILE_ALIAS;
    size_t len;
    const char **data_in;
  ALIAS:
    GetData::Dirfile::put_sarray_slice = 1
  CODE:
    dtrace("%p, \"%s\", %lli, %p ...[%li]", dirfile, field_code,
        (long long)start, sv_in, (long)items - 4);

    data_in = gdp_convert_strarr(&len, items, ax, 3, gdp_package,
        "put_sarray_slice");

    RETVAL = gd_put_sarray_slice(dirfile, field_code, start, len, data_in);

    GDP_UNDEF_ON_ERROR(safefree(data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    safefree(data_in);
    dreturn("%i", RETVAL);

int
add_sarray(dirfile, field_code, fragment_index, sv_in, ...)
    DIRFILE * dirfile
    const char * field_code
    int fragment_index
    SV *sv_in
  PREINIT:
    GDP_DIRFILE_ALIAS;
    size_t len;
    const char **data_in;
  ALIAS:
    GetData::Dirfile::add_sarray = 1
  CODE:
    dtrace("%p, \"%s\", %i, %p, ...[%li]", dirfile, field_code, fragment_index,
        sv_in, (long)items - 4);

    data_in = gdp_convert_strarr(&len, items, ax, 3, gdp_package, "add_sarray");

    RETVAL = gd_add_sarray(dirfile, field_code, len, data_in, fragment_index);

    GDP_UNDEF_ON_ERROR(safefree(data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    safefree(data_in);
    dreturn("%i", RETVAL);

int
madd_sarray(dirfile, parent, field_code, sv_in, ...)
    DIRFILE * dirfile
    const char * parent
    const char * field_code
    SV *sv_in
  PREINIT:
    GDP_DIRFILE_ALIAS;
    size_t len;
    const char **data_in;
  ALIAS:
    GetData::Dirfile::madd_sarray = 1
  CODE:
    dtrace("%p, \"%s\", \"%s\", %p, ...[%li]", dirfile, parent, field_code,
        sv_in, (long)items - 4);

    data_in = gdp_convert_strarr(&len, items, ax, 3, gdp_package,
        "madd_sarray");

    RETVAL = gd_madd_sarray(dirfile, parent, field_code, len, data_in);

    GDP_UNDEF_ON_ERROR(safefree(data_in));
  OUTPUT:
    RETVAL
  CLEANUP:
    safefree(data_in);
    dreturn("%i", RETVAL);

void
msarrays(dirfile, parent)
  DIRFILE * dirfile
  const char * parent
  PREINIT:
    int i;
    const char ***data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::msarrays = 1
  PPCODE:
    dtrace("%p, \"%s\"; %i", dirfile, parent, (int)GIMME_V);
    data_out = gd_msarrays(dirfile, parent);

    GDP_UNDEF_ON_ERROR();

    /* in array context, return an array of arrays of strings.
     * Otherwise, return a reference to the same. */
    if (GIMME_V == G_ARRAY)
      for (i = 0; data_out[i]; ++i)
        XPUSHs(sv_2mortal(gdp_newRVavpv0(data_out[i])));
    else {
      AV *av = newAV();
      for (i = 0; data_out[i]; ++i)
        av_store(av, i, gdp_newRVavpv0(data_out[i]));
      XPUSHs(sv_2mortal(newRV_noinc((SV*)av)));
    }

    dreturnvoid();

INCLUDE: simple_funcs.xs
