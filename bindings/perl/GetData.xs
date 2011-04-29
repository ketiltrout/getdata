#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#undef _BSD_SOURCE
#undef _POSIX_SOURCE
#undef _SVID_SOURCE
#include "../../src/internal.h"

/* Perl 5.8.9 and earlier don't provide hv_fetchs */
#ifdef hv_fetchs
# define gdp_hv_fetchs hv_fetchs
#else
# define gdp_hv_fetchs(hv,key,lval) hv_fetch(hv, key, sizeof(key) - 1, lval)
#endif

#define undef &PL_sv_undef

/* lfs hackery */
#if defined _LARGEFILE64_SOURCE || (defined _FILE_OFFSET_BITS && \
    _FILE_OFFSET_BITS == 64) || (defined __CYGWIN__) || (defined __APPLE__)
# define gdp64(x) x ## 64
#else
# define gdp64(x) x
#endif

/* fake data types to simplify our typemap */
typedef complex double gdp_complex_in;
typedef _Complex double gdpu_complex;
typedef gd_bit_t gdpu_bitnum_t;
typedef gd_bit_t gdpu_numbits_t;
typedef gd_shift_t gdpu_shift_t;
typedef gd_spf_t gdpu_spf_t;
typedef gd_type_t gdpu_type_t;
typedef int gdpu_int;
typedef const char gdpu_char;

#define GDP_DIRFILE_ALIAS \
  const char *gdp_package = ix ? "GetData::Dirifle" : "GetData";

#define GDP_UNDEF_ON_ERROR(x) \
      if (gd_error(dirfile)) { x; dreturnvoid(); XSRETURN_UNDEF; }

#define GDP_PUSHpvn(s)      XPUSHs(sv_2mortal(newSVpvn(s, sizeof(s) - 1)))
#define GDP_PUSHpvz(s)      XPUSHs(sv_2mortal(newSVpv(s, 0)))
#define GDP_PUSHuv(s)       XPUSHs(sv_2mortal(newSVuv(s)))
#define GDP_PUSHiv(s)       XPUSHs(sv_2mortal(newSViv(s)))
#define GDP_PUSHnv(s)       XPUSHs(sv_2mortal(newSVnv(s)))
#define GDP_PUSHrv(s)       XPUSHs(sv_2mortal(newRV_noinc((SV*)s)))
#define GDP_PUSHcmp(s)      XPUSHs(sv_2mortal(gdp_newSVcmp(s)))
#define GDP_PUSHrvavpv(s,n) XPUSHs(sv_2mortal(gdp_newRVavpv((const char**)s,n)))
#define GDP_PUSHrvavcmp(s,n) XPUSHs(sv_2mortal(gdp_newRVavcmp(s,n)))
#define GDP_PUSHrvavnv(s,n) XPUSHs(sv_2mortal(gdp_newRVavnv(s,n)))

struct gdp_callback_stuff_t {
  SV *func;
  SV *data;
};

struct gdp_dirfile_t {
  DIRFILE *D;
  struct gdp_callback_stuff_t cbdata;
};

static DIRFILE *gdp_invalid = NULL;


/* sv might be NULL, indicating undef */
static gd_type_t gdp_get_type(SV **sv, const char *pkg, const char *func)
{
  dtrace("%p, \"%s\", \"%s\"", sv, pkg, func);

  if (sv == NULL && *sv == undef)
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

/* convert a Perl object into a c99 complex number */
static complex double gdp_cmp_to_c99(SV *src, int *ok, const char* pkg,
  const char *func)
{
  dtrace("%p, %p, \"%s\", \"%s\"", src, ok, pkg, func);
  double complex val = 0;

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
      val = SvNV(*m) * cexp(SvNV(*a));
    } else {
      SV **sv = gdp_hv_fetchs(hv, "cartesian", 0);
      if (sv == NULL || !SvROK(*sv) || SvTYPE(SvRV(*sv)) != SVt_PVAV)
        croak("%s::%s() - Malformed Math::Complex object", pkg, func);

      AV *data = (AV*)SvRV(*sv);
      SV **r = av_fetch(data, 0, 0);
      SV **i = av_fetch(data, 1, 0);
      if (r == NULL || i == NULL)
        croak("%s::%s() - Malformed Math::Complex object", pkg, func);
      val = SvNV(*r) + _Complex_I * SvNV(*i);
    }
  } else if (ok) /* if ok is non-NULL, the caller is prepared to handle
                      non-complex data */
      *ok = 0;
    else
      val = SvNV(src);

  dreturn("%g;%g", creal(val), cimag(val));
  return val;
}

#define GDP_EHASH_FETCH(key) \
  v = gdp_hv_fetchs((HV*)sv, key, 0); \
  if (v == NULL) \
    croak("%s::%s() - Missing required key '" key "' in entry hash", pkg, func)

#define GDP_EHASH_FETCH_CMP(key,member) do { \
    GDP_EHASH_FETCH(key); E->member = gdp_cmp_to_c99(*v, NULL, pkg, func); \
  } while(0)

#define GDP_EHASH_FETCH_IV(key,member,type) \
  do { GDP_EHASH_FETCH(key); E->member = (type)SvIV(*v); } while(0)

#define GDP_EHASH_FETCH_NV(key,member) \
  do { GDP_EHASH_FETCH(key); E->member = SvNV(*v); } while(0)

#define GDP_EHASH_FETCH_UV(key,member,type) \
  do { GDP_EHASH_FETCH(key); E->member = (type)SvUV(*v); } while(0)

#define GDP_EHASH_FETCH_UV_REQ(key,member,type) \
  do { GDP_EHASH_FETCH_REQ(key); E->member = (type)SvUV(*v); } while(0)

#define GDP_EHASH_FETCH_PV(key,member) \
 do { \
    GDP_EHASH_FETCH(key); \
    E->member = (!SvOK(*v)) ? NULL : SvPV_nolen(*v); \
  } while(0)

/* populate a complex double array in gd_entry_t */
static void gdp_fetch_cmp_list(complex double *c, HV *hv, const char* key,
  int n, const char *pkg, const char *func)
{
  dtrace("%p, %p, \"%s\", %i, \"%s\", \"%s\"", c, hv, key, n, pkg, func);
  int i;
  SV **v = hv_fetch(hv, key, strlen(key), 0);

  if (v == NULL)
    croak("%s::%s() - Missing required key '%s' in entry hash", pkg, func, key);

  if (SvTYPE(*v) != SVt_PVAV)
    croak("%s::%s() - Key '%s' must be list in entry hash", pkg, func, key);

  for (i = 0; i < n; ++i) {
    v = av_fetch((AV*)*v, i, 0);
    if (v)
      c[i] = gdp_cmp_to_c99(*v, NULL, pkg, func);
  }

  dreturnvoid();
}

/* populate in_fields in gd_entry_t */
static void gdp_fetch_in_fields(char **in_fields, SV *sv, int n,
  const char *pkg, const char *func)
{
  dtrace("%p, %p, %i, \"%s\", \"%s\"", in_fields, sv, n, pkg, func);
  int i;
  SV **v;

  GDP_EHASH_FETCH("in_fields");

  if (SvTYPE(*v) != SVt_PVAV)
    croak("%s::%s() - Key 'in_fields' must be list in entry hash", pkg, func);

  for (i = 0; i < n; ++i) {
    v = av_fetch((AV*)*v, i, 0);
    if (v)
      in_fields[i] = SvPV_nolen(*v);
  }

  dreturnvoid();
}

/* populate scalar elements of gd_entry_t */
static void gdp_fetch_scalars(gd_entry_t *E, HV *hv, unsigned int mask,
  const char *pkg, const char* func)
{
  dtrace("%p, %p, %06x \"%s\", \"%s\"", E, hv, mask, pkg, func);
  int i;

  SV **scalar = gdp_hv_fetchs(hv, "scalar", 0);
  SV **scalar_ind = gdp_hv_fetchs(hv, "scalar_ind", 0);
  SV **v;

  /* there's no point in recording scalar indicies if we don't have scalars */
  if (scalar == NULL) {
    dreturnvoid();
    return;
  }

  if (SvTYPE(*scalar) != SVt_PVAV)
    croak("%s::%s() - Key 'scalar' must be list in entry hash", pkg, func);
  if (scalar_ind && SvTYPE(*scalar_ind) != SVt_PVAV)
    croak("%s::%s() - Key 'scalar_ind' must be list in entry hash", pkg, func);

  for (i = 0; i <= GD_MAX_POLYORD; ++i)
    if (mask & (1 << i)) {
      v = av_fetch((AV*)*scalar, i, 0);
      if (v) {
        E->scalar[i] = SvPV_nolen(*v);
        if (scalar_ind) {
          v = av_fetch((AV*)*scalar_ind, i, 0);
          if (v)
            E->scalar_ind[i] = SvIV(*v);
        }
      }
    }

  dreturnvoid();
}

/* convert a Perl hash into a gd_entry_t */
static void gdp_to_entry(gd_entry_t *E, SV *sv, const char *pkg,
  const char *func)
{
  dtrace("%p, %p, \"%s\", \"%s\"", E, sv, pkg, func);
  SV **v;
  int n;

  memset(E, 0, sizeof(gd_entry_t));

  /* de-reference as needed */
  while (SvROK(sv))
    sv = SvRV(sv);

  if (SvTYPE(sv) != SVt_PVHV) 
    croak("%s::%s() - Entry must be hash", pkg, func);

  GDP_EHASH_FETCH_UV("field_type", field_type, gd_entype_t);
  GDP_EHASH_FETCH_PV("field", field);
  GDP_EHASH_FETCH_UV("fragment_index", fragment_index, int);

  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      GDP_EHASH_FETCH_PV("in_field", in_fields[0]);
      GDP_EHASH_FETCH_UV("bitnum", bitnum, gd_bit_t);
      GDP_EHASH_FETCH_UV("numbits", bitnum, gd_bit_t);
      gdp_fetch_scalars(E, (HV*)sv, 0x3, pkg, func);
      break;
    case GD_CARRAY_ENTRY:
      GDP_EHASH_FETCH_IV("array_len", array_len, size_t);
      /* fallthrough */
    case GD_CONST_ENTRY:
      GDP_EHASH_FETCH_UV("const_type", const_type, gd_type_t);
      break;
    case GD_LINCOM_ENTRY:
      GDP_EHASH_FETCH_IV("n_fields", n_fields, int);
      n = (E->n_fields > GD_MAX_LINCOM) ? GD_MAX_LINCOM : E->n_fields;
      gdp_fetch_in_fields(E->in_fields, sv, n, pkg, func);
      E->comp_scal = 1;
      gdp_fetch_cmp_list(E->cm, (HV*)sv, "cm", n, pkg, func);
      gdp_fetch_cmp_list(E->cb, (HV*)sv, "cb", n, pkg, func);
      gdp_fetch_scalars(E, (HV*)sv, ((1 << n) - 1) * 9, pkg, func);
      break;
    case GD_LINTERP_ENTRY:
      GDP_EHASH_FETCH_PV("in_field", in_fields[0]);
      GDP_EHASH_FETCH_PV("table", table);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
      gdp_fetch_in_fields(E->in_fields, sv, 2, pkg, func);
      break;
    case GD_PHASE_ENTRY:
      GDP_EHASH_FETCH_PV("in_field", in_fields[0]);
      GDP_EHASH_FETCH_IV("shift", shift, gd_shift_t);
      gdp_fetch_scalars(E, (HV*)sv, 1, pkg, func);
      break;
    case GD_POLYNOM_ENTRY:
      GDP_EHASH_FETCH_PV("in_field", in_fields[0]);
      GDP_EHASH_FETCH_IV("poly_ord", poly_ord, int);
      n = (E->poly_ord > GD_MAX_POLYORD) ? GD_MAX_POLYORD : E->poly_ord;
      E->comp_scal = 1;
      gdp_fetch_cmp_list(E->ca, (HV*)sv, "ca", n, pkg, func);
      gdp_fetch_scalars(E, (HV*)sv, (1 << (n + 1)) - 1, pkg, func);
      break;
    case GD_RECIP_ENTRY:
      GDP_EHASH_FETCH_PV("in_field", in_fields[0]);
      E->comp_scal = 1;
      GDP_EHASH_FETCH_CMP("cdividend", cdividend);
      gdp_fetch_scalars(E, (HV*)sv, 1, pkg, func);
      break;
    case GD_RAW_ENTRY:
      GDP_EHASH_FETCH_UV("spf", spf, gd_spf_t);
      GDP_EHASH_FETCH_UV("data_type", data_type, gd_type_t);
      gdp_fetch_scalars(E, (HV*)sv, 1, pkg, func);
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
  double complex c;
  int cmp = 1;

  /* undef results in randomness */
  if (src == NULL) {
    dreturnvoid();
    return;
  }

  /* check for and convert complex data */
  c = gdp_cmp_to_c99(src, &cmp, pkg, func);

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
      *(complex float*)dest = cmp ? (complex float)c : (complex float)SvNV(src);
      break;
    case GD_COMPLEX128:
      *(complex double*)dest = cmp ? c : (complex double)SvNV(src);
      break;
    case GD_NULL:
    case GD_UNKNOWN:
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

  dtrace("%p, %i, %i, %zi, \"%s\", \"%s\"", d, (int)items, (int)ax, idx, pkg,
      func);

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
    din.data_in = safemalloc(din.nsamp * GD_SIZE(din.type));

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

  dreturn("{ %p, %03x, %i, %zi }", din.data_in, din.type, din.arg_type,
    din.nsamp);
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
    memset(dest, 0, 1);
    type = GD_UINT8;
  } else {
    int cmp = 0;
    complex double c = gdp_cmp_to_c99(src, &cmp, pkg, func);

    if (cmp) {
      memcpy(dest, &c, sizeof(complex double));
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

static SV *gdp_newSVcmp(double complex v)
{
  SV **dummy;
  SV *sv;
  /* build a list containing the data, and take it's reference */
  AV *av = newAV();
  av_extend(av, 1);
  av_store(av, 0, newSVnv(creal(v)));
  av_store(av, 1, newSVnv(cimag(v)));
  sv = newRV_noinc((SV*)av);

  /* create a Math::Complex object */
  HV *hv = newHV();
  dummy = hv_store(hv, "p_dirty", 7, newSVuv(1), 0);
  dummy = hv_store(hv, "c_dirty", 7, newSVuv(0), 0);
  dummy = hv_store(hv, "cartesian", 9, sv, 0);
  HV *stash = gv_stashpv("Math::Complex", GV_ADD);
  sv = sv_bless(newRV_noinc((SV*)hv), stash);
  return sv;
}

/* convert a char ** into a reference to a list of strings */
static SV *gdp_newRVavpv(const char **l, size_t n)
{
  dtrace("%p, %zi", l, n);
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
static SV *gdp_newRVavcmp(const complex double *l, size_t n)
{
  dtrace("%p, %zi", l, n);
  SV *rv;
  int i;
  AV *av = newAV();
  av_extend(av, n - 1);

  for (i = 0; i < n; ++i)
    av_store(av, i, gdp_newSVcmp(l[i]));

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
  int was_rv = 0;

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

  XPUSHs(sv_2mortal(newRV_noinc((SV *)phash)));
  XPUSHs(callback_data);

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
    was_rv = 1;
  }

  /* ferret out response */
  switch (SvTYPE(ret)) {
    AV *av;
    int len;

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
        pdata->line = strdup(SvPV_nolen(*val));
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
        pdata->line = strdup(SvPV_nolen(*val1));
      } else {
        croak("GetData: bad data type in array returned by parser callback.");
        return GD_SYNTAX_ABORT; /* ca'n't get here */
      }
    }
    break;
    case SVt_PV:
    pdata->line = strdup(SvPV_nolen(ret));
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
  if (sp) for (i = 0; i < n; ++i) XPUSHs(sv_2mortal(newSVuv(((t*)data)[i]))); \
  else    for (i = 0; i < n; ++i) av_store(av, i,   newSVuv(((t*)data)[i]));

#define GDP_UNPACKI(t) \
  if (sp) for (i = 0; i < n; ++i) XPUSHs(sv_2mortal(newSViv(((t*)data)[i]))); \
  else    for (i = 0; i < n; ++i) av_store(av, i,   newSViv(((t*)data)[i]));

#define GDP_UNPACKN(t) \
  if (sp) for (i = 0; i < n; ++i) XPUSHs(sv_2mortal(newSVnv(((t*)data)[i]))); \
  else    for (i = 0; i < n; ++i) av_store(av, i,   newSVnv(((t*)data)[i]));

#define GDP_UNPACKC(t) \
  if (sp) for (i = 0; i < n; ++i) \
                    XPUSHs(sv_2mortal(gdp_newSVcmp(((t*)data)[i]))); \
  else for (i = 0; i < n; ++i) av_store(av, i, gdp_newSVcmp(((t*)data)[i]));

/* unpack data. If sp is NULL, return an AV, otherwise push it onto the perl
 * stack; returns the updated stack pointer */
static void * gdp_unpack(SV **sp, const void *data, size_t n, gd_type_t type)
{
  dtrace("%p, %p, %zi, %03x", sp, data, n, type);
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
      GDP_UNPACKC(complex float);
      break;
    case GD_COMPLEX128:
      GDP_UNPACKC(complex double);
      break;
    case GD_UNKNOWN:
    case GD_NULL:
      break;
  }

  dreturn("%p", sp ? (void *)sp : (void *)av);
  return sp ? (void *)sp : (void *)av;
}

/* Module starts here --------------------------------------------------- */
MODULE = GetData  PACKAGE = GetData
PROTOTYPES: ENABLE

BOOT:
  gdp_invalid = gd_invalid_dirfile();

void
DESTROY(dirfile)
  DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::DESTROY = 1
  CODE:
    dtrace("%p", dirfile);
    if (dirfile != gdp_invalid)
      gd_discard(dirfile);
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


DIRFILE *
open(dirfilename, flags, sehandler=undef, extra=undef)
  const char * dirfilename
  unsigned long flags
  SV * sehandler
  SV * extra
  PREINIT:
    struct gdp_dirfile_t *gdp_dirfile =
    (struct gdp_dirfile_t *)safemalloc(sizeof(struct gdp_dirfile_t));
  CODE:
    dtrace("\"%s\", %lu, %p, %p", dirfilename, flags, sehandler, extra);
    if (sehandler == undef) {
      gdp_dirfile->cbdata.func = NULL;
      gdp_dirfile->cbdata.data = NULL;

      RETVAL = gd_cbopen(dirfilename, flags, NULL, NULL);
    } else {
      gdp_dirfile->cbdata.func = sehandler;
      gdp_dirfile->cbdata.data = extra;

      RETVAL = gd_cbopen(dirfilename, flags, gdp_parser_callback,
          &gdp_dirfile->cbdata);
    }
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%p", gdp_dirfile);

DIRFILE *
invalid_dirfile()
  PREINIT:
    struct gdp_dirfile_t *gdp_dirfile =
    (struct gdp_dirfile_t *)safemalloc(sizeof(struct gdp_dirfile_t));
  CODE:
    dtracevoid();
    gdp_dirfile->cbdata.func = NULL;
    gdp_dirfile->cbdata.data = NULL;

    RETVAL = gd_invalid_dirfile();
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%p", gdp_dirfile);

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
    dtrace("%p, \"%s\", %03x; %lu", dirfile, field_code, return_type,
        GIMME_V);
    size_t len = gd_carray_len(dirfile, field_code);
    data_out = safemalloc(GD_SIZE(return_type) * len);
    gd_get_carray(dirfile, field_code, return_type, data_out);

    GDP_UNDEF_ON_ERROR(safefree(data_out));

    if (GIMME_V == G_ARRAY)
      sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
    else
      XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

    safefree(data_out);
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
    dtrace("%p, \"%s\", %u, %zi, %03x; %lu", dirfile, field_code, return_type,
        start, len, GIMME_V);
    data_out = safemalloc(GD_SIZE(return_type) * len);
    gd_get_carray_slice(dirfile, field_code, start, len, return_type, data_out);

    GDP_UNDEF_ON_ERROR(safefree(data_out));

    if (GIMME_V == G_ARRAY)
      sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
    else
      XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

    safefree(data_out);
    dreturnvoid();

SV *
get_constant(dirfile, field_code, return_type)
  DIRFILE * dirfile
  const char * field_code
  gd_type_t return_type
  PREINIT:
    void *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::get_constant = 1
  CODE:
    gd_type_t type;
    dtrace("%p, \"%s\", %03x", dirfile, field_code, return_type);
    data_out = safemalloc(16);

    if (return_type & GD_COMPLEX)
      type = GD_COMPLEX128;
    else if (return_type & GD_IEEE754)
      type = GD_FLOAT64;
    else if (return_type & GD_SIGNED)
      type = GD_INT64;
    else
      type = GD_UINT64;

    gd_get_constant(dirfile, field_code, type, data_out);

    GDP_UNDEF_ON_ERROR(safefree(data_out));

    if (type == GD_COMPLEX128)
      RETVAL = gdp_newSVcmp(*(complex double*)data_out);
    else if (type == GD_FLOAT64)
      RETVAL = newSVnv(*(double*)data_out);
    else if (type == GD_INT64)
      RETVAL = newSViv(*(int64_t*)data_out);
    else
      RETVAL = newSVuv(*(uint64_t*)data_out);
  OUTPUT:
    RETVAL
  CLEANUP:
    safefree(data_out);
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
    dtrace("%p, %03x; %lu", dirfile, return_type, GIMME_V);
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
    dtrace("%p, %03x; %lu", dirfile, return_type, GIMME_V);
    I32 i, len = (I32)gd_nfields_by_type(dirfile, GD_CARRAY_ENTRY);
    data_out = gd_carrays(dirfile, return_type);

    GDP_UNDEF_ON_ERROR();

    /* in array context, return an array of arrays of unpacked data.
     * Otherwise, return a reference to an array of packed data. */
    if (GIMME_V == G_ARRAY)
      for (i = 0; i < len; ++i)
        XPUSHs(sv_2mortal(newRV_noinc((SV *)gdp_unpack(NULL, data_out[i].d,
                  data_out[i].n, return_type))));
    else {
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
    dtrace("%p, \"%s\"; %lu", dirfile, field_code, GIMME_V);

    if (GIMME_V == G_ARRAY) {
      gd_entry_t E;
      gd_entry(dirfile, field_code, &E);

      GDP_UNDEF_ON_ERROR();

      /* push the hash onto the stack */
      GDP_PUSHpvn("field");
      GDP_PUSHpvz(E.field);
      GDP_PUSHpvn("field_type");
      GDP_PUSHuv(E.field_type);
      GDP_PUSHpvn("fragment_index");
      GDP_PUSHuv(E.fragment_index);
      switch (E.field_type) {
        case GD_BIT_ENTRY:
        case GD_SBIT_ENTRY:
          GDP_PUSHpvn("in_field");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("bitnum");
          GDP_PUSHuv(E.bitnum);
          GDP_PUSHpvn("numbits");
          GDP_PUSHuv(E.numbits);
          sp = gdp_store_scalars(sp, &E, 0x3);
          break;
        case GD_CARRAY_ENTRY:
          GDP_PUSHpvn("array_len");
          GDP_PUSHuv(E.array_len);
          /* fallthrough */
        case GD_CONST_ENTRY:
          GDP_PUSHpvn("const_type");
          GDP_PUSHuv(E.const_type);
          break;
        case GD_LINCOM_ENTRY:
          GDP_PUSHpvn("n_fields");
          GDP_PUSHiv(E.n_fields);
          GDP_PUSHpvn("in_fields");
          GDP_PUSHrvavpv(E.in_fields, E.n_fields);
          GDP_PUSHpvn("m");
          GDP_PUSHrvavcmp(E.cm, E.n_fields);
          GDP_PUSHpvn("b");
          GDP_PUSHrvavcmp(E.cb, E.n_fields);
          sp = gdp_store_scalars(sp, &E, ((1 << E.n_fields) - 1) * 9);
          break;
        case GD_LINTERP_ENTRY:
          GDP_PUSHpvn("in_field");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("table");
          GDP_PUSHpvz(E.table);
          break;
        case GD_MULTIPLY_ENTRY:
        case GD_DIVIDE_ENTRY:
          GDP_PUSHpvn("in_fields");
          GDP_PUSHrvavpv(E.in_fields, 2);
          break;
        case GD_PHASE_ENTRY:
          GDP_PUSHpvn("in_field");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("shift");
          GDP_PUSHiv(E.shift);
          sp = gdp_store_scalars(sp, &E, 1);
          break;
        case GD_POLYNOM_ENTRY:
          GDP_PUSHpvn("poly_ord");
          GDP_PUSHiv(E.poly_ord);
          GDP_PUSHpvn("in_field");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("a");
          GDP_PUSHrvavcmp(E.ca, E.poly_ord + 1);
          sp = gdp_store_scalars(sp, &E, (1 << (E.poly_ord + 1)) - 1);
          break;
        case GD_RECIP_ENTRY:
          GDP_PUSHpvn("in_field");
          GDP_PUSHpvz(E.in_fields[0]);
          GDP_PUSHpvn("dividend");
          GDP_PUSHcmp(E.cdividend);
          sp = gdp_store_scalars(sp, &E, 1);
          break;
        case GD_RAW_ENTRY:
          GDP_PUSHpvn("spf");
          GDP_PUSHuv(E.spf);
          GDP_PUSHpvn("data_type");
          GDP_PUSHuv(E.data_type);
          sp = gdp_store_scalars(sp, &E, 1);
          break;
        case GD_INDEX_ENTRY:
        case GD_STRING_ENTRY:
        case GD_NO_ENTRY:
          break;
      }

      gd_free_entry_strings(&E);
    } else {
      gd_entype_t t = gd_entry_type(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

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
    free(s);
    dreturn("%p", RETVAL);

AV *
mcarrays(dirfile, parent, return_type, unpacked=0)
  DIRFILE * dirfile
  const char * parent;
  gd_type_t return_type
  IV unpacked
  PREINIT:
    const gd_carray_t *data_out = NULL;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::mcarrays = 1
  CODE:
    dtrace("%p, %03x, %i", dirfile, return_type, (int)unpacked);
    I32 i, len = (I32)gd_nmfields_by_type(dirfile, parent, GD_CARRAY_ENTRY);
    data_out = gd_mcarrays(dirfile, parent, return_type);

    GDP_UNDEF_ON_ERROR();

    RETVAL = newAV();
    for (i = 0; i < len; ++i)
    av_store(RETVAL, i, newSVpvn(data_out[i].d,
          data_out[i].n * GD_SIZE(return_type)));
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%p", RETVAL);

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
    dtrace("%p, %03x; %lu", dirfile, return_type, GIMME_V);
    int len = gd_nmfields_by_type(dirfile, parent, GD_CONST_ENTRY);
    data_out = gd_mconstants(dirfile, parent, return_type);

    GDP_UNDEF_ON_ERROR();

    if (GIMME_V == G_ARRAY)
      sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
    else
      XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

    dreturnvoid();

void
parser_callback(dirfile, sehandler, extra=undef)
  DIRFILE * dirfile
  SV *sehandler
  SV *extra
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::parser_callback = 1
  CODE:
    dtrace("%p, %p, %p", dirfile, sehandler, extra);
    if (sehandler == undef) {
      gdp_dirfile->cbdata.func = NULL;
      gdp_dirfile->cbdata.data = NULL;

      gd_parser_callback(dirfile, NULL, NULL);
    } else {
      gdp_dirfile->cbdata.func = sehandler;
      gdp_dirfile->cbdata.data = extra;

      gd_parser_callback(dirfile, gdp_parser_callback, &gdp_dirfile->cbdata);
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
    RETVAL = safemalloc(len);

    /* get string */
    gd_get_string(dirfile, field_code, len, RETVAL);
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("\"%s\"", RETVAL);
    safefree(RETVAL);

int
close(dirfile)
  DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::close = 1
  CODE:
    dtrace("%p", dirfile);

    if (dirfile != gdp_invalid) {
      RETVAL = gd_close(dirfile);

      if (!RETVAL)
        gdp_dirfile->D = NULL;
    } else
      RETVAL = 0;
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);

int
discard(dirfile)
  DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::discard = 1
  CODE:
    dtrace("%p", dirfile);

    if (dirfile != gdp_invalid) {
      RETVAL = gd_discard(dirfile);

      if (!RETVAL)
        gdp_dirfile->D = NULL;
    } else
      RETVAL = 0;
  OUTPUT:
    RETVAL
  CLEANUP:
    dreturn("%i", RETVAL);

void
getdata(dirfile, field_code, first_frame, first_samp, num_frames, num_samp, return_type)
  DIRFILE * dirfile
  const char * field_code
  off64_t first_frame
  off64_t first_samp
  size_t num_frames
  size_t num_samp
  gd_type_t return_type
  PREINIT:
    void *data_out = NULL;
    gd_spf_t spf = 1;
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::getdata = 1
  PPCODE:
    dtrace("%p, \"%s\", %lli, %lli, %zi, %zi, %03x; %lu", dirfile, field_code,
        (long long)first_frame, (long long)first_samp, num_frames, num_samp,
        return_type, GIMME_V);

    if (num_frames) {
      spf = gd_spf(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();
    }

    data_out = safemalloc(GD_SIZE(return_type) * (spf * num_frames + num_samp));

    size_t len = gdp64(gd_getdata)(dirfile, field_code, first_frame, first_samp,
        num_frames, num_samp, return_type, data_out);

    GDP_UNDEF_ON_ERROR(safefree(data_out));

    /* In array context, unpack the array and push it onto the stack, otherwise
     * just return the packed data */
    if (GIMME_V == G_ARRAY)
      sp = (SV **)gdp_unpack(sp, data_out, len, return_type);
    else
      XPUSHs(sv_2mortal(newSVpvn(data_out, len * GD_SIZE(return_type))));

    safefree(data_out);
    dreturnvoid();

void
field_list(dirfile)
    DIRFILE * dirfile
  PREINIT:
    GDP_DIRFILE_ALIAS;
  ALIAS:
    GetData::Dirfile::field_list = 1
  PPCODE:
    dtrace("%p; %lu", dirfile, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_field_list(dirfile);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nfields(dirfile);

      GDP_UNDEF_ON_ERROR();

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
    dtrace("%p, %i; %lu", dirfile, type, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_field_list_by_type(dirfile, type);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nfields_by_type(dirfile, type);

      GDP_UNDEF_ON_ERROR();

      GDP_PUSHuv(nf);
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
    dtrace("%p; %lu", dirfile, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_vector_list(dirfile);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nvectors(dirfile);

      GDP_UNDEF_ON_ERROR();

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
    dtrace("%p; %lu", dirfile, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_strings(dirfile);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nfields_by_type(dirfile, GD_STRING_ENTRY);

      GDP_UNDEF_ON_ERROR();

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
    dtrace("%p, \"%s\"; %lu", dirfile, field_code, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mfield_list(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nmfields(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

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
    dtrace("%p, \"%s\", %i; %lu", dirfile, parent, type, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mfield_list_by_type(dirfile, parent, type);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nmfields_by_type(dirfile, parent, type);

      GDP_UNDEF_ON_ERROR();

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
    dtrace("%p; %lu", dirfile, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mvector_list(dirfile, parent);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nmvectors(dirfile, parent);

      GDP_UNDEF_ON_ERROR();

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
    dtrace("%p, \"%s\"; %lu", dirfile, field_code, GIMME_V);

    /* in array context, return the field list, otherwise return nfields */
    if (GIMME_V == G_ARRAY) {
      int i;
      const char **fl = gd_mstrings(dirfile, field_code);

      GDP_UNDEF_ON_ERROR();

      for (i = 0; fl[i]; ++i)
        GDP_PUSHpvz(fl[i]);
    } else {
      unsigned int nf = gd_nmfields_by_type(dirfile, field_code,
          GD_STRING_ENTRY);

      GDP_UNDEF_ON_ERROR();

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
    dtrace("%p, \"%s\", %p, ...[%li]", dirfile, field_code, d, items - 3);
    
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
    off64_t start
    SV *d;
  PREINIT:
    GDP_DIRFILE_ALIAS;
    struct gdp_din din;
  ALIAS:
    GetData::Dirfile::put_carray_slice = 1
  CODE:
    dtrace("%p, \"%s\", %lli, %p, ...[%li]", dirfile, field_code,
        (long long)start, d, items - 4);
    
    din = gdp_convert_data(d, items, ax, 3, gdp_package, "put_carray");

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
        const_type, fragment_index, d, items - 5);
    
    din = gdp_convert_data(d, items, ax, 4, gdp_package, "put_carray");

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
        field_code, const_type, d, items - 5);
    
    din = gdp_convert_data(d, items, ax, 4, gdp_package, "put_carray");

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
    off64_t first_frame
    off64_t first_sample
    SV *d;
  PREINIT:
    GDP_DIRFILE_ALIAS;
    struct gdp_din din;
  ALIAS:
    GetData::Dirfile::putdata = 1
  CODE:
    dtrace("%p, \"%s\", %lli, %lli, %p, ...[%li]", dirfile, field_code,
        (long long)first_frame, (long long)first_sample, d, items - 5);

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
    dreturn("%zi", RETVAL);

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
    unsigned int nf;
  ALIAS:
    GetData::Dirfile::fragments = 1
  PPCODE:
    dtrace("%p; %lu", dirfile, GIMME_V);

    nf = gd_nfragments(dirfile);

    if (GIMME_V == G_ARRAY) {
      unsigned int i;
      for (i = 0; i < nf; ++i)
        GDP_PUSHpvz(gd_fragmentname(dirfile, i));
    } else
      GDP_PUSHuv(nf);

    dreturnvoid();


INCLUDE: simple_funcs.xs
