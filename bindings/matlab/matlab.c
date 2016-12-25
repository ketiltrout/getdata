/* Copyright (C) 2013, 2014, 2015, 2016 D. V. Wiebe
 *
 ***************************************************************************
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

#ifdef HAVE_CONFIG_H
#include "gd_config.h"
#endif

#ifdef GD_EXTERNAL
# define GD_C89_API
# define dtracevoid()
# define dtrace(...)
# define dprintf(...)
# define dreturnvoid()
# define dreturn(...)
# define dwatch(...)
# define GD_SIZE_T_MAX ((size_t)-1)
# define GD_INT_TYPE ((gd_type_t)((sizeof(int)) | GD_SIGNED))
# define GD_UINT_TYPE ((gd_type_t)(sizeof(unsigned int)))
# define EN(t,v) u.t.v
# define GD_DCOMPLEXP_t double *
# define GD_DCOMPLEXV(v) double v[][2]
# define gd_cs2cs_(a,b) do { (a)[0] = (b)[0]; (a)[1] = (b)[1]; } while(0)
# define gd_ca2cs_(a,b,i) gd_cs2cs_((a),(b) + 2 * i)
#else
#include "internal.h"
#endif

#define GDMXLIB
#include "gd_matlab.h"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <string.h>

void gdmx_initialise(void)
{
  static int initialised = 0;

  if (!initialised) {
    gd_alloc_funcs(mxMalloc, mxFree);
    initialised = 1;
  }
}

#define GD_LIBCOMP "GetData:Lib:"
static const char *gdmx_msgid[GD_N_ERROR_CODES] =
{
  NULL, /* GD_E_OK */
  GD_LIBCOMP "Format",
  GD_LIBCOMP "Creat",
  GD_LIBCOMP "BadCode",
  GD_LIBCOMP "BadType",
  GD_LIBCOMP "IO",
  GD_LIBCOMP "InternalError",
  GD_LIBCOMP "Alloc",
  GD_LIBCOMP "Range",
  GD_LIBCOMP "LUT",
  GD_LIBCOMP "RecurseLevel",
  GD_LIBCOMP "BadDirfile",
  GD_LIBCOMP "BadFieldType",
  GD_LIBCOMP "AccMode",
  GD_LIBCOMP "Unsupported",
  GD_LIBCOMP "UnknownEncoding",
  GD_LIBCOMP "BadEntry",
  GD_LIBCOMP "Duplicate",
  GD_LIBCOMP "Dimension",
  GD_LIBCOMP "BadIndex",
  GD_LIBCOMP "BadScalar",
  GD_LIBCOMP "BadReference",
  GD_LIBCOMP "Protected",
  GD_LIBCOMP "Delete",
  GD_LIBCOMP "Argument",
  GD_LIBCOMP "Callback",
  GD_LIBCOMP "Exists",
  GD_LIBCOMP "UncleanDB",
  GD_LIBCOMP "Domain",
  GD_LIBCOMP "Bounds",
  GD_LIBCOMP "LineTooLong"
};

/* for error reporting */
struct gdmx_context_t {
  const char *name;
  const char *field;
  int num;
};

static char gdmx_context_buf[1024];
const char *gdmx_context(const struct gdmx_context_t *x, int uc)
{
  dtrace("%p, %i", x, uc);

  if (x == NULL)
    strcpy(gdmx_context_buf, "(unknown item)");
  else {
    if (x->name == NULL) {
      if (x->field == NULL)
        sprintf(gdmx_context_buf, "parameter %i", x->num);
      else
        sprintf(gdmx_context_buf, "field '%s' of struct parameter %i",
            x->field, x->num);
    } else {
      if (x->field == NULL)
        strcpy(gdmx_context_buf, x->name);
      else
        sprintf(gdmx_context_buf, "field '%s' of %s", x->field, x->name);
    }

    gdmx_context_buf[1023] = 0;
    if (uc)
      gdmx_context_buf[0] += 'A' - 'a';
  }

  dreturn("\"%s\"", gdmx_context_buf);
  return gdmx_context_buf;
}

/* doesn't return on error */
void gdmx_err(DIRFILE *D, int discard)
{
  char *gdmx_estring;
  int gdmx_errno;
  dtrace("%p, %i", D, discard);

  gdmx_errno = gd_error(D);

  if (gdmx_errno == GD_E_OK) {
    dreturnvoid();
    return;
  }

  gdmx_estring = gd_error_string(D, NULL, 0);

  if (discard)
    gd_discard(D);

#ifdef GETDATA_DEBUG
  gd_colclear();
#endif

  mexErrMsgIdAndTxt(gdmx_msgid[-gdmx_errno], gdmx_estring);
  mxFree(gdmx_estring);
}

static gd_type_t gdmx_type(const mxArray *a, const struct gdmx_context_t *ctx,
    int cflag)
{
  gd_type_t t = GD_UNKNOWN;
  mxClassID id;

  dtrace("%p, %p, %i", a, ctx, cflag);

  id = mxGetClassID(a);

  switch (id) {
    case mxDOUBLE_CLASS:
      t = GD_FLOAT64;
      break;
    case mxSINGLE_CLASS:
      t = GD_FLOAT32;
      break;
    case mxINT8_CLASS:
      t = GD_INT8;
      break;
    case mxUINT8_CLASS:
      t = GD_UINT8;
      break;
    case mxINT16_CLASS:
      t = GD_INT16;
      break;
    case mxUINT16_CLASS:
      t = GD_UINT16;
      break;
    case mxINT32_CLASS:
      t = GD_INT32;
      break;
    case mxUINT32_CLASS:
      t = GD_UINT32;
      break;
    case mxINT64_CLASS:
      t = GD_INT64;
      break;
    case mxUINT64_CLASS:
      t = GD_UINT64;
      break;
    default:
      if (ctx)
        mexErrMsgIdAndTxt("GetData:GDMX:NotNumber", "%s must be numerical.",
            gdmx_context(ctx, 1));
  }

  /* handle complex data */
  if (cflag) {
    if (t == GD_FLOAT64)
      t = GD_COMPLEX128;
    else if (t == GD_FLOAT32)
      t = GD_COMPLEX64;
    else if (ctx)
      mexErrMsgIdAndTxt("GetData:GDMX:ComplexInt",
          "GetData cannot handle integer complex data in %s.",
          gdmx_context(ctx, 0));
    else
      t = GD_UNKNOWN;
  }

  dreturn("0x%X", (unsigned)t);
  return t;
}

mxClassID gdmx_classid(gd_type_t t)
{
  mxClassID id = mxUNKNOWN_CLASS;

  dtrace("0x%X", t);

  switch (t) {
    case GD_UINT8:
      id = mxUINT8_CLASS;
      break;
    case GD_INT8:
      id = mxINT8_CLASS;
      break;
    case GD_UINT16:
      id = mxUINT16_CLASS;
      break;
    case GD_INT16:
      id = mxINT16_CLASS;
      break;
    case GD_UINT32:
      id = mxUINT32_CLASS;
      break;
    case GD_INT32:
      id = mxINT32_CLASS;
      break;
    case GD_UINT64:
      id = mxUINT64_CLASS;
      break;
    case GD_INT64:
      id = mxINT64_CLASS;
      break;
    case GD_FLOAT32:
    case GD_COMPLEX64:
      id = mxSINGLE_CLASS;
      break;
    case GD_FLOAT64:
    case GD_COMPLEX128:
      id = mxDOUBLE_CLASS;
      break;
    default:
      mexErrMsgIdAndTxt("GetData:GDMX:BadType",
          "Bad GetData data type: 0x%X", t);
  }

  dreturn("%i", (int)id);
  return id;
}

static size_t gdmx_get_length(const mxArray *a)
{
  const mwSize *dims;
  int i, ndims;
  size_t nelem = 1;

  dtrace("%p", a);

  ndims = mxGetNumberOfDimensions(a);

  dims = mxGetDimensions(a);
  for (i = 0; i < ndims; ++i)
    nelem *= dims[i];

  dreturn("%zu", nelem);
  return nelem;
}

/* returns zero if it's a scalar */
static int gdmx_check_scalar(const mxArray *a, const struct gdmx_context_t *ctx)
{
  size_t nelem;

  dtrace("%p, %p", a, ctx);

  nelem = gdmx_get_length(a);

  if (ctx && nelem > 1)
    mexErrMsgIdAndTxt("GetData:GDMX:NotScalar", "%s must be scalar.",
        gdmx_context(ctx, 1));

  dreturn("%i", (nelem > 1) ? 1 : 0);
  return (nelem > 1) ? 1 : 0;
}

/* convert to a desired scalar type */
#define CONVERT_TO_CTYPE(f,t) \
  switch (t) { \
    case GD_UINT8:       *((uint8_t*)v) = ((f*)datum)[i]; break; \
    case GD_INT8:         *((int8_t*)v) = ((f*)datum)[i]; break; \
    case GD_UINT16:     *((uint16_t*)v) = ((f*)datum)[i]; break; \
    case GD_INT16:       *((int16_t*)v) = ((f*)datum)[i]; break; \
    case GD_UINT32:     *((uint32_t*)v) = ((f*)datum)[i]; break; \
    case GD_INT32:       *((int32_t*)v) = ((f*)datum)[i]; break; \
    case GD_UINT64:     *((uint64_t*)v) = ((f*)datum)[i]; break; \
    case GD_INT64:       *((int64_t*)v) = ((f*)datum)[i]; break; \
    case GD_FLOAT32:       *((float*)v) = ((f*)datum)[i]; break; \
    case GD_FLOAT64:      *((double*)v) = ((f*)datum)[i]; break; \
    case GD_COMPLEX64:   ((float*)v)[0] = ((f*)datum)[i]; \
                         ((float*)v)[1] = c ? ((f*)idatum)[i] : 0; break; \
    case GD_COMPLEX128: ((double*)v)[0] = ((f*)datum)[i]; \
                        ((double*)v)[1] = c ? ((f*)idatum)[i] : 0; break; \
    default: GDMX_INTERNAL_ERROR; \
  }

static mxArray *gdmx_get_field(const mxArray *a,
    const struct gdmx_context_t *ctx, const char *field)
{
  mxArray *v;

  dtrace("%p, %p, \"%s\"", a, ctx, field);

  v = mxGetField(a, 0, field);
  if (ctx && v == NULL)
    mexErrMsgIdAndTxt("GetData:GDMX:BadStruct",
        "Missing required field '%s' in %s", field, gdmx_context(ctx, 0));

  dreturn("%p", v);
  return v;
}

static int gdmx_convert_scalar(const mxArray *a,
    const struct gdmx_context_t *ctx, gd_type_t t, int i, void *v)
{
  mxClassID id;
  void *datum, *idatum = NULL;
  int c;

  dtrace("%p, %p, 0x%X, %i, %p", a, ctx, t, v);

  /* check for scalarity */
  if (i == -1) {
    gdmx_check_scalar(a, ctx);
    i = 0;
  }

  /* matlab type */
  c = mxIsComplex(a);
  id = mxGetClassID(a);

  /* data pointer */
  datum = mxGetData(a);
  if (c)
    idatum = mxGetImagData(a);

  switch (id) {
    case mxDOUBLE_CLASS:
      CONVERT_TO_CTYPE(double,t);
      break;
    case mxSINGLE_CLASS:
      CONVERT_TO_CTYPE(float,t);
      break;
    case mxINT8_CLASS:
      CONVERT_TO_CTYPE(int8_t,t);
      break;
    case mxUINT8_CLASS:
      CONVERT_TO_CTYPE(uint8_t,t);
      break;
    case mxINT16_CLASS:
      CONVERT_TO_CTYPE(int16_t,t);
      break;
    case mxUINT16_CLASS:
      CONVERT_TO_CTYPE(uint16_t,t);
      break;
    case mxINT32_CLASS:
      CONVERT_TO_CTYPE(int32_t,t);
      break;
    case mxUINT32_CLASS:
      CONVERT_TO_CTYPE(uint32_t,t);
      break;
    case mxINT64_CLASS:
      CONVERT_TO_CTYPE(int64_t,t);
      break;
    case mxUINT64_CLASS:
      CONVERT_TO_CTYPE(uint64_t,t);
      break;
    default:
      if (ctx)
        mexErrMsgIdAndTxt("GetData:GDMX:NotNumber",
            "%s must be numerical.", gdmx_context(ctx, 1));
      dreturn("%i", 1);
      return 1;
  }

  dreturn("%i", 0);
  return 0;
}

/* this is only really appropriate for small arrays */
static void *gdmx_convert_array(const mxArray *a,
    const struct gdmx_context_t *ctx, gd_type_t t, size_t *n)
{
  char *ptr;
  size_t i, nelem;

  dtrace("%p, %p, 0x%X, %p(%zu)", a, ctx, t, n, *n);

  nelem = gdmx_get_length(a);
  if (nelem > *n)
    nelem = *n;
  else if (nelem < *n)
    *n = nelem;

  if (nelem == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  ptr = (char*)mxMalloc(GD_SIZE(t) * nelem);
  if (!ptr)
    mexErrMsgIdAndTxt("GetData:GDMX:Alloc", "Out of memory.");

  for (i = 0; i < nelem; ++i)
    gdmx_convert_scalar(a, ctx, t, i, ptr + GD_SIZE(t) * i);

  dreturn("%p", ptr);
  return ptr;
}

static void gdmx_convert_struct_scalar(const mxArray *a,
    const struct gdmx_context_t *ctx_in, const char *field, gd_type_t t,
    void *v)
{
  mxArray *e;
  struct gdmx_context_t ctx = { ctx_in->name, field, ctx_in->num };

  dtrace("%p, %p, \"%s\", 0x%X, %p", a, ctx_in, field, t, v);

  e = gdmx_get_field(a, ctx_in, field);

  gdmx_convert_scalar(e, &ctx, t, -1, v);

  dreturnvoid();
}

static int gdmx_is_zero(const mxArray *a)
{
  double v;

  dtrace("%p", a);

  if (gdmx_convert_scalar(a, NULL, GD_FLOAT64, -1, &v)) {
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", v ? 0 : 1);
  return v ? 0 : 1;
}


/* strings created thus need to be mxFree'd by the caller */
static char *gdmx_to_string_(const mxArray *a, const struct gdmx_context_t *ctx,
    int null_ok)
{
  char *s = NULL;

  dtrace("%p, %p, %i", a, ctx, null_ok);

  s = mxArrayToString(a);

  if (s == NULL) {
    if (null_ok && gdmx_is_zero(a)) {
      dreturn("%p", NULL);
      return NULL;
    }
    if (ctx)
      mexErrMsgIdAndTxt("GetData:GDMX:StringConversion",
          "Unable to interpret %s as string.", gdmx_context(ctx, 0));
  }

  dreturn("\"%s\"", s);

  return s;
}

char *gdmx_to_string(const mxArray **rhs, int n, int null_ok)
{
  struct gdmx_context_t ctx = { NULL, NULL, n };
  return gdmx_to_string_(rhs[n], &ctx, null_ok);
}

static char *gdmx_convert_struct_string(const mxArray *a,
    const struct gdmx_context_t *ctx_in, const char *field)
{
  char *s;
  mxArray *e;
  struct gdmx_context_t ctx = { ctx_in->name, field, ctx_in->num };

  dtrace("%p, %p, \"%s\"", a, ctx_in, field);

  e = gdmx_get_field(a, ctx_in, field);

  s = gdmx_to_string_(e, &ctx, 1);

  dreturn("\"%s\"", s ? s : "(nil)");
  return s;
}

static double gdmx_to_double_(const mxArray *a,
    const struct gdmx_context_t *ctx)
{
  double v;

  dtrace("%p, %p", a, ctx);

  gdmx_convert_scalar(a, ctx, GD_FLOAT64, -1, &v);

  dreturn("%g", v);

  return v;
}

double gdmx_to_double(const mxArray **rhs, int n)
{
  struct gdmx_context_t ctx = { NULL, NULL, n };
  return gdmx_to_double_(rhs[n], &ctx);
}

static int gdmx_to_int_(const mxArray *a, const struct gdmx_context_t *ctx)
{
  int v;

  dtrace("%p, %p", a, ctx);

  gdmx_convert_scalar(a, ctx, GD_INT_TYPE, -1, &v);

  dreturn("%i", v);

  return v;
}

int gdmx_to_int(const mxArray **rhs, int n)
{
  struct gdmx_context_t ctx = { NULL, NULL, n };
  return gdmx_to_int_(rhs[n], &ctx);
}


static int64_t gdmx_to_int64_(const mxArray *a,
    const struct gdmx_context_t *ctx)
{
  int64_t v;

  dtrace("%p, %p", a, ctx);

  gdmx_convert_scalar(a, ctx, GD_INT64, -1, &v);

  dreturn("%" PRId64, v);

  return v;
}

int64_t gdmx_to_int64(const mxArray **rhs, int n)
{
  struct gdmx_context_t ctx = { NULL, NULL, n };
  return gdmx_to_int64_(rhs[n], &ctx);
}

static uint64_t gdmx_to_uint64_(const mxArray *a,
    const struct gdmx_context_t *ctx)
{
  uint64_t v;

  dtrace("%p, %p", a, ctx);

  gdmx_convert_scalar(a, ctx, GD_UINT64, -1, &v);

  dreturn("%" PRIu64, v);

  return v;
}

uint64_t gdmx_to_uint64(const mxArray **rhs, int n)
{
  struct gdmx_context_t ctx = { NULL, NULL, n };
  return gdmx_to_uint64_(rhs[n], &ctx);
}

static unsigned long gdmx_to_ulong_(const mxArray *a,
    const struct gdmx_context_t *ctx)
{
  uint64_t v;

  dtrace("%p, %p", a, ctx);

  gdmx_convert_scalar(a, ctx, GD_UINT64, -1, &v);

  dreturn("%lu", (unsigned long)v);

  return (unsigned long)v;
}

unsigned long gdmx_to_ulong(const mxArray **rhs, int n)
{
  struct gdmx_context_t ctx = { NULL, NULL, n };
  return gdmx_to_ulong_(rhs[n], &ctx);
}

static mxArray *gdmx_from_cdouble(double* x)
{
  mxArray *lhs;

  dtrace("{%g, %g}", x[0], x[1]);

  lhs = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxCOMPLEX);
  *((double*)mxGetData(lhs)) = x[0];
  *((double*)mxGetImagData(lhs)) = x[1];

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_bool(int x)
{
  mxArray *lhs;

  dtrace("%i", x);

  lhs = mxCreateLogicalScalar(x ? true : false);

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_long(long x)
{
  mxArray *lhs;

  dtrace("%li", x);

  lhs = mxCreateNumericMatrix(1, 1, mxINT32_CLASS, mxREAL);
  *((int32_t*)mxGetData(lhs)) = x;

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_int64(int64_t x)
{
  mxArray *lhs;

  dtrace("%" PRId64, x);

  lhs = mxCreateNumericMatrix(1, 1, mxINT64_CLASS, mxREAL);
  *((int64_t*)mxGetData(lhs)) = x;

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_ulong(unsigned long x)
{
  mxArray *lhs;

  dtrace("%lu", x);

  lhs = mxCreateNumericMatrix(1, 1, mxUINT32_CLASS, mxREAL);
  *((uint32_t*)mxGetData(lhs)) = x;

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_uint64(uint64_t x)
{
  mxArray *lhs;

  dtrace("%" PRIu64, x);

  lhs = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
  *((uint64_t*)mxGetData(lhs)) = x;

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_triplet(gd_triplet_t datum, gd_windop_t windop)
{
  mxArray *a;

  dtrace("{%g, %" PRIu64 ", %lli}, %i", datum.r, datum.u, datum.i, windop);

  switch (windop) {
    case GD_WINDOP_EQ:
    case GD_WINDOP_NE:
      a = gdmx_from_int64(datum.i);
      break;
    case GD_WINDOP_SET:
    case GD_WINDOP_CLR:
      a = gdmx_from_uint64(datum.u);
      break;
    default:
      a = gdmx_from_double(datum.r);
      break;
  }

  dreturn("%p", a);
  return a;
}

mxArray *gdmx_from_data(const void *data, gd_type_t type, size_t n)
{
  mxArray *a;

  dtrace("%p, 0x%X, %zu", data, type, n);

  a = mxCreateNumericMatrix(1, n, gdmx_classid(type),
      (type & GD_COMPLEX) ? mxCOMPLEX : mxREAL);

  if (type == GD_COMPLEX64) {
    size_t i;
    float *p, *pr, *pi;

    p = (float*)data;
    pr = (float*)mxGetData(a);
    pi = (float*)mxGetImagData(a);

    for (i = 0; i < n; ++i) {
      pr[i] = *(p++);
      pi[i] = *(p++);
    }
  } else if (type == GD_COMPLEX128) {
    size_t i;
    double *p, *pr, *pi;

    p = (double*)data;
    pr = (double*)mxGetData(a);
    pi = (double*)mxGetImagData(a);

    for (i = 0; i < n; ++i) {
      pr[i] = *(p++);
      pi[i] = *(p++);
    }
  } else
    memcpy(mxGetData(a), data, GD_SIZE(type) * n);

  dreturn("%p", a);
  return a;
}

mxArray *gdmx_from_nstring_list(const char **l, size_t n)
{
  mxArray *lhs;
  size_t i;

  dtrace("%p, %zu", l, n);

  /* create and populate cell array */
  lhs = mxCreateCellMatrix(1, n);
  for (i = 0; i < n; ++i)
    if (l[i] == NULL)
      mxSetCell(lhs, i, mxCreateDoubleScalar(0));
    else
      mxSetCell(lhs, i, mxCreateString(l[i]));

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_string_list(const char **l)
{
  mxArray *lhs;
  mxArray **s = NULL, **ptr;
  int i;

  dtrace("%p", l);

  /* count and stringify */
  for (i = 0; l[i]; ++i) {
    ptr = (mxArray **)mxRealloc(s, sizeof(mxArray*) * (i + 1));
    if (ptr == NULL) {
      mxFree(s);
      mexErrMsgIdAndTxt("GetData:GDMX:Alloc", "Out of memory.");
    }
    s = ptr;
    s[i] = mxCreateString(l[i]);
  }

  /* create and populate cell array */
  lhs = mxCreateCellMatrix(1, i);
  for (--i; i >= 0; --i)
    mxSetCell(lhs, i, s[i]);

  mxFree(s);

  dreturn("%p", lhs);
  return lhs;
}

#define GDMX_COMMON_FIELDS "field", "field_type", "fragment_index"
#define GDMX_NCOMMON 3
#define GDMX_SCALAR_FIELDS "scalar", "scalar_ind"
#define GDMX_NSCALAR 2
mxArray *gdmx_from_entry(const gd_entry_t *E)
{
  mxArray *lhs;
  int nfields = 0;
  int nscalar = 0;
  const char **field_names = NULL;

  /* field lists */
  const int bit_nfields = GDMX_NSCALAR + 3;
  const char *bit_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "bitnum",
    "numbits", GDMX_SCALAR_FIELDS};
  const int carray_nfields = 2;
  const char *carray_fields[] = {GDMX_COMMON_FIELDS, "const_type", "array_len"};
  const int const_nfields = 1;
  const char *const_fields[] = {GDMX_COMMON_FIELDS, "const_type"};
  const int lincom_nfields = GDMX_NSCALAR + 3;
  const char *lincom_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "m", "b",
    GDMX_SCALAR_FIELDS};
  const int linterp_nfields = 2;
  const char *linterp_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "table"};
  const int mplex_nfields = GDMX_NSCALAR + 3;
  const char *mplex_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "count_val",
    "period", GDMX_SCALAR_FIELDS};
  const int multiply_nfields = 1;
  const char *multiply_fields[] = {GDMX_COMMON_FIELDS, "in_fields"};
  const int phase_nfields = GDMX_NSCALAR + 2;
  const char *phase_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "shift",
    GDMX_SCALAR_FIELDS};
  const int polynom_nfields = GDMX_NSCALAR + 2;
  const char *polynom_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "a",
    GDMX_SCALAR_FIELDS};
  const int raw_nfields = GDMX_NSCALAR + 2;
  const char *raw_fields[] = {GDMX_COMMON_FIELDS, "spf", "data_type",
    GDMX_SCALAR_FIELDS};
  const int recip_nfields = GDMX_NSCALAR + 2;
  const char *recip_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "dividend",
    GDMX_SCALAR_FIELDS};
  const int sarray_nfields = 1;
  const char *sarray_fields[] = {GDMX_COMMON_FIELDS, "array_len"};
  const int window_nfields = GDMX_NSCALAR + 3;
  const char *window_fields[] = {GDMX_COMMON_FIELDS, "in_fields", "windop",
    "threshold", GDMX_SCALAR_FIELDS};
  const char *common_fields[] = {GDMX_COMMON_FIELDS};

  dtrace("%p", E);

  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      nfields = GDMX_NCOMMON + bit_nfields;
      field_names = bit_fields;
      break;
    case GD_SARRAY_ENTRY:
      nfields = GDMX_NCOMMON + sarray_nfields;
      field_names = sarray_fields;
      break;
    case GD_CARRAY_ENTRY:
      nfields = GDMX_NCOMMON + carray_nfields;
      field_names = carray_fields;
      break;
    case GD_CONST_ENTRY:
      nfields = GDMX_NCOMMON + const_nfields;
      field_names = const_fields;
      break;
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      nfields = GDMX_NCOMMON;
      field_names = common_fields;
      break;
    case GD_LINCOM_ENTRY:
      nfields = GDMX_NCOMMON + lincom_nfields;
      field_names = lincom_fields;
      break;
    case GD_LINTERP_ENTRY:
      nfields = GDMX_NCOMMON + linterp_nfields;
      field_names = linterp_fields;
      break;
    case GD_MPLEX_ENTRY:
      nfields = GDMX_NCOMMON + mplex_nfields;
      field_names = mplex_fields;
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      nfields = GDMX_NCOMMON + multiply_nfields;
      field_names = multiply_fields;
      break;
    case GD_PHASE_ENTRY:
      nfields = GDMX_NCOMMON + phase_nfields;
      field_names = phase_fields;
      break;
    case GD_POLYNOM_ENTRY:
      nfields = GDMX_NCOMMON + polynom_nfields;
      field_names = polynom_fields;
      break;
    case GD_RAW_ENTRY:
      nfields = GDMX_NCOMMON + raw_nfields;
      field_names = raw_fields;
      break;
    case GD_RECIP_ENTRY:
      nfields = GDMX_NCOMMON + recip_nfields;
      field_names = recip_fields;
      break;
    case GD_WINDOW_ENTRY:
      nfields = GDMX_NCOMMON + window_nfields;
      field_names = window_fields;
      break;
    default:
      GDMX_INTERNAL_ERROR;
  }

  lhs = mxCreateStructMatrix(1, 1, nfields, field_names);

  /* populate */
  mxSetField(lhs, 0, "field", mxCreateString(E->field));
  mxSetField(lhs, 0, "field_type", gdmx_from_entype(E->field_type));
  mxSetField(lhs, 0, "fragment_index", gdmx_from_int(E->fragment_index));
  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      mxSetField(lhs, 0, "in_fields", mxCreateString(E->in_fields[0]));
      mxSetField(lhs, 0, "bitnum", gdmx_from_int(E->EN(bit,bitnum)));
      mxSetField(lhs, 0, "numbits", gdmx_from_int(E->EN(bit,numbits)));
      nscalar = 2;
      break;
    case GD_CARRAY_ENTRY:
      mxSetField(lhs, 0, "array_len",
          gdmx_from_size_t(E->EN(scalar,array_len)));
      /* fallthrough */
    case GD_CONST_ENTRY:
      mxSetField(lhs, 0, "const_type",
          gdmx_from_gd_type(E->EN(scalar,const_type)));
      break;
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      break;
    case GD_LINCOM_ENTRY:
      mxSetField(lhs, 0, "in_fields",
          gdmx_from_nstring_list((const char**)E->in_fields,
            E->EN(lincom,n_fields)));
      if (E->flags & GD_EN_COMPSCAL) {
        mxSetField(lhs, 0, "m", gdmx_from_data(E->EN(lincom,cm), GD_COMPLEX128,
              E->EN(lincom,n_fields)));
        mxSetField(lhs, 0, "b", gdmx_from_data(E->EN(lincom,cb), GD_COMPLEX128,
              E->EN(lincom,n_fields)));
      } else {
        mxSetField(lhs, 0, "m", gdmx_from_data(E->EN(lincom,m), GD_FLOAT64,
              E->EN(lincom,n_fields)));
        mxSetField(lhs, 0, "b", gdmx_from_data(E->EN(lincom,b), GD_FLOAT64,
              E->EN(lincom,n_fields)));
      }
      nscalar = GD_MAX_LINCOM * 2;
      break;
    case GD_LINTERP_ENTRY:
      mxSetField(lhs, 0, "in_fields", mxCreateString(E->in_fields[0]));
      mxSetField(lhs, 0, "table", mxCreateString(E->EN(linterp,table)));
      break;
    case GD_MPLEX_ENTRY:
      mxSetField(lhs, 0, "in_fields",
          gdmx_from_nstring_list((const char**)E->in_fields, 2));
      mxSetField(lhs, 0, "count_val", gdmx_from_int(E->EN(mplex,count_val)));
      mxSetField(lhs, 0, "period", gdmx_from_int(E->EN(mplex,period)));
      nscalar = 2;
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      mxSetField(lhs, 0, "in_fields",
          gdmx_from_nstring_list((const char**)E->in_fields, 2));
      break;
    case GD_PHASE_ENTRY:
      mxSetField(lhs, 0, "in_fields", mxCreateString(E->in_fields[0]));
      mxSetField(lhs, 0, "shift", gdmx_from_int64(E->EN(phase,shift)));
      nscalar = 1;
      break;
    case GD_POLYNOM_ENTRY:
      mxSetField(lhs, 0, "in_fields", mxCreateString(E->in_fields[0]));
      if (E->flags & GD_EN_COMPSCAL) {
        mxSetField(lhs, 0, "a", gdmx_from_data(E->EN(polynom,ca), GD_COMPLEX128,
              E->EN(polynom,poly_ord) + 1));
      } else {
        mxSetField(lhs, 0, "a", gdmx_from_data(E->EN(polynom,a), GD_FLOAT64,
              E->EN(polynom,poly_ord) + 1));
      }
      nscalar = E->EN(polynom,poly_ord) + 1;
      break;
    case GD_RAW_ENTRY:
      mxSetField(lhs, 0, "data_type", gdmx_from_gd_type(E->EN(raw,data_type)));
      mxSetField(lhs, 0, "spf", gdmx_from_uint(E->EN(raw,spf)));
      nscalar = 1;
      break;
    case GD_RECIP_ENTRY:
      mxSetField(lhs, 0, "in_fields", mxCreateString(E->in_fields[0]));
      if (E->flags & GD_EN_COMPSCAL)
        mxSetField(lhs, 0, "dividend",
            gdmx_from_cdouble((double*)&E->EN(recip,cdividend)));
      else
        mxSetField(lhs, 0, "dividend", gdmx_from_double(E->EN(recip,dividend)));
      nscalar = 1;
      break;
    case GD_WINDOW_ENTRY:
      mxSetField(lhs, 0, "in_fields",
          gdmx_from_nstring_list((const char**)E->in_fields, 2));
      mxSetField(lhs, 0, "windop", gdmx_from_windop(E->EN(window,windop)));
      mxSetField(lhs, 0, "threshold", gdmx_from_triplet(E->EN(window,threshold),
            E->EN(window,windop)));
      nscalar = 1;
      break;
    case GD_SARRAY_ENTRY:
      mxSetField(lhs, 0, "array_len",
          gdmx_from_size_t(E->EN(scalar,array_len)));
      break;
    default:
      GDMX_INTERNAL_ERROR;
  }

  /* handle scalars */
  if (nscalar > 0) {
    mxSetField(lhs, 0, "scalar",
        gdmx_from_nstring_list((const char **)E->scalar, nscalar));
    mxSetField(lhs, 0, "scalar_ind", gdmx_from_data(E->scalar_ind, GD_INT_TYPE,
          nscalar));
  }

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_carrays(const gd_carray_t *c, gd_type_t type)
{
  size_t i, n, comp;
  mxArray *lhs;
  mxClassID id;

  dtrace("%p, 0x%X", c, type);

  id = gdmx_classid(type);
  comp = type & GD_COMPLEX;

  /* count */
  for (n = 0; c[n].n; ++n)
    ;

  lhs = mxCreateCellMatrix(1, n);

  for (n = 0; c[n].n; ++n) {
    mxArray *a = mxCreateNumericMatrix(1, c[n].n, id,
        comp ? mxCOMPLEX : mxREAL);
    void *pr = mxGetData(a);

    if (type == GD_COMPLEX128) {
      double *pi = mxGetImagData(a);
      for (i = 0; i < c[n].n; ++i) {
        ((double*)pr)[i] = ((double*)c[n].d)[2 * i];
        pi[i] = ((double*)c[n].d)[2 * i + 1];
      }
    } else if (type == GD_COMPLEX64) {
      float *pi = mxGetImagData(a);
      for (i = 0; i < c[n].n; ++i) {
        ((float*)pr)[i] = ((float*)c[n].d)[2 * i];
        pi[i] = ((float*)c[n].d)[2 * i + 1];
      }
    } else
      memcpy(pr, c[n].d, GD_SIZE(type) * c[n].n);

    mxSetCell(lhs, n, a);
  }

  dreturn("%p", lhs);
  return lhs;
}

mxArray *gdmx_from_sarrays(const char ***l)
{
  mxArray *lhs;
  size_t i, n;

  dtrace("%p", l);

  /* count */
  for (n = 0; l[n]; ++n)
    ;

  /* create and populate cell array of cell arrays */
  lhs = mxCreateCellMatrix(1, n);
  for (i = 0; i < n; ++i)
    mxSetCell(lhs, i, gdmx_from_string_list(l[i]));

  dreturn("%p", lhs);
  return lhs;
}


#define GDMX_MXDIRFILE_FMT "MXDIRFILE:%p;"
mxArray *gdmx_from_dirfile(const DIRFILE *D)
{
  dtrace("%p", D);

  mxArray *a;
  char s[100];

  sprintf(s, GDMX_MXDIRFILE_FMT, D);

  a = mxCreateString(s);

  dreturn("%p (\"%s\")", a, s);
  return a;
}

DIRFILE *gdmx_to_dirfile(const mxArray *mxa)
{
  DIRFILE *D = NULL;
  const char *dirfile;

  dtrace("%p", mxa);

  dirfile = mxArrayToString(mxa);

  if (dirfile)
    sscanf(dirfile, GDMX_MXDIRFILE_FMT, &D);
  else
    mexErrMsgIdAndTxt("GetData:GDMX:BadMXDirfile",
        "Indeciperhable MXDirfile.", dirfile);

  if (D == NULL)
    mexErrMsgIdAndTxt("GetData:GDMX:BadMXDirfile",
        "Indeciperhable MXDirfile: '%s'.", dirfile);

  dreturn("%p", D);

  return D;
}

void gdmx_free_entry(gd_entry_t *E)
{
  int i, ns = 0, ni = 0;

  dtrace("%p", E);

  if (E) {
    switch (E->field_type) {
      case GD_LINCOM_ENTRY:
        ni = E->EN(lincom,n_fields);
        ns = ni * 2;
        break;
      case GD_LINTERP_ENTRY:
        mxFree(E->EN(linterp,table));
        ni = 1;
        break;
      case GD_DIVIDE_ENTRY:
      case GD_MULTIPLY_ENTRY:
      case GD_INDIR_ENTRY:
      case GD_SINDIR_ENTRY:
        ni = 2;
        break;
      case GD_BIT_ENTRY:
      case GD_SBIT_ENTRY:
        ni = 1;
        ns = 2;
        break;
      case GD_PHASE_ENTRY:
      case GD_RECIP_ENTRY:
        ns = ni = 1;
        break;
      case GD_POLYNOM_ENTRY:
        ni = 1;
        ns = E->EN(polynom,poly_ord) + 1;
        break;
      case GD_RAW_ENTRY:
        ni = 1;
        break;
      case GD_WINDOW_ENTRY:
        ni = 2;
        ns = 1;
        break;
      case GD_MPLEX_ENTRY:
        ni = ns = 2;
        break;
      default:
        break;
    }

    for (i = 0; i < ni; ++i)
      if (E->in_fields[i])
        mxFree(E->in_fields[i]);

    for (i = 0; i < ns; ++i)
      if (E->scalar[i])
        mxFree(E->scalar[i]);
    mxFree(E);
  }

  dreturnvoid();
}

static int gdmx_convert_in_fields(const mxArray *str,
    const struct gdmx_context_t *ctx, gd_entry_t *E)
{
  mxArray *a;
  size_t i, nelem = 1;
  struct gdmx_context_t ectx = { ctx->name, "in_fields", ctx->num };

  dtrace("%p, %p, %p", str, ctx, E);

  a = gdmx_get_field(str, ctx, "in_fields");

  if (mxIsCell(a)) {
    nelem = gdmx_get_length(a);
    if (nelem > GD_MAX_LINCOM)
      nelem = GD_MAX_LINCOM;

    for (i = 0; i < nelem; ++i) {
      E->in_fields[i] = gdmx_to_string_(mxGetCell(a, i), &ectx, 1);
    }
  } else if (mxIsChar(a)) /* a single in_field */
    E->in_fields[0] = gdmx_to_string_(a, &ectx, 0);
  else if (gdmx_is_zero(a))
    E->in_fields[0] = NULL;
  else
    mexErrMsgIdAndTxt("GetData:GDMX:BadEntry",
        "Expected cell array or string %s.", gdmx_context(&ectx, 0));

  dreturn("%i", nelem);
  return nelem;
}

static int gdmx_convert_entry_array(const mxArray *str,
    const struct gdmx_context_t *ctx, const char *field, GD_DCOMPLEXV(d),
    size_t nelem)
{
  size_t i;
  mxArray *a;
  GD_DCOMPLEXP_t v;
  struct gdmx_context_t ectx = { ctx->name, field, ctx->num };

  dtrace("%p, %p, \"%s\", %p, %zu", str, ctx, field, d, nelem);

  a = gdmx_get_field(str, ctx, field);
  v = gdmx_convert_array(a, &ectx, GD_COMPLEX128, &nelem);

  for (i = 0; i < nelem; ++i)
    gd_ca2cs_(d[i],v,i);

  mxFree(v);

  dreturn("%i", nelem);
  return nelem;
}


gd_entry_t *gdmx_to_entry(const mxArray **rhs, int n, unsigned flags)
{
  int v;
  uint64_t s;
  size_t nscalar = 0;
  gd_entry_t *E;
  struct gdmx_context_t ctx = { NULL, NULL, n };

  dtrace("%p, %i, %x", rhs, n, flags);

  /* check for struct */
  if (!mxIsStruct(rhs[n]))
    mexErrMsgIdAndTxt("GetData:GDMX:BadEntry",
        "Expected entry struct for %s.", gdmx_context(&ctx, 0));

  E = mxMalloc(sizeof(gd_entry_t));
  memset(E, 0, sizeof(gd_entry_t));

  /* fill the entry */
  if (~flags & GDMX_ENO_FIELD)
    E->field = gdmx_convert_struct_string(rhs[n], &ctx, "field");

  gdmx_convert_struct_scalar(rhs[n], &ctx, "field_type", GD_INT_TYPE, &v);
  E->field_type = v;
  if (~flags & GDMX_ENO_FRAG)
    gdmx_convert_struct_scalar(rhs[n], &ctx, "fragment_index", GD_INT_TYPE,
        &E->fragment_index);

  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      gdmx_convert_struct_scalar(rhs[n], &ctx, "bitnum", GD_INT_TYPE,
          &E->EN(bit,bitnum));
      gdmx_convert_struct_scalar(rhs[n], &ctx, "numbits", GD_INT_TYPE,
          &E->EN(bit,numbits));
      break;
    case GD_SARRAY_ENTRY:
      gdmx_convert_struct_scalar(rhs[n], &ctx, "array_len", GD_UINT64, &s);
      E->EN(scalar,array_len) = (size_t)s;
      break;
    case GD_CARRAY_ENTRY:
      gdmx_convert_struct_scalar(rhs[n], &ctx, "array_len", GD_UINT64, &s);
      E->EN(scalar,array_len) = (size_t)s;
      /* fallthrough */
    case GD_CONST_ENTRY:
      gdmx_convert_struct_scalar(rhs[n], &ctx, "const_type", GD_INT_TYPE, &v);
      E->EN(scalar,const_type) = v;
      break;
    case GD_DIVIDE_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      break;
    case GD_LINCOM_ENTRY:
      E->EN(lincom,n_fields) = gdmx_convert_in_fields(rhs[n], &ctx, E);
      E->flags |= GD_EN_COMPSCAL;
      gdmx_convert_entry_array(rhs[n], &ctx, "m", E->EN(lincom,cm),
          E->EN(lincom,n_fields));
      gdmx_convert_entry_array(rhs[n], &ctx, "b", E->EN(lincom,cb),
          E->EN(lincom,n_fields));
      break;
    case GD_LINTERP_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      E->EN(linterp,table) = gdmx_convert_struct_string(rhs[n], &ctx, "table");
      break;
    case GD_MPLEX_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      gdmx_convert_struct_scalar(rhs[n], &ctx, "count_val", GD_INT_TYPE,
          &E->EN(mplex,count_val));
      gdmx_convert_struct_scalar(rhs[n], &ctx, "period", GD_INT_TYPE,
          &E->EN(mplex,period));
      break;
    case GD_PHASE_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      gdmx_convert_struct_scalar(rhs[n], &ctx, "shift", GD_INT64,
          &E->EN(phase,shift));
      break;
    case GD_POLYNOM_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      E->flags |= GD_EN_COMPSCAL;
      E->EN(polynom,poly_ord) = gdmx_convert_entry_array(rhs[n], &ctx, "a",
          E->EN(polynom,ca), GD_MAX_POLYORD + 1) - 1;
      break;
    case GD_RAW_ENTRY:
      gdmx_convert_struct_scalar(rhs[n], &ctx, "data_type", GD_INT_TYPE, &v);
      E->EN(raw,data_type) = v;
      gdmx_convert_struct_scalar(rhs[n], &ctx, "spf", GD_UINT_TYPE,
          &E->EN(raw,spf));
      nscalar = 1;
      break;
    case GD_RECIP_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      E->flags |= GD_EN_COMPSCAL;
      gdmx_convert_struct_scalar(rhs[n], &ctx, "dividend", GD_COMPLEX128,
          &E->EN(recip,cdividend));
      break;
    case GD_STRING_ENTRY:
      break;
    case GD_WINDOW_ENTRY:
      gdmx_convert_in_fields(rhs[n], &ctx, E);
      gdmx_convert_struct_scalar(rhs[n], &ctx, "windop", GD_INT_TYPE, &v);
      E->EN(window,windop) = v;
      switch (v) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          gdmx_convert_struct_scalar(rhs[n], &ctx, "threshold", GD_INT64,
              &E->EN(window,threshold).i);
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          gdmx_convert_struct_scalar(rhs[n], &ctx, "threshold", GD_UINT64,
              &E->EN(window,threshold).u);
          break;
        default:
          gdmx_convert_struct_scalar(rhs[n], &ctx, "threshold", GD_FLOAT64,
              &E->EN(window,threshold).r);
          break;
      }
      break;
    default:
      mexErrMsgIdAndTxt("GetData:GDMX:BadEntryType",
          "Invalid field_type %i for entry struct in parameter %i.",
          E->field_type, n);
  }

  if (nscalar > 0) {
    mxArray *scalar = gdmx_get_field(rhs[n], NULL, "scalar");

    if (scalar) {
      int *si = NULL;
      mxArray *scalar_ind = gdmx_get_field(rhs[n], NULL, "scalar_ind");

      if (scalar_ind) {
        struct gdmx_context_t si_ctx = { NULL, "scalar_ind", n };
        si = gdmx_convert_array(scalar_ind, &si_ctx, GD_INT_TYPE, &nscalar);
      }

      if (mxIsChar(scalar)) {
        /* a single scalar */
        E->scalar[0] = gdmx_to_string_(scalar, NULL, 0);
        if (si)
          E->scalar_ind[0] = si[0];
        else
          E->scalar_ind[0] = -1;
      } else if (!mxIsCell(scalar))
        mexErrMsgIdAndTxt("GetData:GDMX:BadEntryScalars",
            "Field 'scalar' must be a string or cell array in parameter %i", n);
      else {
        abort();
      }

      mxFree(si);
    }
  }

  dreturn("%p", E);
  return E;
}

/* convert nframes and nsamples from the rhs into nsamples */
size_t gdmx_to_nsamp(DIRFILE *D, const char *field_code, const mxArray **rhs,
    int nf, int ns)
{
  size_t nframes;
  size_t nsamp;

  dtrace("%p, %i, %i", rhs, nf, ns);

  nframes = gdmx_to_size_t(rhs, nf);
  nsamp = gdmx_to_size_t(rhs, ns);

  if (nframes > 0) {
    unsigned int spf = gd_spf(D, field_code);
    gdmx_err(D, 0);

    /* overflow */
    if ((GD_SIZE_T_MAX - nsamp) / spf <= nframes)
      mexErrMsgIdAndTxt("GetData:GDMX:Overflow", "Data range too large.");

    nsamp += spf * nframes;
  }

  dreturn("%zu", nsamp);
  return nsamp;
}

/* create a matlab vector; call gdmx_fix_vector after loading data */
mxArray *gdmx_vector(gd_type_t type, size_t nsamp, void **data)
{
  mxArray *lhs;
  mxComplexity cflag = (type & GD_COMPLEX) ? mxCOMPLEX : mxREAL;

  dtrace("0x%X, %zu, %p", type, nsamp, data);

  lhs = mxCreateNumericMatrix(1, (mwSize)nsamp, gdmx_classid(type), cflag);

  if (data) {
    /* in the complex place, we have to do the load out-of-place */
    if (type & GD_COMPLEX)
      *data = mxMalloc(GD_SIZE(type) * nsamp);
    else
      *data = mxGetData(lhs);
  }

  dreturn("%p (%p)", lhs, data ? *data : NULL);
  return lhs;
}

/* store complex data, if necessary */
void gdmx_fix_vector(mxArray *lhs, gd_type_t type, size_t nsamp, void *data)
{
  dtrace("%p, 0x%x, %zu, %p", lhs, type, nsamp, data);

  if (type & GD_COMPLEX) {
    size_t i;

    if (type == GD_COMPLEX64) {
      const float *ptr = (float *)data;
      float *pr = (float*)mxGetData(lhs);
      float *pi = (float*)mxGetImagData(lhs);

      for (i = 0; i < nsamp; ++i) {
        pr[i] = *(ptr++);
        pi[i] = *(ptr++);
      }
    } else {
      const double *ptr = (double *)data;
      double *pr = (double*)mxGetData(lhs);
      double *pi = (double*)mxGetImagData(lhs);

      for (i = 0; i < nsamp; ++i) {
        pr[i] = *(ptr++);
        pi[i] = *(ptr++);
      }
    }
    mxFree(data);
  }

  dreturnvoid();
}

void gdmx_to_data(void **data, gd_type_t *type, size_t *nsamp,
    const mxArray *rhs, int n)
{
  int cflag = 0;
  size_t i, ndims, ns = 1;
  gd_type_t t;
  const mwSize *dims;
  struct gdmx_context_t ctx = { NULL, NULL, n };

  dtrace("%p, %p, %p, %p, %i", data, type, nsamp, rhs, n);

  /* check for complex data */
  if (mxIsComplex(rhs))
    cflag = 1;

  /* datatype */
  *type = t = gdmx_type(rhs, &ctx, cflag);

  /* length */
  ndims = mxGetNumberOfDimensions(rhs);

  dims = mxGetDimensions(rhs);
  for (i = 0; i < ndims; ++i)
    ns *= dims[i];

  *nsamp = ns;

  /* complex data needs a temporary buffer */
  if (t == GD_COMPLEX64) {
    float *d, *pr, *pi;
    *data = mxMalloc(sizeof(float) * 2 * ns);
    d = (float*)*data;

    pr = (float*)mxGetData(rhs);
    pi = (float*)mxGetImagData(rhs);

    for (i = 0; i < ns; ++i) {
      d[i * 2] = pr[i];
      d[i * 2 + 1] = pi[i];
    }
  } else if (t == GD_COMPLEX128) {
    double *d, *pr, *pi;
    *data = mxMalloc(sizeof(double) * 2 * ns);
    d = (double*)*data;

    pr = (double*)mxGetData(rhs);
    pi = (double*)mxGetImagData(rhs);

    for (i = 0; i < ns; ++i) {
      d[i * 2] = pr[i];
      d[i * 2 + 1] = pi[i];
    }
  } else
    *data = mxGetData(rhs);

  dreturn("%p; 0x%X, %zu", *data, *type, *nsamp);
}

void gdmx_free_data(void *data, gd_type_t type)
{
  dtrace("%p, 0x%X", data, type);

  if (type & GD_COMPLEX)
    mxFree(data);

  dreturnvoid();
}

void gdmx_to_sdata(const char ***data, size_t *nsamp, const mxArray *rhs, int n)
{
  size_t i, ndims, ns;
  const mwSize *dims;
  struct gdmx_context_t ctx = { NULL, NULL, n };

  dtrace("%p, %p, %p, %i", data, nsamp, rhs, n);

  if (!mxIsCell(rhs))
    mexErrMsgIdAndTxt("GetData:GDMX:BadData",
        "Expected cell array of strings in %s, %i.",
        gdmx_context(&ctx, 0), (int)mxGetClassID(rhs));
  
  *nsamp = ns = gdmx_get_length(rhs);

  /* create the string list */
  *data = mxMalloc(sizeof(**data) * ns);

  for (i = 0; i < ns; ++i)
    (*data)[i] = gdmx_to_string_(mxGetCell(rhs, i), &ctx, 1);

  dreturn("%p; %zu", *data, *nsamp);
}

void gdmx_free_sdata(const char **data, size_t n)
{
  size_t i;

  dtrace("%p, %zu", data, n);

  for (i = 0; i < n; ++i)
    mxFree((char*)data[i]);
  mxFree(data);

  dreturnvoid();
}
