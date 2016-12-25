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
#ifndef GD_MATLAB_H

/* mex runs GCC in -ansi mode */
#ifndef GDMXLIB
#define GD_C89_API

# define uint64_t gd_uint64_t
# define int64_t gd_int64_t
#else

#ifdef HAVE_INTTYPES_H
#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif
#include <inttypes.h>
#endif

#endif

#define GD_64BIT_API
#include "getdata.h"

#include <mex.h>

#define GDMX_INTERNAL_ERROR \
  mexErrMsgIdAndTxt("GetData:GDMX:InternalError", \
      "Internal error at [%s,%i]", __FILE__, __LINE__ )

#define GDMX_NO_LHS \
  if (nlhs > 0) \
    mexErrMsgIdAndTxt("GetData:GDMX:nLHS", \
        "Too many output arguments.")

#define GDMX_CHECK_RHS(n) \
  if (nrhs != (n)) \
    mexErrMsgIdAndTxt("GetData:GDMX:nRHS", \
        "Invalid argument count; wanted %i, given %i", \
        (n), nrhs)

#define GDMX_CHECK_RHS2(m,n) \
  if (nrhs < (m)) \
    mexErrMsgIdAndTxt("GetData:GDMX:nRHS", \
        "Invalid argument count; wanted at least %i, given %i", \
        (m), nrhs); \
  else if (nrhs > (n)) \
    mexErrMsgIdAndTxt("GetData:GDMX:nRHS", \
        "Invalid argument count; wanted at most %i, given %i", \
        (n), nrhs)

/* integer type aliases */
#if SIZEOF_SIZE_T == 8
#define gdmx_to_size_t(a,b) ((size_t)gdmx_to_uint64(a,b))
#define gdmx_from_size_t gdmx_from_uint64
#elif SIZEOF_SIZE_T == SIZEOF_LONG
#define gdmx_to_size_t(a,b) ((size_t)gdmx_to_ulong(a,b))
#define gdmx_from_size_t gdmx_from_ulong
#else
#define gdmx_to_size_t(a,b) ((size_t)gdmx_to_uint(a,b))
#define gdmx_from_size_t gdmx_from_uint
#endif
#define gdmx_to_gd_type(a,b) ((gd_type_t)gdmx_to_int(a,b))
#define gdmx_from_gd_type gdmx_from_int
#define gdmx_from_entype gdmx_from_int
#define gdmx_from_windop gdmx_from_int

#ifdef __cplusplus
extern "C" {
#endif

/* initialiser */
void gdmx_initialise(void);

void gdmx_err(DIRFILE *, int);
mxClassID gdmx_classid(gd_type_t);
void gdmx_free_entry(gd_entry_t *);

/* convert to MATLAB type */
#define gdmx_from_string mxCreateString
mxArray *gdmx_from_dirfile(const DIRFILE*);
mxArray *gdmx_from_data(const void *, gd_type_t, size_t);
mxArray *gdmx_from_entry(const gd_entry_t*);
mxArray *gdmx_from_nstring_list(const char **, size_t);
mxArray *gdmx_from_string_list(const char **);

#define gdmx_from_int gdmx_from_long
#define gdmx_from_uint gdmx_from_ulong
#define gdmx_from_double mxCreateDoubleScalar
mxArray *gdmx_from_bool(int);
mxArray *gdmx_from_long(long);
mxArray *gdmx_from_int64(int64_t);
mxArray *gdmx_from_ulong(unsigned long);
mxArray *gdmx_from_uint64(uint64_t);

mxArray *gdmx_from_carrays(const gd_carray_t *, gd_type_t);
mxArray *gdmx_from_sarrays(const char ***);
mxArray *gdmx_vector(gd_type_t, size_t, void**);
void gdmx_fix_vector(mxArray *, gd_type_t, size_t, void*);

/* convert from MATLAB type */
DIRFILE *gdmx_to_dirfile(const mxArray *);
char *gdmx_to_string(const mxArray **, int, int);
int gdmx_to_int(const mxArray **, int);

#define GDMX_ENO_FRAG  0x1
#define GDMX_ENO_FIELD 0x2
gd_entry_t *gdmx_to_entry(const mxArray **, int, unsigned);

double gdmx_to_double(const mxArray **, int);
long gdmx_to_long(const mxArray **, int);
int64_t gdmx_to_int64_t(const mxArray **, int);
#define gdmx_to_uint(a,b) ((unsigned int)gdmx_to_ulong(a,b))
unsigned long gdmx_to_ulong(const mxArray **, int);
uint64_t gdmx_to_uint64(const mxArray **, int);

size_t gdmx_to_nsamp(DIRFILE*, const char*, const mxArray**, int, int);
void gdmx_to_data(void**, gd_type_t*, size_t*, const mxArray*, int);
void gdmx_free_data(void*, gd_type_t);

void gdmx_to_sdata(const char***, size_t*, const mxArray*, int);
void gdmx_free_sdata(const char**, size_t);

#ifdef __cplusplus
}
#endif

#endif
