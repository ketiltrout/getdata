/* Copyright (C) 2009-2015 D. V. Wiebe
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
#include "../../src/internal.h"

/* Deal with pyconfig.h */
#undef _XOPEN_SOURCE
#undef _BSD_SOURCE
#undef _SVID_SOURCE
#undef _POSIX_C_SOURCE

#include <Python.h>

#ifdef GDPY_INCLUDE_NUMPY
# ifdef HAVE_NUMPY_ARRAYOBJECT_H
#  define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#  define PY_ARRAY_UNIQUE_SYMBOL gdpy_array_api
#  include <numpy/arrayobject.h>
#  if NPY_ABI_VERSION < 0x02000000
#   error "NumPy 2.0 or later is required. Please upgrade NumPy: pip install 'numpy>=2.0'"
#  endif
# endif
#endif

#define GDPY_UNSIGNED        0x00
#define GDPY_SIGNED          0x01
#define GDPY_IEEE754         0x02
#define GDPY_COMPLEX         0x03

#define GDPY_INT             0x00
#define GDPY_LONG            0x10
#define GDPY_FLOAT           0x20
#define GDPY_PYCOMPLEX       0x40

#if HAVE_PYCAPSULE_NEW
#define PYGETDATA_MODULE
#include "pygetdata.h"
#define PYGETDATA_CAPI
#endif

#define GDPY_INT_AS_LONG        (GDPY_INT       | GDPY_SIGNED)
#define GDPY_LONG_AS_ULL        (GDPY_LONG      | GDPY_UNSIGNED)
#define GDPY_LONG_AS_SLL        (GDPY_LONG      | GDPY_SIGNED)
#define GDPY_LONG_AS_DOUBLE     (GDPY_LONG      | GDPY_IEEE754)
#define GDPY_FLOAT_AS_DOUBLE    (GDPY_FLOAT     | GDPY_IEEE754)
#define GDPY_COMPLEX_AS_COMPLEX (GDPY_PYCOMPLEX | GDPY_COMPLEX)

#ifndef HAVE_PYERR_NEWEXCEPTIONWITHDOC
#define PyErr_NewExceptionWithDoc(a,b,c,d) PyErr_NewException(a,c,d)
#endif

/* Python3 does away with Int objects */
#if PY_MAJOR_VERSION < 3
#define gdpyint_fromlong PyInt_FromLong
#define gdpyint_check(o) (PyInt_Check(o) || PyLong_Check(o))
#else
#define gdpyint_fromlong PyLong_FromLong
#define gdpyint_check PyLong_Check
#endif

#define GDPY_INVALID_OP(t) ( \
    t != GD_WINDOP_EQ && t != GD_WINDOP_NE && \
    t != GD_WINDOP_GE && t != GD_WINDOP_GT && \
    t != GD_WINDOP_LE && t != GD_WINDOP_LT && \
    t != GD_WINDOP_SET && t != GD_WINDOP_CLR )

#define GDPY_INVALID_TYPE(t) ( \
    t != GD_UINT8     && t != GD_INT8    && \
    t != GD_UINT16    && t != GD_INT16   && \
    t != GD_UINT32    && t != GD_INT32   && \
    t != GD_UINT64    && t != GD_INT64   && \
    t != GD_FLOAT32   && t != GD_FLOAT64 && \
    t != GD_COMPLEX64 && t != GD_COMPLEX128 )

#define GDPY_CHECK_ERROR(D,R,ce) GDPY_CHECK_ERROR2(D,R,,ce)

#define GDPY_CHECK_ERROR2(D,R,E,ce) \
  do { \
    if (gdpy_report_error(D,ce)) { \
      E; \
      dreturnvoid(); \
      return (R); \
    } \
  } while(0)

extern PyTypeObject gdpy_dirfile;
extern PyTypeObject gdpy_entry;
extern PyTypeObject gdpy_fragment;

extern const struct gdpy_constant_t {
  char *name;
  long value;
} gdpy_constant_list[];

struct gdpy_dirfile_t {
  PyObject_HEAD
  DIRFILE *D;
  int mplex_lookback;
  char *verbose_prefix;
  PyObject *callback_data;
  PyObject *callback;
  int callback_exception;
  char *char_enc;
};

struct gdpy_entry_t {
  PyObject_HEAD
  gd_entry_t *E;
  char *char_enc;
};

struct gdpy_fragment_t {
  PyObject_HEAD
  int n;
  struct gdpy_dirfile_t *dirfile;
};

union gdpy_quadruple_value {
  uint64_t u;
  int64_t s;
  double f;
  GD_DCOMPLEXA(c);
};

#define gdpy_as_complex(v,o) do { \
  Py_complex c = PyComplex_AsCComplex(o); \
  gd_li2cp_((v), c.real, c.imag); \
} while(0)

#define gdpy_from_complexp(c) PyComplex_FromDoubles((c)[0], (c)[1])
#define gdpy_from_complex(c) PyComplex_FromDoubles(creal(c), cimag(c))

/* Deal with PyString, PyBytes, PyUnicode changes between Python2 and 3 */
#if PY_MAJOR_VERSION < 3

/* Check for an encoded string pyobj */
#  define gdpy_encobj_check PyString_Check

/* Convert to a "native" Python string object
 * (ie. PyString in Python2 and PyUnicode in Python3) */
#  define gdpystrobj_from_string PyString_FromString

/* For already-encoded python input strings */
#  define gdpy_string_from_encobj PyString_AsString

/* For non-decoded returned strings */
#  define gdpy_encobj_from_string PyString_FromString
#else
#  define gdpy_encobj_check PyBytes_Check
#  define gdpystrobj_from_string PyUnicode_FromString
#  define gdpy_string_from_encobj PyBytes_AsString
#  define gdpy_encobj_from_string PyBytes_FromString
#endif

/* Python3 changes to the modinit */
#if PY_MAJOR_VERSION < 3
#define GDPY_MODINITFUNC PyMODINIT_FUNC initpygetdata(void)
#define GDPY_MODINITSUCCESS return
#define GDPY_MODINITFAILURE return
#else
#define GDPY_MODINITFUNC PyMODINIT_FUNC PyInit_pygetdata(void)
#define GDPY_MODINITSUCCESS return gdpy_mod
#define GDPY_MODINITFAILURE return NULL
#endif

/* Python3 compatibility */
#ifndef PyVarObject_HEAD_INIT
#define PyVarObject_HEAD_INIT(type, size) PyObject_HEAD_INIT(type) size,
#endif

/* Handle filesystem encoding */
#if PY_MAJOR_VERSION < 3
#define gdpy_path_from_pyobj(o,c) gdpy_string_from_pyobj(o,c,NULL)
#define gdpyobj_from_path PyString_FromString
#else
extern char *gdpy_path_from_pyobj_(PyObject*, int);
#define gdpy_path_from_pyobj(o,c) gdpy_path_from_pyobj_(o,1)
#define gdpyobj_from_path PyUnicode_DecodeFSDefault
#endif

/* Declarations for the CAPI */
#ifdef PYGETDATA_CAPI
extern DIRFILE *gdpy_dirfile_dirfile(struct gdpy_dirfile_t *);
extern int gdpy_dirfile_raise(struct gdpy_dirfile_t *);
#endif

extern char *gdpy_strdup(const char*);
extern int gdpy_report_error(DIRFILE*, char*);
extern char *gdpy_copy_global_charenc(void);
extern PyObject *gdpyobj_from_string(const char*, const char*);
extern PyObject *gdpyobj_from_estring(const char*, const char*);
extern PyObject *gdpy_charenc_obj(const char*);
extern int gdpy_parse_charenc(char**, PyObject*);
extern long gdpy_long_from_pyobj(PyObject*);
extern unsigned long gdpy_ulong_from_pyobj(PyObject*);
extern char *gdpy_string_from_pyobj(PyObject*, const char*, const char*);
extern int gdpylist_append(PyObject*, PyObject*);
extern int gdpy_convert_from_pyobj(PyObject*, union gdpy_quadruple_value*,
    gd_type_t);
extern int gdpy_coerce_from_pyobj(PyObject*, gd_type_t, void*);
extern gd_type_t gdpy_convert_from_pylist(PyObject*, void*, gd_type_t, size_t);
extern PyObject *gdpy_convert_to_pyobj(const void*, gd_type_t, int);
extern PyObject *gdpy_convert_to_pylist(const void*, gd_type_t, size_t);
extern int gdpy_npytype_from_type(gd_type_t type);
extern gd_type_t gdpy_type_from_npytype(int npytype);
PyMODINIT_FUNC initpygetdata(void);
