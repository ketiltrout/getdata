/* (C) 2009 D. V. Wiebe
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
#include <Python.h>
#define NO_GETDATA_LEGACY_API

#undef _BSD_SOURCE
#undef _POSIX_SOURCE
#undef _SVID_SOURCE
#undef _POSIX_C_SOURCE
#undef SIZEOF_OFF_T
#include "../../src/internal.h"

#ifdef HAVE_NUMPY_ARRAYOBJECT_H
# define PY_ARRAY_UNIQUE_SYMBOL gdpy_array_api
# include <numpy/arrayobject.h>
#endif

#define GDPY_UNSIGNED        0x00
#define GDPY_SIGNED          0x01
#define GDPY_IEEE754         0x02
#define GDPY_COMPLEX         0x03

#define GDPY_INT             0x00
#define GDPY_LONG            0x10
#define GDPY_FLOAT           0x20
#define GDPY_PYCOMPLEX       0x40

#define GDPY_INT_AS_LONG        (GDPY_INT       | GDPY_SIGNED)
#define GDPY_LONG_AS_ULL        (GDPY_LONG      | GDPY_UNSIGNED)
#define GDPY_LONG_AS_SLL        (GDPY_LONG      | GDPY_SIGNED)
#define GDPY_LONG_AS_DOUBLE     (GDPY_LONG      | GDPY_IEEE754)
#define GDPY_FLOAT_AS_DOUBLE    (GDPY_FLOAT     | GDPY_IEEE754)
#define GDPY_COMPLEX_AS_COMPLEX (GDPY_PYCOMPLEX | GDPY_COMPLEX)

#define GDPY_INVALID_TYPE(t) ( \
    t != GD_UINT8     && t != GD_INT8    && \
    t != GD_UINT16    && t != GD_INT16   && \
    t != GD_UINT32    && t != GD_INT32   && \
    t != GD_UINT64    && t != GD_INT64   && \
    t != GD_FLOAT32   && t != GD_FLOAT64 && \
    t != GD_COMPLEX64 && t != GD_COMPLEX128 )

#define PYGD_CHECK_ERROR(D,R) PYGD_CHECK_ERROR2(D,R,)

#define PYGD_CHECK_ERROR2(D,R,E) \
  do { \
    int the_error; \
    if ((the_error = get_error(D))) { \
      char buffer[GD_MAX_LINE_LENGTH]; \
      PyErr_SetString(gdpy_exceptions[the_error], get_error_string((D), \
            buffer, GD_MAX_LINE_LENGTH)); \
      E; \
      dreturnvoid(); \
      return (R); \
    } \
  } while(0)

extern PyObject *gdpy_exceptions[GD_N_ERROR_CODES];
extern PyTypeObject gdpy_dirfile;
extern PyTypeObject gdpy_entry;
extern PyTypeObject gdpy_fragment;

extern const struct gdpy_constant_t {
  char* name;
  long value;
} gdpy_constant_list[];

struct gdpy_dirfile_t {
  PyObject_HEAD
  DIRFILE* D;
  PyObject* callback_data;
  PyObject* callback;
  int callback_exception;
};

struct gdpy_entry_t {
  PyObject_HEAD
  gd_entry_t* E;
};

struct gdpy_fragment_t {
  PyObject_HEAD
  int n;
  struct gdpy_dirfile_t* dirfile;
};

union gdpy_quadruple_value {
  uint64_t u;
  int64_t s;
  double f;
  double complex c;
};

static inline double complex gdpy_as_complex(PyObject* o)
{
  Py_complex c = PyComplex_AsCComplex(o);
  return c.real + _Complex_I * c.imag;
}

#define gdpy_from_complex(c) PyComplex_FromDoubles(creal(c), cimag(c))

extern int gdpy_convert_from_pyobj(PyObject*, union gdpy_quadruple_value*,
    gd_type_t);
extern gd_type_t gdpy_convert_from_pylist(PyObject*, void*, gd_type_t, size_t);
extern PyObject* gdpy_convert_to_pyobj(const void*, gd_type_t);
extern PyObject* gdpy_convert_to_pylist(const void*, gd_type_t, size_t);
extern int gdpy_npytype_from_type(gd_type_t type);
extern gd_type_t gdpy_type_from_npytype(int npytype);
PyMODINIT_FUNC initpygetdata(void);
