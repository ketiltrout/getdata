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
#include "../../src/internal.h"

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
  const char* name;
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

union gdpy_triple_value {
  uint64_t u;
  int64_t s;
  double f;
};

extern int gdpy_convert_from_pyobj(PyObject*, union gdpy_triple_value*,
    gd_type_t);
extern gd_type_t gdpy_convert_from_pylist(PyObject*, void*, gd_type_t, size_t);
extern PyObject* gdpy_convert_to_pyobj(const void*, gd_type_t);
extern PyObject* gdpy_convert_to_pylist(const void*, gd_type_t, size_t);
