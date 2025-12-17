/* Copyright (C) 2009-2016 D. V. Wiebe
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
#define GDPY_INCLUDE_NUMPY
#include "gdpy_intern.h"

static PyObject *gdpy_mod = NULL;

static struct {
  char *name;
  char *doc;
} gdpy_exception_list[GD_N_ERROR_CODES] = {
  { NULL, NULL },
  { "Format", "Syntax error in Dirfile metadata (GD_E_FORMAT)." },
  { "Creation", "Unable to create a Dirfile. (GD_E_CREAT)." },
  { "BadCode", "Bad field code. (GD_E_BAD_CODE)." },
  { "BadType", "Bad data type. (GD_E_BAD_TYPE)." },
  { "IO", "I/O error encountered. (GD_E_IO)." },
  { "Internal", "Internal library error (GD_E_INTERNAL_ERROR).\n"
    "Please report to <" PACKAGE_BUGREPORT ">" },
  { NULL, NULL }, /* GD_E_ALLOC.  Reported via PyErr_NoMemory() */
  { "Range", "Invalid frame or sample number (GD_E_RANGE)." },
  { "LUT", "Malformed LINTERP table file (GD_E_LUT)." },
  { "RecurseLevel", "Recursion too deep (GD_E_RECURSE_LEVEL)." },
  { "BadDirfile", "Dirfile is invalid (GD_E_BAD_DIRFILE)." },
  { "BadFieldType", "Bad field type (GD_E_BAD_FIELD_TYPE)." },
  { "AccessMode", "Write attempt on read-only Dirfile (GD_E_ACCMODE)." },
  { "Unsupported",
    "Operation not supported by current encoding scheme (GD_E_UNSUPPORTED)." },
  { "UnknownEncoding", "Unknown encoding scheme (GD_E_UNKNOWN_ENCODING)." },
  { "BadEntry", "Invalid entry metadata (GD_E_BAD_ENTRY)." },
  { "Duplicate", "Duplicate field name (GD_E_DUPLICATE)." },
  { "Dimension",
    "Scalar field found where vector field expected. (GD_E_DIMENSION)." },
  { "BadIndex", "Invalid Fragment index (GD_E_BAD_INDEX)." },
  { "BadScalar", "Scalar field code not found (GD_E_BAD_SCALAR)." },
  { "BadReference", "Bad reference field (GD_E_BAD_REFERENCE)." },
  { "Protected", "Operation prohibited by protection level (GD_E_PROTECTED)." },
  { "Deletion", "Error deleting field (GD_E_DELETE)." },
  { "Argument", "Bad argument passed to function (GD_E_BAD_ARGUMENT)." },
  { "Callback", "Bad return from parser callback (GD_E_CALLBACK)." },
  { "Exists", "Dirfile already exists (GD_E_EXISTS)." },
  { "UncleanDatabase",
    "Error updating Dirfile: database is unclean (GD_E_UNCLEAN_DB)." },
  { "Domain", "Improper domain (GD_E_DOMAIN)." },
  { "Bounds", "CARRAY access out-of-bounds (GD_E_BOUNDS)." },
  { "LineTooLong", "Metadata line is too long (GD_E_LINE_TOO_LONG)." }
};
static PyObject *gdpy_exceptions[GD_N_ERROR_CODES];

/* These are unused but for backwards compatibility are defined as aliases of
 * current exceptions */
static struct {
  const char *name;
  int e;
} gdpy_dead_exceptions[] = {
  { "BadEndianness", -GD_E_ARGUMENT },
  { "BadProtection", -GD_E_ARGUMENT },
  { "BadRepr", -GD_E_BAD_CODE },
  { "BadVersion", -GD_E_ARGUMENT },
  { "OpenLinfile", -GD_E_LUT },
  { "Flush", -GD_E_IO },
  { "Open", -GD_E_IO },
  { "OpenFragment", -GD_E_IO },
  { "OpenFragment", -GD_E_IO },
  { "OpenInclude", -GD_E_IO },
  { "RawIO", -GD_E_IO },
  { "Trunc", -GD_E_IO },
  { NULL, 0}
};

char *gdpy_strdup(const char *str)
{
  char *ptr = NULL;

  dtrace("\"%s\"", str);

  if (str) {
    size_t len = strlen(str) + 1;
    ptr = PyMem_Malloc(len);

    if (ptr)
      memcpy(ptr, str, len);
  }

  dreturn("%p", ptr);
  return ptr;
}

int gdpylist_append(PyObject *list, PyObject *item)
{
  int ret;

  dtrace("%p, %p", list, item);

  if (item == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  ret = PyList_Append(list, item);
  Py_DECREF(item);

  dreturn("%i", ret);
  return ret;
}

int gdpy_convert_from_pyobj(PyObject *value, union gdpy_quadruple_value *data,
    gd_type_t type)
{
  int data_type;

  dtrace("%p, %p, %02x", value, data, type);

  /* check value type, and figure out autotype, if needed */
#if PY_MAJOR_VERSION < 3
  if (PyInt_Check(value)) {
    data->s = PyInt_AsLong(value);
    data_type = GDPY_INT_AS_LONG;

    if (PyErr_Occurred()) {
      dreturn("%i", -1);
      return -1;
    }
  } else
#endif
  {
    if (PyLong_Check(value)) {
      if (type == GD_UNKNOWN) {
        /* try unsigned long long first */
        data->u = PyLong_AsUnsignedLongLong(value);
        data_type = GDPY_LONG_AS_ULL;

        if (PyErr_Occurred()) {
          if (PyErr_ExceptionMatches(PyExc_OverflowError)) { /* too big */
            data->f = PyLong_AsDouble(value);
            data_type = GDPY_LONG_AS_DOUBLE;

            if (PyErr_Occurred()) {
              dreturn("%i", -1);
              return -1;
            }
          } else if (PyErr_ExceptionMatches(PyExc_TypeError)) { /* too small */
            data->f = PyLong_AsDouble(value);
            data_type = GDPY_LONG_AS_DOUBLE;

            if (PyErr_Occurred()) {
              if (PyErr_ExceptionMatches(PyExc_TypeError)) { /*still too small*/
                data->s = PyLong_AsLongLong(value);
                data_type = GDPY_LONG_AS_SLL;

                if (PyErr_Occurred()) {
                  dreturn("%i", -1);
                  return -1;
                }
              } else { /* some other error */
                dreturn("%i", -1);
                return -1;
              }
            }
          } else { /* some other error */
            dreturn("%i", -1);
            return -1;
          }
        }
      } else if (type & GD_SIGNED) {
        data->s = PyLong_AsLongLong(value);
        data_type = GDPY_LONG_AS_SLL;

        if (PyErr_Occurred()) {
          dreturn("%i", -1);
          return -1;
        }
      } else if (type & GD_IEEE754) {
        data->f = PyLong_AsDouble(value);
        data_type = GDPY_LONG_AS_DOUBLE;

        if (PyErr_Occurred()) {
          dreturn("%i", -1);
          return -1;
        }
      } else {
        data->u = PyLong_AsLongLong(value);
        data_type = GDPY_LONG_AS_ULL;

        if (PyErr_Occurred()) {
          dreturn("%i", -1);
          return -1;
        }
      }
    } else if (PyFloat_Check(value)) {
      data->f = PyFloat_AsDouble(value);
      data_type = GDPY_FLOAT_AS_DOUBLE;

      if (PyErr_Occurred()) {
        dreturn("%i", -1);
        return -1;
      }
    } else if (PyComplex_Check(value)) {
      gdpy_as_complex(gd_csp_(data->c), value);
      data_type = GDPY_COMPLEX_AS_COMPLEX;

      if (PyErr_Occurred()) {
        dreturn("%i", -1);
        return -1;
      }
    } else { /* a non-numeric type */
      PyErr_SetString(PyExc_TypeError, "a numeric type was expected");
      dreturn("%i", -1);
      return -1;
    }
  }

  dreturn("%02x", data_type);
  return data_type;
}

unsigned long gdpy_ulong_from_pyobj(PyObject *pyobj)
{
  unsigned long v = 0;

  dtrace("%p", pyobj);

  if (PyLong_Check(pyobj))
    v = PyLong_AsUnsignedLong(pyobj);
#if PY_MAJOR_VERSION < 3
  else if (PyInt_Check(pyobj))
    v = (unsigned long)PyInt_AsLong(pyobj);
#endif
  else
    PyErr_SetString(PyExc_TypeError, "an integer type was expected");

  dreturn("%lu", v);
  return v;
}

long gdpy_long_from_pyobj(PyObject *pyobj)
{
  long v = 0;

  dtrace("%p", pyobj);

  if (PyLong_Check(pyobj))
    v = PyLong_AsLong(pyobj);
#if PY_MAJOR_VERSION < 3
  else if (PyInt_Check(pyobj))
    v = PyInt_AsLong(pyobj);
#endif
  else
    PyErr_SetString(PyExc_TypeError, "an integer type was expected");

  dreturn("%li", v);
  return v;
}

/* Convert a Python string-like object to a C string, returns a PyMem_Malloc'd
 * string except on error */
char *gdpy_string_from_pyobj(PyObject *pyobj, const char *char_enc,
    const char *err_string)
{
  PyObject *encobj = NULL;
  char *s = NULL;

  dtrace("%p, \"%s\"", pyobj, char_enc);

  if (PyUnicode_Check(pyobj)) {
    /* Encode string */
    if (char_enc == NULL)
      encobj = PyUnicode_AsUTF8String(pyobj);
    else
      encobj = PyUnicode_AsEncodedString(pyobj, char_enc, "strict");

    if (encobj == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }
  } else if (!gdpy_encobj_check(pyobj)) {
    if (err_string)
      PyErr_SetString(PyExc_TypeError, err_string);
    dreturn("%p", NULL);
    return NULL;
  }

  /* now convert to python object */
  if (encobj) {
    s = gdpy_string_from_encobj(encobj);
    Py_DECREF(encobj);
  } else
    s = gdpy_string_from_encobj(pyobj);

  /* strdup */
  if (s) {
    s = gdpy_strdup(s);

    if (s == NULL)
      PyErr_NoMemory();
  }

  dreturn("\"%s\"", s);
  return s;
}

/* Return a python string, optionally decoding it */
PyObject *gdpyobj_from_string(const char *string, const char *char_enc)
{
  PyObject *pyobj;

  dtrace("\"%s\", %p", string, char_enc);

  if (char_enc == NULL)
    pyobj = gdpy_encobj_from_string(string); /* don't decode */
  else
    pyobj = PyUnicode_Decode(string, strlen(string), char_enc, "strict");

  dreturn("%p", pyobj);
  return pyobj;
}

/* Convert a GetData error string into a Python string.  These are always
 * first decoded, if possible, and then ASCII re-encoded, since we can't know
 * the capabilities of the controlling tty */
PyObject *gdpyobj_from_estring(const char* string, const char *char_enc)
{
  PyObject *pyobj, *unicode = NULL;

  dtrace("\"%s\", %p", string, char_enc);

  if (char_enc) 
    unicode = PyUnicode_Decode(string, strlen(string), char_enc, "strict");

  if (unicode) {
    pyobj = PyUnicode_AsEncodedString(unicode, "ascii", "backslashreplace");
    Py_DECREF(unicode);
  } else {
    PyErr_Clear();
    pyobj = gdpy_encobj_from_string(string);
  }

  dreturn("%p", pyobj);
  return pyobj;
}

#if PY_MAJOR_VERSION >= 3
/* In Python3, filesystem encoding from Unicode is handled specially */
char *gdpy_path_from_pyobj_(PyObject *pyobj, int dup)
{
  char *s;

  dtrace("%p, %i", pyobj, dup);

  /* Encode */
  if (PyUnicode_Check(pyobj))
    pyobj = PyUnicode_EncodeFSDefault(pyobj);
  else if (!gdpy_encobj_check(pyobj)) {
    PyErr_SetString(PyExc_TypeError, "a path was expected");
    dreturn("%p", NULL);
    return NULL;
  }

  s = gdpy_string_from_encobj(pyobj);

  /* strdup, if requested */
  if (s && dup) {
    s = gdpy_strdup(s);

    if (s == NULL)
      PyErr_NoMemory();
  }

  dreturn("\"%s\"", s);
  return s;
}
#endif

#define GDPY_COERCE_REAL(t,p,v,d) \
  if ((p) == GDPY_INT_AS_LONG || (p) == GDPY_LONG_AS_SLL) \
    *((t*)(v)) = (t)(d).s; \
  else if ((p) == GDPY_LONG_AS_ULL) \
    *((t*)(v)) = (t)(d).u; \
  else if ((p) == GDPY_LONG_AS_DOUBLE || (p) == GDPY_FLOAT_AS_DOUBLE) \
    *((t*)(v)) = (t)(d).f; \
  else \
    *((t*)(v)) = (t)creal((d).c);

#ifdef GD_NO_C99_API
#define GDPY_COERCE_CPLX(t,p,v,d) \
  if ((p) == GDPY_INT_AS_LONG || (p) == GDPY_LONG_AS_SLL) { \
    ((t*)(v))[0] = (t)(d).s; ((t*)(v))[1] = 0; \
  } else if ((p) == GDPY_LONG_AS_ULL) { \
    ((t*)(v))[0] = (t)(d).u; ((t*)(v))[1] = 0; \
  } else if ((p) == GDPY_LONG_AS_DOUBLE || (p) == GDPY_FLOAT_AS_DOUBLE) { \
    ((t*)(v))[0] = (t)(d).f; ((t*)(v))[1] = 0; \
  } else { \
    ((t*)(v))[0] = (t)(d).c[0]; ((t*)(v))[1] = (t)(d).c[1]; \
  }
#else
#define GDPY_COERCE_CPLX(t,p,v,d) \
  if ((p) == GDPY_INT_AS_LONG || (p) == GDPY_LONG_AS_SLL) \
    *((t _Complex*)(v)) = (t)(d).s; \
  else if ((p) == GDPY_LONG_AS_ULL) \
    *((t _Complex*)(v)) = (t)(d).u; \
  else if ((p) == GDPY_LONG_AS_DOUBLE || (p) == GDPY_FLOAT_AS_DOUBLE) \
    *((t _Complex*)(v)) = (t)(d).f; \
  else \
    *((t _Complex*)(v)) = (t _Complex)(d).c;
#endif

int gdpy_coerce_from_pyobj(PyObject *pyobj, gd_type_t type, void *value)
{
  union gdpy_quadruple_value d;
  int conv;

  dtrace("%p, 0x%X, %p", pyobj, type, value);
  
  conv = gdpy_convert_from_pyobj(pyobj, &d, type);

  if (conv == -1) {
    dreturn("%i", -1);
    return -1;
  }

  switch (type) {
    case GD_UINT8:      GDPY_COERCE_REAL(uint8_t,  conv, value, d); break;
    case GD_INT8:       GDPY_COERCE_REAL(int8_t,   conv, value, d); break;
    case GD_UINT16:     GDPY_COERCE_REAL(uint16_t, conv, value, d); break;
    case GD_INT16:      GDPY_COERCE_REAL(int16_t,  conv, value, d); break;
    case GD_UINT32:     GDPY_COERCE_REAL(uint32_t, conv, value, d); break;
    case GD_INT32:      GDPY_COERCE_REAL(int32_t,  conv, value, d); break;
    case GD_UINT64:     GDPY_COERCE_REAL(uint64_t, conv, value, d); break;
    case GD_INT64:      GDPY_COERCE_REAL(int64_t , conv, value, d); break;
    case GD_FLOAT32:    GDPY_COERCE_REAL(float   , conv, value, d); break;
    case GD_FLOAT64:    GDPY_COERCE_REAL(double  , conv, value, d); break;
    case GD_COMPLEX64:  GDPY_COERCE_CPLX(float   , conv, value, d); break;
    case GD_COMPLEX128: GDPY_COERCE_CPLX(double  , conv, value, d); break;
    default: break;
  }

  dreturn("%i", 0);
  return 0;
}

gd_type_t gdpy_convert_from_pylist(PyObject *value, void *data, gd_type_t type,
    size_t ns)
{
  size_t i;
  int data_type;
  union gdpy_quadruple_value tmp;

  dtrace("%p, %p, %02x, %zi", value, data, type, ns);

  /* use the first element to determine the data type */
  data_type = gdpy_convert_from_pyobj(PyList_GetItem(value, 0), &tmp, type);

  if (data_type == -1) {
    dreturn("%02x", GD_UNKNOWN);
    return GD_UNKNOWN;
  }

  switch(data_type) {
#if PY_MAJOR_VERSION < 3
    case GDPY_INT_AS_LONG:
      type = GD_INT32;
      *(int32_t*)data = (int32_t)tmp.s;
      for (i = 1; i < ns; ++i)
        ((int32_t*)data)[i] = (int32_t)PyInt_AsLong(PyList_GetItem(value, i));
      break;
#endif
    case GDPY_LONG_AS_ULL: 
      type = GD_UINT64;
      *(uint64_t*)data = tmp.u;
      for (i = 1; i < ns; ++i)
        ((uint64_t*)data)[i] = PyLong_AsUnsignedLongLong(PyList_GetItem(value,
            i));
      break;
    case GDPY_LONG_AS_SLL: 
      type = GD_INT64;
      *(int64_t*)data = tmp.s;
      for (i = 1; i < ns; ++i)
        ((int64_t*)data)[i] = PyLong_AsLongLong(PyList_GetItem(value, i));
      break;
    case GDPY_LONG_AS_DOUBLE: 
      type = GD_FLOAT64;
      *(double*)data = tmp.f;
      for (i = 1; i < ns; ++i)
        ((double*)data)[i] = PyLong_AsDouble(PyList_GetItem(value, i));
      break;
    case GDPY_FLOAT_AS_DOUBLE:
      type = GD_FLOAT64;
      *(double*)data = tmp.f;
      for (i = 1; i < ns; ++i)
        ((double*)data)[i] = PyFloat_AsDouble(PyList_GetItem(value, i));
      break;
    case GDPY_COMPLEX_AS_COMPLEX:
      type = GD_COMPLEX128;
      gd_cs2ca_(data, 0, tmp.c, double);
      for (i = 1; i < ns; ++i)
        gdpy_as_complex(((double*)data) + 2 * i, PyList_GetItem(value, i));
      break;
  }

  dreturn("%02x", type);
  return type;
}

/* generic utitily functions */
gd_type_t gdpy_type_from_npytype(int npytype)
{
  gd_type_t type;

  dtrace("%i", npytype);

  switch(npytype)
  {
    case NPY_UBYTE:
      type = GD_UINT8;
      break;
    case NPY_BYTE:
      type = GD_INT8;
      break;
#if NPY_SIZEOF_SHORT <= 8
    case NPY_SHORT:
      type = (gd_type_t)(NPY_SIZEOF_SHORT | GD_SIGNED);
      break;
    case NPY_USHORT:
      type = (gd_type_t)NPY_SIZEOF_SHORT;
      break;
#endif
#if NPY_SIZEOF_INT <= 8
    case NPY_INT:
      type = (gd_type_t)(NPY_SIZEOF_INT | GD_SIGNED);
      break;
    case NPY_UINT:
      type = (gd_type_t)NPY_SIZEOF_INT;
      break;
#endif
#if NPY_SIZEOF_LONG <= 8
    case NPY_LONG:
      type = (gd_type_t)(NPY_SIZEOF_LONG | GD_SIGNED);
      break;
    case NPY_ULONG:
      type = (gd_type_t)NPY_SIZEOF_LONG;
      break;
#endif
#if NPY_SIZEOF_LONGLONG <= 8
    case NPY_LONGLONG:
      type = (gd_type_t)(NPY_SIZEOF_LONGLONG | GD_SIGNED);
      break;
    case NPY_ULONGLONG:
      type = (gd_type_t)NPY_SIZEOF_LONGLONG;
      break;
#endif
#if NPY_SIZEOF_FLOAT <= 8
    case NPY_FLOAT:
      type = (gd_type_t)NPY_SIZEOF_FLOAT | GD_IEEE754;
      break;
    case NPY_CFLOAT:
      type = (gd_type_t)((2 * NPY_SIZEOF_FLOAT) | GD_COMPLEX);
      break;
#endif
#if NPY_SIZEOF_DOUBLE <= 8
    case NPY_DOUBLE:
      type = (gd_type_t)(NPY_SIZEOF_DOUBLE | GD_IEEE754);
      break;
    case NPY_CDOUBLE:
      type = (gd_type_t)((2 * NPY_SIZEOF_DOUBLE) | GD_COMPLEX);
      break;
#endif
    default:
      type = GD_UNKNOWN;
      break;
  }

  dreturn("0x%03x", type);
  return type;
}

int gdpy_npytype_from_type(gd_type_t type)
{
  int npytype;

  dtrace("0x%03x", type);

  switch(type)
  {
    case GD_UINT8:
      npytype = NPY_UINT8;
      break;
    case GD_INT8:
      npytype = NPY_INT8;
      break;
    case GD_UINT16:
      npytype = NPY_UINT16;
      break;
    case GD_INT16:
      npytype = NPY_INT16;
      break;
    case GD_UINT32:
      npytype = NPY_UINT32;
      break;
    case GD_INT32:
      npytype = NPY_INT32;
      break;
    case GD_UINT64:
      npytype = NPY_UINT64;
      break;
    case GD_INT64:
      npytype = NPY_INT64;
      break;
    case GD_FLOAT32:
      npytype = NPY_FLOAT32;
      break;
    case GD_FLOAT64:
      npytype = NPY_FLOAT64;
      break;
    case GD_COMPLEX64:
      npytype = NPY_COMPLEX64;
      break;
    case GD_COMPLEX128:
      npytype = NPY_COMPLEX128;
      break;
    default:
      npytype = NPY_NOTYPE;
      break;
  }

  dreturn("%i", npytype);
  return npytype;
}

static PyObject *gdpy_maybe_complex(double re, double im, int force)
{
  PyObject *pyobj;

  dtrace("%g, %g, %i", re, im, force);

  if (force || im)
    pyobj = PyComplex_FromDoubles(re, im);
  else
    pyobj = PyFloat_FromDouble(re);

  dreturn("%p", pyobj);
  return pyobj;
}

int gdpy_report_error(DIRFILE *D, char *char_enc)
{
  int e;
  
  dtrace("%p, \"%s\"", D, char_enc);

  e = gd_error(D);

  if (e == GD_E_ALLOC)
    PyErr_NoMemory();
  else if (e) {
    char *buffer = gd_error_string(D, NULL, 0);
    if (buffer) {
      PyErr_SetObject(gdpy_exceptions[-e], gdpyobj_from_estring(buffer,
            char_enc));
      PyMem_Free(buffer); 
    } else
      PyErr_NoMemory(); /* Errors with errors */
  }

  dreturn("%i", e);
  return e;
}

PyObject *gdpy_convert_to_pylist(const void *data, gd_type_t type, size_t ns)
{
  size_t i;
  PyObject *pyobj;

  dtrace("%p, %02x, %zi", data, type, ns);

  if (type == GD_NULL) {
    Py_INCREF(Py_None);
    dreturn("%p", Py_None);
    return Py_None;
  }
  pyobj = PyList_New(0);

  switch(type) {
    case GD_UINT8:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, gdpyint_fromlong((long)((uint8_t*)data)[i])))
          return NULL;
      break;
    case GD_INT8:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, gdpyint_fromlong((long)((int8_t*)data)[i])))
          return NULL;
      break;
    case GD_UINT16:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj,
              gdpyint_fromlong((long)((uint16_t*)data)[i])))
        {
          return NULL;
        }
      break;
    case GD_INT16:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, gdpyint_fromlong((long)((int16_t*)data)[i])))
          return NULL;
      break;
    case GD_UINT32:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj,
              PyLong_FromUnsignedLong((unsigned long)((uint32_t*)data)[i])))
          return NULL;
      break;
    case GD_INT32:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, gdpyint_fromlong((long)((int32_t*)data)[i])))
          return NULL;
      break;
    case GD_UINT64:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, PyLong_FromUnsignedLongLong(
                (unsigned PY_LONG_LONG)((uint64_t*)data)[i])))
          return NULL;
      break;
    case GD_INT64:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj,
              PyLong_FromLongLong((PY_LONG_LONG)((int64_t*)data)[i])))
          return NULL;
      break;
    case GD_FLOAT32:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj,
              PyFloat_FromDouble((double)((float*)data)[i])))
          return NULL;
      break;
    case GD_FLOAT64:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, PyFloat_FromDouble(((double*)data)[i])))
          return NULL;
      break;
    case GD_COMPLEX64:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, gdpy_from_complexp(((float*)data) + 2 * i)))
          return NULL;
      break;
    case GD_COMPLEX128:
      for (i = 0; i < ns; ++i)
        if (gdpylist_append(pyobj, gdpy_from_complexp(((double*)data) + 2 * i)))
          return NULL;
      break;
    case GD_STRING:
    case GD_NULL:
    case GD_UNKNOWN: /* prevent compiler warning */
      break;
  }

  dreturn("%p", pyobj);
  return pyobj;
}

PyObject *gdpy_convert_to_pyobj(const void *data, gd_type_t type,
    int force_complex)
{
  PyObject *pyobj = NULL;

  dtrace("%p, 0x%X, %i", data, type, force_complex);

  switch(type) {
    case GD_NULL:
      Py_INCREF(Py_None);
      pyobj = Py_None;
      break;
    case GD_UINT8:
      pyobj = gdpyint_fromlong((long)*(uint8_t*)data);
      break;
    case GD_INT8:
      pyobj = gdpyint_fromlong((long)*(int8_t*)data);
      break;
    case GD_UINT16:
      pyobj = gdpyint_fromlong((long)*(uint16_t*)data);
      break;
    case GD_INT16:
      pyobj = gdpyint_fromlong((long)*(int16_t*)data);
      break;
    case GD_UINT32:
      pyobj = PyLong_FromUnsignedLong((unsigned long)*(uint32_t*)data);
      break;
    case GD_INT32:
      pyobj = gdpyint_fromlong((long)*(int32_t*)data);
      break;
    case GD_UINT64:
      pyobj = PyLong_FromUnsignedLongLong(
          (unsigned PY_LONG_LONG)*(uint64_t*)data);
      break;
    case GD_INT64:
      pyobj = PyLong_FromLongLong((PY_LONG_LONG)*(int64_t*)data);
      break;
    case GD_FLOAT32:
      pyobj = PyFloat_FromDouble((double)*(float*)data);
      break;
    case GD_FLOAT64:
      pyobj = PyFloat_FromDouble(*(double*)data);
      break;
    case GD_COMPLEX64:
      pyobj = gdpy_maybe_complex(((float*)data)[0], ((float*)data)[1],
          force_complex);
      break;
    case GD_COMPLEX128:
      pyobj = gdpy_maybe_complex(((double*)data)[0], ((double*)data)[1],
          force_complex);
      break;
    case GD_STRING:
    case GD_UNKNOWN: /* prevent compiler warning */
      break;
  }

  dreturn("%p", pyobj);
  return pyobj;
}

/* If value is a valid encoding (or None), PyMem_Free the old *char_enc and
 * update */
int gdpy_parse_charenc(char** char_enc, PyObject *value)
{
  dtrace("%p, %p", char_enc, value);

  if (value == NULL || value == Py_None) {
    PyMem_Free(*char_enc);
    *char_enc = NULL;
  } else {
    char *new_enc = gdpy_string_from_pyobj(value, NULL,
        "character_encoding must be string or None");
  
    if (new_enc == NULL) {
      dreturn("%i", -1);
      return -1;
    }

    PyMem_Free(*char_enc);
    *char_enc = new_enc;
  }

  dreturn("%i", 0);
  return 0;
}


/* Convert an object's character_encoding data to an appropriate Python
 * attribute (the module global character_encoding is stored as a PyObject
 * so we don't have to do anything about that). */
PyObject *gdpy_charenc_obj(const char *char_enc)
{
  PyObject *pyobj;

  dtrace("%p", char_enc);

  if (char_enc == NULL) {
    Py_INCREF(Py_None);
    pyobj = Py_None;
  } else
    pyobj = gdpystrobj_from_string(char_enc);

  dreturn("%p", pyobj);
  return pyobj;
}

/* Return a copy of the current value of the global
 * pygetdata.character_encoding, or NULL.
 */
char *gdpy_copy_global_charenc(void)
{
  PyObject *global;
  char *char_enc;

  dtracevoid();

  global = PyDict_GetItemString(PyModule_GetDict(gdpy_mod),
      "character_encoding");

  if (global == NULL)
    char_enc = NULL;
  else
    char_enc = gdpy_string_from_pyobj(global, NULL, NULL);

  dreturn("\"%s\"", char_enc);
  return char_enc;
}

static PyObject *gdpy_encoding_support(struct gdpy_fragment_t *self,
    PyObject *args, PyObject *keys)
{
  char *keywords[] = { "encoding", NULL };
  unsigned long enc;
  PyObject *pyobj;
  int n;

  dtrace("%p, %p, %p", self, args, keys);

  if (!PyArg_ParseTupleAndKeywords(args, keys, "k:pygetdata.encoding_support",
        keywords, &enc))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  n = gd_encoding_support(enc);

  if (n == 0) {
    Py_INCREF(Py_None);
    dreturn("%p", Py_None);
    return Py_None;
  }

  pyobj = gdpyint_fromlong(n);

  dreturn("%p", pyobj);
  return pyobj;
}

/* GetData */
static PyMethodDef GetDataMethods[] = {
  { "encoding_support", (PyCFunction)gdpy_encoding_support,
    METH_VARARGS | METH_KEYWORDS, "encoding_support(encoding)\n\n"
      "The 'encoding' parameter should be one of the pygetdata.*_ENCODED\n"
      "symbols.  This method will return pygetdata.RDWR if the library can\n"
      "read and write the encoding, pygetdata.RDONLY if the library can\n"
      /* ------- handy ruler ---------------------------------------------| */
      "only read the encoding, or None otherwise.  See\n"
      "gd_encoding_support(3)."
  },
  { NULL, NULL, 0, NULL }
};

/* Documentation differences between Python2 and Python3 */
#if PY_MAJOR_VERSION >= 3
#define GDPY_MODULE_DOC23(two,three) three
#else
#define GDPY_MODULE_DOC23(two,three) two
#endif

#define GDPY_MODULE_DOC \
  "Bindings to the GetData library for Dirfile access\n" \
"\n" \
"This module provides interfaces to the C GetData library.  It defines\n" \
"three main classes:\n" \
"\n" \
"  o dirfile, encapsulating the C API's DIRFILE object,\n" \
"  o entry, encapsulating the C API's gd_entry_t object, and\n" \
"  o fragment, containing fragment metadata.\n" \
"\n" \
"Second, it defines various symbolic constants defined by the C API.\n" \
"These symbols are identical to the C API's symbols, except lacking the\n" \
"GD_ prefix.  So, for example, the C API's GD_INT8 is available in these\n" \
"bindings as pygetdata.INT8.\n" \
"\n" \
"Finally, it defines a number of exceptions corresponding to C API\n" \
"dirfile error codes.  These exceptions have similar names to the C API's\n" \
"names, so, for example, pygetdata.BadCodeError corresponds to the C\n"\
"API's GD_E_BAD_CODE error code.  Excluding pygetdata.AllocError, which\n"\
/*---- handy ruler: closing quote as indicated (or earlier)--------------\n" */\
"is simply an alias for the standard MemoryError, these exceptions are\n"\
"derived from a common pygetdata.DirfileError exception class, itself\n"\
"derived from RuntimeError.  Exceptions are thrown by the bindings in\n"\
"lieu of returning a dirfile error value.\n" \
"\n" \
"Where possible, pygetdata will, by default, return vector data as NumPy\n" \
"arrays.  If pygetdata has been built with NumPy support,\n" \
"pygetdata.__numpy_supported__ will be non-zero.  If NumPy support is not\n" \
"npresent, vector data will be returned as Python lists.  Vector data\n" \
"passed to pygetdata may either be a Python list or a NumPy array.\n" \
"\n" \
"The input data type argument to bindings for functions such as\n" \
"gd_putdata(3), which is required in the C API, are typically optional,\n" \
"as pygetdata can determine the input data type by itself, and convert it\n" \
"to an appropriate type for the C API.  If the data type is supplied,\n" \
"pygetdata will coerce the input data to the specified C type as best it\n" \
"can.  For gd_getdata(3) and similar, the C API types are converted to\n" \
"Python types as follows:\n\n" \
"  o int     -- UINT8, INT8, UINT16, INT16, INT32\n" \
"  o long    -- UINT32, UINT64, INT64\n" \
"  o float   -- FLOAT32, FLOAT64\n" \
"  o complex -- COMPLEX64, COMPLEX128\n\n" \
"or to NumPy data types, as appropriate.  For convenience, the following\n" \
"type code aliases are defined:\n" \
"\n" \
"  o pygetdata.INT     = pygetdata.INT32\n" \
"  o pygetdata.LONG    = pygetdata.INT64\n" \
"  o pygetdata.ULONG   = pygetdata.UINT64\n" \
"  o pygetdata.FLOAT   = pygetdata.FLOAT64\n" \
"  o pygetdata.COMPLEX = pygetdata.COMPLEX128\n\n" \
"Note that pygetdata.FLOAT is different than the C API's GD_FLOAT alias.\n" \
"\n" \
"All pygetdata functions may be given positional or keyword parameters.\n" \
"\n" \
"CHARACTER STRINGS AND ENCODINGS:\n" \
"\n" \
"The Dirfile Standards do not specify the character encoding scheme used\n" \
"in the Dirfile metadata (other than requiring it to be 7-bit ASCII\n" \
"compatible).  As a result, pygetdata does not by default decode most\n" \
"strings provided by the library, instead returning them as encoded" \
GDPY_MODULE_DOC23("bytes\nobjects.\n\n", "\nstrings.\n\n") \
"This behaviour may be changed by specifying the character encoding to use\n" \
"for decoding strings, either globally, by assigning to the\n" \
"pygetdata.character_encoding, or else per-object by assigning to a \n"\
"pygetdata.dirfile or pygetdata.entry object's character_encoding attribute.\n"\
"(pygetdata.fragment objects use the character_encoding of their containing\n"\
"dirfile.)  The value assigned should either be None, indicating no decoding\n"\
"should occur (this is the default), or else a string giving the name of the\n"\
"character encoding to use.  This should be one of the encodings supported\n"\
"by the " \
GDPY_MODULE_DOC23("unicode()","str()") \
"built-in." \
"\n"\
"The global pygetdata.character_encoding is never used directly, but is\n" \
"copied when new pygetdata objects are created.  If the global value is\n" \
"not valid when an object is created, it is ignored and that object's\n" \
"character encoding attribute is simply set to None.\n" \
"\n" \
"This also determines how Unicode strings passed to pygetdata are encoding\n" \
"before being passed to the C library.  If character_encoding is None, the\n" \
"current locale's default encoding is used instead.\n" \
"\n"\
"String conversion occurs on demand: changing an object's character_encoding\n"\
"attribute affects all strings subsequently passed to or returned by the\n" \
"object.  Changing the global pygetdata.character_encoding only affects\n" \
"object created after the change: it does not alter the character_encoding\n" \
"attribute of pre-existing objects, even if the attribute is None.\n"

GDPY_MODINITFUNC
{
  int i;
  PyObject *mdict, *dirfileerror;
#if PY_MAJOR_VERSION >= 3
  static struct PyModuleDef moduledef = {
    PyModuleDef_HEAD_INIT,
    "pygetdata",     /* m_name */
    GDPY_MODULE_DOC, /* m_doc */
    -1,              /* m_size */
    GetDataMethods,  /* m_methods */
    NULL,            /* m_reload */
    NULL,            /* m_traverse */
    NULL,            /* m_clear */
    NULL             /* m_free */
  };
#endif

#ifdef PYGETDATA_CAPI
  PyObject *capi;
  static void *gdpy_api[PyDirfile_API_length];
#endif

  if (PyType_Ready(&gdpy_dirfile) < 0)
    GDPY_MODINITFAILURE;

  if (PyType_Ready(&gdpy_entry) < 0)
    GDPY_MODINITFAILURE;

  if (PyType_Ready(&gdpy_fragment) < 0)
    GDPY_MODINITFAILURE;

  /* The following macro will cause this function to return if importing numpy
   * fails */
  import_array()

#if PY_MAJOR_VERSION < 3
  gdpy_mod = Py_InitModule3("pygetdata", GetDataMethods, GDPY_MODULE_DOC);
#else
  gdpy_mod = PyModule_Create(&moduledef);
#endif

  if (gdpy_mod == NULL)
    GDPY_MODINITFAILURE;

  Py_INCREF(&gdpy_dirfile);
  PyModule_AddObject(gdpy_mod, "dirfile", (PyObject *)&gdpy_dirfile);

  Py_INCREF(&gdpy_entry);
  PyModule_AddObject(gdpy_mod, "entry", (PyObject *)&gdpy_entry);

  Py_INCREF(&gdpy_fragment);
  PyModule_AddObject(gdpy_mod, "fragment", (PyObject *)&gdpy_fragment);

  /* version */
  PyModule_AddObject(gdpy_mod, "__version__", Py_BuildValue("(iiis)",
        GETDATA_MAJOR, GETDATA_MINOR, GETDATA_REVISION,
        GETDATA_VERSION_SUFFIX));

  /* author */
  PyModule_AddStringConstant(gdpy_mod, "__author__",
      "The GetData Project <http://getdata.sourceforge.net/>");

  /* character_encoding */
  Py_INCREF(Py_None);
  PyModule_AddObject(gdpy_mod, "character_encoding", Py_None);

  /* add constants */
  for (i = 0; gdpy_constant_list[i].name != NULL; ++i)
    PyModule_AddIntConstant(gdpy_mod, gdpy_constant_list[i].name,
        gdpy_constant_list[i].value);

  PyModule_AddIntConstant(gdpy_mod, "__numpy_supported__", 1);

  /* add exceptions */
  dirfileerror = PyErr_NewExceptionWithDoc("pygetdata.DirfileError",
      "The base exception for all Dirfile-specific exceptions.",
      PyExc_RuntimeError, NULL);
  Py_INCREF(dirfileerror);
  PyModule_AddObject(gdpy_mod, "DirfileError", dirfileerror);

  for (i = 1; i < GD_N_ERROR_CODES; ++i) {
    if (gdpy_exception_list[i].name) {
      char name[40];
      sprintf(name, "pygetdata.%sError", gdpy_exception_list[i].name);
      gdpy_exceptions[i] = PyErr_NewExceptionWithDoc(name,
          gdpy_exception_list[i].doc, dirfileerror, NULL);
      Py_INCREF(gdpy_exceptions[i]);
      PyModule_AddObject(gdpy_mod, name + 10, gdpy_exceptions[i]);
    } else
      gdpy_exceptions[i] = dirfileerror;
  }

  /* add dead exceptions -- we do this through manual dictionary editing */
  mdict = PyModule_GetDict(gdpy_mod);
  if (mdict)
    for (i = 0; gdpy_dead_exceptions[i].name; ++i) {
      char name[40];
      sprintf(name, "%sError", gdpy_dead_exceptions[i].name);
      Py_INCREF(gdpy_exceptions[gdpy_dead_exceptions[i].e]);
      PyDict_SetItemString(mdict, name,
          gdpy_exceptions[gdpy_dead_exceptions[i].e]);
    }

  /* Also add an alias for AllocError */
  if (mdict) {
    Py_INCREF(PyExc_MemoryError);
    PyDict_SetItemString(mdict, "AllocError", PyExc_MemoryError);
  }

  /* Create the CAPI Capsule, if we can */
#ifdef PYGETDATA_CAPI
  gdpy_api[PyDirfile_Type_NUM] = &gdpy_dirfile;
  gdpy_api[PyDirfile_Dirfile_NUM] = &gdpy_dirfile_dirfile;
  gdpy_api[PyDirfile_Raise_NUM] = &gdpy_dirfile_raise;

  capi = PyCapsule_New(gdpy_api, PYDIRFILE_CAPSULENAME, NULL);
  
  if (capi)
    PyModule_AddObject(gdpy_mod, "__CAPI", capi);
#endif

  /* Tell GetData to use Python's heap (in places) */
  gd_alloc_funcs(PyMem_Malloc, PyMem_Free);

  GDPY_MODINITSUCCESS;
}
