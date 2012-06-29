/* Copyright (C) 2009-2012 D. V. Wiebe
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
#include "pygetdata.h"

static PyObject *GdPy_DirfileError;
static const char *gdpy_exception_list[GD_N_ERROR_CODES] = {
  NULL,
  "Open",
  "Format",
  "Truncate",
  "Creation",
  "BadCode",
  "BadType",
  "RawIO",
  "OpenFragment",
  "Internal",
  "Alloc",
  "Range",
  "OpenLinfile",
  "RecurseLevel",
  "BadDirfile",
  "BadFieldType",
  "AccessMode",
  "Unsupported",
  "UnknownEncoding",
  "BadEntry",
  "Duplicate",
  "Dimension",
  "BadIndex",
  "BadScalar",
  "BadReference",
  "Protected",
  "Deletion",
  "Argument",
  "Callback",
  "Exists",
  "UncleanDatabase",
  "Domain",
  "BadRepr",
  NULL,
  "Flush",
  "Bounds",
  "LineTooLong"
};
PyObject *gdpy_exceptions[GD_N_ERROR_CODES];

int gdpy_convert_from_pyobj(PyObject* value, union gdpy_quadruple_value *data,
    gd_type_t type)
{
  dtrace("%p, %p, %02x", value, data, type);
  int data_type;

  /* check value type, and figure out autotype, if needed */
  if (PyInt_Check(value)) {
    data->s = PyInt_AsLong(value);
    data_type = GDPY_INT_AS_LONG;

    if (PyErr_Occurred()) {
      dreturn("%i", -1);
      return -1;
    }
  } else if (PyLong_Check(value)) {
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
            if (PyErr_ExceptionMatches(PyExc_TypeError)) { /* still too small */
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
    data->c = gdpy_as_complex(value);
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

  dreturn("%02x", data_type);
  return data_type;
}

gd_type_t gdpy_convert_from_pylist(PyObject* value, void *data, gd_type_t type,
    size_t ns)
{
  size_t i;
  union gdpy_quadruple_value tmp;

  dtrace("%p, %p, %02x, %zi", value, data, type, ns);

  /* use the first element to determine the data type */
  int data_type = gdpy_convert_from_pyobj(PyList_GetItem(value, 0), &tmp, type);

  if (data_type == -1) {
    dreturn("%02x", GD_UNKNOWN);
    return GD_UNKNOWN;
  }

  switch(data_type) {
    case GDPY_INT_AS_LONG:
      type = GD_INT32;
      *(int32_t*)data = tmp.s;
      for (i = 1; i < ns; ++i)
        ((int32_t*)data)[i] = PyInt_AsLong(PyList_GetItem(value, i));
      break;
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
      *(complex double*)data = tmp.c;
      for (i = 1; i < ns; ++i)
        ((double complex*)data)[i] = gdpy_as_complex(PyList_GetItem(value, i));
      break;
  }

  dreturn("%02x", type);
  return type;
}

/* generic utitily functions */
#ifdef USE_NUMPY
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

  dreturn("0x%03x\n", type);
  return type;
}

int gdpy_npytype_from_type(gd_type_t type)
{
  int npytype;

  dtrace("0x%03x", type);

  switch(type)
  {
    case GD_UINT8:
      npytype = PyArray_UINT8;
      break;
    case GD_INT8:
      npytype = PyArray_INT8;
      break;
    case GD_UINT16:
      npytype = PyArray_UINT16;
      break;
    case GD_INT16:
      npytype = PyArray_INT16;
      break;
    case GD_UINT32:
      npytype = PyArray_UINT32;
      break;
    case GD_INT32:
      npytype = PyArray_INT32;
      break;
    case GD_UINT64:
      npytype = PyArray_UINT64;
      break;
    case GD_INT64:
      npytype = PyArray_INT64;
      break;
    case GD_FLOAT32:
      npytype = PyArray_FLOAT32;
      break;
    case GD_FLOAT64:
      npytype = PyArray_FLOAT64;
      break;
    case GD_COMPLEX64:
      npytype = PyArray_COMPLEX64;
      break;
    case GD_COMPLEX128:
      npytype = PyArray_COMPLEX128;
      break;
    default:
      npytype = NPY_NOTYPE;
      break;
  }

  dreturn("%i", npytype);
  return npytype;
}
#endif

PyObject* gdpy_convert_to_pylist(const void* data, gd_type_t type, size_t ns)
{
  size_t i;

  dtrace("%p, %02x, %zi", data, type, ns);

  PyObject* pylist;

  if (type != GD_NULL)
    pylist = PyList_New(0);

  switch(type) {
    case GD_NULL:
      Py_INCREF(Py_None);
      dreturn("%p", Py_None);
      return Py_None;
    case GD_UINT8:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist, PyInt_FromLong((long)((uint8_t*)data)[i])))
          return NULL;
      break;
    case GD_INT8:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist, PyInt_FromLong((long)((int8_t*)data)[i])))
          return NULL;
      break;
    case GD_UINT16:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist, PyInt_FromLong((long)((uint16_t*)data)[i])))
          return NULL;
      break;
    case GD_INT16:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist, PyInt_FromLong((long)((int16_t*)data)[i])))
          return NULL;
      break;
    case GD_UINT32:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist,
              PyLong_FromUnsignedLong((unsigned long)((uint32_t*)data)[i])))
          return NULL;
      break;
    case GD_INT32:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist, PyInt_FromLong((long)((int32_t*)data)[i])))
          return NULL;
      break;
    case GD_UINT64:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist, PyLong_FromUnsignedLongLong(
                (unsigned long long)((uint64_t*)data)[i])))
          return NULL;
      break;
    case GD_INT64:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist,
              PyLong_FromLongLong((long long)((int64_t*)data)[i])))
          return NULL;
      break;
    case GD_FLOAT32:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist,
              PyFloat_FromDouble((double)((float*)data)[i])))
          return NULL;
      break;
    case GD_FLOAT64:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist, PyFloat_FromDouble(((double*)data)[i])))
          return NULL;
      break;
    case GD_COMPLEX64:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist,
              gdpy_from_complex((double complex)((float complex*)data)[i])))
          return NULL;
      break;
    case GD_COMPLEX128:
      for (i = 0; i < ns; ++i)
        if (PyList_Append(pylist,
              gdpy_from_complex(((double complex*)data)[i])))
          return NULL;
      break;
    case GD_UNKNOWN: /* prevent compiler warning */
      break;
  }

  dreturn("%p", pylist);
  return pylist;
}

PyObject* gdpy_convert_to_pyobj(const void* data, gd_type_t type)
{
  dtrace("%p, %02x", data, type);
  PyObject* pyobj = NULL;

  switch(type) {
    case GD_NULL:
      Py_INCREF(Py_None);
      pyobj = Py_None;
      break;
    case GD_UINT8:
      pyobj = PyInt_FromLong((long)*(uint8_t*)data);
      break;
    case GD_INT8:
      pyobj = PyInt_FromLong((long)*(int8_t*)data);
      break;
    case GD_UINT16:
      pyobj = PyInt_FromLong((long)*(uint16_t*)data);
      break;
    case GD_INT16:
      pyobj = PyInt_FromLong((long)*(int16_t*)data);
      break;
    case GD_UINT32:
      pyobj = PyLong_FromUnsignedLong((unsigned long)*(uint32_t*)data);
      break;
    case GD_INT32:
      pyobj = PyInt_FromLong((long)*(int32_t*)data);
      break;
    case GD_UINT64:
      pyobj = PyLong_FromUnsignedLongLong((unsigned long long)*(uint64_t*)data);
      break;
    case GD_INT64:
      pyobj = PyLong_FromLongLong((long long)*(int64_t*)data);
      break;
    case GD_FLOAT32:
      pyobj = PyFloat_FromDouble((double)*(float*)data);
      break;
    case GD_FLOAT64:
      pyobj = PyFloat_FromDouble(*(double*)data);
      break;
    case GD_COMPLEX64:
      pyobj = gdpy_from_complex((double complex)*(float complex*)data);
      break;
    case GD_COMPLEX128:
      pyobj = gdpy_from_complex(*(double complex*)data);
      break;
    case GD_UNKNOWN: /* prevent compiler warning */
      break;
  }

  dreturn("%p", pyobj);
  return pyobj;
}

/* GetData */
static PyMethodDef GetDataMethods[] = {
  { NULL, NULL, 0, NULL }
};

PyMODINIT_FUNC initpygetdata(void)
{
  int i;
  PyObject* mod;

  if (PyType_Ready(&gdpy_dirfile) < 0)
    return;

  if (PyType_Ready(&gdpy_entry) < 0)
    return;

  if (PyType_Ready(&gdpy_fragment) < 0)
    return;

#ifdef USE_NUMPY
  /* The following macro will cause this function to return if importing numpy
   * fails */
  import_array()
#endif

  mod = Py_InitModule3("pygetdata", GetDataMethods,
      "Bindings to the GetData library for Dirfile access\n\n"
      "This module provides interfaces to the C GetData library.  It defines "
      "three\nmain classes:\n\n"
      "  o dirfile, encapsulating the C API's DIRFILE object,\n"
      "  o entry, encapsulating the C API's gd_entry_t object, and\n"
      "  o fragment, containing fragment metadata.\n\n"
      "Second, it defines various symbolic constants defined by the C API.  "
      "These\nsymbols are identical to the C API's symbols, except lacking the "
      "GD_ prefix.\nSo, for example, the C API's GD_INT8 is available in these "
      "bindings as\npygetdata.INT8.\n\n"
      "Finally, it defines a number of exceptions corresponding to C API "
      "dirfile\nerror codes.  These exceptions have similar names to the C "
      "API's error\nnames, so, for example, pygetdata.BadCodeError corresponds "
      "to the C API's\nGD_E_BAD_CODE error code.  All these exceptions are "
      "derived from a common\npygetdata.DirfileError exception class, itself "
      "derived from RuntimeError.\nExceptions are thrown by the bindings in "
      "lieu of returning a dirfile error\nvalue.\n\n"
      "Where possible, pygetdata will, by default, return vector data as "
      "NumPy\narrays.  If "
      "pygetdata has been built with NumPy support,\n"
      "pygetdata.__numpy_supported__ will be non-zero.  If NumPy support is "
      "not\npresent, vector data will be returned as Python lists.  Vector "
      "data passed\nto pygetdata may either be a Python list or a NumPy array."
      "\n\n"
      "The input data type argument to bindings for functions such as\n"
      "gd_putdata(3), which is required in the C API, are typically optional,\n"
      "as pygetdata can determine the input data type by itself, and convert "
      "it to\nan appropriate type for the C API.  If the data type is supplied,"
      " pygetdata\nwill coerce the input data to the specified C type as best "
      "it can.  For\ngd_getdata(3) and similar, the C API types are converted "
      "to Python types as\nfollows:\n\n"
      "  o int     -- UINT8, INT8, UINT16, INT16, INT32\n"
      "  o long    -- UINT32, UINT64, INT64\n"
      "  o float   -- FLOAT32, FLOAT64\n"
      "  o complex -- COMPLEX64, COMPLEX128\n\n"
      "or to NumPy data types, as appropriate.  "
      "For convenience, the following type\ncode aliases are defined:\n\n"
      "  o pygetdata.INT     = pygetdata.INT32\n"
      "  o pygetdata.LONG    = pygetdata.INT64\n"
      "  o pygetdata.ULONG   = pygetdata.UINT64\n"
      "  o pygetdata.FLOAT   = pygetdata.FLOAT64\n"
      "  o pygetdata.COMPLEX = pygetdata.COMPLEX128\n\n"
      "Note that pygetdata.FLOAT is different than the C API's GD_FLOAT "
      "alias.\n\n"
      "All pygetdata functions may be given positional or keyword parameters."
      );

  if (mod == NULL)
    return;

  Py_INCREF(&gdpy_dirfile);
  PyModule_AddObject(mod, "dirfile", (PyObject *)&gdpy_dirfile);

  Py_INCREF(&gdpy_entry);
  PyModule_AddObject(mod, "entry", (PyObject *)&gdpy_entry);

  Py_INCREF(&gdpy_fragment);
  PyModule_AddObject(mod, "fragment", (PyObject *)&gdpy_fragment);

  /* version */
  PyModule_AddObject(mod, "__version__", Py_BuildValue("(iiis)", GETDATA_MAJOR,
        GETDATA_MINOR, GETDATA_REVISION, GETDATA_VERSION_SUFFIX));

  /* author */
  PyModule_AddStringConstant(mod, "__author__",
      "D. V. Wiebe <getdata@ketiltrout.net>");

  /* add constants */
  for (i = 0; gdpy_constant_list[i].name != NULL; ++i)
    PyModule_AddIntConstant(mod, gdpy_constant_list[i].name,
        gdpy_constant_list[i].value);

  PyModule_AddIntConstant(mod, "__numpy_supported__",
#ifdef USE_NUMPY
      1
#else
      0
#endif
      );

  /* add exceptions */
  GdPy_DirfileError = PyErr_NewException("pygetdata.DirfileError",
      PyExc_RuntimeError, NULL);
  Py_INCREF(GdPy_DirfileError);
  PyModule_AddObject(mod, "DirfileError", GdPy_DirfileError);

  for (i = 1; i < GD_N_ERROR_CODES; ++i) {
    if (gdpy_exception_list[i]) {
      char name[40];
      sprintf(name, "pygetdata.%sError", gdpy_exception_list[i]);
      gdpy_exceptions[i] = PyErr_NewException(name, GdPy_DirfileError, NULL);
      Py_INCREF(gdpy_exceptions[i]);
      PyModule_AddObject(mod, name + 10, gdpy_exceptions[i]);
    } else
      gdpy_exceptions[i] = GdPy_DirfileError;
  }
}
