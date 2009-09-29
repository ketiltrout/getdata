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
  "OpenInclude",
  "InternalError",
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
  "BadEndianess",
  "Callback",
  "BadProtection",
  "UncleanDatabase",
  "Domain",
  "BadRepr"
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
  } else { /* an non numeric type */
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
  dtrace("%p, %p, %02x, %zi", value, data, type, ns);

  size_t i;

  /* use the first element to determine the data type */
  int data_type = gdpy_convert_from_pyobj(PyList_GetItem(value, 0),
      (union gdpy_quadruple_value*)data, type);

  if (data_type == -1) {
    dreturn("%02x", GD_UNKNOWN);
    return GD_UNKNOWN;
  }

  switch(data_type) {
    case GDPY_INT_AS_LONG:
      type = GD_INT64;
      for (i = 1; i < ns; ++i)
        ((int64_t*)data)[i] = PyInt_AsLong(PyList_GetItem(value, i));
      break;
    case GDPY_LONG_AS_ULL: 
      type = GD_UINT64;
      for (i = 1; i < ns; ++i)
        ((uint64_t*)data)[i] = PyLong_AsUnsignedLongLong(PyList_GetItem(value,
            i));
      break;
    case GDPY_LONG_AS_SLL: 
      type = GD_INT64;
      for (i = 1; i < ns; ++i)
        ((int64_t*)data)[i] = PyLong_AsLongLong(PyList_GetItem(value, i));
      break;
    case GDPY_LONG_AS_DOUBLE: 
      type = GD_FLOAT64;
      for (i = 1; i < ns; ++i)
        ((double*)data)[i] = PyLong_AsDouble(PyList_GetItem(value, i));
      break;
    case GDPY_FLOAT_AS_DOUBLE:
      type = GD_FLOAT64;
      for (i = 1; i < ns; ++i)
        ((double*)data)[i] = PyFloat_AsDouble(PyList_GetItem(value, i));
      break;
    case GDPY_COMPLEX_AS_COMPLEX:
      type = GD_COMPLEX128;
      for (i = 1; i < ns; ++i)
        ((double complex*)data)[i] = gdpy_as_complex(PyList_GetItem(value, i));
      break;
  }

  dreturn("%02x", type);
  return type;
}

/* generic utitily functions */
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

  mod = Py_InitModule3("pygetdata", GetDataMethods,
      "Bindings to the GetData library for Dirfile access\n\n"
      "This module provides interfaces to the C GetData library.");
  if (mod == NULL)
    return;

  Py_INCREF(&gdpy_dirfile);
  PyModule_AddObject(mod, "dirfile", (PyObject *)&gdpy_dirfile);

  Py_INCREF(&gdpy_entry);
  PyModule_AddObject(mod, "entry", (PyObject *)&gdpy_entry);

  Py_INCREF(&gdpy_fragment);
  PyModule_AddObject(mod, "fragment", (PyObject *)&gdpy_fragment);

  /* version */
  PyModule_AddObject(mod, "__version__", Py_BuildValue("(iii)", GETDATA_MAJOR,
        GETDATA_MINOR, GETDATA_REVISION));

  /* author */
  PyModule_AddStringConstant(mod, "__author__",
      "D. V. Wiebe <getdata@ketiltrout.net>");

  /* add constants */
  for (i = 0; gdpy_constant_list[i].name != NULL; ++i)
    PyModule_AddIntConstant(mod, gdpy_constant_list[i].name,
        gdpy_constant_list[i].value);

  /* add exceptions */
  GdPy_DirfileError = PyErr_NewException("getdata.DirfileError",
      PyExc_RuntimeError, NULL);
  Py_INCREF(GdPy_DirfileError);
  PyModule_AddObject(mod, "DirfileError", GdPy_DirfileError);

  for (i = 1; i < GD_N_ERROR_CODES; ++i) {
    char name[40];
    sprintf(name, "getdata.%sError", gdpy_exception_list[i]);
    gdpy_exceptions[i] = PyErr_NewException(name, GdPy_DirfileError, NULL);
    Py_INCREF(gdpy_exceptions[i]);
    sprintf(name, "%sError", gdpy_exception_list[i]);
    PyModule_AddObject(mod, name, gdpy_exceptions[i]);
  }
}
