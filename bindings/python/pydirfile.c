/* (C) 2009, 2010 D. V. Wiebe
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
#define NO_IMPORT_ARRAY
#include "pygetdata.h"

/* Dirfile */
static int gdpy_callback_func(gd_parser_data_t* pdata, void* extra)
{
  dtrace("%p, %p", pdata, extra);

  int r = GD_SYNTAX_ABORT;
  struct gdpy_dirfile_t* self = extra;

  if (self->callback != NULL) {
    char buffer[GD_MAX_LINE_LENGTH];
    gd_error_string(pdata->dirfile, buffer, GD_MAX_LINE_LENGTH);

    PyObject* arglist = Py_BuildValue("({sssisssiss}O)", "error_string", buffer,
        "suberror", pdata->suberror, "line", pdata->line, "linenum",
        pdata->linenum, "filename", pdata->filename, self->callback_data);

    /* an exception results in an abort */
    if (arglist == NULL) {
      self->callback_exception = 1;
      dreturn("%i", GD_SYNTAX_ABORT);
      return GD_SYNTAX_ABORT;
    }

    PyObject* result = PyEval_CallObject(self->callback, arglist);
    Py_DECREF(arglist);

    /* result may be:
     * - an Int -- the return code; line not changed
     * - a String - the new line -- GD_SYNTAX_RESCAN is assumed
     * - a Tuple containing an Int and then, optionally, a String
     */
    if (result == NULL)
      self->callback_exception = 1;
    else if (PyTuple_Check(result)) {
      switch (PyTuple_Size(result))
      {
        case 0:
          PyErr_SetString(PyExc_TypeError,
              "callback must return at least one object");
          self->callback_exception = 1;
          break;
        case 1:
          r = (int)PyInt_AsLong(PyTuple_GetItem(result, 0));

          if (PyErr_Occurred()) {
            self->callback_exception = 1;
            r = GD_SYNTAX_ABORT;
          }
          break;
        default:
          r = (int)PyInt_AsLong(PyTuple_GetItem(result, 0));

          if (PyErr_Occurred()) {
            self->callback_exception = 1;
            r = GD_SYNTAX_ABORT;
          }

          char* new_string = PyString_AsString(PyTuple_GetItem(result, 1));

          if (new_string == NULL) {
            self->callback_exception = 1;
            r = GD_SYNTAX_ABORT;
          }
          
          strncpy(pdata->line, new_string, GD_MAX_LINE_LENGTH - 1);
          pdata->line[GD_MAX_LINE_LENGTH - 1] = '\0';
      }

      if (PyTuple_Size(result) == 1) {
      }
    } else if (PyString_Check(result)) {
      char* new_string = PyString_AsString(result);

      if (new_string == NULL) {
        self->callback_exception = 1;
        r = GD_SYNTAX_ABORT;
      }
          
      r = GD_SYNTAX_RESCAN;
      strncpy(pdata->line, new_string, GD_MAX_LINE_LENGTH - 1);
      pdata->line[GD_MAX_LINE_LENGTH - 1] = '\0';
    } else if (PyInt_Check(result))
      r = (int)PyInt_AsLong(result);
    else {
      PyErr_SetString(PyExc_TypeError,
          "bad return type from callback function");
      self->callback_exception = 1;
    }
  }

  dreturn("%i", r);
  return r;
}

static void gdpy_dirfile_delete(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  gd_close(self->D);

  dreturnvoid();
}

static PyObject* gdpy_dirfile_create(PyTypeObject *type, PyObject *args,
    PyObject *keys)
{
  dtrace("%p, %p, %p", type, args, keys);

  struct gdpy_dirfile_t *self = (struct gdpy_dirfile_t*)type->tp_alloc(type, 0);

  if (self) {
    self->D = NULL;
    self->callback = NULL;
    self->callback_data = NULL;
  }

  dreturn("%p", self);
  return (PyObject*)self;
}

static int gdpy_dirfile_init(struct gdpy_dirfile_t* self, PyObject *args,
    PyObject *keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  PyObject* pycallback = NULL;
  PyObject* pycallback_data = Py_None;
  char *keywords[] = {"name", "flags", "callback", "extra", NULL};
  PyObject* name = NULL;
  unsigned long flags = GD_RDWR;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "|OkOO:pygetdata.dirfile.__init__", keywords, &name, &flags,
        &pycallback, &pycallback_data))
  {
    dreturn("%i", -1);
    return -1;
  }

  /* An invalid dirfile was requested */
  if (name == NULL || name == Py_None) {
    self->D = gd_invalid_dirfile();

    PYGD_CHECK_ERROR(self->D, -1);

    dreturn("%i", 0);
    return 0;
  }

  if (!PyString_Check(name)) {
    PyErr_SetString(PyExc_TypeError, "name must be a string or None");
    dreturn("%i", -1);
    return -1;
  }

  if (pycallback && pycallback != Py_None && !PyCallable_Check(pycallback)) {
    PyErr_SetString(PyExc_TypeError, "callback function must be callable");
    dreturn("%i", -1);
    return -1;
  }

  Py_XINCREF(pycallback);
  Py_XINCREF(pycallback_data);
  Py_XDECREF(self->callback);
  Py_XDECREF(self->callback_data);
  self->callback = pycallback;
  self->callback_data = pycallback_data;
  self->callback_exception = 0;

  self->D = gd_cbopen(PyString_AsString(name), (unsigned int)flags,
      (pycallback == NULL) ? NULL : gdpy_callback_func, self);

  if (self->callback_exception) {
    dreturn("%i", -1);
    return -1;
  }

  PYGD_CHECK_ERROR(self->D, -1);

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_dirfile_add(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "entry", NULL };
  struct gdpy_entry_t* entry = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "O!:pygetdata.dirfile.add",
        keywords, &gdpy_entry, &entry))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_add(self->D, entry->E);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_addspec(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "spec", "fragment_index", NULL };
  const char* spec;
  int fragment = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s|i:pygetdata.dirfile.add_spec",
        keywords, &spec, &fragment))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_add_spec(self->D, spec, fragment);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_alter(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", "entry", "recode", NULL };
  struct gdpy_entry_t* entry = NULL;
  int recode = 0;
  char *field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "sO!|i:pygetdata.dirfile.alter",
        keywords, &field_code, &gdpy_entry, &entry, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_alter_entry(self->D, field_code, entry->E, recode);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_alterspec(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "spec", "recode", NULL };
  const char* spec;
  int recode = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s|i:pygetdata.dirfile.alter_spec", keywords, &spec, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_alter_spec(self->D, spec, recode);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_close(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  if (gd_close(self->D))
    PYGD_CHECK_ERROR(self->D, NULL);

  self->D = gd_invalid_dirfile();

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_delentry(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"field_code", "flags", NULL};
  const char* field_code;
  int flags = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s|i:pygetdata.dirfile.delete", keywords, &field_code, &flags))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_delete(self->D, field_code, flags);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_discard(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  if (gd_discard(self->D)) {
    PYGD_CHECK_ERROR(self->D, NULL);
  }

  /* Here we replace D with an empty, invalid dirfile object.  */
  self->D = gd_invalid_dirfile();

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_getcarray(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"field_code", "return_type", "start", "len", "as_list",
    NULL};
  const char* field_code;
  unsigned int start = 0, len = 0;
  int as_list = 0;
  gd_type_t return_type;
  PyObject* pylist = NULL;
#ifdef USE_NUMPY
  npy_intp dims[] = { 0 };
#endif

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si|IIi:pygetdata.dirfile.get_carray", keywords, &field_code,
        &return_type, &start, &len, &as_list))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if (len == 0) {
    len = gd_carray_len(self->D, field_code);
    if (len > start)
      len -= start;
    else
      len = 0;
  }

  if (len == 0) {
#ifdef USE_NUMPY
    if (!as_list)
      pylist = PyArray_ZEROS(1, dims, NPY_INT, 0);
    else
#endif
      pylist = Py_BuildValue("[]");
  } else {
    void* data;
#ifdef USE_NUMPY
    if (!as_list) {
      dims[0] = (npy_intp)len;
      pylist = PyArray_SimpleNew(1, dims, gdpy_npytype_from_type(return_type));
      data = PyArray_DATA(pylist);
    } else
#endif
      data = malloc(len * GD_SIZE(return_type));

    gd_get_carray_slice(self->D, field_code, start, (size_t)len, return_type,
        data);

#ifdef USE_NUMPY
    if (!as_list)
      PYGD_CHECK_ERROR(self->D, NULL);
    else
#endif
    {
      PYGD_CHECK_ERROR2(self->D, NULL, free(data));
      pylist = gdpy_convert_to_pylist(data, return_type, len);

      free(data);
    }
  }

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getconstant(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"field_code", "return_type", NULL};
  const char* field_code;
  gd_type_t return_type;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si:pygetdata.dirfile.get_constant", keywords, &field_code,
        &return_type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  char data[16];

  gd_get_constant(self->D, field_code, return_type, data);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = gdpy_convert_to_pyobj(data, return_type);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_carraylen(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:pygetdata.dirfile.carray_len",
        keywords, &field_code)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  size_t len = gd_carray_len(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong((long)len);
  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_carrays(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"return_type", "as_list", NULL};
  const char **fields;
  int as_list = 0, i;
  gd_type_t return_type;
#ifdef USE_NUMPY
  npy_intp dims[] = { 0 };
#endif

  if (!PyArg_ParseTupleAndKeywords(args, keys, "i|i:pygetdata.dirfile.carrays",
        keywords, &return_type, &as_list))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = gd_field_list_by_type(self->D, GD_CARRAY_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  const gd_carray_t *carrays = gd_carrays(self->D, return_type);

  PyObject *pylist = PyList_New(0);

  for (i = 0; carrays[i].n != 0; ++i) {
    PyObject *pydata;
#ifdef USE_NUMPY
    if (!as_list) {
      dims[0] = (npy_intp)carrays[i].n;
      pydata = PyArray_SimpleNew(1, dims, gdpy_npytype_from_type(return_type));
      memcpy(PyArray_DATA(pydata), carrays[i].d, GD_SIZE(return_type) *
          carrays[i].n);
    } else
#endif
      pydata = gdpy_convert_to_pylist(carrays[i].d, return_type, carrays[i].n);

    PyList_Append(pylist, Py_BuildValue("sN", fields[i], pydata));
  }

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getconstants(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  int i;
  char* keywords[] = {"return_type", NULL};
  const char** fields;
  const char* values;
  gd_type_t return_type;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "i:pygetdata.dirfile.constants", keywords, &return_type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = gd_field_list_by_type(self->D, GD_CONST_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = gd_constants(self->D, return_type);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* list = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(list, Py_BuildValue("sN", fields[i],
          gdpy_convert_to_pyobj(values + i * GD_SIZE(return_type),
            return_type)));

  dreturn("%p", list);
  return list;
}

static PyObject* gdpy_dirfile_getdata(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", "return_type", "first_frame",
    "first_sample", "num_frames", "num_samples", "as_list", NULL };
  const char* field_code;
  PY_LONG_LONG first_frame = 0, first_sample = 0;
  long int num_frames = 0, num_samples = 0;
  int as_list = 0;
  gd_type_t return_type;
  gd_spf_t spf = 1;
  PyObject* pylist = NULL;
#ifdef USE_NUMPY
  npy_intp dims[] = { 0 };
#endif

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si|LLlli:pygetdata.dirfile.getdata", keywords, &field_code,
        &return_type, &first_frame, &first_sample, &num_frames, &num_samples,
        &as_list))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  /* we need the SPF to know how many samples we have to allocate */
  if (num_frames) {
    spf = gd_spf(self->D, field_code);

    PYGD_CHECK_ERROR(self->D, NULL);
  }

  size_t ns = num_samples + num_frames * spf;

  if (ns == 0) {
#ifdef USE_NUMPY
    if (!as_list)
      pylist = PyArray_ZEROS(1, dims, NPY_INT, 0);
    else
#endif
      pylist = Py_BuildValue("[]");
  } else {
    void* data;
#ifdef USE_NUMPY
    if (!as_list) {
      dims[0] = (npy_intp)ns;
      pylist = PyArray_SimpleNew(1, dims, gdpy_npytype_from_type(return_type));
      data = PyArray_DATA(pylist);
    } else
#endif
      data = malloc(ns * GD_SIZE(return_type));

    ns = gd_getdata(self->D, field_code, first_frame, first_sample,
        (size_t)num_frames, (size_t)num_samples, return_type, data);


#ifdef USE_NUMPY
    if (!as_list)
      PYGD_CHECK_ERROR(self->D, NULL);
    else
#endif
    {
      PYGD_CHECK_ERROR2(self->D, NULL, free(data));
      pylist = gdpy_convert_to_pylist(data, return_type, ns);

      free(data);
    }
  }

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getentry(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"field_code", NULL};
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:pygetdata.dirfile.entry",
        keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* E = malloc(sizeof(gd_entry_t));

  if (E == NULL) {
    PyErr_NoMemory();
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry(self->D, field_code, E);

  PYGD_CHECK_ERROR(self->D, NULL);

  struct gdpy_entry_t *obj =
    (struct gdpy_entry_t*)gdpy_entry.tp_alloc(&gdpy_entry, 0);

  if (obj == NULL) {
    PyErr_NoMemory();
    dreturn("%p", NULL);
    return NULL;
  }

  obj->E = E;
  Py_INCREF(obj);
  dreturn("%p", obj);
  return (PyObject*)obj;
}

static PyObject* gdpy_dirfile_geterror(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  PyObject* error = PyInt_FromLong(gd_error(self->D));

  dreturn("%p", error);
  return error;
}

static PyObject* gdpy_dirfile_getfragment(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"fragment_index", NULL};
  int fragment_index;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "i:pygetdata.dirfile.fragment",
        keywords, &fragment_index))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  struct gdpy_fragment_t *obj =
    (struct gdpy_fragment_t*)gdpy_fragment.tp_alloc(&gdpy_fragment, 0);

  if (obj == NULL) {
    PyErr_NoMemory();
    dreturn("%p", NULL);
    return NULL;
  }

  obj->n = fragment_index;
  obj->dirfile = self;

  Py_INCREF(obj);
  dreturn("%p", obj);
  return (PyObject*)obj;
}

static PyObject* gdpy_dirfile_getfragmentindex(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"field_code", NULL};
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.fragment_index", keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  int index = gd_fragment_index(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong(index);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_geterrorstring(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  char buffer[GD_MAX_LINE_LENGTH];

  PyObject* error = PyString_FromString(gd_error_string(self->D, buffer,
        GD_MAX_LINE_LENGTH));

  dreturn("%p", error);
  return error;
}

static PyObject* gdpy_dirfile_getvectorlist(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  int i;

  const char **vectors = gd_vector_list(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pylist = PyList_New(0);

  for (i = 0; vectors[i] != NULL; ++i)
    PyList_Append(pylist, PyString_FromString(vectors[i]));

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getfieldlist(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  const char **fields;
  char* keywords[] = { "type", NULL };
  int i, type = (int)GD_NO_ENTRY;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "|i:pygetdata.dirfile.field_list", keywords, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if (type == GD_NO_ENTRY)
    fields = gd_field_list(self->D);
  else
    fields = gd_field_list_by_type(self->D, (gd_type_t)type);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pylist = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(pylist, PyString_FromString(fields[i]));

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_flush(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "|s:pygetdata.dirfile.flush",
        keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_flush(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_include(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "file", "fragment_index", "flags", NULL };
  const char* file = NULL;
  int fragment_index = 0;
  unsigned int flags = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s|ii:pygetdata.dirfile.include",
        keywords, &file, &fragment_index, &flags))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  self->callback_exception = 0;

  long index = gd_include(self->D, file, fragment_index, flags);

  if (self->callback_exception) {
    dreturn("%p", NULL);
    return NULL;
  }

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* obj = PyInt_FromLong(index);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_dirfile_madd(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "entry", "parent", NULL };
  struct gdpy_entry_t* entry = NULL;
  const char* parent;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "O!s:pygetdata.dirfile.madd",
        keywords, &gdpy_entry, &entry, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_madd(self->D, entry->E, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_maddspec(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "spec", "parent", NULL };
  const char* spec;
  const char* parent;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "ss:pygetdata.dirfile.madd_spec",
        keywords, &spec, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_madd_spec(self->D, spec, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_malterspec(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "spec", "parent", "recode", NULL };
  const char *spec, *parent;
  int recode = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "ss|i:pygetdata.dirfile.malter_spec", keywords, &spec, &parent,
        &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_malter_spec(self->D, spec, parent, recode);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_mcarrays(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"parent", "return_type", "as_list", NULL};
  const char **fields;
  const char *parent;
  int as_list = 0, i;
  gd_type_t return_type;
#ifdef USE_NUMPY
  npy_intp dims[] = { 0 };
#endif

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si|i:pygetdata.dirfile.mcarrays", keywords, &parent, &return_type,
        &as_list))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = gd_mfield_list_by_type(self->D, parent, GD_CARRAY_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  const gd_carray_t *carrays = gd_mcarrays(self->D, parent, return_type);

  PyObject *pylist = PyList_New(0);

  for (i = 0; carrays[i].n != 0; ++i) {
    PyObject *pydata;
#ifdef USE_NUMPY
    if (!as_list) {
      dims[0] = (npy_intp)carrays[i].n;
      pydata = PyArray_SimpleNew(1, dims, gdpy_npytype_from_type(return_type));
      memcpy(PyArray_DATA(pydata), carrays[i].d, GD_SIZE(return_type) *
          carrays[i].n);
    } else
#endif
      pydata = gdpy_convert_to_pylist(carrays[i].d, return_type, carrays[i].n);

    PyList_Append(pylist, Py_BuildValue("sN", fields[i], pydata));
  }

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getmconstants(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  int i;
  char* keywords[] = {"parent", "return_type", NULL};
  const char** fields;
  const char* values;
  const char* parent = NULL;
  gd_type_t return_type;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si:pygetdata.dirfile.mconstants", keywords, &parent, &return_type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = gd_mfield_list_by_type(self->D, parent, GD_CONST_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = gd_mconstants(self->D, parent, return_type);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* list = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(list, Py_BuildValue("sN", fields[i],
          gdpy_convert_to_pyobj(values + i * GD_SIZE(return_type),
            return_type)));

  dreturn("%p", list);
  return list;
}

static PyObject* gdpy_dirfile_metaflush(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  gd_metaflush(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_getmfieldlist(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  const char** fields;
  char* keywords[] = { "parent", "type", NULL };
  const char* parent = NULL;
  gd_entype_t type = GD_NO_ENTRY;
  int i;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s|i:pygetdata.dirfile.field_list_by_type", keywords, &parent, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if (type == GD_NO_ENTRY)
    fields = gd_mfield_list(self->D, parent);
  else
    fields = gd_mfield_list_by_type(self->D, parent, (gd_type_t)type);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pylist = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(pylist, PyString_FromString(fields[i]));

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getname(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  const char* name = gd_dirfilename(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyname = PyString_FromString(name);

  dreturn("%p", pyname);
  return pyname;
}

static PyObject* gdpy_dirfile_getmstrings(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  int i;
  char* keywords[] = {"parent", NULL};
  const char** fields;
  const char** values;
  const char* parent = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.mconstants", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = gd_mfield_list_by_type(self->D, parent, GD_STRING_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = gd_mstrings(self->D, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* list = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(list, Py_BuildValue("ss", fields[i], values[i]));

  dreturn("%p", list);
  return list;
}

static PyObject* gdpy_dirfile_getmvectorlist(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  int i;

  char* keywords[] = {"parent", NULL};
  const char* parent = NULL;
  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.mvector_list", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  const char **fields = gd_mvector_list(self->D, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pylist = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(pylist, PyString_FromString(fields[i]));

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getrawfilename(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;
  const char* filename;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.raw_filename", keywords, &field_code))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  filename = gd_raw_filename(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyString_FromString(filename);
  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getnativetype(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.native_type", keywords, &field_code))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_type_t ntype = gd_native_type(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong((long)ntype);
  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getnativetypename(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;
  char tbuffer[11];

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.native_type_name", keywords, &field_code))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_type_t t = gd_native_type(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  sprintf(tbuffer, "%s%i", ((t & GD_COMPLEX) ? "COMPLEX" :
        (t & GD_IEEE754) ? "FLOAT" : (t & GD_SIGNED) ?  "INT" : "UINT"),
      (int)(8 * GD_SIZE(t)));

  PyObject* pyobj = PyString_FromString(tbuffer);
  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getnfields(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  unsigned int nfields;
  char* keywords[] = { "type", NULL };
  int type = GD_NO_ENTRY;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "|i:pygetdata.dirfile.nfields",
        keywords, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if (type == GD_NO_ENTRY)
    nfields = gd_nfields(self->D);
  else
    nfields = gd_nfields_by_type(self->D, (gd_entype_t)type);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynfields = PyInt_FromLong((long)nfields);

  dreturn("%p", pynfields);
  return pynfields;
}

static PyObject* gdpy_dirfile_getnfragments(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  long nfragments = gd_nfragments(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynfragments = PyInt_FromLong(nfragments);

  dreturn("%p", pynfragments);
  return pynfragments;
}

static PyObject* gdpy_dirfile_getnframes(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  off_t nframes = gd_nframes(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynframes = PyLong_FromLongLong(nframes);

  dreturn("%p", pynframes);
  return pynframes;
}

static PyObject* gdpy_dirfile_getnmfields(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "parent", "type", NULL };
  const char* parent = NULL;
  int type = GD_NO_ENTRY;
  unsigned int nmfields;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s|i:pygetdata.dirfile.nmfields",
        keywords, &parent, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if (type == GD_NO_ENTRY)
    nmfields = gd_nmfields(self->D, parent);
  else
    nmfields = gd_nmfields_by_type(self->D, parent, (gd_type_t)type);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong((long)nmfields);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getnmvectors(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "parent", NULL };
  const char* parent = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.nmvectors", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  unsigned int nmvectors = gd_nmvectors(self->D, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong((long)nmvectors);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getbof(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.bof", keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  off_t bof = gd_bof(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pybof = PyLong_FromLongLong(bof);

  dreturn("%p", pybof);
  return pybof;
}

static PyObject* gdpy_dirfile_geteof(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:pygetdata.dirfile.eof", keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  off_t eof = gd_eof(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyeof = PyLong_FromLongLong(eof);

  dreturn("%p", pyeof);
  return pyeof;
}

static PyObject* gdpy_dirfile_getnvectors(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  unsigned int nvectors = gd_nvectors(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynvectors = PyInt_FromLong((long)nvectors);

  dreturn("%p", pynvectors);
  return pynvectors;
}

static PyObject* gdpy_dirfile_getreference(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  const char *ref = gd_reference(self->D, NULL);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyref = PyString_FromString(ref);

  dreturn("%p", pyref);
  return pyref;
}

static int gdpy_dirfile_setreference(struct gdpy_dirfile_t* self,
    PyObject *value, void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  const char* ref = PyString_AsString(value);

  /* TypeError already raised on error */
  if (ref == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  gd_reference(self->D, ref);

  PYGD_CHECK_ERROR(self->D, -1);

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_dirfile_getstring(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:pygetdata.dirfile.get_string",
        keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  char data[GD_MAX_LINE_LENGTH];

  gd_get_string(self->D, field_code, GD_MAX_LINE_LENGTH, data);

  PYGD_CHECK_ERROR(self->D, NULL);

  /* using the \u escape, it is possible to create a string longer than
   * GD_MAX_LINE_LENGTH -- this ensures we remain NULL terminated */
  data[GD_MAX_LINE_LENGTH - 1] = 0;

  PyObject* pyobj = PyString_FromString(data);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getstrings(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  int i;
  const char** fields;
  const char** values;

  fields = gd_field_list_by_type(self->D, GD_STRING_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = gd_strings(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* list = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(list, Py_BuildValue("ss", fields[i], values[i]));

  dreturn("%p", list);
  return list;
}

static PyObject* gdpy_dirfile_putconstant(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"field_code", "value", "type", NULL};
  const char* field_code;
  PyObject* value;
  gd_type_t type = GD_UNKNOWN;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "sO|i:pygetdata.dirfile.put_constant", keywords, &field_code, &value,
        &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  union gdpy_quadruple_value data;
  int data_type = gdpy_convert_from_pyobj(value, &data, type);

  if (data_type == -1) {
    dreturn("%p", NULL);
    return NULL;
  }

  if ((data_type & 0xf) == GDPY_SIGNED)
    gd_put_constant(self->D, field_code, GD_INT64, &data.s);
  else if ((data_type & 0xf) == GDPY_IEEE754)
    gd_put_constant(self->D, field_code, GD_FLOAT64, &data.f);
  else if ((data_type & 0xf) == GDPY_COMPLEX)
    gd_put_constant(self->D, field_code, GD_COMPLEX128, &data.c);
  else
    gd_put_constant(self->D, field_code, GD_UINT64, &data.u);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_putcarray(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", "data", "type", "start", NULL };
  const char* field_code;
  unsigned int start = 0, len;
  gd_type_t type = GD_UNKNOWN;
  PyObject* pyobj;
  size_t ns;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "sO|iI:pygetdata.dirfile.putdata", keywords, &field_code, &pyobj,
        &type, &start)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  /* we only handle list or ndarray data */
#ifdef USE_NUMPY
  int have_ndarray = 0;
  if (PyArray_Check(pyobj)) {
    if (PyArray_NDIM(pyobj) != 1) {
      PyErr_SetString(PyExc_ValueError,
          "pygetdata.dirfile.put_carray() argument 2 must be one dimensional");
      dreturn("%p", NULL);
      return NULL;
    }
    have_ndarray = 1;
    len = PyArray_DIM(pyobj, 0);
  } else
#endif
  {
    if (!PyList_Check(pyobj)) {
      PyErr_SetString(PyExc_TypeError,
          "pygetdata.dirfile.put_carray() argument 2 must be list"
#ifdef USE_NUMPY
          " or NumPy array"
#endif
          ".");
      dreturn("%p", NULL);
      return NULL;
    }

    len = PyList_Size(pyobj);
  }

  if (len > 0) {
    void* data;

#ifdef USE_NUMPY
    if (have_ndarray) {
      type = gdpy_type_from_npytype(PyArray_TYPE(pyobj));

      if (type == GD_UNKNOWN) {
        PyErr_SetString(PyExc_ValueError,
            "pygetdata.dirfile.put_carray() unknown data type for argument 2.");
        dreturn ("%p", NULL);
        return NULL;
      }

      if (!(PyArray_FLAGS(pyobj) & NPY_ALIGNED)) {
        PyErr_SetString(PyExc_ValueError,
            "pygetdata.dirfile.put_carray() argument 2 must be aligned.");
        dreturn ("%p", NULL);
        return NULL;
      }

      if (!(PyArray_FLAGS(pyobj) & NPY_C_CONTIGUOUS)) {
        PyErr_SetString(PyExc_ValueError, "pygetdata.dirfile.put_carray()"
            " argument 2 must be C-style contiguous.");
        dreturn ("%p", NULL);
        return NULL;
      }

      data = PyArray_DATA(pyobj);
    } else
#endif
    {
      data = malloc(len * 16);
      type = gdpy_convert_from_pylist(pyobj, data, type, len);

      if (type == GD_UNKNOWN) {
        PyErr_SetString(PyExc_ValueError,
            "pygetdata.dirfile.put_carray() unknown data type for argument 2.");
        dreturn ("%p", NULL);
        return NULL;
      }
    }

    ns = gd_put_carray_slice(self->D, field_code, start, len, type, data);

#ifdef USE_NUMPY
    if (have_ndarray)
      PYGD_CHECK_ERROR(self->D, NULL);
    else
#endif
    {
      PYGD_CHECK_ERROR2(self->D, NULL, free(data));

      free(data);
    }
  }

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_putdata(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", "data", "type", "first_frame",
    "first_sample", NULL };
  const char* field_code;
  off_t first_frame = 0, first_sample = 0;
  gd_type_t type = GD_UNKNOWN;
  PyObject* pyobj;
  size_t ns;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "sO|iLL:pygetdata.dirfile.putdata", keywords, &field_code, &pyobj,
        &type, &first_frame, &first_sample)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  /* we only handle list or ndarray data */
#ifdef USE_NUMPY
  int have_ndarray = 0;
  if (PyArray_Check(pyobj)) {
    if (PyArray_NDIM(pyobj) != 1) {
      PyErr_SetString(PyExc_ValueError,
          "pygetdata.dirfile.putdata() argument 2 must be one dimensional");
      dreturn("%p", NULL);
      return NULL;
    }
    have_ndarray = 1;
    ns = PyArray_DIM(pyobj, 0);
  } else
#endif
  {
    if (!PyList_Check(pyobj)) {
      PyErr_SetString(PyExc_TypeError,
          "pygetdata.dirfile.putdata() argument 2 must be list"
#ifdef USE_NUMPY
          " or NumPy array"
#endif
          ".");
      dreturn("%p", NULL);
      return NULL;
    }

    ns = PyList_Size(pyobj);
  }

  if (ns > 0) {
    void* data;

#ifdef USE_NUMPY
    if (have_ndarray) {
      type = gdpy_type_from_npytype(PyArray_TYPE(pyobj));

      if (type == GD_UNKNOWN) {
        PyErr_SetString(PyExc_ValueError,
            "pygetdata.dirfile.putdata() unknown data type for argument 2.");
        dreturn ("%p", NULL);
        return NULL;
      }

      if (!(PyArray_FLAGS(pyobj) & NPY_ALIGNED)) {
        PyErr_SetString(PyExc_ValueError,
            "pygetdata.dirfile.putdata() argument 2 must be aligned.");
        dreturn ("%p", NULL);
        return NULL;
      }

      if (!(PyArray_FLAGS(pyobj) & NPY_C_CONTIGUOUS)) {
        PyErr_SetString(PyExc_ValueError, "pygetdata.dirfile.putdata()"
            " argument 2 must be C-style contiguous.");
        dreturn ("%p", NULL);
        return NULL;
      }

      data = PyArray_DATA(pyobj);
    } else
#endif
    {
      data = malloc(ns * 16);
      type = gdpy_convert_from_pylist(pyobj, data, type, ns);

      if (type == GD_UNKNOWN) {
        PyErr_SetString(PyExc_ValueError,
            "pygetdata.dirfile.putdata() unknown data type for argument 2.");
        dreturn ("%p", NULL);
        return NULL;
      }
    }

    ns = gd_putdata(self->D, field_code, first_frame, first_sample, 0, ns, type,
        data);

#ifdef USE_NUMPY
    if (have_ndarray)
      PYGD_CHECK_ERROR(self->D, NULL);
    else
#endif
    {
      PYGD_CHECK_ERROR2(self->D, NULL, free(data));

      free(data);
    }
  }

  pyobj = PyLong_FromLongLong(ns);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_putstring(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", "data", NULL };
  const char* field_code;
  const char* data;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "ss:pygetdata.dirfile.put_string", keywords, &field_code, &data))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_put_string(self->D, field_code, data);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_getspf(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:pygetdata.dirfile.spf",
        keywords, &field_code)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  unsigned int spf = gd_spf(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong((long)spf);
  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_validate(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", NULL };
  const char* field_code;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:pygetdata.dirfile.validate",
        keywords, &field_code)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_validate(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_getframenum(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", "value", "start", "end", NULL };
  const char* field_code;
  double value;
  off_t frame_start = 0;
  off_t frame_end = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "sd|KK:pygetdata.dirfile.framenum", keywords, &field_code, &value,
        &frame_start, &frame_end))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  double frame = gd_framenum_subset(self->D, field_code, value, frame_start,
      frame_end);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyFloat_FromDouble(frame);
  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_callback(struct gdpy_dirfile_t* self,
    PyObject *args, PyObject *keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  PyObject* pycallback = NULL;
  PyObject* pycallback_data = Py_None;
  char *keywords[] = {"callback", "extra", NULL};

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "OO:pygetdata.dirfile.set_callback", keywords, &pycallback,
        &pycallback_data))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if (pycallback && pycallback != Py_None && !PyCallable_Check(pycallback)) {
    PyErr_SetString(PyExc_TypeError, "callback function must be callable");
    dreturn("%p", NULL);
    return NULL;
  }

  Py_XINCREF(pycallback);
  Py_XINCREF(pycallback_data);
  Py_XDECREF(self->callback);
  Py_XDECREF(self->callback_data);
  self->callback = pycallback;
  self->callback_data = pycallback_data;

  gd_parser_callback(self->D, (pycallback == NULL) ? NULL :
      gdpy_callback_func, self);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_uninclude(struct gdpy_dirfile_t* self,
    PyObject *args, PyObject *keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char *keywords[] = {"fragment_index", "del", NULL};
  int fragment_index;
  int del = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "i|i:pygetdata.dirfile.uninclude", keywords, &fragment_index, &del))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_uninclude(self->D, fragment_index, del);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_move(struct gdpy_dirfile_t* self, PyObject* args,
    PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "field_code", "new_fragment", "move_data", NULL };
  const char* field_code;
  int new_fragment;
  int move_data = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "si|i:pygetdata.dirfile.move",
        keywords, &field_code, &new_fragment, &move_data)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_move(self->D, field_code, new_fragment, move_data);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_rename(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "old_code", "new_name", "move_data", NULL };
  const char* old_code;
  const char* new_name;
  int move_data = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "ss|i:pygetdata.dirfile.move",
        keywords, &old_code, &new_name, &move_data)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_rename(self->D, old_code, new_name, move_data);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_getstandards(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  int vers = gd_dirfile_standards(self->D, GD_VERSION_CURRENT);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong(vers);

  dreturn("%p", pyobj);
  return pyobj;
}

static int gdpy_dirfile_setstandards(struct gdpy_dirfile_t* self,
    PyObject *value, void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  int vers = (int)PyInt_AsLong(value);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  gd_dirfile_standards(self->D, vers);

  PYGD_CHECK_ERROR(self->D, -1);

  dreturn("%i", 0);
  return 0;
}

static PyGetSetDef gdpy_dirfile_getset[] = {
  { "error", (getter)gdpy_dirfile_geterror, NULL,
    "The numerical error code encountered by the last call to the GetData\n"
      "library for this dirfile.  If the last call was successful, this\n"
      "will be zero.  Because pygetdata throws exceptions on GetData\n"
      "errors, it is typically not necessary to check this value; use a\n"
      "try/except statement instead.  See gd_error(3).",
    NULL },
  { "error_string", (getter)gdpy_dirfile_geterrorstring, NULL,
    "A human-readable description of the last error encountered by the\n"
      "GetData library for this dirfile.  See gd_error_string(3).",
    NULL },
  { "name", (getter)gdpy_dirfile_getname, NULL,
    "The name of the Dirfile.  See gd_dirfilename(3).",
    NULL },
  { "nfragments", (getter)gdpy_dirfile_getnfragments, NULL,
    "The number of format file fragments in the dirfile.  See\n"
      "gd_nfragments(3)",
    NULL },
  { "nframes", (getter)gdpy_dirfile_getnframes, NULL,
    "The number of frames in the dirfile.  See gd_nframes(3).", NULL },
  { "reference", (getter)gdpy_dirfile_getreference,
    (setter)gdpy_dirfile_setreference,
    "The reference field for the dirfile, which may be modified.  See\n"
      "gd_reference(3).",
    NULL },
  { "standards", (getter)gdpy_dirfile_getstandards,
    (setter)gdpy_dirfile_setstandards,
    "The current Standards Version of the loaded dirfile.  Setting this\n"
      "to pygetdata.VERSION_EARLIEST or pygetdata.VERSION_LATEST has the\n"
      "same effect as passing the corresponding C API symbols to\n"
      "gd_dirfile_standards(3), q.v.",
    NULL },
  { NULL }
};

static PyMethodDef gdpy_dirfile_methods[] = {
  {"add", (PyCFunction)gdpy_dirfile_add, METH_VARARGS | METH_KEYWORDS,
    "add(entry)\n\n"
      "Add a field described by 'entry', which should be a pygetdata.entry\n"
      "object, to the database.  See gd_add(3)."
  },
  {"add_spec", (PyCFunction)gdpy_dirfile_addspec, METH_VARARGS | METH_KEYWORDS,
    "add_spec(line [, fragment_index])\n\n"
      "Add a field described by the field specification line 'line' to the\n"
      "database, by adding it to the format file fragment indexed by\n"
      "'fragment_index', which defaults to 0, if not given.  See\n"
      "gd_add_spec(3)."
  },
  {"alter", (PyCFunction)gdpy_dirfile_alter, METH_VARARGS | METH_KEYWORDS,
    "alter(field_code, entry [, recode])\n\n"
      "Modify the field metadata of the field specified by 'field_code',\n"
      "as described by 'entry', which should be a pygetdata.entry object.\n"
      "If recode is given and is non-zero, the data on disk will also be\n"
      "updated, if relevant, to reflect changes in the field metatdata.\n"
      "The entry.name and entry.fragment_index data descriptors are ignored\n"
      "by this function.  To rename or change the fragment of a field\n"
      "use the rename() or move() methods. See gd_alter_entry(3)."
  },
  {"alter_spec", (PyCFunction)gdpy_dirfile_alterspec, METH_VARARGS |
    METH_KEYWORDS, "alter_spec(line [, recode])\n\n"
      "Modify the field metadata described by the field specification line\n"
      "'line'.  If recode is given and is non-zero, the data on disk will\n"
      "also be updated, if relevant, to reflect changes in the field\n"
      "metadata.  See gd_alter_spec(3)."
  },
  {"close", (PyCFunction)gdpy_dirfile_close, METH_NOARGS,
    "close()\n\n"
      "Flush all pending changes to disk (as if flush() were called) and\n"
      "then close the dirfile.  It is not normally necessary to call this\n"
      "method explicitly: the dirfile will be closed when the dirfile\n"
      "object is destroyed.  After successful completion, the dirfile will\n"
      "be invalidated, prohibiting further use of this object.  See\n"
      "gd_close(3)."
  },
  {"delete", (PyCFunction)gdpy_dirfile_delentry, METH_VARARGS | METH_KEYWORDS,
    "delete(field_code [, flags])\n\n"
      "Delete the field 'field_code' from the database.  If 'flags' is\n"
      "omitted, it is assumed to be zero.  Otherwise, 'flags' should be a\n"
      "bitwise or'd collection of the pygetdata.DEL_* symbols, whose\n"
      "meanings are described in the gd_delete manual page.  See\n"
      "gd_delete(3)."
  },
  {"discard", (PyCFunction)gdpy_dirfile_discard, METH_NOARGS,
    "discard()\n\n"
      "Discard pending metatdata changes to the dirfile (data changes are\n"
      "still written) and then close the dirfile.  After this method\n"
      "successfully returns, the dirfile will be invalidated, prohibiting\n"
      "further use of this object.  Call this method if you don't want the\n"
      "object to implicitly call gd_close() when it is deleted.  See\n"
      "gd_discard(3)."
  },
  {"flush", (PyCFunction)gdpy_dirfile_flush, METH_VARARGS | METH_KEYWORDS,
    "flush([field_code])\n\n"
      "Flush pending writes to the specified field to disk.  This does\n"
      "not flush pending metadata changes.  For that, use metaflush.\n"
      "However, if field_code is omitted, all data *and* metadata will be\n"
      "written to disk.  See gd_flush(3)."
  },
  {"bof", (PyCFunction)gdpy_dirfile_getbof, METH_VARARGS | METH_KEYWORDS,
    "bof(field_code)\n\n"
      "Retrieve the sample number corresponding to the beginning-of-field\n"
      "marker for the field specified by 'field_code'.  See gd_bof(3)."
  },
  {"get_carray", (PyCFunction)gdpy_dirfile_getcarray,
    METH_VARARGS | METH_KEYWORDS,
    "get_carray(field_code, return_type [, start, len, as_list])\n\n"
      "Retrieve the value of the CARRAY field specified by 'field_code'.\n"
      "The 'return_type' parameter indicates the desired type of the\n"
      "elements of the array returned, and should be (typically) one of:\n"
      "pygetdata.INT, pygetdata.LONG, pygetdata.ULONG, pygetdata.FLOAT,\n"
      "or pygetdata.COMPLEX, although any GetData data type code is\n"
      "permitted.  If NumPy support is present in pygetdata, and 'as_list'\n"
      "is not given or is zero, a NumPy array will be returned.  Otherwise\n"
      "a list of values will be returned.\n\n"
      "The first element returned is given by 'start', counting from zero.\n"
      "If omitted, zero is assumed.  The number of elements returned is\n"
      "given by 'len'.  If omitted or zero, all elements from 'start' to\n"
      "the end of the array are returned.  See gd_get_carray_slice(3)."
  },
  {"get_constant", (PyCFunction)gdpy_dirfile_getconstant,
    METH_VARARGS | METH_KEYWORDS,
    "get_constant(field_code, return_type)\n\n"
      "Retrieve the value of the CONST field specified by 'field_code'.\n"
      "The 'return_type' parameter indicates the desired type of the object\n"
      "returned, and should be (typically) one of: pygetdata.INT,\n"
      "pygetdata.LONG, pygetdata.ULONG, pygetdata.FLOAT, or\n"
      "pygetdata.COMPLEX, although any GetData data type code is permitted.\n"
      "See gd_get_constant(3)."
  },
  {"constants", (PyCFunction)gdpy_dirfile_getconstants,
    METH_VARARGS | METH_KEYWORDS,
    "constants(return_type)\n\n"
      "Retrieve all CONST fields, and their values.  A list of tuples\n"
      "will be returned, each tuple containing the name and value of the\n"
      "field.  The 'return_type' parameter indicates the desired type of\n"
      "the values returned, and should be (typically) one of:\n"
      "pygetdata.INT, pygetdata.LONG, pygetdata.ULONG, pygetdata.FLOAT, or\n"
      "pygetdata.COMPLEX, although any GetData data type code is permitted.\n"
      "See gd_constants(3), but note that this method returns both names\n"
      "and values, unlike the C API counterpart."
  },
  {"carray_len", (PyCFunction)gdpy_dirfile_carraylen, METH_VARARGS |
    METH_KEYWORDS,
    "carray_len(field_code)\n\n"
      "Returns the length of the CARRAY specified by 'field_code'.  See\n"
      "gd_carray_len(3)."
  },
  {"carrays", (PyCFunction)gdpy_dirfile_carrays, METH_VARARGS | METH_KEYWORDS,
    "carrays(return_type [, as_list])\n\n"
      "Retrieve all CARRAY fields, and their values.  A list of tuples\n"
      "will be returned, each tuple containing the name and values of the\n"
      "field.  If NumPy support is present in pygetdata, and 'as_list' is\n"
      "not given or zero, the values will be returned in NumPy arrays;\n"
      "otherwise, the values will be returned as lists.\n\n"
      /* -----------------------------------------------------------------| */
      "The 'return_type' parameter indicates the desired type of the values\n"
      "returned, and should be (typically) one of: pygetdata.INT,\n"
      "pygetdata.LONG, pygetdata.ULONG, pygetdata.FLOAT, or\n"
      "pygetdata.COMPLEX, although any GetData data type code is permitted.\n"
      "See gd_carrays(3), but note that this method returns both names\n"
      "and values, unlike the C API counterpart."
  },
  {"getdata", (PyCFunction)gdpy_dirfile_getdata, METH_VARARGS | METH_KEYWORDS,
    "gd_getdata(field_code, return_type [, first_frame, first_sample,\n"
      "num_frames, num_samples, as_list])\n\n"
      "Retrieve a data vector from the dirfile.  If NumPy support is\n"
      "present in pygetdata, and 'as_list' is not given or zero, a NumPy\n"
      "array will be returned.  Otherwise a list of data values will\n"
      "be returned.  NumPy arrays should be preferred for large datasets:\n"
      "they are more efficient both in terms of memory usage and retrieval\n"
      "time.\n\n"
      "The 'return_type' parameter indicates the desired type of the values\n"
      "returned.  For NumPy data, the NumPy array will have the dtype\n"
      "indicated.  For list data it should be (typically) one of:\n"
      "pygetdata.INT, pygetdata.LONG, pygetdata.ULONG, pygetdata.FLOAT, or\n"
      "pygetdata.COMPLEX, although any GetData data type code is permitted.\n"
      "The 'first_frame' and 'first_samples' parameters indicate first\n"
      "datum to read.  If they are both omitted, data is read from the\n"
      "first sample.  Similarly, 'num_frames' and 'num_samples' indicate\n"
      "the amount of data.  Omitting both of these results in an error.\n"
      "Fewer samples than requested may be returned without causing an\n"
      "error.  See gd_getdata(3)."
  },
  { "entry", (PyCFunction)gdpy_dirfile_getentry,
    METH_VARARGS | METH_KEYWORDS,
    "entry(field_code)\n\n"
      "Retrieve the field metadata for the specified 'field_code'.  A\n"
      "pygetdata.entry object is returned.  See gd_entry(3).\n"
  },
  {"eof", (PyCFunction)gdpy_dirfile_geteof, METH_VARARGS | METH_KEYWORDS,
    "eof(field_code)\n\n"
      "Retrieve the sample number corresponding to the end-of-field marker\n"
      "(ie. the number of samples in the field) for the field specified by\n"
      "'field_code'.  See gd_eof(3)."
  },
  { "field_list", (PyCFunction)gdpy_dirfile_getfieldlist,
    METH_VARARGS | METH_KEYWORDS,
    "field_list([entry_type])\n\n"
      "Return a list of field names.  If 'entry_type' is given, it should\n"
      "be one of the pygetdata.*_ENTRY symbols.  If 'entry_type' is not\n"
      "given, or is pygetdata.NO_ENTRY, all fields in the database are\n"
      "returned.  Otherwise, only the fields of the given type are\n"
      "returned.  See gd_field_list(3) and gd_field_list_by_type(3)."
  },
  { "fragment", (PyCFunction)gdpy_dirfile_getfragment,
    METH_VARARGS | METH_KEYWORDS,
    "fragment(fragment_index)\n\n"
      "Retrieve the metadata associated with the format file fragment\n"
      "indexed by 'fragment_index'.  A pygetdata.fragment object is\n"
      "returned."
  },
  {"fragment_index", (PyCFunction)gdpy_dirfile_getfragmentindex,
    METH_VARARGS | METH_KEYWORDS,
    "fragment_index(field_code)\n\n"
      "Return the fragment index of the format file fragment which defines\n"
      "the field specified by 'field_code'.  See gd_fragment_index(3)."
  },
  {"framenum", (PyCFunction)gdpy_dirfile_getframenum,
    METH_VARARGS | METH_KEYWORDS,
    "framenum(field_code, value [, start, end])\n\n"
      "Perform a reverse look-up on the field specified by 'field_code'\n"
      "(which must be monotonic) and returns the fractional frame number\n"
      "where the field equals 'value'.  The search is performed between\n"
      "the frame limits 'start' and 'end'.  If 'start' is omitted, the\n"
      "search begins at the first sample.  If 'end' is omitted, the search\n"
      "ends at the last sample.  See gd_framenum_subset(3)."
  },
  {"mcarrays", (PyCFunction)gdpy_dirfile_mcarrays, METH_VARARGS | METH_KEYWORDS,
    "mcarrays(parent, return_type [, as_list])\n\n"
      "Retrieve all CARRAY metafields, and their values, for the parent\n"
      "field 'parent'.  A list of tuples will be returned, each tuple\n"
      "containing the name and values of the field.  If NumPy support is\n"
      "present in pygetdata, and 'as_list' is not given or zero, the values\n"
      "will be returned in NumPy arrays; otherwise, the values will be\n"
      "returned as lists.\n\n"
      "The 'return_type' parameter indicates the desired type of the values\n"
      "returned, and should be (typically) one of: pygetdata.INT,\n"
      "pygetdata.LONG, pygetdata.ULONG, pygetdata.FLOAT, or\n"
      "pygetdata.COMPLEX, although any GetData data type code is permitted.\n"
      "See gd_mcarrays(3), but note that this method returns both names\n"
      "and values, unlike the C API counterpart."
  },
  {"mconstants", (PyCFunction)gdpy_dirfile_getmconstants,
    METH_VARARGS | METH_KEYWORDS,
    "mconstants(parent, return_type)\n\n"
      "Retrieve all CONST metafields defined for the parent field 'parent',\n"
      "and their values.  A list of tuples will be returned, each tuple\n"
      "containing the name and value of a field.  The 'return_type'\n"
      "parameter indicates the desired type of the values returned, and\n"
      "should be (typically) one of: pygetdata.INT, pygetdata.LONG,\n"
      "pygetdata.ULONG, pygetdata.FLOAT, or pygetdata.COMPLEX, although any\n"
      "GetData data type code is permitted.  See gd_mconstants(3), but\n"
      "note that this method returns both names and values, unlike the\n"
      "C API counterpart."
  },
  { "mfield_list", (PyCFunction)gdpy_dirfile_getmfieldlist,
    METH_VARARGS | METH_KEYWORDS,
    "mfield_list(parent [, entry_type])\n\n"
      "Return a list of metafield names for the parent field 'parent'.  If\n"
      "'entry_type' is given, it should be one of the pygetdata.*_ENTRY\n"
      "symbols.  If 'entry_type' is not given, or is pygetdata.NO_ENTRY,\n"
      "all metafields for the given parent field are returned.  Otherwise,\n"
      "only the metafields of the given type are returned.  See\n"
      "gd_mfield_list(3) and gd_mfield_list_by_type(3)."
  },
  {"mstrings", (PyCFunction)gdpy_dirfile_getmstrings,
    METH_VARARGS | METH_KEYWORDS,
    "mstrings(parent, return_type)\n\n"
      "Retrieve all STRING metafields defined for the parent field\n"
      "'parent', and their values.  A list of tuples will be returned, each\n"
      "tuple containing the name and value of a field.  See\n"
      "gd_mstrings(3), but note that this method returns both names and\n"
      "values, unlike the C API counterpart."
  },
  { "mvector_list", (PyCFunction)gdpy_dirfile_getmvectorlist,
    METH_VARARGS | METH_KEYWORDS,
    "mvector_list(parent)\n\n"
      "Retrieve a list of all vector type metafields (that is: BIT, DIVIDE,\n"
      "LINCOM, LINTERP, MULTIPLY, PHASE, POLYNOM, RECIP, and SBIT\n"
      "metafields) for the parent field 'parent'.  See gd_mvector_list(3)."
  },
  {"native_type", (PyCFunction)gdpy_dirfile_getnativetype,
    METH_VARARGS | METH_KEYWORDS,
    "native_type(field_code)\n\n"
      "Retrieve the native data type of the field specified by\n"
      "'field_code'.  The return value will be one of the data type codes:\n"
      "pygetdata.UINT8, pygetdata.INT8, &c.  The native_type_name\n"
      "method behaves identically, but returns a human-readable string\n"
      "describing the data type.  See gd_native_type(3)."
  },
  {"native_type_name", (PyCFunction)gdpy_dirfile_getnativetypename,
    METH_VARARGS | METH_KEYWORDS,
    "native_type_name(field_code)\n\n"
      "Retrieve the native data type of the field specified by\n"
      "'field_code'.  The return value will be a string describing the\n"
      "data type: 'UINT8', 'INT8', &c.  The native_type method behaves\n"
      "identically, but returns a numeric data type code.  See\n"
      "gd_native_type(3)."
  },
  {"nfields", (PyCFunction)gdpy_dirfile_getnfields,
    METH_VARARGS | METH_KEYWORDS,
    "nfields([entry_type])\n\n"
      "Return the number of fields in the database.  If 'entry_type' is\n"
      "given, it should be one of the pygetdata.*_ENTRY symbols.  If\n"
      "'entry_type' is not given, or is pygetdata.NO_ENTRY, the total\n"
      "number of fields in the database is returned.  Otherwise, only\n"
      "the number of fields of the given type is returned.  See\n"
      "gd_nfields(3) and gd_nfields_by_type(3)."
  },
  {"nmfields", (PyCFunction)gdpy_dirfile_getnmfields,
    METH_VARARGS | METH_KEYWORDS,
    "nmfields(parent [, entry_type])\n\n"
      "Return the number of metafields defined for the parent field\n"
      "'parent'.  If 'entry_type' is given, it should be one of the\n"
      "pygetdata.*_ENTRY symbols.  If 'entry_type' is not given, or is\n"
      "pygetdata.NO_ENTRY, the total number of metafields for this parent\n"
      "is returned.  Otherwise, only the number of metafields of the given\n"
      "type is returned.  See gd_nmfields(3) and gd_nmfields_by_type(3)."
  },
  {"nmvectors", (PyCFunction)gdpy_dirfile_getnmvectors,
    METH_VARARGS | METH_KEYWORDS,
    "nmvectors(parent)\n\n"
      "Return the number of vector type metafields (that is: BIT, DIVIDE,\n"
      "LINCOM, LINTERP, MULTIPLY, PHASE, POLYNOM, RECIP, and SBIT\n"
      "metafields) for the parent field 'parent'.  See gd_nmvectors(3)."
  },
  {"nvectors", (PyCFunction)gdpy_dirfile_getnvectors, METH_NOARGS,
    "nvectors()\n\n"
      "Return the number of vector type fields (that is: BIT, DIVIDE,\n"
      "INDEX, LINCOM, LINTERP, MULTIPLY, PHASE, POLYNOM, RAW, RECIP, and\n"
      "SBIT fields) defined in the database.  See gd_nvectors(3)."
  },
  {"raw_filename", (PyCFunction)gdpy_dirfile_getrawfilename,
    METH_VARARGS | METH_KEYWORDS,
    "raw_filename(field_code)\n\n"
      "Return the pathname of the data file on disk backing the RAW field\n"
      "specified by 'field_code'.  See gd_raw_filename(3)."
  },
  {"spf", (PyCFunction)gdpy_dirfile_getspf, METH_VARARGS | METH_KEYWORDS,
    "spf(field_code)\n\n"
      "Return the number of samples per frame of the field specified by\n"
      "field code.  See gd_spf(3)."
  },
  {"get_string", (PyCFunction)gdpy_dirfile_getstring,
    METH_VARARGS | METH_KEYWORDS,
    "get_string(field_code)\n\n"
      "Retrieve the value of the STRING field specified by 'field_code'.\n"
      "See gd_get_string(3)."
  },
  { "strings", (PyCFunction)gdpy_dirfile_getstrings, METH_NOARGS,
    "strings()\n\n"
      "Retrieve all STRING fields defined in the database.  A list of\n"
      "tuples will be returned, each tuple containing the name and value of\n"
      "a field.  See gd_strings(3), but note that this method returns both\n"
      "names and values, unlike the C API counterpart."
  },
  { "vector_list", (PyCFunction)gdpy_dirfile_getvectorlist, METH_NOARGS,
    "vector_list()\n\n"
      "Retrieve a list of all vector type fields (that is: BIT, DIVIDE,\n"
      "INDEX, LINCOM, LINTERP, MULTIPLY, PHASE, POLYNOM, RAW, RECIP, and\n"
      "SBIT metafields) defined in the database.  See gd_vector_list(3)."
  },
  {"include", (PyCFunction)gdpy_dirfile_include, METH_VARARGS | METH_KEYWORDS,
    "include(filename [, fragment_index, flags])\n\n"
      "Add (and possibly create) a new format file fragment specified by\n"
      "'filename' to the database, as an include in the existing fragment\n"
      "indexed by 'fragment_index'.  If 'fragment_index' is not given,\n"
      "zero is assumed (ie. the primary format file).  If flags is given,\n"
      "it should be a bitwise or'd collection of flags listed in the\n"
      "gd_include manual page.  See gd_include(3)."
  },
  {"madd", (PyCFunction)gdpy_dirfile_madd, METH_VARARGS | METH_KEYWORDS,
    "madd(entry, parent)\n\n"
      "Add a field described by 'entry', which should be a pygetdata.entry\n"
      "object, to the database as a metafield under the parent field given\n"
      "by 'parent'.  See gd_madd(3)."
  },
  {"madd_spec", (PyCFunction)gdpy_dirfile_maddspec,
    METH_VARARGS | METH_KEYWORDS,
    "madd_spec(line, parent)\n\n"
      "Add a field described by the field specification line 'line' to the\n"
      "database as a metafield under the parent field given by 'parent'.\n"
      "See gd_madd_spec(3)."
  },
  {"malter_spec", (PyCFunction)gdpy_dirfile_malterspec,
    METH_VARARGS | METH_KEYWORDS,
    "malter_spec(line, parent [, recode])\n\n"
      "Modify the metadata described by the field specification line\n"
      "'line' for a metafield under the parent field 'parent'.  If recode\n"
      "is given and is non-zero, the data on disk will also be updated, if\n"
      "relevant, to reflect changes in the metafield metadata.  See\n"
      "gd_malter_spec(3)."
  },
  {"metaflush", (PyCFunction)gdpy_dirfile_metaflush, METH_NOARGS,
    "metaflush()\n\n"
      "Flush all pending metadata changes to disk.  See gd_metaflush(3)."
  },
  {"move", (PyCFunction)gdpy_dirfile_move, METH_VARARGS | METH_KEYWORDS,
    "move(field_code, new_fragment [, move_data])\n\n"
      "Move the specification of the field given by 'field_code' to the\n"
      "format file fragment indexed by 'new_fragment'.  If 'move_data' is\n"
      "given and is non-zero, and 'field_code' specifies a RAW field, the\n"
      "associated file on disk will also be moved, if necessary.  See\n"
      "gd_move(3)."
  },
  {"put_carray", (PyCFunction)gdpy_dirfile_putcarray,
    METH_VARARGS | METH_KEYWORDS,
    "put_carray(field_code, data [, start])\n\n"
      "Store the data in the list or NumPy array 'data' in the CARRAY given\n"
      "by 'field_code'.  If a list is provided, all entries must be of the\n"
      "same type.  The parameter 'start' indicates where the first sample in\n"
      "which the data will be stored.  Zero is assumed if not given.\n"
      "See gd_put_carray_slice(3)."
  },
  {"put_constant", (PyCFunction)gdpy_dirfile_putconstant,
    METH_VARARGS | METH_KEYWORDS,
    "put_constant(field_code, value [, type])\n\n"
      "Store the value of 'value' in the CONST field given by 'field_code'.\n"
      "The 'value' parameter must be a simple numeric type.  This method\n"
      "will use the most appropriate data type when passing the value to\n"
      "the C API.  However, this behaviour can be overridden by explicitly\n"
      "specifying a C API type for the data using 'type'.  If specified,\n"
      "'type' should be one of the data type codes: pygetdata.UINT8,\n"
      "pygetdata.INT8, &c.  Note that this does not affect the storage type\n"
      "of the CONST field, merely how the data is transferred to the C API.\n"
      "See gd_put_constant(3)."
  },
  {"putdata", (PyCFunction)gdpy_dirfile_putdata, METH_VARARGS | METH_KEYWORDS,
    "putdata(field_code, data [, type, first_frame, first_sample])\n\n"
      "Store the data in the list or NumPy array 'data' in the vector field\n"
      "given by 'field_code'.  If a list is provided, all entries must be\n"
      "of the same type.  The parameters 'first_frame' and 'first_sample'\n"
      "indicate the first sample in which the data will be stored.  If\n"
      "neither are given, the data will be stored starting from the first\n"
      "sample in the field.  The number of samples actually written is\n"
      "returned.\n\n"
      "This method will use the most appropriate data type when passing the\n"
      "data to the C API.  However, this behaviour can be overridden by\n"
      "explicitly specifying a C API type for the data using 'type'.  If\n"
      "specified, 'type' should be one of the data type codes:\n"
      "pygetdata.UINT8, pygetdata.INT8, &c.  Note that this does not affect\n"
      "the storage type of the field, merely how the data is transferred to\n"
      "the C API.  See gd_putdata(3)."
  },
  {"put_string", (PyCFunction)gdpy_dirfile_putstring,
    METH_VARARGS | METH_KEYWORDS,
    "put_string(field_code, value)\n\n"
      "Store the string given by 'value' in the STRING field specified by\n"
      "'field_code'.  See gd_put_string(3)."
  },
  {"rename", (PyCFunction)gdpy_dirfile_rename, METH_VARARGS | METH_KEYWORDS,
    "rename(old_code, new_name [, move_data])\n\n"
      "Change the name of the field specified by 'old_code' to 'new_name'.\n"
      "If 'move_data' is given and is non-zero, and if 'old_code' specifies\n"
      "a RAW field, the file on disk will also be renamed accordingly.\n"
      "See gd_rename(3)."
  },
  {"set_callback", (PyCFunction)gdpy_dirfile_callback,
    METH_VARARGS | METH_KEYWORDS,
    "set_callback(sehandler, extra)\n\n"
      "Change or remove the parser callback function, and the caller object\n"
      "passed to it.  If 'sehandler' is None, the current parser callback\n"
      "(if any) will be removed, otherwise, the current callback will be\n"
      "replaced by 'sehandler', which must be a callable object.  The\n"
      "'extra' parameter is any object which will be passed to the callback\n"
      "handler, or None, if no such object is needed.  See\n"
      "gd_parser_callback(3)."
  },
  {"uninclude", (PyCFunction)gdpy_dirfile_uninclude,
    METH_VARARGS | METH_KEYWORDS,
    "uninclude(fragment_index [, del])\n\n"
      "Remove the format file fragment indexed by 'fragment_index' from the\n"
      "database.  If 'del' is given, and is non-zero, the file will also be\n"
      "deleted from disk.  This also removes all field defined in the\n"
      "specified fragment from the database.  See gd_uninclude(3)."
  },
  {"validate", (PyCFunction)gdpy_dirfile_validate, METH_VARARGS | METH_KEYWORDS,
    "validate(field_code)\n\n"
      "Check whether the field specified by 'field_code' is valid for\n"
      "reading and writing.  Throws an error if it is not.  See\n"
      "gd_validate(3)."
  },
  { NULL, NULL, 0, NULL }
};

#define DIRFILE_DOC \
  "dirfile([name [, flags [, sehandler [, extra ]]])\n\n" \
"If 'name' is omitted or None, returns an invalid dirfile, as if\n" \
"gd_invalid_dirfile(3) were called.  Othwerwise, if 'name' is a string,\n" \
"returns a dirfile object representing the dirfile specified by 'name'.\n" \
"The dirfile is opened by a call to gd_cbopen(3).  See that manual page\n" \
"for full details on arguments.  If present, 'flags' should be a bitwise\n" \
"or'd collection of gd_cbopen flags.  If it is omitted, the default,\n" \
"pygetdata.RDRW, is used.\n\n" \
"If a callback handler is desired, 'sehandler' should be a callable\n"\
"object (ie. a function) which accepts two objects.  The first object is\n"\
"a dictionary with keys: 'suberror', 'line', 'linenum', and 'filename',\n"\
"providing the same information as the gd_pdata_t structure in the C API.\n"\
"The second object is the 'extra' object passed to this constructor, and\n"\
"may be any object desired by the caller.  If no extra parameter was\n"\
"specified, this will be None.  The sehandler should return one of the\n"\
"pygetdata.SYNTAX_* symbols.\n\n"\
"The dirfile will be automatically closed when garbage collection is run\n"\
"on the object.  In general, however, an explicit call to close() or\n"\
"discard() is preferable on a writeable dirfile, since the implicit close\n"\
/* ---------------------------------------------------------------------| */\
"performed on the dirfile when the object is deleted silently discards\n"\
"any errors encountered when the dirfile is flushed to disc.  After\n"\
"explicitly calling close() or discard(), the dirfile will be\n"\
"invalidated, prohibiting further use of it.\n"


PyTypeObject gdpy_dirfile =
{
  PyObject_HEAD_INIT(NULL)
    0,                             /* ob_size */
  "pygetdata.dirfile",             /* tp_name */
  sizeof(struct gdpy_dirfile_t),   /* tp_basicsize */
  0,                               /* tp_itemsize */
  (destructor)gdpy_dirfile_delete, /* tp_dealloc */
  0,                               /* tp_print */
  0,                               /* tp_getattr */
  0,                               /* tp_setattr */
  0,                               /* tp_compare */
  0,                               /* tp_repr */
  0,                               /* tp_as_number */
  0,                               /* tp_as_sequence */
  0,                               /* tp_as_mapping */
  0,                               /* tp_hash */
  0,                               /* tp_call */
  0,                               /* tp_str */
  0,                               /* tp_getattro */
  0,                               /* tp_setattro */
  0,                               /* tp_as_buffer */
  Py_TPFLAGS_DEFAULT,              /* tp_flags */
  DIRFILE_DOC,                     /* tp_doc */
  0,                               /* tp_traverse */
  0,                               /* tp_clear */
  0,                               /* tp_richcompare */
  0,                               /* tp_weaklistoffset */
  0,                               /* tp_iter */
  0,                               /* tp_iternext */
  gdpy_dirfile_methods,            /* tp_methods */
  0,                               /* tp_members */
  gdpy_dirfile_getset,             /* tp_getset */
  0,                               /* tp_base */
  0,                               /* tp_dict */
  0,                               /* tp_descr_get */
  0,                               /* tp_descr_set */
  0,                               /* tp_dictoffset */
  (initproc)gdpy_dirfile_init,     /* tp_init */
  0,                               /* tp_alloc */
  gdpy_dirfile_create,             /* tp_new */
};
