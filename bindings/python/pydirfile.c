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

/* Dirfile */
static int gdpy_callback_func(gd_parser_data_t* pdata, void* extra)
{
  dtrace("%p, %p", pdata, extra);

  int r = GD_SYNTAX_ABORT;
  struct gdpy_dirfile_t* self = extra;

  if (self->callback != NULL) {
    char buffer[GD_MAX_LINE_LENGTH];
    get_error_string(pdata->dirfile, buffer, GD_MAX_LINE_LENGTH);

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

  dirfile_close(self->D);

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
  const char* name = NULL;
  unsigned long flags = GD_RDWR;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s|kOO:getdata.dirfile.__init__",
        keywords, &name, &flags, &pycallback, &pycallback_data))
  {
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

  self->D = dirfile_cbopen(name, (unsigned int)flags,
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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "O!:getdata.dirfile.add",
        keywords, &gdpy_entry, &entry))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_add(self->D, entry->E);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s|i:getdata.dirfile.add_spec",
        keywords, &spec, &fragment))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_add_spec(self->D, spec, fragment);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_alter(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "entry", "recode", NULL };
  struct gdpy_entry_t* entry = NULL;
  int recode = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "O!|i:getdata.dirfile.alter",
        keywords, &gdpy_entry, &entry, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_alter_entry(self->D, entry->E->field, entry->E, recode);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s|i:getdata.dirfile.alter_spec",
        keywords, &spec, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_alter_spec(self->D, spec, recode);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_close(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  dirfile_close(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  /* Here we replace D with an empty, invalid dirfile object.  The following
   * uses getdata internals, but this could be done just as simply (but with
   * more overhead) using the public API by calling dirfile_open("", 0)
   */
  self->D = malloc(sizeof(struct _GD_DIRFILE));
  memset(self->D, 0, sizeof(struct _GD_DIRFILE));
  self->D->flags = GD_INVALID;

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
  int flags;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si:getdata.dirfile.delete", keywords, &field_code, &flags))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_delete(self->D, field_code, flags);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_discard(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  dirfile_discard(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  /* Here we replace D with an empty, invalid dirfile object.  The following
   * uses getdata internals, but this could be done just as simply (but with
   * more overhead) using the public API by calling dirfile_open("", 0)
   */
  self->D = malloc(sizeof(struct _GD_DIRFILE));
  memset(self->D, 0, sizeof(struct _GD_DIRFILE));
  self->D->flags = GD_INVALID;

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_getconstant(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"field_code", "return_type", NULL};
  const char* field_code;
  gd_type_t return_type;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si:getdata.dirfile.get_constant", keywords, &field_code, &return_type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  char data[16];

  get_constant(self->D, field_code, return_type, data);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = gdpy_convert_to_pyobj(data, return_type);

  dreturn("%p", pyobj);
  return pyobj;
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
        "i:getdata.dirfile.get_constants", keywords, &return_type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = get_field_list_by_type(self->D, GD_CONST_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = get_constants(self->D, return_type);

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
    "first_sample", "num_frames", "num_samples", NULL };
  const char* field_code;
  off64_t first_frame = 0, first_sample = 0;
  size_t num_frames = 0, num_samples = 0;
  gd_type_t return_type;
  PyObject* pylist;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si|LLii:getdata.dirfile.getdata", keywords, &field_code, &return_type,
        &first_frame, &first_sample, &num_frames, &num_samples))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  /* we need the SPF to know how many samples we have to allocate */
  unsigned int spf = get_spf(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  size_t ns = num_samples + num_frames * spf;

  if (ns == 0)
    pylist = Py_BuildValue("[]");
  else {
    void* data = malloc(ns * GD_SIZE(return_type));

    ns = getdata64(self->D, field_code, first_frame, first_sample, num_frames,
        num_samples, return_type, data);

    PYGD_CHECK_ERROR2(self->D, NULL, free(data));

    pylist = gdpy_convert_to_pylist(data, return_type, ns);

    free(data);
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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:getdata.dirfile.get_entry",
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
  
  get_entry(self->D, field_code, E);

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

  PyObject* error = PyInt_FromLong(get_error(self->D));

  dreturn("%p", error);
  return error;
}

static PyObject* gdpy_dirfile_getfragment(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = {"fragment_index", NULL};
  int fragment_index;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "i:getdata.dirfile.get_entry",
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
        "s:getdata.dirfile.get_fragment_index", keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  int index = get_fragment_index(self->D, field_code);

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

  PyObject* error = PyString_FromString(get_error_string(self->D, buffer,
        GD_MAX_LINE_LENGTH));

  dreturn("%p", error);
  return error;
}

static PyObject* gdpy_dirfile_getfieldlist(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  int i;

  const char **fields = get_field_list(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pylist = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(pylist, PyString_FromString(fields[i]));

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getvectorlist(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  int i;

  const char **vectors = get_vector_list(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pylist = PyList_New(0);

  for (i = 0; vectors[i] != NULL; ++i)
    PyList_Append(pylist, PyString_FromString(vectors[i]));

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getfieldlistbytype(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "type", NULL };
  int i, type;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "i:getdata.dirfile.field_list_by_type", keywords, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  const char **fields = get_field_list_by_type(self->D, type);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "|s:getdata.dirfile.flush",
        keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_flush(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_include(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "file", "fragment", "flags", NULL };
  const char* file = NULL;
  int fragment_index = 0;
  unsigned int flags = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s|ii:getdata.dirfile.include",
        keywords, &file, &fragment_index, &flags))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  self->callback_exception = 0;

  long index = dirfile_include(self->D, file, fragment_index, flags);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "O!s:getdata.dirfile.madd",
        keywords, &gdpy_entry, &entry, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_madd(self->D, entry->E, parent);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "ss:getdata.dirfile.madd_spec",
        keywords, &spec, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_madd_spec(self->D, spec, parent);

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
        "ss|i:getdata.dirfile.malter_spec", keywords, &spec, &parent, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_malter_spec(self->D, spec, parent, recode);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
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
        "si:getdata.dirfile.get_mconstants", keywords, &parent, &return_type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = get_mfield_list_by_type(self->D, parent, GD_CONST_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = get_mconstants(self->D, parent, return_type);

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

  dirfile_metaflush(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_dirfile_getmfieldlist(struct gdpy_dirfile_t* self,
  void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  int i;

  char* keywords[] = {"parent", NULL};
  const char* parent = NULL;
  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:getdata.dirfile.get_mfield_list", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  const char **fields = get_mfield_list(self->D, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pylist = PyList_New(0);

  for (i = 0; fields[i] != NULL; ++i)
    PyList_Append(pylist, PyString_FromString(fields[i]));

  dreturn("%p", pylist);
  return pylist;
}

static PyObject* gdpy_dirfile_getmfieldlistbytype(struct gdpy_dirfile_t* self,
    void* args, void* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "parent", "type", NULL };
  const char* parent = NULL;
  gd_type_t type;
  int i;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si:getdata.dirfile.field_list_by_type", keywords, &parent, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  const char **fields = get_mfield_list_by_type(self->D, parent, type);

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

  const char* name = dirfilename(self->D);

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
        "s:getdata.dirfile.get_mconstants", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  fields = get_mfield_list_by_type(self->D, parent, GD_STRING_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = get_mstrings(self->D, parent);

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
        "s:getdata.dirfile.get_mvector_list", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  const char **fields = get_mvector_list(self->D, parent);

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
        "s:getdata.dirfile.get_raw_filename", keywords, &field_code))
  {
    dreturn ("%p", NULL);
    return NULL;
  }
  
  filename = get_raw_filename(self->D, field_code);

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
        "s:getdata.dirfile.get_native_type", keywords, &field_code))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_type_t ntype = get_native_type(self->D, field_code);

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
  char buffer[11];

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:getdata.dirfile.get_native_type_name", keywords, &field_code))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  gd_type_t t = get_native_type(self->D, field_code);

  PYGD_CHECK_ERROR(self->D, NULL);

  sprintf(buffer, "%s%i", (t & GD_COMPLEX) ? "COMPLEX" :
        (t & GD_IEEE754) ? "FLOAT" : (t & GD_SIGNED) ?  "INT" : "UINT",
        8 * GD_SIZE(t));

  PyObject* pyobj = PyString_FromString(buffer);
  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getnfields(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  unsigned int nfields = get_nfields(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynfields = PyInt_FromLong((long)nfields);

  dreturn("%p", pynfields);
  return pynfields;
}

static PyObject* gdpy_dirfile_getnfieldsbytype(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "type", NULL };
  int type;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "i:getdata.dirfile.nfields_by_type", keywords, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  unsigned int nfields = get_nfields_by_type(self->D, (gd_entype_t)type);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynfields = PyInt_FromLong((long)nfields);

  dreturn("%p", pynfields);
  return pynfields;
}

static PyObject* gdpy_dirfile_getnfragments(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  long nfragments = get_nfragments(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynfragments = PyInt_FromLong(nfragments);

  dreturn("%p", pynfragments);
  return pynfragments;
}

static PyObject* gdpy_dirfile_getnframes(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  off64_t nframes = get_nframes64(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynframes = PyLong_FromLongLong(nframes);

  dreturn("%p", pynframes);
  return pynframes;
}

static PyObject* gdpy_dirfile_getnmfields(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "parent", NULL };
  const char* parent = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "s:getdata.dirfile.nmfields", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  unsigned int nmfields = get_nmfields(self->D, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong((long)nmfields);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getnmfieldsbytype(struct gdpy_dirfile_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "parent", "type", NULL };
  const char* parent = NULL;
  int type;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "si:getdata.dirfile.nmfields_by_type", keywords, &parent, &type))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  unsigned int nmfields = get_nmfields_by_type(self->D, parent, type);

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
        "s:getdata.dirfile.nmvectors", keywords, &parent))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  unsigned int nmvectors = get_nmvectors(self->D, parent);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pyobj = PyInt_FromLong((long)nmvectors);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_dirfile_getnvectors(struct gdpy_dirfile_t* self)
{
  dtrace("%p", self);

  unsigned int nvectors = get_nvectors(self->D);

  PYGD_CHECK_ERROR(self->D, NULL);

  PyObject* pynvectors = PyInt_FromLong((long)nvectors);

  dreturn("%p", pynvectors);
  return pynvectors;
}

static PyObject* gdpy_dirfile_getreference(struct gdpy_dirfile_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  const char *ref = dirfile_reference(self->D, NULL);

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

  dirfile_reference(self->D, ref);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:getdata.dirifle.get_string",
        keywords, &field_code))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  char data[GD_MAX_LINE_LENGTH];

  get_string(self->D, field_code, GD_MAX_LINE_LENGTH, data);

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

  fields = get_field_list_by_type(self->D, GD_STRING_ENTRY);

  PYGD_CHECK_ERROR(self->D, NULL);

  values = get_strings(self->D);

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
        "sO|i:getdata.dirfile.put_constant", keywords, &field_code, &value,
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
    put_constant(self->D, field_code, GD_INT64, &data.s);
  else if ((data_type & 0xf) == GDPY_IEEE754)
    put_constant(self->D, field_code, GD_FLOAT64, &data.f);
  else if ((data_type & 0xf) == GDPY_COMPLEX)
    put_constant(self->D, field_code, GD_COMPLEX128, &data.c);
  else
    put_constant(self->D, field_code, GD_UINT64, &data.u);

  PYGD_CHECK_ERROR(self->D, NULL);

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
  off64_t first_frame = 0, first_sample = 0;
  gd_type_t type = GD_UNKNOWN;
  PyObject* pyobj;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "sO|iLL:getdata.dirfile.putdata", keywords, &field_code, &pyobj, &type,
        &first_frame, &first_sample)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  /* we only handle list data */
  if (!PyList_Check(pyobj)) {
    PyErr_SetString(PyExc_TypeError,
        "getdata.dirfile.putdata() argument 2 must be list");
    dreturn ("%p", NULL);
    return NULL;
  }

  size_t ns = PyList_Size(pyobj);

  if (ns > 0) {
    void* data = malloc(ns * 16);

    type = gdpy_convert_from_pylist(pyobj, data, type, ns);

    if (type == GD_UNKNOWN) {
      dreturn ("%p", NULL);
      return NULL;
    }

    ns = putdata64(self->D, field_code, first_frame, first_sample, 0, ns, type,
        data);

    PYGD_CHECK_ERROR2(self->D, NULL, free(data));

    free(data);
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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "ss:getdata.dirfile.put_string",
        keywords, &field_code, &data)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  put_string(self->D, field_code, data);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:getdata.dirfile.get_spf",
        keywords, &field_code)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  unsigned int spf = get_spf(self->D, field_code);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "s:getdata.dirfile.validate",
        keywords, &field_code)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  dirfile_validate(self->D, field_code);

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
  off64_t frame_start = 0;
  off64_t frame_end = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "sd|KK:getdata.dirfile.get_framenum", keywords, &field_code, &value,
        &frame_start, &frame_end))
  {
    dreturn ("%p", NULL);
    return NULL;
  }

  double frame = get_framenum_subset64(self->D, field_code, value, frame_start,
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
        "OO:getdata.dirfile.set_callback", keywords, &pycallback,
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

  dirfile_parser_callback(self->D, (pycallback == NULL) ? NULL :
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
        "i|i:getdata.dirfile.uninclude", keywords, &fragment_index, &del))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_uninclude(self->D, fragment_index, del);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "si|i:getdata.dirfile.move",
        keywords, &field_code, &new_fragment, &move_data)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  dirfile_move(self->D, field_code, new_fragment, move_data);

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

  if (!PyArg_ParseTupleAndKeywords(args, keys, "ss|i:getdata.dirfile.move",
        keywords, &old_code, &new_name, &move_data)) {
    dreturn ("%p", NULL);
    return NULL;
  }

  dirfile_rename(self->D, old_code, new_name, move_data);

  PYGD_CHECK_ERROR(self->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyGetSetDef gdpy_dirfile_getset[] = {
  { "error", (getter)gdpy_dirfile_geterror, NULL, "The Dirfile error.", NULL },
  { "error_string", (getter)gdpy_dirfile_geterrorstring, NULL,
    "A description of the Dirfile error.", NULL },
  { "name", (getter)gdpy_dirfile_getname, NULL, "The name of the Dirfile.",
    NULL },
  { "nfragments", (getter)gdpy_dirfile_getnfragments, NULL,
    "The number of format file fragments in the Dirfile.", NULL },
  { "nframes", (getter)gdpy_dirfile_getnframes, NULL,
    "The number of frames in the Dirfile.", NULL },
  { "reference", (getter)gdpy_dirfile_getreference,
    (setter)gdpy_dirfile_setreference, "The reference field for the Dirfile.",
    NULL },
  { NULL }
};

static PyMethodDef gdpy_dirfile_methods[] = {
  {"add", (PyCFunction)gdpy_dirfile_add, METH_VARARGS | METH_KEYWORDS,
    "Add a new field to a Dirfile."},
  {"add_spec", (PyCFunction)gdpy_dirfile_addspec, METH_VARARGS | METH_KEYWORDS,
    "Add a new field to a Dirfile."},
  {"alter", (PyCFunction)gdpy_dirfile_alter, METH_VARARGS | METH_KEYWORDS,
    "Alter the metadata of an existing field in the Dirfile."},
  {"alter_spec", (PyCFunction)gdpy_dirfile_alterspec, METH_VARARGS |
    METH_KEYWORDS, "Modify the metadata of an existing field in the Dirfile."},
  {"close", (PyCFunction)gdpy_dirfile_close, METH_NOARGS,
    "Flush and close the Dirfile."},
  {"delete", (PyCFunction)gdpy_dirfile_delentry, METH_VARARGS | METH_KEYWORDS,
    "Delete a field from the Dirfile."},
  {"discard", (PyCFunction)gdpy_dirfile_discard, METH_NOARGS,
    "Close the Dirfile without flushing metadata to disc first."},
  {"flush", (PyCFunction)gdpy_dirfile_flush, METH_VARARGS | METH_KEYWORDS,
    "Flush pending changes to the Dirfile to disc."},
  {"get_constant", (PyCFunction)gdpy_dirfile_getconstant,
    METH_VARARGS | METH_KEYWORDS,
    "Retrieve the value of a CONST field from the Dirfile."},
  {"get_constants", (PyCFunction)gdpy_dirfile_getconstants,
    METH_VARARGS | METH_KEYWORDS, "Retrieve a list of the name and value "
      "of all CONST fields from the Dirfile."},
  {"getdata", (PyCFunction)gdpy_dirfile_getdata, METH_VARARGS | METH_KEYWORDS,
    "Retrieve a data vector from the Dirfile."},
  { "get_entry", (PyCFunction)gdpy_dirfile_getentry, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the metatdata of one field in the Dirfile."},
  { "get_field_list", (PyCFunction)gdpy_dirfile_getfieldlist, METH_NOARGS,
    "Retrieve a list of the Dirfile field names."},
  {"get_field_list_by_type", (PyCFunction)gdpy_dirfile_getfieldlistbytype,
    METH_VARARGS | METH_KEYWORDS,
    "Retrieve a list of the Dirfile field names of the given type."},
  { "get_fragment", (PyCFunction)gdpy_dirfile_getfragment, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the metatdata of one fragment in the Dirfile."},
  {"get_fragment_index", (PyCFunction)gdpy_dirfile_getfragmentindex,
    METH_VARARGS | METH_KEYWORDS, "Returns the index number of the fragment"
      "defining the supplied field."},
  {"get_framenum", (PyCFunction)gdpy_dirfile_getframenum,
    METH_VARARGS | METH_KEYWORDS, "Given a data value, returns the fraction "
      "frame number associated with the value, based on a monotonic field."},
  {"get_mconstants", (PyCFunction)gdpy_dirfile_getmconstants, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the name and value of all CONST subfields for a "
      "given field from the Dirfile."},
  { "get_mfield_list", (PyCFunction)gdpy_dirfile_getmfieldlist,
    METH_VARARGS | METH_KEYWORDS,
    "Retrieve a list of the subfield names for a field from the Dirfile."},
  {"get_mfield_list_by_type", (PyCFunction)gdpy_dirfile_getmfieldlistbytype,
    METH_VARARGS | METH_KEYWORDS, "Retrieve a list of the subfield names of a "
      "given type for a field from the Dirfile."},
  {"get_mstrings", (PyCFunction)gdpy_dirfile_getmstrings, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the name and value of all STRING subfields for a "
      "given field from the Dirfile."},
  { "get_mvector_list", (PyCFunction)gdpy_dirfile_getmvectorlist,
    METH_VARARGS | METH_KEYWORDS, "Retrieve a list of the vector subfield "
      "names for a field from the Dirfile."},
  {"get_native_type", (PyCFunction)gdpy_dirfile_getnativetype,
    METH_VARARGS | METH_KEYWORDS,
    "Retrieve the native type of a field from the Dirfile."},
  {"get_native_type_name", (PyCFunction)gdpy_dirfile_getnativetypename,
    METH_VARARGS | METH_KEYWORDS, "Retrieve a string representation of the "
      "native type of a field from the Dirfile."},
  {"get_nfields", (PyCFunction)gdpy_dirfile_getnfields, METH_NOARGS,
    "Retrieve the number of fields in the Dirfile."},
  {"get_nfields_by_type", (PyCFunction)gdpy_dirfile_getnfieldsbytype,
    METH_VARARGS | METH_KEYWORDS,
    "Retrieve the number of fields of a given type from the Dirfile."},
  {"get_nmfields", (PyCFunction)gdpy_dirfile_getnmfields, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the number of subfields for a given field from "
      "the Dirfile."},
  {"get_nmfields_by_type", (PyCFunction)gdpy_dirfile_getnmfieldsbytype,
    METH_VARARGS | METH_KEYWORDS, "Retrieve the number of subfields of a given "
      "type for a field from the Dirfile."},
  {"get_nmvectors", (PyCFunction)gdpy_dirfile_getnmvectors, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the number of vector subfields for a given field "
      "from the Dirfile."},
  {"get_nvectors", (PyCFunction)gdpy_dirfile_getnvectors, METH_NOARGS,
    "Retrieve the number of vector fields from the Dirfile."},
  {"get_raw_filename", (PyCFunction)gdpy_dirfile_getrawfilename, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the name of the file on disk containing the raw "
      "data backing a RAW field."},
  {"get_spf", (PyCFunction)gdpy_dirfile_getspf, METH_VARARGS | METH_KEYWORDS,
    "Retrieve the samples-per-frame of a field from the Dirfile."},
  {"get_string", (PyCFunction)gdpy_dirfile_getstring, METH_VARARGS |
    METH_KEYWORDS, "Retrieve the value of a STRING field from the Dirfile."},
  { "get_strings", (PyCFunction)gdpy_dirfile_getstrings, METH_NOARGS,
    "Retrieve a list of all STRING fields and their values from the Dirfile."},
  { "get_vector_list", (PyCFunction)gdpy_dirfile_getvectorlist, METH_NOARGS,
    "Retrieve a list of the Dirfile vector field names."},
  {"include", (PyCFunction)gdpy_dirfile_include, METH_VARARGS | METH_KEYWORDS,
    "Add a new format file fragment to the Dirfile."},
  {"madd", (PyCFunction)gdpy_dirfile_madd, METH_VARARGS | METH_KEYWORDS,
    "Add a new subfield to a Dirfile."},
  {"madd_spec", (PyCFunction)gdpy_dirfile_maddspec, METH_VARARGS |
    METH_KEYWORDS, "Add a new subfield to a Dirfile."},
  {"malter_spec", (PyCFunction)gdpy_dirfile_malterspec, METH_VARARGS |
    METH_KEYWORDS,
    "Modify the metadata of an existing subfield in the Dirfile."},
  {"metaflush", (PyCFunction)gdpy_dirfile_metaflush, METH_NOARGS,
    "Flush pending metadata changes to the Dirfile to disc."},
  {"move", (PyCFunction)gdpy_dirfile_move, METH_VARARGS | METH_KEYWORDS,
    "Move a field specification to a differend format file fragment."},
  {"put_constant", (PyCFunction)gdpy_dirfile_putconstant, METH_VARARGS |
    METH_KEYWORDS, "Store the value of a CONST field to the Dirfile."},
  {"putdata", (PyCFunction)gdpy_dirfile_putdata, METH_VARARGS | METH_KEYWORDS,
    "Store a data to the Dirfile."},
  {"put_string", (PyCFunction)gdpy_dirfile_putstring, METH_VARARGS |
    METH_KEYWORDS, "Store the value of a STRING field to the Dirfile."},
  {"rename", (PyCFunction)gdpy_dirfile_rename, METH_VARARGS | METH_KEYWORDS,
    "Rename a field in the dirfile."},
  {"set_callback", (PyCFunction)gdpy_dirfile_callback, METH_VARARGS |
    METH_KEYWORDS, "Change or remove the parser callback function."},
  {"uninclude", (PyCFunction)gdpy_dirfile_uninclude, METH_VARARGS |
    METH_KEYWORDS, "Remove a format file fragment from the Dirfile."},
  {"validate", (PyCFunction)gdpy_dirfile_validate, METH_VARARGS | METH_KEYWORDS,
    "Returns non-zero if the specified field is invalid."},
  { NULL, NULL, 0, NULL }
};

PyTypeObject gdpy_dirfile = {
  PyObject_HEAD_INIT(NULL)
    0,                             /* ob_size */
  "getdata.dirfile",               /* tp_name */
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
  "The Dirfile object",            /* tp_doc */
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
