/* (C) 2009 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "pygetdata.h"

static const char* gdpy_entry_type_names[] = 
{
  "NO_ENTRY",       /* 0x00 */
  "RAW_ENTRY",      /* 0x01 */
  "LINCOM_ENTRY",   /* 0x02 */
  "LINTERP_ENTRY",  /* 0x03 */
  "BIT_ENTRY",      /* 0x04 */
  "MULTIPLY_ENTRY", /* 0x05 */
  "PHASE_ENTRY",    /* 0x06 */
  "INDEX_ENTRY",    /* 0x07 */
  "POLYNOM_ENTRY",  /* 0x08 */
  "SBIT_ENTRY",     /* 0x09 */
  NULL,             /* 0x0A - unused */
  NULL,             /* 0x0B - unused */
  NULL,             /* 0x0C - unused */
  NULL,             /* 0x0D - unused */
  NULL,             /* 0x0E - unused */
  NULL,             /* 0x0F - unused */
  "CONST_ENTRY",    /* 0x10 */
  "STRING_ENTRY",   /* 0x11 */
};

/* Entry */
static char* gdpy_dup_pystring(PyObject* obj)
{
  dtrace("%p", obj);
  char* s = PyString_AsString(obj);

  if (s != NULL) {
    s = strdup(s);

    if (s == NULL)
      PyErr_NoMemory();
  }

  dreturn("%p", s);
  return s;
}

static void gdpy_entry_delete(struct gdpy_entry_t* self)
{
  dtrace("%p", self);

  dirfile_free_entry_strings(self->E);
  free(self->E);

  dreturnvoid();
}

static PyObject* gdpy_entry_create(PyTypeObject *type, PyObject *args,
    PyObject *keys)
{
  dtrace("%p, %p, %p", type, args, keys);

  struct gdpy_entry_t *self = (struct gdpy_entry_t*)type->tp_alloc(type, 0);

  if (self) {
    self->E = NULL;
  }

  dreturn("%p", self);
  return (PyObject*)self;
}

static void gdpy_set_entry_from_tuple(gd_entry_t *E, PyObject* tuple,
    const char* name)
{
  PyObject *parm1;
  PyObject *parm2;
  PyObject *parm3;
  int i, count;

  dtrace("%p, %p, \"%s\"", E, tuple, name);

  int min;
  switch (E->field_type)
  {
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      min = 0;
      break;
    case GD_CONST_ENTRY:
      min = 1;
      break;
    case GD_RAW_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
      min = 2;
      break;
    case GD_LINCOM_ENTRY:
      min = 3;
      break;
    default:
      PyErr_Format(PyExc_TypeError, "%s: unrecognised field type", name);
      dreturnvoid();
      return;
  }

  if (min == 0) { /* nothing to do */
    dreturnvoid();
    return;
  }

  int size = PyTuple_Size(tuple);
  if (size < min) {
    PyErr_Format(PyExc_TypeError, "'getdata.entry' "
        "%s: needed %d entry parameters, but got only %d",
        name, min, size);
    dreturnvoid();
    return;
  }

  switch (E->field_type)
  {
    case GD_RAW_ENTRY:
      E->data_type = (int)PyInt_AsLong(PyTuple_GetItem(tuple, 0));
      if (E->data_type != GD_UINT8   && E->data_type != GD_INT8  &&
          E->data_type != GD_UINT16  && E->data_type != GD_INT16 &&
          E->data_type != GD_UINT32  && E->data_type != GD_INT32 &&
          E->data_type != GD_UINT64  && E->data_type != GD_INT64 &&
          E->data_type != GD_FLOAT32 && E->data_type != GD_FLOAT64)
      {
        PyErr_SetString(PyExc_ValueError, "'getdata.entry' invalid data type");
      }

      E->spf = (unsigned int)PyLong_AsUnsignedLong(PyTuple_GetItem(tuple, 1));
      break;
    case GD_LINCOM_ENTRY:
      parm1 = PyTuple_GetItem(tuple, 0);
      parm2 = PyTuple_GetItem(tuple, 1);
      parm3 = PyTuple_GetItem(tuple, 2);
      if (!PyTuple_Check(parm1) || !PyTuple_Check(parm2) ||
          !PyTuple_Check(parm3))
      {
        PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
            "LINCOM parameters must be tuples");
        dreturnvoid();
        return;
      }

      count = E->n_fields = PyTuple_Size(parm1);
      if (count > GD_MAX_LINCOM)
        count = GD_MAX_LINCOM;

      if (PyTuple_Size(parm2) < count || PyTuple_Size(parm3) < count) {
        PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
            "Missing data in LINCOM parameters");
        dreturnvoid();
        return;
      }

      for (i = 0; i < count; ++i) {
        E->in_fields[i] = gdpy_dup_pystring(PyTuple_GetItem(parm1, i));

        if (PyErr_Occurred()) {
          dreturnvoid();
          return;
        }

        E->m[i] = PyFloat_AsDouble(PyTuple_GetItem(parm2, i));
        E->b[i] = PyFloat_AsDouble(PyTuple_GetItem(parm3, i));
      }
      break;
    case GD_LINTERP_ENTRY:
      E->in_fields[0] = gdpy_dup_pystring(PyTuple_GetItem(tuple, 0));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }

      E->table = gdpy_dup_pystring(PyTuple_GetItem(tuple, 1));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      E->in_fields[0] = gdpy_dup_pystring(PyTuple_GetItem(tuple, 0));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }

      E->bitnum = (int)PyInt_AsLong(PyTuple_GetItem(tuple, 1));
      if (size > 2)
        E->numbits = (int)PyInt_AsLong(PyTuple_GetItem(tuple, 2));
      else
        E->numbits = 1;
      break;
    case GD_MULTIPLY_ENTRY:
      E->in_fields[0] = gdpy_dup_pystring(PyTuple_GetItem(tuple, 0));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }

      E->in_fields[1] = gdpy_dup_pystring(PyTuple_GetItem(tuple, 1));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }
      break;
    case GD_PHASE_ENTRY:
      E->in_fields[0] = gdpy_dup_pystring(PyTuple_GetItem(tuple, 0));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }

      E->shift = (int)PyInt_AsLong(PyTuple_GetItem(tuple, 0));
      break;
    case GD_POLYNOM_ENTRY:
      parm2 = PyTuple_GetItem(tuple, 1);
      if (!PyTuple_Check(parm2)) {
        PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
            "POLYNOM requires a tuple of co-efficients");
        dreturnvoid();
        return;
      }

      E->poly_ord = count = PyTuple_Size(parm2) - 1;
      if (count > GD_MAX_POLYORD)
        count = GD_MAX_POLYORD;

      E->in_fields[0] = gdpy_dup_pystring(PyTuple_GetItem(tuple, 0));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }

      for (i = 0; i <= count; ++i)
        E->a[i] = PyFloat_AsDouble(PyTuple_GetItem(parm2, i));
      break;
    case GD_CONST_ENTRY:
      E->const_type = (int)PyInt_AsLong(PyTuple_GetItem(tuple, 0));
      if (E->const_type != GD_UINT8   && E->const_type != GD_INT8  &&
          E->const_type != GD_UINT16  && E->const_type != GD_INT16 &&
          E->const_type != GD_UINT32  && E->const_type != GD_INT32 &&
          E->const_type != GD_UINT64  && E->const_type != GD_INT64 &&
          E->const_type != GD_FLOAT32 && E->const_type != GD_FLOAT64)
      {
        PyErr_SetString(PyExc_ValueError, "'getdata.entry' invalid data type");
      }
    case GD_NO_ENTRY:
    case GD_STRING_ENTRY:
    case GD_INDEX_ENTRY:
      break;
  }

  dreturnvoid();
}

static void gdpy_set_entry_from_dict(gd_entry_t *E, PyObject* parms,
    const char* name)
{
  dtrace("%p, %p, \"%s\"", E, parms, name);

  PyObject* tuple;
  const char* key[3];
  int i, size = 0;

  /* convert the dictionary to a tuple */

  /* variadic objects for entry types:
   * RAW:      type, spf                   = 2
   * LINCOM:   in_fields, m, b             = 3
   * LINTERP:  in_field, table             = 2
   * (S)BIT:   in_field, bitnum, (numbits) = 2/3
   * PHASE:    in_field, shift             = 2
   * MULTIPLY: in_field1, in_field2        = 2
   * POLYNOM:  in_field, a                 = 2
   * CONST:    type                        = 1
   * STRING:   (none)                      = 0
   * INDEX:    (none)                      = 0
   */

  switch(E->field_type)
  {
    case GD_RAW_ENTRY:
      key[0] = "type";
      key[1] = "a";
      size = 2;
      break;
    case GD_LINCOM_ENTRY:
      key[0] = "in_fields";
      key[1] = "m";
      key[2] = "b";
      size = 3;
      break;
    case GD_LINTERP_ENTRY:
      key[0] = "in_field";
      key[1] = "table";
      size = 2;
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      key[0] = "in_field";
      key[1] = "bitnum";
      if (PyDict_GetItemString(parms, "numbits")) {
        key[2] = "numbits";
        size = 3;
      } else
        size = 2;
      break;
    case GD_MULTIPLY_ENTRY:
      key[0] = "in_field1";
      key[1] = "in_field2";
      size = 2;
      break;
    case GD_PHASE_ENTRY:
      key[0] = "in_field";
      key[1] = "shift";
      size = 2;
      break;
    case GD_POLYNOM_ENTRY:
      key[0] = "in_field";
      key[1] = "a";
      size = 2;
      break;
    case GD_CONST_ENTRY:
      key[0] = "type";
      size = 1;
      break;
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
    case GD_INDEX_ENTRY:
      tuple = Py_None;
      size = 0;
      break;
  }

  if (size > 0) {
    tuple = PyTuple_New(size);
    for (i = 0; i < size; ++i) {
      PyObject* o = PyDict_GetItemString(parms, key[i]);

      if (o == NULL) {
        PyErr_Format(PyExc_KeyError, "%s: missing required parameter key %s",
            name, key[i]);
        dreturnvoid();
        return;
      }

      PyTuple_SET_ITEM(tuple, i, o);
    }
  }

  gdpy_set_entry_from_tuple(E, tuple, name);

  dreturnvoid();
}

static int gdpy_entry_init(struct gdpy_entry_t* self, PyObject *args,
    PyObject *keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  gd_entry_t E;
  char *keywords[] = {"type", "name", "fragment_index", "parameters", NULL};
  PyObject* parms;
  const char* field_name;

  memset(&E, 0, sizeof(gd_entry_t));

  if (!PyArg_ParseTupleAndKeywords(args, keys, "isiO:getdata.entry.__init__",
        keywords, &E.field_type, &field_name, &E.fragment_index, &parms))
  {
    dreturn("%i", -1);
    return -1;
  }

  E.field = strdup(field_name);
  if (E.field == NULL) {
    PyErr_NoMemory();
    dreturn("%i", -1);
    return -1;
  }

  if (PyDict_Check(parms))
    gdpy_set_entry_from_dict(&E, parms, "getdata.entry.__init__");
  else if (PyTuple_Check(parms))
    gdpy_set_entry_from_tuple(&E, parms, "getdata.entry.__init__");
  else 
    PyErr_SetString(PyExc_TypeError,
        "getdata.dirfile.__init__() argument 3 must be a tuple or dictionary");

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  if (self->E == NULL) {
    self->E = malloc(sizeof(gd_entry_t));

    if (self->E == NULL) {
      dreturn("%i", -1);
      return -1;
    }
  } else 
    dirfile_free_entry_strings(self->E);

  memcpy(self->E, &E, sizeof(gd_entry_t));

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getname(struct gdpy_entry_t* self, void* closure)
{
  dtrace("%p, %p", self, closure);

  PyObject* pyname = PyString_FromString(self->E->field);

  dreturn("%p", pyname);
  return pyname;
}

static int gdpy_entry_setname(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  char *s = gdpy_dup_pystring(value);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  free(self->E->field);
  self->E->field = s;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getfragment(struct gdpy_entry_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  PyObject* pyobj = PyInt_FromLong(self->E->fragment_index);

  dreturn("%p", pyobj);
  return pyobj;
}

static int gdpy_entry_setfragment(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  int t = (int)PyInt_AsLong(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->fragment_index = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_gettypename(struct gdpy_entry_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);

  PyObject* pyobj =
    PyString_FromString(gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_entry_gettype(struct gdpy_entry_t* self, void* closure)
{
  dtrace("%p, %p", self, closure);

  PyObject* pyobj = PyInt_FromLong(self->E->field_type);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject* gdpy_entry_getinfields(struct gdpy_entry_t* self,
    void* closure)
{
  int i;

  dtrace("%p, %p", self, closure);

  PyObject* tuple = NULL;

  switch (self->E->field_type)
  {
    case GD_LINCOM_ENTRY:
      tuple = PyTuple_New(self->E->n_fields);
      for (i = 0; i < self->E->n_fields; ++i)
        PyTuple_SetItem(tuple, i, PyString_FromString(self->E->in_fields[i]));
      break;
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
      tuple = Py_BuildValue("(s)", self->E->in_fields[0]);
      break;
    case GD_MULTIPLY_ENTRY:
      tuple = Py_BuildValue("(ss)", self->E->in_fields[0],
          self->E->in_fields[1]);
      break;
    case GD_NO_ENTRY:
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
      PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
          "attribute 'in_fields' not available for entry type %s",
          gdpy_entry_type_names[self->E->field_type]); 
      break;
  }

  dreturn("%p", tuple);
  return tuple;
}

static int gdpy_entry_setinfields(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  char*  s[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);

  switch (self->E->field_type)
  {
    case GD_LINCOM_ENTRY:
      if (!PyTuple_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
            "attribute 'in_fields' must be a tuple");
        dreturn("%i", -1);
        return -1;
      }

      if (PyTuple_Size(value) < self->E->n_fields) {
        PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
            "not enough items in tuple for in_fields");
        dreturn("%i", -1);
        return -1;
      }

      for (i = 0; i < self->E->n_fields; ++i)
        s[i] = gdpy_dup_pystring(PyTuple_GetItem(value, i));

      if (PyErr_Occurred()) {
        dreturn("%i", -1);
        return -1;
      }

      for (i = 0; i < self->E->n_fields; ++i) {
        free(self->E->in_fields[i]);
        self->E->in_fields[i] = s[i];
      }
      break;
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
      if (!PyTuple_Check(value))
        s[0] = gdpy_dup_pystring(value);
      else {
        if (PyTuple_Size(value) < 1) {
          PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
              "not enough items in tuple for in_fields");
          dreturn("%i", -1);
        }

        s[0] = gdpy_dup_pystring(PyTuple_GetItem(value, 0));
      }

      if (PyErr_Occurred()) {
        dreturn("%i", -1);
        return -1;
      }

      free(self->E->in_fields[0]);
      self->E->in_fields[0] = s[0];
      break;
    case GD_MULTIPLY_ENTRY:
      if (!PyTuple_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
            "attribute 'in_fields' must be a tuple");
        dreturn("%i", -1);
        return -1;
      }

      if (PyTuple_Size(value) < 2) {
        PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
            "not enough items in tuple for in_fields");
        dreturn("%i", -1);
        return -1;
      }

      for (i = 0; i < 2; ++i)
        s[i] = gdpy_dup_pystring(PyTuple_GetItem(value, i));

      if (PyErr_Occurred()) {
        dreturn("%i", -1);
        return -1;
      }

      for (i = 0; i < 2; ++i) {
        free(self->E->in_fields[i]);
        self->E->in_fields[i] = s[i];
      }
      break;
    case GD_NO_ENTRY:
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
      PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
          "attribute 'in_fields' not available for entry type %s",
          gdpy_entry_type_names[self->E->field_type]); 
      break;
  }

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getdatatypename(struct gdpy_entry_t* self,
    void* closure)
{
  PyObject* obj = NULL;

  int t = -1;
  char buffer[11];

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY)
    t = self->E->data_type;
  else if (self->E->field_type == GD_CONST_ENTRY)
    t = self->E->const_type;
  else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'data_type_name' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  if (t != -1) {
    sprintf(buffer, "%s%i", (t & GD_IEEE754) ? "FLOAT" : (t & GD_SIGNED) ?
        "INT" : "UINT", 8 * GD_SIZE(t));
    obj = PyString_FromString(buffer);
  }

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_entry_getdatatype(struct gdpy_entry_t* self,
    void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY)
    obj = PyInt_FromLong(self->E->data_type);
  else if (self->E->field_type == GD_CONST_ENTRY)
    obj = PyInt_FromLong(self->E->const_type);
  else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'data_type' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setdatatype(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_RAW_ENTRY &&
      self->E->field_type != GD_CONST_ENTRY)
  {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'data_type' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  int t = PyInt_AsLong(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  /* The C library is significantly more lax about this. (It just checks a few
   * key bits) */
  if (t != GD_UINT8 && t != GD_INT8 && t != GD_UINT16 && t != GD_INT16 &&
      t != GD_UINT32 && t != GD_INT32 && t != GD_UINT64 && t != GD_INT64 &&
      t != GD_FLOAT32 && t != GD_FLOAT64)
  {
    PyErr_SetString(PyExc_ValueError, "'getdata.entry' invalid data type");
    dreturn("%i", -1);
    return -1;
  }

  if (self->E->field_type == GD_RAW_ENTRY)
    self->E->data_type = t;
  else
    self->E->const_type = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getspf(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY)
    obj = PyInt_FromLong(self->E->spf);
  else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'spf' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setspf(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_RAW_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'spf' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  unsigned int t = (unsigned int)PyLong_AsUnsignedLongLong(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->spf = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getnfields(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_LINCOM_ENTRY) {
    obj = PyInt_FromLong(self->E->n_fields);
  } else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'n_fields' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setnfields(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'n_fields' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  int n = (int)PyInt_AsLong(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  } else if (n < 0 || n > GD_MAX_LINCOM) {
    PyErr_SetString(PyExc_ValueError, "'getdata.entry' "
        "attribute 'n_fields' out of range");
    dreturn("%i", -1);
    return -1;
  }

  /* free extra terms */
  for (i = n; i < self->E->n_fields; ++i)
    free(self->E->in_fields[i]);

  /* initialise new terms */
  for (i = self->E->n_fields; i < n; ++i) {
    self->E->in_fields[i] = strdup("");
    self->E->m[i] = self->E->b[i] = 0;
  }

  self->E->n_fields = n;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getm(struct gdpy_entry_t* self, void* closure)
{
  int i;
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_LINCOM_ENTRY) {
    obj = PyTuple_New(self->E->n_fields);
    for (i = 0; i < self->E->n_fields; ++i)
      PyTuple_SetItem(obj, i, PyFloat_FromDouble(self->E->m[i]));
  } else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'm' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setm(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  double m[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'm' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
        "attribute 'm' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->n_fields) {
    PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
        "not enough items in tuple for attribute 'm'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i)
    m[i] = PyFloat_AsDouble(PyTuple_GetItem(value, i));

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i)
    self->E->m[i] = m[i];

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getb(struct gdpy_entry_t* self, void* closure)
{
  int i;
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_LINCOM_ENTRY) {
    obj = PyTuple_New(self->E->n_fields);
    for (i = 0; i < self->E->n_fields; ++i)
      PyTuple_SetItem(obj, i, PyFloat_FromDouble(self->E->b[i]));
  } else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'b' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setb(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  double b[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'b' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
        "attribute 'b' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->n_fields) {
    PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
        "not enough items in tuple for attribute 'b'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i)
    b[i] = PyFloat_AsDouble(PyTuple_GetItem(value, i));

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i)
    self->E->b[i] = b[i];

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_gettable(struct gdpy_entry_t* self, void* closure)
{
  dtrace("%p, %p", self, closure);

  PyObject* obj = NULL;
  if (self->E->field_type == GD_LINTERP_ENTRY)
    obj = PyString_FromString(self->E->table);
  else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'table' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_settable(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type == GD_LINTERP_ENTRY) {
    char *s = gdpy_dup_pystring(value);

    if (PyErr_Occurred()) {
      dreturn("%i", -1);
      return -1;
    }

    free(self->E->field);
    self->E->field = s;
  } else {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'table' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getbitnum(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type != GD_BIT_ENTRY &&
      self->E->field_type != GD_SBIT_ENTRY)
  {
    obj = PyInt_FromLong(self->E->bitnum);
  } else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'bitnum' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setbitnum(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_BIT_ENTRY &&
      self->E->field_type != GD_SBIT_ENTRY)
  {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'bitnum' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  unsigned int t = (unsigned int)PyLong_AsUnsignedLongLong(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->bitnum = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getnumbits(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type != GD_BIT_ENTRY &&
      self->E->field_type != GD_SBIT_ENTRY)
  {
    obj = PyInt_FromLong(self->E->numbits);
  } else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'numbits' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setnumbits(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_BIT_ENTRY &&
      self->E->field_type != GD_SBIT_ENTRY)
  {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'numbits' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  unsigned int t = (unsigned int)PyLong_AsUnsignedLongLong(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->numbits = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getshift(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY)
    obj = PyInt_FromLong(self->E->shift);
  else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'shift' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setshift(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_RAW_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'shift' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  unsigned int t = (unsigned int)PyLong_AsUnsignedLongLong(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->shift = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_geta(struct gdpy_entry_t* self, void* closure)
{
  int i;
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_POLYNOM_ENTRY) {
    obj = PyTuple_New(self->E->poly_ord + 1);
    for (i = 0; i <= self->E->poly_ord; ++i)
      PyTuple_SetItem(obj, i, PyFloat_FromDouble(self->E->a[i]));
  } else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'a' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_seta(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  double a[GD_MAX_POLYORD + 1];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'm' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
        "attribute 'a' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->poly_ord + 1) {
    PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
        "not enough items in tuple for attribute 'a'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i <= self->E->poly_ord; ++i)
    a[i] = PyFloat_AsDouble(PyTuple_GetItem(value, i));

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i <= self->E->poly_ord; ++i)
    self->E->a[i] = a[i];

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getpolyord(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_POLYNOM_ENTRY)
    obj = PyInt_FromLong(self->E->poly_ord);
  else
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'poly_ord' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setpolyord(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_POLYNOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'getdata.entry' "
        "attribute 'poly_ord' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  int n = (int)PyLong_AsUnsignedLongLong(value);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  } else if (n < 1 || n > GD_MAX_POLYORD) {
    PyErr_SetString(PyExc_ValueError, "'getdata.entry' "
        "attribute 'poly_ord' out of range");
    dreturn("%i", -1);
    return -1;
  }

  self->E->poly_ord = n;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getparms(struct gdpy_entry_t* self, void* closure)
{
  int i;

  dtrace("%p, %p", self, closure);

  PyObject *a, *tuple = NULL;

  switch (self->E->field_type)
  {
    case GD_NO_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      tuple = Py_BuildValue("()");
      break;
    case GD_CONST_ENTRY:
      tuple = Py_BuildValue("(i)", self->E->const_type);
      break;
    case GD_RAW_ENTRY:
      tuple = Py_BuildValue("(iI)", self->E->data_type, self->E->spf);
      break;
    case GD_LINTERP_ENTRY:
      tuple = Py_BuildValue("(ss)", self->E->in_fields[0], self->E->table);
      break;
    case GD_MULTIPLY_ENTRY:
      tuple = Py_BuildValue("(ss)", self->E->in_fields[0],
          self->E->in_fields[1]);
      break;
    case GD_PHASE_ENTRY:
      tuple = Py_BuildValue("(si)", self->E->in_fields[0], self->E->shift);
      break;
    case GD_POLYNOM_ENTRY:
      a = PyTuple_New(self->E->poly_ord + 1);
      for (i = 0; i <= self->E->poly_ord; ++i)
        PyTuple_SetItem(a, i, PyFloat_FromDouble(self->E->a[i]));
      tuple = Py_BuildValue("(sO)", self->E->in_fields[0], a);
      break;
    case GD_LINCOM_ENTRY:
      switch (self->E->n_fields) {
        case 1:
          tuple = Py_BuildValue("((s)(d)(d))", self->E->in_fields[0],
              self->E->m[0], self->E->b[0]);
          break;
        case 2:
          tuple = Py_BuildValue("((ss)(dd)(dd))", self->E->in_fields[0],
              self->E->in_fields[1], self->E->m[0], self->E->m[1],
              self->E->b[0], self->E->b[1]);
          break;
        case 3:
          tuple = Py_BuildValue("((sss)(ddd)(ddd))", self->E->in_fields[0],
              self->E->in_fields[1], self->E->in_fields[2], self->E->m[0],
              self->E->m[1], self->E->m[2], self->E->b[0], self->E->b[1],
              self->E->b[2]);
          break;
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      tuple = Py_BuildValue("(sii)", self->E->in_fields[0], self->E->bitnum,
          self->E->numbits);
      break;
  }

  dreturn("%p", tuple);
  return tuple;
}

static int gdpy_entry_setparms(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));

  E.field = self->E->field;
  E.field_type = self->E->field_type;
  E.fragment_index = self->E->fragment_index;

  if (PyDict_Check(value))
    gdpy_set_entry_from_dict(&E, value, "getdata.entry");
  else if (PyTuple_Check(value))
    gdpy_set_entry_from_tuple(&E, value, "getdata.entry");
  else 
    PyErr_SetString(PyExc_TypeError, "'getdata.entry' "
        "attribute 'parameters' must be a tuple or dictionary");
  
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->field = NULL;

  dirfile_free_entry_strings(self->E);
  memcpy(self->E, &E, sizeof(gd_entry_t));

  dreturn("%i", 0);
  return 0;
}

static PyGetSetDef gdpy_entry_getset[] = {
  { "a", (getter)gdpy_entry_geta, (setter)gdpy_entry_seta,
    "The POLYNOM co-efficients.", NULL },
  { "b", (getter)gdpy_entry_getb, (setter)gdpy_entry_setb,
    "The LINCOM offsets.", NULL },
  { "bitnum", (getter)gdpy_entry_getbitnum, (setter)gdpy_entry_setbitnum,
    "The starting bit for a bitfield.", NULL },
  { "const_type", (getter)gdpy_entry_getdatatype,
    (setter)gdpy_entry_setdatatype, "An alias for the data_type attribute.",
    NULL },
  { "data_type", (getter)gdpy_entry_getdatatype, (setter)gdpy_entry_setdatatype,
    "The undelying data type of the field's data.", NULL },
  { "data_type_name", (getter)gdpy_entry_getdatatypename, NULL,
    "A string representation of the data type.", NULL },
  { "field_type", (getter)gdpy_entry_gettype, NULL, "The field type.", NULL },
  { "field_type_name", (getter)gdpy_entry_gettypename, NULL,
    "A string representation of the field type.", NULL },
  { "fragment", (getter)gdpy_entry_getfragment, (setter)gdpy_entry_setfragment,
    "The number of the fragment that defines this field.", NULL },
  { "in_fields", (getter)gdpy_entry_getinfields, (setter)gdpy_entry_setinfields,
    "A tuple containing the input field codes for a derived field.", NULL },
  { "m", (getter)gdpy_entry_getm, (setter)gdpy_entry_setm,
    "The LINCOM gains.", NULL },
  { "n_fields", (getter)gdpy_entry_getnfields, (setter)gdpy_entry_setnfields,
    "The number of terms in the LINCOM.", NULL },
  { "name", (getter)gdpy_entry_getname, (setter)gdpy_entry_setname,
    "The name of this field.", NULL },
  { "numbits", (getter)gdpy_entry_getnumbits, (setter)gdpy_entry_setnumbits,
    "The length of a bitfield.", NULL },
  { "parameters", (getter)gdpy_entry_getparms, (setter)gdpy_entry_setparms,
    "The type-specific parameters for the field.", NULL },
  { "poly_ord", (getter)gdpy_entry_getpolyord, (setter)gdpy_entry_setpolyord,
    "The polynomial order of a POLYORD field.", NULL },
  { "shift", (getter)gdpy_entry_getshift, (setter)gdpy_entry_setshift,
    "The shift of a PHASE field.", NULL },
  { "spf", (getter)gdpy_entry_getspf, (setter)gdpy_entry_setspf,
    "The samples-per-frame of a RAW field.", NULL },
  { "table", (getter)gdpy_entry_gettable, (setter)gdpy_entry_settable,
    "The look-up table path of a LINTERP field.", NULL },
  { NULL }
};

PyTypeObject gdpy_entry = {
  PyObject_HEAD_INIT(NULL)
    0,                           /* ob_size */
  "getdata.entry",               /* tp_name */
  sizeof(struct gdpy_entry_t),   /* tp_basicsize */
  0,                             /* tp_itemsize */
  (destructor)gdpy_entry_delete, /* tp_dealloc */
  0,                             /* tp_print */
  0,                             /* tp_getattr */
  0,                             /* tp_setattr */
  0,                             /* tp_compare */
  0,                             /* tp_repr */
  0,                             /* tp_as_number */
  0,                             /* tp_as_sequence */
  0,                             /* tp_as_mapping */
  0,                             /* tp_hash */
  0,                             /* tp_call */
  0,                             /* tp_str */
  0,                             /* tp_getattro */
  0,                             /* tp_setattro */
  0,                             /* tp_as_buffer */
  Py_TPFLAGS_DEFAULT,            /* tp_flags */
  "The Dirfile entry object",    /* tp_doc */
  0,                             /* tp_traverse */
  0,                             /* tp_clear */
  0,                             /* tp_richcompare */
  0,                             /* tp_weaklistoffset */
  0,                             /* tp_iter */
  0,                             /* tp_iternext */
  0,                             /* tp_methods */
  0,                             /* tp_members */
  gdpy_entry_getset,             /* tp_getset */
  0,                             /* tp_base */
  0,                             /* tp_dict */
  0,                             /* tp_descr_get */
  0,                             /* tp_descr_set */
  0,                             /* tp_dictoffset */
  (initproc)gdpy_entry_init,     /* tp_init */
  0,                             /* tp_alloc */
  gdpy_entry_create,             /* tp_new */
};
