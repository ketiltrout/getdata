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
  "DIVIDE_ENTRY",   /* 0x0A */
  "RECIP_ENTRY",    /* 0x0B */
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

  gd_free_entry_strings(self->E);
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

static void gdpy_set_scalar_from_pyobj(PyObject* pyobj, gd_type_t type,
    char** scalar, void* data)
{
  dtrace("%p, %x, %p, %p", pyobj, type, scalar, data);

  if (PyString_Check(pyobj))
    *scalar = gdpy_dup_pystring(pyobj);
  else {
    *scalar = NULL;
    if (type == GD_INT64)
      *(int64_t*)data = (int64_t)PyLong_AsLongLong(pyobj);
    else if (type & GD_COMPLEX128)
      *(double complex*)data = gdpy_as_complex(pyobj);
    else if (type & GD_FLOAT64)
      *(double*)data = PyFloat_AsDouble(pyobj);
    else if (type & GD_INT16)
      *(int16_t*)data = PyLong_AsUnsignedLong(pyobj);
    else if (type & GD_UINT16)
      *(uint16_t*)data = PyLong_AsUnsignedLong(pyobj);
  }

  dreturnvoid();
}

static void gdpy_set_entry_from_tuple(gd_entry_t *E, PyObject* tuple,
    const char* name)
{
  PyObject *parm1;
  PyObject *parm2;
  PyObject *parm3;
  PyObject *obj;
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
    case GD_DIVIDE_ENTRY:
    case GD_RECIP_ENTRY:
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
    PyErr_Format(PyExc_TypeError, "'pygetdata.entry' "
        "%s: needed %d entry parameters, but got only %d",
        name, min, size);
    dreturnvoid();
    return;
  }

  switch (E->field_type)
  {
    case GD_RAW_ENTRY:
      E->data_type = (gd_type_t)PyInt_AsLong(PyTuple_GetItem(tuple, 0));
      if (GDPY_INVALID_TYPE(E->data_type))
        PyErr_SetString(PyExc_ValueError,
            "'pygetdata.entry' invalid data type");

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 1), GD_UINT16,
          &E->scalar[0], &E->spf);
      break;
    case GD_LINCOM_ENTRY:
      parm1 = PyTuple_GetItem(tuple, 0);
      parm2 = PyTuple_GetItem(tuple, 1);
      parm3 = PyTuple_GetItem(tuple, 2);
      if (!PyTuple_Check(parm1) || !PyTuple_Check(parm2) ||
          !PyTuple_Check(parm3))
      {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
            "LINCOM parameters must be tuples");
        dreturnvoid();
        return;
      }

      count = E->n_fields = PyTuple_Size(parm1);
      if (count > GD_MAX_LINCOM)
        count = GD_MAX_LINCOM;

      if (PyTuple_Size(parm2) < count || PyTuple_Size(parm3) < count) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
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

        obj = PyTuple_GetItem(parm2, i);
        if (PyComplex_Check(obj)) {
          E->comp_scal = 1;
          E->cm[i] = gdpy_as_complex(obj);
        } else if (E->comp_scal)
          gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, &E->scalar[i],
              &E->cm[i]);
        else {
          gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, &E->scalar[i], &E->m[i]);
          E->cm[i] = E->m[i];
        }

        if (PyErr_Occurred()) {
          dreturnvoid();
          return;
        }

        obj = PyTuple_GetItem(parm3, i);
        if (PyComplex_Check(obj)) {
          E->comp_scal = 1;
          E->cb[i] = gdpy_as_complex(obj);
        } else if (E->comp_scal)
          gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128,
              &E->scalar[i + GD_MAX_LINCOM], &E->cb[i]);
        else {
          gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64,
              &E->scalar[i + GD_MAX_LINCOM], &E->b[i]);
          E->cb[i] = E->b[i];
        }

        if (PyErr_Occurred()) {
          dreturnvoid();
          return;
        }
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

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 1), GD_INT16,
          &E->scalar[0], &E->bitnum);
      if (size > 2)
        gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 2), GD_INT16,
            &E->scalar[1], &E->numbits);
      else {
        E->numbits = 1;
        E->scalar[1] = NULL;
      }
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
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
    case GD_RECIP_ENTRY:
      E->in_fields[0] = gdpy_dup_pystring(PyTuple_GetItem(tuple, 0));

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }

      obj = PyTuple_GetItem(tuple, 1);
      if (PyComplex_Check(obj)) {
        E->comp_scal = 1;
        E->cdividend = gdpy_as_complex(obj);
      } else if (E->comp_scal)
        gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, &E->scalar[0],
            &E->cdividend);
      else {
        gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, &E->scalar[0],
            &E->dividend);
        E->cdividend = E->dividend;
      }

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

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 1), GD_INT64,
          &E->scalar[0], &E->shift);
      break;
    case GD_POLYNOM_ENTRY:
      parm2 = PyTuple_GetItem(tuple, 1);
      if (!PyTuple_Check(parm2)) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
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

      for (i = 0; i <= count; ++i) {
        obj = PyTuple_GetItem(parm2, i);
        if (PyComplex_Check(obj)) {
          E->comp_scal = 1;
          E->ca[i] = gdpy_as_complex(obj);
          E->scalar[i] = NULL;
        } else if (E->comp_scal)
          gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, &E->scalar[i],
              &E->ca[i]);
        else {
          gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, &E->scalar[i], &E->a[i]);
          E->ca[i] = E->a[i];
        }

        if (PyErr_Occurred()) {
          dreturnvoid();
          return;
        }
      }
      break;
    case GD_CONST_ENTRY:
      E->const_type = (gd_type_t)PyInt_AsLong(PyTuple_GetItem(tuple, 0));
      if (GDPY_INVALID_TYPE(E->const_type))
        PyErr_SetString(PyExc_ValueError,
            "'pygetdata.entry' invalid data type");
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
   * DIVIDE:   in_field1, in_field2        = 2
   * RECIP:    in_field, dividend          = 2
   * POLYNOM:  in_field, a                 = 2
   * CONST:    type                        = 1
   * STRING:   (none)                      = 0
   * INDEX:    (none)                      = 0
   */

  switch(E->field_type)
  {
    case GD_RAW_ENTRY:
      key[0] = "type";
      key[1] = "spf";
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
    case GD_DIVIDE_ENTRY:
      key[0] = "in_field1";
      key[1] = "in_field2";
      size = 2;
      break;
    case GD_RECIP_ENTRY:
      key[0] = "in_field";
      key[1] = "dividend";
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
  PyObject* parms = NULL;
  const char* field_name;

  memset(&E, 0, sizeof(gd_entry_t));

  if (!PyArg_ParseTupleAndKeywords(args, keys, "isi|O:pygetdata.entry.__init__",
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

  /* check for valid field type */
  if (E.field_type > 0x11 || E.field_type <= 0 ||
      gdpy_entry_type_names[E.field_type] == NULL) {
    PyErr_SetString(PyExc_ValueError,
        "'pygetdata.entry.__init__' invalid entry type");
    dreturn("%i", -1);
    return -1;
  }

  if (E.field_type == GD_STRING_ENTRY) 
    ; /* no parameters required */
  else if (parms == NULL)
    PyErr_Format(PyExc_TypeError, "pygetdata.entry.__init__() initialisation "
        "of %s require parameter tuple or dictionary",
        gdpy_entry_type_names[E.field_type]);
  else if (PyDict_Check(parms))
    gdpy_set_entry_from_dict(&E, parms, "pygetdata.entry.__init__");
  else if (PyTuple_Check(parms))
    gdpy_set_entry_from_tuple(&E, parms, "pygetdata.entry.__init__");
  else 
    PyErr_SetString(PyExc_TypeError, "pygetdata.dirfile.__init__() argument 3 "
        "must be a tuple or dictionary");

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
    gd_free_entry_strings(self->E);

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
    case GD_RECIP_ENTRY:
      tuple = Py_BuildValue("(s)", self->E->in_fields[0]);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
      tuple = Py_BuildValue("(ss)", self->E->in_fields[0],
          self->E->in_fields[1]);
      break;
    case GD_NO_ENTRY:
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
      PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
            "attribute 'in_fields' must be a tuple");
        dreturn("%i", -1);
        return -1;
      }

      if (PyTuple_Size(value) < self->E->n_fields) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
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
    case GD_RECIP_ENTRY:
      if (!PyTuple_Check(value))
        s[0] = gdpy_dup_pystring(value);
      else {
        if (PyTuple_Size(value) < 1) {
          PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
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
    case GD_DIVIDE_ENTRY:
      if (!PyTuple_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
            "attribute 'in_fields' must be a tuple");
        dreturn("%i", -1);
        return -1;
      }

      if (PyTuple_Size(value) < 2) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
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
      PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'data_type_name' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  if (t != -1) {
    sprintf(buffer, "%s%i", (t & GD_COMPLEX) ? "COMPLEX" :
        (t & GD_IEEE754) ? "FLOAT" : (t & GD_SIGNED) ?  "INT" : "UINT",
        8 * GD_SIZE(t));
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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
  if (GDPY_INVALID_TYPE(t)) {
    PyErr_SetString(PyExc_ValueError, "'pygetdata.entry' invalid data type");
    dreturn("%i", -1);
    return -1;
  }

  if (self->E->field_type == GD_RAW_ENTRY)
    self->E->data_type = (gd_type_t)t;
  else
    self->E->const_type = (gd_type_t)t;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getspf(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY) {
    if (self->E->scalar[0] == NULL)
      obj = PyInt_FromLong(self->E->spf);
    else
      obj = PyString_FromString(self->E->scalar[0]);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'spf' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setspf(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  gd_spf_t spf;
  char *scalar;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_RAW_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'spf' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_UINT16, &scalar, &spf);

  if (PyErr_Occurred()) {
    free(scalar);
    dreturn("%i", -1);
    return -1;
  }

  free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;
  self->E->spf = spf;

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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
    PyErr_SetString(PyExc_ValueError, "'pygetdata.entry' "
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
      PyTuple_SetItem(obj, i, (self->E->scalar[i] == NULL) ?
          (self->E->comp_scal) ?  gdpy_from_complex(self->E->cm[i]) :
          PyFloat_FromDouble(self->E->m[i]) :
          PyString_FromString(self->E->scalar[i]));
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'm' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setm(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  int comp_scal = 0;
  double m[GD_MAX_LINCOM];
  double complex cm[GD_MAX_LINCOM];
  char *scalar[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'm' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "attribute 'm' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->n_fields) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "not enough items in tuple for attribute 'm'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i) {
    PyObject *obj = PyTuple_GetItem(value, i);
    if (PyComplex_Check(obj)) {
      comp_scal = 1;
      m[i] = (double)(cm[i] = gdpy_as_complex(obj));
      scalar[i] = NULL;
    } else if (comp_scal) {
      gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, scalar + i, cm + i);
      m[i] = creal(cm[i]);
    } else {
      gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, scalar + i, m + i);
      cm[i] = m[i];
    }
  }

  if (PyErr_Occurred()) {
    for (i = 0; i < GD_MAX_LINCOM; ++i)
      free(scalar[i]);
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i) {
    if (cimag(self->E->cb[i]))
      comp_scal = 1;
    self->E->cm[i] = cm[i];
    self->E->m[i] = m[i];
    free(self->E->scalar[i]);
    self->E->scalar[i] = scalar[i];
  }
  self->E->comp_scal = comp_scal;

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
      PyTuple_SetItem(obj, i, (self->E->scalar[i + GD_MAX_LINCOM] == NULL) ?
          (self->E->comp_scal) ?  gdpy_from_complex(self->E->cb[i]) :
          PyFloat_FromDouble(self->E->b[i]) :
          PyString_FromString(self->E->scalar[i + GD_MAX_LINCOM]));
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'b' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setb(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  int comp_scal = 0;
  double b[GD_MAX_LINCOM];
  double complex cb[GD_MAX_LINCOM];
  char *scalar[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'b' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "attribute 'b' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->n_fields) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "not enough items in tuple for attribute 'b'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i) {
    PyObject *obj = PyTuple_GetItem(value, i);
    if (PyComplex_Check(obj)) {
      comp_scal = 1;
      b[i] = (double)(cb[i] = gdpy_as_complex(obj));
      scalar[i] = NULL;
    } else if (comp_scal) {
      gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, scalar + i, cb + i);
      b[i] = creal(cb[i]);
    } else {
      gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, scalar + i, b + i);
      cb[i] = b[i];
    }
  }

  if (PyErr_Occurred()) {
    for (i = 0; i < GD_MAX_LINCOM; ++i)
      free(scalar[i]);
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->n_fields; ++i) {
    if (cimag(self->E->cm[i]))
      comp_scal = 1;
    self->E->cb[i] = cb[i];
    self->E->b[i] = b[i];
    free(self->E->scalar[i + GD_MAX_LINCOM]);
    self->E->scalar[i + GD_MAX_LINCOM] = scalar[i];
  }
  self->E->comp_scal = comp_scal;

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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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

  if (self->E->field_type == GD_BIT_ENTRY ||
      self->E->field_type == GD_SBIT_ENTRY)
  {
    if (self->E->scalar[0] == NULL)
      obj = PyInt_FromLong(self->E->bitnum);
    else
      obj = PyString_FromString(self->E->scalar[0]);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'bitnum' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setbitnum(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int bitnum;
  char *scalar;
  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_BIT_ENTRY &&
      self->E->field_type != GD_SBIT_ENTRY)
  {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'bitnum' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_INT16, &scalar, &bitnum);
  if (PyErr_Occurred()) {
    free(scalar);
    dreturn("%i", -1);
    return -1;
  }

  self->E->bitnum = (gd_bit_t)bitnum;
  free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getnumbits(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_BIT_ENTRY ||
      self->E->field_type == GD_SBIT_ENTRY)
  {
    if (self->E->scalar[1] == NULL)
      obj = PyInt_FromLong(self->E->numbits);
    else
      obj = PyString_FromString(self->E->scalar[1]);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'numbits' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setnumbits(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int numbits;
  char *scalar;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_BIT_ENTRY &&
      self->E->field_type != GD_SBIT_ENTRY)
  {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'numbits' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_INT16, &scalar, &numbits);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->numbits = (gd_bit_t)numbits;
  free(self->E->scalar[1]);
  self->E->scalar[1] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getdividend(struct gdpy_entry_t* self,
    void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RECIP_ENTRY) {
    if (self->E->scalar[0])
      obj = PyString_FromString(self->E->scalar[0]);
    else if (self->E->comp_scal)
      obj = gdpy_from_complex(self->E->cdividend);
    else
      obj = PyFloat_FromDouble(self->E->dividend);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'dividend' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setdividend(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int comp_scal = 0;
  char *scalar;
  double complex cdividend = 0;
  double dividend = 0;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_RECIP_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'dividend' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  if (PyComplex_Check(value) || PyString_Check(value))
    comp_scal = 1;

  if (comp_scal) {
    gdpy_set_scalar_from_pyobj(value, GD_COMPLEX128, &scalar, &cdividend);
    dividend = creal(cdividend);
  } else {
    gdpy_set_scalar_from_pyobj(value, GD_FLOAT64, &scalar, &dividend);
    cdividend = dividend;
  }

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->comp_scal = comp_scal;
  self->E->cdividend = cdividend;
  self->E->dividend = dividend;
  free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_entry_getshift(struct gdpy_entry_t* self, void* closure)
{
  PyObject* obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_PHASE_ENTRY) {
    if (self->E->scalar[0] == NULL)
      obj = PyLong_FromLongLong((PY_LONG_LONG)self->E->shift);
    else
      obj = PyString_FromString(self->E->scalar[0]);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'shift' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setshift(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int64_t shift;
  char *scalar;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_PHASE_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'shift' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_INT64, &scalar, &shift);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->shift = shift;
  free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;

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
      PyTuple_SetItem(obj, i, (self->E->scalar[i] == NULL) ?
          (self->E->comp_scal) ?  gdpy_from_complex(self->E->ca[i]) :
          PyFloat_FromDouble(self->E->a[i]) :
          PyString_FromString(self->E->scalar[i]));
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'a' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_seta(struct gdpy_entry_t* self, PyObject *value,
    void *closure)
{
  int i;
  int comp_scal = 0;
  double a[GD_MAX_POLYORD + 1];
  double complex ca[GD_MAX_POLYORD + 1];
  char* scalar[GD_MAX_POLYORD + 1];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_POLYNOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'a' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]); 
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "attribute 'a' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->poly_ord + 1) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "not enough items in tuple for attribute 'a'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i <= self->E->poly_ord; ++i) {
    PyObject *obj = PyTuple_GetItem(value, i);
    if (PyComplex_Check(obj)) {
      comp_scal = 1;
      scalar[i] = NULL;
    } else if (comp_scal) {
      gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, scalar + i, ca + i);
      a[i] = (double)ca[i];
    } else {
      gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, scalar + i, a + i);
      ca[i] = a[i];
    }
  }

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i <= self->E->poly_ord; ++i) {
    self->E->a[i] = a[i];
    self->E->ca[i] = ca[i];
    free(self->E->scalar[i]);
    self->E->scalar[i] = scalar[i];
  }
  self->E->comp_scal = comp_scal;

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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
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
    PyErr_SetString(PyExc_ValueError, "'pygetdata.entry' "
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
    case GD_DIVIDE_ENTRY:
      tuple = Py_BuildValue("(ss)", self->E->in_fields[0],
          self->E->in_fields[1]);
      break;
    case GD_RECIP_ENTRY:
      if (self->E->comp_scal) 
        tuple = Py_BuildValue("(sO)", self->E->in_fields[0],
            gdpy_from_complex(self->E->cdividend));
      else
        tuple = Py_BuildValue("(sd)", self->E->in_fields[0], self->E->dividend);
      break;
    case GD_PHASE_ENTRY:
      tuple = Py_BuildValue("(si)", self->E->in_fields[0], self->E->shift);
      break;
    case GD_POLYNOM_ENTRY:
      a = PyTuple_New(self->E->poly_ord + 1);
      if (self->E->comp_scal)
        for (i = 0; i <= self->E->poly_ord; ++i)
          PyTuple_SetItem(a, i, gdpy_from_complex(self->E->ca[i]));
      else
        for (i = 0; i <= self->E->poly_ord; ++i)
          PyTuple_SetItem(a, i, PyFloat_FromDouble(self->E->a[i]));
      tuple = Py_BuildValue("(sO)", self->E->in_fields[0], a);
      break;
    case GD_LINCOM_ENTRY:
      switch (self->E->n_fields) {
        case 1:
          if (self->E->comp_scal)
            tuple = Py_BuildValue("((s)(O)(O))", self->E->in_fields[0],
                gdpy_from_complex(self->E->cm[0]),
                gdpy_from_complex(self->E->cb[0]));
          else
            tuple = Py_BuildValue("((s)(d)(d))", self->E->in_fields[0],
                self->E->m[0], self->E->b[0]);
          break;
        case 2:
          if (self->E->comp_scal)
            tuple = Py_BuildValue("((ss)(OO)(OO))", self->E->in_fields[0],
                self->E->in_fields[1], gdpy_from_complex(self->E->cm[0]),
                gdpy_from_complex(self->E->cm[1]),
                gdpy_from_complex(self->E->cb[0]),
                gdpy_from_complex(self->E->cb[1]));
          else
            tuple = Py_BuildValue("((ss)(dd)(dd))", self->E->in_fields[0],
                self->E->in_fields[1], self->E->m[0], self->E->m[1],
                self->E->b[0], self->E->b[1]);
          break;
        case 3:
          if (self->E->comp_scal)
            tuple = Py_BuildValue("((sss)(OOO)(OOO))", self->E->in_fields[0],
                self->E->in_fields[1], self->E->in_fields[2],
                gdpy_from_complex(self->E->cm[0]),
                gdpy_from_complex(self->E->cm[1]),
                gdpy_from_complex(self->E->cm[2]),
                gdpy_from_complex(self->E->cb[0]),
                gdpy_from_complex(self->E->cb[1]),
                gdpy_from_complex(self->E->cb[2]));
          else
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
    gdpy_set_entry_from_dict(&E, value, "pygetdata.entry");
  else if (PyTuple_Check(value))
    gdpy_set_entry_from_tuple(&E, value, "pygetdata.entry");
  else 
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "attribute 'parameters' must be a tuple or dictionary");

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->field = NULL;

  gd_free_entry_strings(self->E);
  memcpy(self->E, &E, sizeof(gd_entry_t));

  dreturn("%i", 0);
  return 0;
}

static PyGetSetDef gdpy_entry_getset[] = {
  { "a", (getter)gdpy_entry_geta, (setter)gdpy_entry_seta,
    "The POLYNOM co-efficients.  A tuple of numerical and/or string data.\n"
      "If a CONST scalar is used as a co-efficient, the corresponding\n"
      "element of the tuple will be the field code of this scalar field.\n"
      "Otherwise, the elements will be numerical data.  When assigning to\n"
      "this attribute, the tuple (which may similarly mix numerical and\n"
      "string data) must have as many elements as the current polynomial\n"
      "order dictates: assigning a tuple of different size to this\n"
      "attribute will not result in a change in the polynomial order.  To\n"
      "do that, modify the poly_ord attribute directly.\n",
    NULL },
  { "b", (getter)gdpy_entry_getb, (setter)gdpy_entry_setb,
    "The LINCOM offset terms.  A tuple of numerical and/or string data.\n"
      "If a CONST scalar is used as an offset, the corresponding element of\n"
      "the tuple will be the field code of this scalar field.  Otherwise,\n"
      "the elements will be numerical data.  When assigning to this\n"
      "attribute, the tuple (which may similarly mix numerical and string\n"
      "data) must have as many elements as the current number of fields in\n"
      "the LINCOM: assigning a tuple of different size to this attribute\n"
      "will not result in a change in the number of fields used.  To do\n"
      "that, modify the n_fields attribute directly.\n",
    NULL },
  { "bitnum", (getter)gdpy_entry_getbitnum, (setter)gdpy_entry_setbitnum,
    "The starting bit for a BIT field.  If this is specified using a\n"
      "CONST scalar field, this will be the field code of that field,\n"
      "otherwise, it will be the number itself.",
    NULL },
  { "const_type", (getter)gdpy_entry_getdatatype,
    (setter)gdpy_entry_setdatatype, "An alias for the data_type attribute.",
    NULL },
  { "data_type", (getter)gdpy_entry_getdatatype, (setter)gdpy_entry_setdatatype,
    "A numeric code indicating the underlying data type of a CONST or RAW\n"
      "field.  It should be one of the data type symbols: pygetdata.UINT8,\n"
      "pygetdata.INT8, &c.  The data_type_name attribute provides a human-\n"
      "readable version of this data.\n",
    NULL },
  { "data_type_name", (getter)gdpy_entry_getdatatypename, NULL,
    "A human-readable string indicating the underlying data type of a\n"
      "CONST or RAW field.  This attribute is read-only.  To change the\n"
      "data type modify the data_type attribute.\n",
    NULL },
  { "dividend", (getter)gdpy_entry_getdividend, (setter)gdpy_entry_setdividend,
    "The dividend of a RECIP field.  If this is specified using a CONST\n"
      /* -----------------------------------------------------------------| */
      "scalar field, this will be the field code of that field, otherwise,\n"
      "it will be the number itself.",
    NULL },
  { "field_type", (getter)gdpy_entry_gettype, NULL,
    "A numeric code indicating the field type.  This will be one of the\n"
      "pygetdata.*_ENTRY symbols.  This attribute is read-only.  An entry's\n"
      "field type may not be changed after creation.  See also the\n"
      "field_type_name attribute for a human-readable version of this data.\n",
    NULL },
  { "field_type_name", (getter)gdpy_entry_gettypename, NULL,
    "A human-readable string indicating the field type.  This attribute\n"
      "is read-only.  An entry's field type may not be changed after\n"
      "creation.  See also the field_type attribute for a numeric version\n"
      "of this data.\n",
    NULL },
  { "fragment", (getter)gdpy_entry_getfragment, (setter)gdpy_entry_setfragment,
    "If this entry object was created by a call to dirfile.entry,\n"
      "this is index number of the format file fragment which defines the\n"
      "field described by this entry object.  Otherwise, it is the index of\n"
      "the fragment to which this entry will be added, if this entry is\n"
      "passed to dirfile.add().  It may be freely modified, but is ignored\n"
      "by dirfile.alter().  To change the fragment index of an existing\n"
      "field in a dirfile, use dirfile.move().\n",
    NULL },
  { "in_fields", (getter)gdpy_entry_getinfields, (setter)gdpy_entry_setinfields,
    "A tuple containing the input vector field codes for a derived field.\n"
      "Except in the case of a LINCOM, this will be a tuple of length one.\n"
      "When modifying this attribute, the new tuple must contain the same\n"
      "number of elements as it did previously: assigning a tuple of a\n"
      "different size will not cause the number of fields used in a LINCOM\n"
      "to change.  To do that, modify the n_fields attribute directly.",
    NULL },
  { "m", (getter)gdpy_entry_getm, (setter)gdpy_entry_setm,
    "The LINCOM scale factors.  A tuple of numerical and/or string data.\n"
      "If a CONST scalar is used as a scale factor, the corresponding\n"
      "element of the tuple will be the field code of this scalar field.\n"
      "Otherwise, the elements will be numerical data.  When assigning to\n"
      "this attribute, the tuple (which may similarly mix numerical and\n"
      "string data) must have as many elements as the current number of\n"
      "fields in the LINCOM: assigning a tuple of different size to this\n"
      "attribute will not result in a change in the number of fields used.\n"
      "To do that, modify the n_fields attribute directly.\n",
    NULL }, 
  { "n_fields", (getter)gdpy_entry_getnfields, (setter)gdpy_entry_setnfields,
    "The number of fields in a LINCOM.  Modifying this will change the\n"
      "number of fields used.  If this number is increased, the added\n"
      "elements in the in_fields, m, and b attributes will contain\n"
      "nonsensical data, and should be initialised (by assignment) before\n"
      "the entry object is otherwise used.",
    NULL },
  { "name", (getter)gdpy_entry_getname, (setter)gdpy_entry_setname,
    "The name of this field.  This may be freely modified, but is ignored\n"
      "by dirfile.alter().  To change the name of an existing field in a\n"
      "dirfile, use dirfile.rename().",
    NULL },
  { "numbits", (getter)gdpy_entry_getnumbits, (setter)gdpy_entry_setnumbits,
    "The length of a BIT field.  If this is specified using a CONST\n"
      "scalar field, this will be the field code of that field, otherwise,\n"
      "it will be the number itself.",
    NULL },
  { "parameters", (getter)gdpy_entry_getparms, (setter)gdpy_entry_setparms,
    "A tuple containing the field specific parameters for this entry.\n"
      "The format is identical to the 'parameters' tuple passed to the\n"
      "entry constructor (q.v.).  If modified, this will update all\n"
      "parameters of the entry.  This attribute may be assigned a\n"
      "dictionary, in which case it will be converted internally to the\n"
      "corresponding parameters tuple.",
    NULL },
  { "poly_ord", (getter)gdpy_entry_getpolyord, (setter)gdpy_entry_setpolyord,
    "The polynomial order of a POLYNOM field.  Modifying this will change\n"
      "the number of terms in the polynomial.  If this number is increased,\n"
      "higher order co-efficients in the 'a' attribute will contain\n"
      "nonsensical data, and should be initialised (by assignment) before\n"
      "the entry object is otherwise used.",
    NULL },
  { "shift", (getter)gdpy_entry_getshift, (setter)gdpy_entry_setshift,
    "The shift of a PHASE field.  If this is specified using a CONST\n"
      "scalar field, this will be the field code of that field, otherwise,\n"
      "it will be the number itself.",
    NULL },
  { "spf", (getter)gdpy_entry_getspf, (setter)gdpy_entry_setspf,
    "The number of sample per frame of the data on disk for a RAW field.\n"
      "If this is specified using a CONST scalar field, this will be the\n"
      "field code of that field, otherwise, it will be the number itself.",
    NULL },
  { "table", (getter)gdpy_entry_gettable, (setter)gdpy_entry_settable,
    "The pathname of the look-up table of a LINTERP field.",
    NULL },
  { NULL }
};

#define ENTRY_DOC \
  "entry(type, name, fragment_index [, parameters])\n\n"\
"Returns an entry object containing the metadata for one dirfile field.\n"\
"The field type is specified by 'type', which should be one of the\n"\
"pygetdata.*_ENTRY symbols.  The field name is specified by 'name', and\n"\
"'fragment_index' indicates the format file fragment which will contain\n"\
"the specification line of this field, once the entry is added to a\n"\
"dirfile.  To add this field to the primary format file, set\n"\
"'fragment_index' to zero.\n\n"\
"The 'parameters' parameter is a tuple or dictionary containing field-\n"\
"specific metadata parameters, and must be present for all field types\n"\
"except STRING, which has no parameters.  If a tuple, 'parameters' should\n"\
"contain the following members, based on field type:\n\n"\
"  BIT or SBIT:  (in_field, bitnum [, numbits])\n"\
"    'in_field':   a string containing the input field code.\n"\
"    'bitnum':     a number or CONST field code specifying the first bit\n"\
"                    to extract.\n"\
"    'numbits':    an optional number or CONST field code specifying the\n"\
"                    number of bits to extract.  If not specified, it\n"\
"                    defaults to 1.\n\n"\
"  CONST:        (type,)\n"\
"    'type':       the storage type of the CONST data.  Should be one of\n"\
"                    the data type symbols: pygetdata.UINT8,\n"\
"                    pygetdata.INT8 &.c.\n\n"\
"  LINCOM:       (in_fields, m, b)\n"\
"    'in_fields':  a tuple of input vector fields for the LINCOM.  The\n"\
"                    number of elements of the tuple determines the\n"\
"                    number of input fields for the LINCOM.  If there are\n"\
"                    more than three elements, the remainder will be\n"\
"                    ignored.\n"\
"    'm', 'b':     the scale factors and offset terms of the LINCOM.\n"\
/* ---------------------------------------------------------------------| */\
"                    These are also tuples and should have the same\n"\
"                    number of elements as 'in_fields'.  Data can be any\n"\
"                    mix of numeric types and, to specify CONST scalars,\n"\
"                    string field code.\n\n"\
"  LINTERP:      (in_field, table)\n"\
"    'in_field':   a string containing the input field code.\n"\
"    'table':      the pathname to the look-up table on disk.\n\n"\
"  MULTIPLY or DIVIDE:\n"\
"                (in_field1, in_field2)\n"\
"    'in_field1':   a string containing the first input field code.\n"\
"    'in_field2':   a string containing the second input field code.\n\n"\
"  PHASE:        (in_field, shift)\n"\
"    'in_field':   a string containing the input field code.\n"\
"    'shift:       a number or CONST field code specifying the number of\n"\
"                    samples to shift the data.\n\n"\
"  PHASE:        (in_field, shift)\n"\
"    'in_field':   a string containing the input field code.\n"\
"    'shift':      a number or CONST field code specifying the number of\n"\
"                    samples to shift the data.\n\n"\
"  POLYNOM:      (in_field, a)\n"\
"    'in_field':   a string containing the input field code.\n"\
"    'a':          a tuple of the co-efficients of the polynomial.  The\n"\
"                    order of the polynomial will be determined from the\n"\
"                    length of the tuple.  If there are more than six\n"\
"                    elements, the remainder will be ignored.  Data can\n"\
"                    be any mix of numeric types and, to specify CONST\n"\
"                    scalars, string field codes.\n\n"\
"  RAW:          (type, spf)\n"\
"    'type':       the storage type of the data on disk.  Should be one\n"\
"                    of the data type symbols: pygetdata.UINT8,\n"\
"                    pygetdata.INT8 &.c\n"\
"    'spf':        the number of samples per frame of the data on disk,\n"\
"                    or a CONST field code specifying the same.\n\n"\
"  RECIP:        (in_field, dividend)\n"\
"    'in_field':   a string containing the input field code.\n"\
"    'dividend':   a number or CONST field code specifying the dividend of\n"\
"                    the RECIP.\n\n"\
"If a dictionary, the keys of 'parameters' should be the names of the\n"\
"tuple parameters listed above (ie. 'type' and 'spf' for a RAW field),\n"\
"and the values the same as their tuple counterparts.\n\n"\
"An entry object provides all these parameters as attributes, which may\n"\
"be modified by assignment.  Attempting to access an attribute which is\n"\
"not available for the specified field type will result in AttributeError\n"\
"being thrown.\n\n"\
"To add a newly created entry object to a dirfile, use dirfile.add() or\n"\
"dirfile.madd() as appropriate.  To propagate changes made to an entry\n"\
"object back to the dirfile, dirfile.alter() is typically used, unless\n"\
"the name or fragment index is to be changed, in which case\n"\
"dirfile.rename() or dirfile,move() should be used.\n"

PyTypeObject gdpy_entry = {
  PyObject_HEAD_INIT(NULL)
    0,                           /* ob_size */
  "pygetdata.entry",             /* tp_name */
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
  ENTRY_DOC,                     /* tp_doc */
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
