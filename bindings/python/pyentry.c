/* Copyright (C) 2009-2017 D. V. Wiebe
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
#include "gdpy_intern.h"

static const char *gdpy_entry_type_names[] =
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
  "WINDOW_ENTRY",   /* 0x0C */
  "MPLEX_ENTRY",    /* 0x0D */
  "INDIR_ENTRY",    /* 0x0E */
  "SINDIR_ENTRY",   /* 0x0F */
  "CONST_ENTRY",    /* 0x10 */
  "STRING_ENTRY",   /* 0x11 */
  "CARRAY_ENTRY",   /* 0x12 */
  "SARRAY_ENTRY"    /* 0x13 */
};
#define GDPY_LAST_ENTYPE 0x13

/* Routine unicode failure check routines */
#define S1CHECK do { \
  if (s1 == NULL) { dreturn("%p", NULL); return NULL; } \
} while(0)
#define S2CHECK do { \
  if (s2 == NULL) { Py_DECREF(s1); dreturn("%p", NULL); return NULL; } \
} while(0)
#define S3CHECK do { \
  if (s3 == NULL) { \
    Py_DECREF(s1); Py_DECREF(s2); dreturn("%p", NULL); return NULL; \
  } \
} while(0)
#define S4CHECK do { \
  if (s4 == NULL) { \
    Py_DECREF(s1); Py_DECREF(s2); Py_DECREF(s3); \
    dreturn("%p", NULL); return NULL; \
  } \
} while(0)

static void gdpy_entry_delete(struct gdpy_entry_t *self)
{
  dtrace("%p", self);

  gd_free_entry_strings(self->E);
  PyMem_Free(self->E);
  PyMem_Free(self->char_enc);
  PyObject_Del(self);

  dreturnvoid();
}

static PyObject *gdpy_entry_create(PyTypeObject *type, PyObject *args,
    PyObject *keys)
{
  struct gdpy_entry_t *self;

  dtrace("%p, %p, %p", type, args, keys);

  self = (struct gdpy_entry_t*)type->tp_alloc(type, 0);

  if (self) {
    self->E = NULL;
    self->char_enc = gdpy_copy_global_charenc();
  }

  dreturn("%p", self);
  return (PyObject*)self;
}

static PyObject *gdpyobj_from_scalar(const gd_entry_t *E, int i, gd_type_t type,
    const void *value, const char *char_enc)
{
  PyObject *pyobj;
  dtrace("%p, %i, 0x%X, %p, %p\n", E, i, type, value, char_enc);

  if (E->scalar[i]) {
    /* Return the scalar field code */
    if (E->scalar_ind[i] >= 0) {
      char *buffer = PyMem_Malloc(strlen(E->scalar[i]) + 23);
      if (buffer == NULL) {
        PyErr_NoMemory();
        dreturn("%p", NULL);
        return NULL;
      }
      sprintf(buffer, "%s<%i>", E->scalar[i], E->scalar_ind[i]);
      pyobj = gdpyobj_from_string(buffer, char_enc);
      PyMem_Free(buffer);
    } else
      pyobj = gdpyobj_from_string(E->scalar[i], char_enc);
  } else /* If scalar is NULL, return the number */
    pyobj = gdpy_convert_to_pyobj(value, type, 0);

  dreturn("%p", pyobj);
  return pyobj;
}

static void gdpy_set_scalar_from_pyobj(PyObject *pyobj, gd_type_t type,
    char **scalar, const char *char_enc, void *data, const char *name)
{
  dtrace("%p, 0x%X, %p, %p, %p", pyobj, type, scalar, char_enc, data);

  if (pyobj == NULL) {
    if (name)
      PyErr_Format(PyExc_TypeError, "deletion of %s", name);
    else {
      /* If name is NULL, the caller has taken care of setting *data to
       * an appropriate default value, but we should still zero the scalar */
      *scalar = NULL;
    }
  } else if (gdpy_encobj_check(pyobj) || PyUnicode_Check(pyobj))
    *scalar = gdpy_string_from_pyobj(pyobj, char_enc, NULL);
  else {
    *scalar = NULL;
    gdpy_coerce_from_pyobj(pyobj, type, data);
  }

  dreturnvoid();
}

static void gdpy_set_entry_from_tuple(gd_entry_t *E, PyObject *tuple,
    const char *char_enc, const char *name)
{
  PyObject *parm1;
  PyObject *parm2;
  PyObject *parm3;
  PyObject *obj;
  int i, count, min;
  long size;

  dtrace("%p, %p, %p, \"%s\"", E, tuple, char_enc, name);

  switch (E->field_type)
  {
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      min = 0;
      break;
    case GD_CONST_ENTRY:
    case GD_SARRAY_ENTRY:
      min = 1;
      break;
    case GD_RAW_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_CARRAY_ENTRY:
      min = 2;
      break;
    case GD_LINCOM_ENTRY:
      min = 3;
      break;
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      min = 4;
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

  size = PyTuple_Size(tuple);
  if (size < min) {
    PyErr_Format(PyExc_TypeError, "'pygetdata.entry' "
        "%s: needed %d entry parameters, but got only %li", name, min, size);
    dreturnvoid();
    return;
  }

  switch (E->field_type)
  {
    case GD_RAW_ENTRY:
      E->EN(raw,data_type) =
        (gd_type_t)gdpy_long_from_pyobj(PyTuple_GetItem(tuple, 0));
      if (GDPY_INVALID_TYPE(E->EN(raw,data_type)))
        PyErr_SetString(PyExc_ValueError,
            "'pygetdata.entry' invalid data type");

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 1), GD_UINT_TYPE,
          &E->scalar[0], char_enc, &E->EN(raw,spf), NULL);
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

      count = E->EN(lincom,n_fields) = (int)PyTuple_Size(parm1);
      if (count > GD_MAX_LINCOM)
        count = GD_MAX_LINCOM;

      if (PyTuple_Size(parm2) < count || PyTuple_Size(parm3) < count) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
            "Missing data in LINCOM parameters");
        dreturnvoid();
        return;
      }

      for (i = 0; i < count; ++i) {
        E->in_fields[i] = gdpy_string_from_pyobj(PyTuple_GetItem(parm1, i),
            char_enc, "in_fields must be string");

        if (E->in_fields[i] == NULL) {
          dreturnvoid();
          return;
        }

        obj = PyTuple_GetItem(parm2, i);
        if (PyComplex_Check(obj)) {
          E->flags |= GD_EN_COMPSCAL;
          gdpy_as_complex(gd_csp_(E->EN(lincom,cm)[i]), obj);
        } else if (E->flags & GD_EN_COMPSCAL)
          gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, &E->scalar[i],
              char_enc, &E->EN(lincom,cm)[i], NULL);
        else {
          gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, &E->scalar[i], char_enc,
              &E->EN(lincom,m)[i], NULL);
          gd_rs2cs_(E->EN(lincom,cm)[i], E->EN(lincom,m)[i]);
        }

        if (PyErr_Occurred()) {
          dreturnvoid();
          return;
        }

        obj = PyTuple_GetItem(parm3, i);
        if (PyComplex_Check(obj)) {
          E->flags |= GD_EN_COMPSCAL;
          gdpy_as_complex(gd_csp_(E->EN(lincom,cb)[i]), obj);
        } else if (E->flags & GD_EN_COMPSCAL)
          gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128,
              &E->scalar[i + GD_MAX_LINCOM], char_enc, &E->EN(lincom,cb)[i],
              NULL);
        else {
          gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64,
              &E->scalar[i + GD_MAX_LINCOM], char_enc, &E->EN(lincom,b)[i],
              NULL);
          gd_rs2cs_(E->EN(lincom,cb)[i], E->EN(lincom,b)[i]);
        }

        if (PyErr_Occurred()) {
          dreturnvoid();
          return;
        }
      }
      break;
    case GD_LINTERP_ENTRY:
      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      E->EN(linterp,table) = gdpy_path_from_pyobj(PyTuple_GetItem(tuple, 1),
          char_enc);

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 1), GD_INT_TYPE,
          &E->scalar[0], char_enc, &E->EN(bit,bitnum), NULL);
      if (size > 2)
        gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 2), GD_INT_TYPE,
            &E->scalar[1], char_enc, &E->EN(bit,numbits), NULL);
      else {
        E->EN(bit,numbits) = 1;
        E->scalar[1] = NULL;
      }
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      E->in_fields[1] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 1),
          char_enc, "in_fields must be string");

      if (E->in_fields[1] == NULL) {
        dreturnvoid();
        return;
      }
      break;
    case GD_RECIP_ENTRY:
      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      obj = PyTuple_GetItem(tuple, 1);
      if (PyComplex_Check(obj)) {
        E->flags |= GD_EN_COMPSCAL;
        gdpy_as_complex(gd_csp_(E->EN(recip,cdividend)), obj);
      } else if (E->flags & GD_EN_COMPSCAL)
        gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, &E->scalar[0], char_enc,
            &E->EN(recip,cdividend), NULL);
      else {
        gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, &E->scalar[0], char_enc,
            &E->EN(recip,dividend), NULL);
        gd_rs2cs_(E->EN(recip,cdividend), E->EN(recip,dividend));
      }

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }
      break;
    case GD_PHASE_ENTRY:
      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 1), GD_INT64,
          &E->scalar[0], char_enc, &E->EN(phase,shift), NULL);
      break;
    case GD_POLYNOM_ENTRY:
      parm2 = PyTuple_GetItem(tuple, 1);
      if (!PyTuple_Check(parm2)) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
            "POLYNOM requires a tuple of co-efficients");
        dreturnvoid();
        return;
      }

      E->EN(polynom,poly_ord) = count = (int)PyTuple_Size(parm2) - 1;
      if (count > GD_MAX_POLYORD)
        count = GD_MAX_POLYORD;

      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      for (i = 0; i <= count; ++i) {
        obj = PyTuple_GetItem(parm2, i);
        if (PyComplex_Check(obj)) {
          E->flags |= GD_EN_COMPSCAL;
          gdpy_as_complex(gd_csp_(E->EN(polynom,ca)[i]), obj);
          E->scalar[i] = NULL;
        } else if (E->flags & GD_EN_COMPSCAL)
          gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, &E->scalar[i],
              char_enc, &E->EN(polynom,ca)[i], NULL);
        else {
          gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, &E->scalar[i], char_enc,
              &E->EN(polynom,a)[i], NULL);
          gd_rs2cs_(E->EN(polynom,ca)[i], E->EN(polynom,a)[i]);
        }

        if (PyErr_Occurred()) {
          dreturnvoid();
          return;
        }
      }
      break;
    case GD_WINDOW_ENTRY:
      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      E->in_fields[1] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 1),
          char_enc, "in_fields must be string");

      if (E->in_fields[1] == NULL) {
        dreturnvoid();
        return;
      }

      E->EN(window,windop) =
        (gd_windop_t)gdpy_long_from_pyobj(PyTuple_GetItem(tuple, 2));
      if (GDPY_INVALID_OP(E->EN(window,windop)))
        PyErr_SetString(PyExc_ValueError,
            "'pygetdata.entry' invalid window operation");

      obj = PyTuple_GetItem(tuple, 3);
      switch (E->EN(window,windop)) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          gdpy_set_scalar_from_pyobj(obj, GD_INT64, &E->scalar[0], char_enc,
              &E->EN(window,threshold).i, NULL);
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          gdpy_set_scalar_from_pyobj(obj, GD_UINT64, &E->scalar[0], char_enc,
              &E->EN(window,threshold).u, NULL);
          break;
        default:
          gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, &E->scalar[0], char_enc,
              &E->EN(window,threshold).r, NULL);
          break;
      }

      if (PyErr_Occurred()) {
        dreturnvoid();
        return;
      }
      break;
    case GD_MPLEX_ENTRY:
      E->in_fields[0] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 0),
          char_enc, "in_fields must be string");

      if (E->in_fields[0] == NULL) {
        dreturnvoid();
        return;
      }

      E->in_fields[1] = gdpy_string_from_pyobj(PyTuple_GetItem(tuple, 1),
          char_enc, "in_fields must be string");

      if (E->in_fields[1] == NULL) {
        dreturnvoid();
        return;
      }

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 2), GD_INT_TYPE,
          &E->scalar[0], char_enc, &E->EN(mplex,count_val), NULL);

      gdpy_set_scalar_from_pyobj(PyTuple_GetItem(tuple, 3), GD_INT_TYPE,
          &E->scalar[1], char_enc, &E->EN(mplex,period), NULL);
      break;
    case GD_SARRAY_ENTRY:
      E->EN(scalar,array_len) =
        (size_t)gdpy_ulong_from_pyobj(PyTuple_GetItem(tuple, 0));
      break;
    case GD_CARRAY_ENTRY:
      E->EN(scalar,array_len) =
        (size_t)gdpy_ulong_from_pyobj(PyTuple_GetItem(tuple, 1));
      /* fallthrough */
    case GD_CONST_ENTRY:
      E->EN(scalar,const_type) =
        (gd_type_t)gdpy_long_from_pyobj(PyTuple_GetItem(tuple, 0));
      if (GDPY_INVALID_TYPE(E->EN(scalar,const_type)))
        PyErr_SetString(PyExc_ValueError,
            "'pygetdata.entry' invalid data type");
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_STRING_ENTRY:
    case GD_INDEX_ENTRY:
      break;
  }

  dreturnvoid();
}

static void gdpy_set_entry_from_dict(gd_entry_t *E, PyObject *parms,
    const char *char_enc, const char *name)
{
  PyObject *tuple = Py_None;
  const char *key[4];
  int i, size = 0;

  dtrace("%p, %p, %p, \"%s\"", E, parms, char_enc, name);

  /* convert the dictionary to a tuple */

  /* variadic objects for entry types:
   * RAW:      type, spf                   = 2
   * LINCOM:   in_fields, m, b             = 3
   * LINTERP:  in_field, table             = 2
   * (S)BIT:   in_field, bitnum, (numbits) = 2/3
   * PHASE:    in_field, shift             = 2
   * MULTIPLY: in_field1, in_field2        = 2
   * DIVIDE:   in_field1, in_field2        = 2
   * INDIR:    in_field1, in_field2        = 2
   * SINDIR:   in_field1, in_field2        = 2
   * RECIP:    in_field, dividend          = 2
   * POLYNOM:  in_field, a                 = 2
   * WINDOW:   in_field1, in_field2, op, thresh = 4
   * MPLEX:    in_field1, in_field2, val, max = 4
   * CONST:    type                        = 1
   * CARRAY:   type, array_len             = 2
   * SARRAY:   array_len                   = 1
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
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
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
    case GD_WINDOW_ENTRY:
      key[0] = "in_field1";
      key[1] = "in_field2";
      key[2] = "windop";
      key[3] = "threshold";
      size = 4;
      break;
    case GD_MPLEX_ENTRY:
      key[0] = "in_field1";
      key[1] = "in_field2";
      key[2] = "count_val";
      key[3] = "period";
      size = 4;
      break;
    case GD_CARRAY_ENTRY:
      key[0] = "type";
      key[1] = "array_len";
      size = 2;
      break;
    case GD_SARRAY_ENTRY:
      key[0] = "array_len";
      size = 1;
      break;
    case GD_CONST_ENTRY:
      key[0] = "type";
      size = 1;
      break;
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_INDEX_ENTRY:
      break;
  }

  if (size > 0) {
    tuple = PyTuple_New(size);
    for (i = 0; i < size; ++i) {
      PyObject *o = PyDict_GetItemString(parms, key[i]);

      if (o == NULL) {
        PyErr_Format(PyExc_KeyError, "%s: missing required parameter key %s",
            name, key[i]);
        dreturnvoid();
        return;
      }

      Py_INCREF(o);
      PyTuple_SET_ITEM(tuple, i, o);
    }
  }

  gdpy_set_entry_from_tuple(E, tuple, char_enc, name);

  dreturnvoid();
}

static int gdpy_entry_init(struct gdpy_entry_t *self, PyObject *args,
    PyObject *keys)
{
  gd_entry_t E;
  char *keywords[] = {"type", "name", "fragment_index", "parameters",
    "character_encoding", NULL};
  PyObject *parms = NULL;
  PyObject *char_enc = NULL;
  PyObject *field_name;
  int field_type;

  dtrace("%p, %p, %p", self, args, keys);

  memset(&E, 0, sizeof(gd_entry_t));

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "iOi|OO:pygetdata.entry.__init__", keywords, &field_type,
        &field_name, &E.fragment_index, &parms, &char_enc))
  {
    dreturn("%i", -1);
    return -1;
  }

  /* First, try to update character_encoding, if requested */
  if (char_enc) {
    if (gdpy_parse_charenc(&self->char_enc, char_enc)) {
      dreturn("%i", -1);
      return -1;
    }
  }

  /* Now try to convert the field name object */
  E.field = gdpy_string_from_pyobj(field_name, self->char_enc,
      "field name should be string");

  if (E.field == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  E.field_type = field_type;

  /* check for valid field type */
  if (E.field_type > GDPY_LAST_ENTYPE || E.field_type <= 0 ||
      gdpy_entry_type_names[E.field_type] == NULL)
  {
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
    gdpy_set_entry_from_dict(&E, parms, self->char_enc,
        "pygetdata.entry.__init__");
  else if (PyTuple_Check(parms))
    gdpy_set_entry_from_tuple(&E, parms, self->char_enc,
        "pygetdata.entry.__init__");
  else
    PyErr_SetString(PyExc_TypeError, "pygetdata.dirfile.__init__() argument 3 "
        "must be a tuple or dictionary");

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  if (self->E == NULL) {
    self->E = PyMem_Malloc(sizeof(gd_entry_t));

    if (self->E == NULL) {
      PyErr_NoMemory();
      dreturn("%i", -1);
      return -1;
    }
  } else
    gd_free_entry_strings(self->E);

  memcpy(self->E, &E, sizeof(gd_entry_t));

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getname(struct gdpy_entry_t *self, void *closure)
{
  PyObject *pyobj;

  dtrace("%p, %p", self, closure);

  pyobj = gdpyobj_from_string(self->E->field, self->char_enc);

  dreturn("%p", pyobj);
  return pyobj;
}

static int gdpy_entry_setname(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  char *s;

  dtrace("%p, %p, %p", self, value, closure);

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of name is not supported");
    dreturn("%i", -1);
    return -1;
  }

  s = gdpy_string_from_pyobj(value, self->char_enc,
      "field name should be string");

  if (s == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  PyMem_Free(self->E->field);
  self->E->field = s;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getfragment(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *pyobj;

  dtrace("%p, %p", self, closure);

  pyobj = gdpyint_fromlong(self->E->fragment_index);

  dreturn("%p", pyobj);
  return pyobj;
}

static int gdpy_entry_setfragment(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int t;

  dtrace("%p, %p, %p", self, value, closure);

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of fragment is not supported");
    dreturn("%i", -1);
    return -1;
  }

  t = (int)gdpy_long_from_pyobj(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->fragment_index = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_gettypename(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *pyobj;

  dtrace("%p, %p", self, closure);

  pyobj = gdpystrobj_from_string(gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject *gdpy_entry_gettype(struct gdpy_entry_t *self, void *closure)
{
  PyObject *pyobj;

  dtrace("%p, %p", self, closure);

  pyobj = gdpyint_fromlong(self->E->field_type);

  dreturn("%p", pyobj);
  return pyobj;
}

static PyObject *gdpy_entry_getinfields(struct gdpy_entry_t *self,
    void *closure)
{
  int i;
  PyObject *tuple = NULL, *s1, *s2;

  dtrace("%p, %p", self, closure);

  switch (self->E->field_type)
  {
    case GD_LINCOM_ENTRY:
      tuple = PyTuple_New(self->E->EN(lincom,n_fields));
      for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
        s1 = gdpyobj_from_string(self->E->in_fields[i], self->char_enc);
        if (s1 == NULL) {
          Py_DECREF(tuple);
          dreturn("%p", NULL);
          return NULL;
        }
        PyTuple_SetItem(tuple, i, s1);
      }
      break;
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      tuple = Py_BuildValue("(N)", s1);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_string(self->E->in_fields[1], self->char_enc);
      S2CHECK;
      tuple = Py_BuildValue("(NN)", s1, s2);
      break;
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
      PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
          "attribute 'in_fields' not available for entry type %s",
          gdpy_entry_type_names[self->E->field_type]);
      break;
  }

  dreturn("%p", tuple);
  return tuple;
}

static int gdpy_entry_setinfields(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int i;
  char *s[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);


  switch (self->E->field_type)
  {
    case GD_LINCOM_ENTRY:
      if (value == NULL) {
        PyErr_SetString(PyExc_TypeError,
            "deletion of in_fields is not supported");
        dreturn("%i", -1);
        return -1;
      }

      if (!PyTuple_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
            "attribute 'in_fields' must be a tuple");
        dreturn("%i", -1);
        return -1;
      }

      if (PyTuple_Size(value) < self->E->EN(lincom,n_fields)) {
        PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
            "not enough items in tuple for in_fields");
        dreturn("%i", -1);
        return -1;
      }

      for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
        s[i] = gdpy_string_from_pyobj(PyTuple_GetItem(value, i), self->char_enc,
            "in_fields should be strings");

        if (s[i] == NULL) {
          dreturn("%i", -1);
          return -1;
        }
      }

      for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
        PyMem_Free(self->E->in_fields[i]);
        self->E->in_fields[i] = s[i];
      }
      break;
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
      if (value == NULL) {
        PyErr_SetString(PyExc_TypeError,
            "deletion of in_fields is not supported");
        dreturn("%i", -1);
        return -1;
      }

      if (!PyTuple_Check(value))
        s[0] = gdpy_string_from_pyobj(value, self->char_enc,
            "in_fields should be strings");
      else {
        if (PyTuple_Size(value) < 1) {
          PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
              "not enough items in tuple for in_fields");
          dreturn("%i", -1);
        }

        s[0] = gdpy_string_from_pyobj(PyTuple_GetItem(value, 0), self->char_enc,
            "in_fields should be strings");
      }

      if (s[0] == NULL) {
        dreturn("%i", -1);
        return -1;
      }

      PyMem_Free(self->E->in_fields[0]);
      self->E->in_fields[0] = s[0];
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      if (value == NULL) {
        PyErr_SetString(PyExc_TypeError,
            "deletion of in_fields is not supported");
        dreturn("%i", -1);
        return -1;
      }

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

      for (i = 0; i < 2; ++i) {
        s[i] = gdpy_string_from_pyobj(PyTuple_GetItem(value, i), self->char_enc,
            "in_fields should be strings");

        if (s[i] == NULL) {
          dreturn("%i", -1);
          return -1;
        }
      }

      for (i = 0; i < 2; ++i) {
        PyMem_Free(self->E->in_fields[i]);
        self->E->in_fields[i] = s[i];
      }
      break;
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
      PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
          "attribute 'in_fields' not available for entry type %s",
          gdpy_entry_type_names[self->E->field_type]);
      break;
  }

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getdatatypename(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *obj = NULL;
  int t = -1;
  char buffer[11];

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY)
    t = self->E->EN(raw,data_type);
  else if (self->E->field_type == GD_CONST_ENTRY ||
      self->E->field_type == GD_CARRAY_ENTRY)
    t = self->E->EN(scalar,const_type);
  else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'data_type_name' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  if (t != -1) {
    sprintf(buffer, "%s%i", (t & GD_COMPLEX) ? "COMPLEX" :
        (t & GD_IEEE754) ? "FLOAT" : (t & GD_SIGNED) ? "INT" : "UINT",
        8 * GD_SIZE(t));
    obj = gdpystrobj_from_string(buffer);
  }

  dreturn("%p", obj);
  return obj;
}

static PyObject *gdpy_entry_getdatatype(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY)
    obj = gdpyint_fromlong(self->E->EN(raw,data_type));
  else if (self->E->field_type == GD_CONST_ENTRY ||
      self->E->field_type == GD_CARRAY_ENTRY)
    obj = gdpyint_fromlong(self->E->EN(scalar,const_type));
  else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'data_type' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setdatatype(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  long t;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_RAW_ENTRY &&
      self->E->field_type != GD_CONST_ENTRY &&
      self->E->field_type != GD_CARRAY_ENTRY)
  {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'data_type' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of data_type is not supported");
    dreturn("%i", -1);
    return -1;
  }

  t = gdpy_long_from_pyobj(value);
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
    self->E->EN(raw,data_type) = (gd_type_t)t;
  else
    self->E->EN(scalar,const_type) = (gd_type_t)t;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getspf(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RAW_ENTRY)
    obj = gdpyobj_from_scalar(self->E, 0, GD_UINT_TYPE, &self->E->EN(raw,spf),
        self->char_enc);
  else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'spf' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setspf(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  unsigned int spf = 0;
  char *scalar = NULL;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_RAW_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'spf' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_UINT_TYPE, &scalar, self->char_enc,
      &spf, "spf");

  if (PyErr_Occurred()) {
    PyMem_Free(scalar);
    dreturn("%i", -1);
    return -1;
  }

  PyMem_Free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;
  self->E->EN(raw,spf) = spf;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getarraylen(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_CARRAY_ENTRY ||
      self->E->field_type == GD_SARRAY_ENTRY)
  {
      obj = PyLong_FromUnsignedLong(self->E->EN(scalar,array_len));
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'array_len' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setarraylen(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  size_t array_len;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_CARRAY_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'array_len' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of array_len is not supported");
    dreturn("%i", -1);
    return -1;
  }

  array_len = gdpy_ulong_from_pyobj(value);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(scalar,array_len) = array_len;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getnfields(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_LINCOM_ENTRY) {
    obj = gdpyint_fromlong(self->E->EN(lincom,n_fields));
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'n_fields' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setnfields(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int i, n;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'n_fields' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of n_fields is not supported");
    dreturn("%i", -1);
    return -1;
  }

  n = (int)gdpy_long_from_pyobj(value);
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
  for (i = n; i < self->E->EN(lincom,n_fields); ++i)
    PyMem_Free(self->E->in_fields[i]);

  /* initialise new terms */
  for (i = self->E->EN(lincom,n_fields); i < n; ++i) {
    self->E->in_fields[i] = gdpy_strdup("");
    self->E->EN(lincom,m)[i] = self->E->EN(lincom,b)[i] = 0;
  }

  self->E->EN(lincom,n_fields) = n;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getm(struct gdpy_entry_t *self, void *closure)
{
  int i;
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_LINCOM_ENTRY) {
    obj = PyTuple_New(self->E->EN(lincom,n_fields));
    for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
      PyObject *scalar = gdpyobj_from_scalar(self->E, i, GD_COMPLEX128,
          &self->E->EN(lincom,cm)[i], self->char_enc);
      if (scalar == NULL) {
        Py_DECREF(obj);
        obj = NULL;
        break;
      }
      PyTuple_SetItem(obj, i, scalar);
    }
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'm' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setm(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int i;
  int comp_scal = 0;
  double m[GD_MAX_LINCOM];
  GD_DCOMPLEXM(cm[GD_MAX_LINCOM]);
  char *scalar[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'm' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of m is not supported");
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "attribute 'm' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->EN(lincom,n_fields)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "not enough items in tuple for attribute 'm'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
    PyObject *obj = PyTuple_GetItem(value, i);
    if (PyComplex_Check(obj)) {
      comp_scal = GD_EN_COMPSCAL;
      gdpy_as_complex(gd_csp_(cm[i]), obj);
      m[i] = creal(cm[i]);
      scalar[i] = NULL;
    } else if (comp_scal) {
      gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, scalar + i, self->char_enc,
          cm + i, NULL);
      m[i] = creal(cm[i]);
    } else {
      gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, scalar + i, self->char_enc,
          m + i, NULL);
      gd_rs2cs_(cm[i], m[i]);
    }
  }

  if (PyErr_Occurred()) {
    for (i = 0; i < GD_MAX_LINCOM; ++i)
      PyMem_Free(scalar[i]);
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
    /* check whether the corresponding cb is complex */
    if (!comp_scal && cimag(self->E->EN(lincom,cb)[i]))
      comp_scal = GD_EN_COMPSCAL;
    gd_cs2cs_(self->E->EN(lincom,cm)[i], cm[i]);
    self->E->EN(lincom,m)[i] = m[i];
    PyMem_Free(self->E->scalar[i]);
    self->E->scalar[i] = scalar[i];
  }
  self->E->flags |= comp_scal;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getb(struct gdpy_entry_t *self, void *closure)
{
  int i;
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_LINCOM_ENTRY) {
    obj = PyTuple_New(self->E->EN(lincom,n_fields));
    for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
      PyObject *scalar = gdpyobj_from_scalar(self->E, i + GD_MAX_LINCOM,
          GD_COMPLEX128, &self->E->EN(lincom,cb)[i], self->char_enc);
      if (scalar == NULL) {
        Py_DECREF(obj);
        obj = NULL;
        break;
      }
      PyTuple_SetItem(obj, i, scalar);
    }
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'b' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setb(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int i;
  int comp_scal = 0;
  double b[GD_MAX_LINCOM];
  GD_DCOMPLEXM(cb[GD_MAX_LINCOM]);
  char *scalar[GD_MAX_LINCOM];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINCOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'b' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of b is not supported");
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "attribute 'b' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->EN(lincom,n_fields)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "not enough items in tuple for attribute 'b'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
    PyObject *obj = PyTuple_GetItem(value, i);
    if (PyComplex_Check(obj)) {
      comp_scal = GD_EN_COMPSCAL;
      gdpy_as_complex(gd_csp_(cb[i]), obj);
      b[i] = creal(cb[i]);
      scalar[i] = NULL;
    } else if (comp_scal) {
      gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, scalar + i, self->char_enc,
          cb + i, NULL);
      b[i] = creal(cb[i]);
    } else {
      gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, scalar + i, self->char_enc,
          b + i, NULL);
      gd_rs2cs_(cb[i], b[i]);
    }
  }

  if (PyErr_Occurred()) {
    for (i = 0; i < GD_MAX_LINCOM; ++i)
      PyMem_Free(scalar[i]);
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
    /* check whether the corresponding cm is complex */
    if (!comp_scal && cimag(self->E->EN(lincom,cm)[i]))
      comp_scal = GD_EN_COMPSCAL;
    gd_cs2cs_(self->E->EN(lincom,cb)[i], cb[i]);
    self->E->EN(lincom,b)[i] = b[i];
    PyMem_Free(self->E->scalar[i + GD_MAX_LINCOM]);
    self->E->scalar[i + GD_MAX_LINCOM] = scalar[i];
  }
  self->E->flags |= comp_scal;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_gettable(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_LINTERP_ENTRY)
    obj = gdpyobj_from_path(self->E->EN(linterp,table));
  else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'table' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_settable(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  char *s;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_LINTERP_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'table' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of table is not supported");
    dreturn("%i", -1);
    return -1;
  }

  s = gdpy_path_from_pyobj(value, self->char_enc);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  PyMem_Free(self->E->EN(linterp,table));
  self->E->EN(linterp,table) = s;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getbitnum(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_BIT_ENTRY ||
      self->E->field_type == GD_SBIT_ENTRY)
  {
    obj = gdpyobj_from_scalar(self->E, 0, GD_INT_TYPE, &self->E->EN(bit,bitnum),
        self->char_enc);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'bitnum' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setbitnum(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int bitnum = 0;
  char *scalar = NULL;
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

  gdpy_set_scalar_from_pyobj(value, GD_INT_TYPE, &scalar, self->char_enc,
      &bitnum, "bitnum");
  if (PyErr_Occurred()) {
    PyMem_Free(scalar);
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(bit,bitnum) = bitnum;
  PyMem_Free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getnumbits(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_BIT_ENTRY ||
      self->E->field_type == GD_SBIT_ENTRY)
  {
    obj = gdpyobj_from_scalar(self->E, 1, GD_INT_TYPE,
        &self->E->EN(bit,numbits), self->char_enc);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'numbits' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setnumbits(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int numbits = 1; /* del x.numbits <==> x.numbits=1 */
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

  gdpy_set_scalar_from_pyobj(value, GD_INT_TYPE, &scalar, self->char_enc,
      &numbits, NULL);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(bit,numbits) = numbits;
  PyMem_Free(self->E->scalar[1]);
  self->E->scalar[1] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getdividend(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_RECIP_ENTRY)
    obj = gdpyobj_from_scalar(self->E, 0, GD_COMPLEX128,
        &self->E->EN(recip,cdividend), self->char_enc);
  else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'dividend' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setdividend(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int comp_scal = 0;
  char *scalar;
  GD_DCOMPLEXA(cdividend);
  double dividend = 0;

  dtrace("%p, %p, %p", self, value, closure);

  gd_li2cs_(cdividend, 0, 0);

  if (self->E->field_type != GD_RECIP_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'dividend' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of dividend is not supported");
    dreturn("%i", -1);
    return -1;
  }

  if (PyComplex_Check(value) || PyUnicode_Check(value) ||
      gdpy_encobj_check(value))
  {
    comp_scal = GD_EN_COMPSCAL;
  }

  if (comp_scal) {
    gdpy_set_scalar_from_pyobj(value, GD_COMPLEX128, &scalar, self->char_enc,
        &cdividend, NULL);
    dividend = creal(cdividend);
  } else {
    gdpy_set_scalar_from_pyobj(value, GD_FLOAT64, &scalar, self->char_enc,
        &dividend, NULL);
    gd_rs2cs_(cdividend, dividend);
  }

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->flags |= comp_scal;
  gd_cs2cs_(self->E->EN(recip,cdividend), cdividend);
  self->E->EN(recip,dividend) = dividend;
  PyMem_Free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getshift(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_PHASE_ENTRY) {
    obj = gdpyobj_from_scalar(self->E, 0, GD_INT64, &self->E->EN(phase,shift),
        self->char_enc);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'shift' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setshift(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int64_t shift = 0;
  char *scalar = NULL;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_PHASE_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'shift' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_INT64, &scalar, self->char_enc, &shift,
      "shift");

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(phase,shift) = shift;
  PyMem_Free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getcountval(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_MPLEX_ENTRY) {
    obj = gdpyobj_from_scalar(self->E, 0, GD_INT_TYPE,
        &self->E->EN(mplex,count_val), self->char_enc);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'count_val' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setcountval(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int count_val = 0;
  char *scalar = NULL;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_MPLEX_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'count_val' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_INT_TYPE, &scalar, self->char_enc,
      &count_val, "count_val");

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(mplex,count_val) = count_val;
  PyMem_Free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getperiod(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_MPLEX_ENTRY) {
    obj = gdpyobj_from_scalar(self->E, 1, GD_INT_TYPE,
        &self->E->EN(mplex,period), self->char_enc);
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'period' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setperiod(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int period = 0; /* del x.period <==> x.period=0 */
  char *scalar;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_MPLEX_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'period' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  gdpy_set_scalar_from_pyobj(value, GD_INT_TYPE, &scalar, self->char_enc,
      &period, NULL);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(mplex,period) = period;
  PyMem_Free(self->E->scalar[1]);
  self->E->scalar[1] = scalar;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_geta(struct gdpy_entry_t *self, void *closure)
{
  int i;
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_POLYNOM_ENTRY) {
    obj = PyTuple_New(self->E->EN(polynom,poly_ord) + 1);
    for (i = 0; i <= self->E->EN(polynom,poly_ord); ++i) {
      PyObject *scalar = gdpyobj_from_scalar(self->E, i, GD_COMPLEX128,
          &self->E->EN(polynom,ca)[i], self->char_enc);
      if (scalar == NULL) {
        Py_DECREF(obj);
        obj = NULL;
        break;
      }
      PyTuple_SetItem(obj, i, scalar);
    }
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'a' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_seta(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int i;
  int comp_scal = 0;
  double a[GD_MAX_POLYORD + 1];
  GD_DCOMPLEXM(ca[GD_MAX_POLYORD + 1]);
  char *scalar[GD_MAX_POLYORD + 1];

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_POLYNOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'a' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of a is not supported");
    dreturn("%i", -1);
    return -1;
  }

  if (!PyTuple_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "attribute 'a' must be a tuple");
    dreturn("%i", -1);
    return -1;
  }

  if (PyTuple_Size(value) < self->E->EN(polynom,poly_ord) + 1) {
    PyErr_SetString(PyExc_TypeError, "'pygetdata.entry' "
        "not enough items in tuple for attribute 'a'");
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i <= self->E->EN(polynom,poly_ord); ++i) {
    PyObject *obj = PyTuple_GetItem(value, i);
    if (PyComplex_Check(obj)) {
      comp_scal = GD_EN_COMPSCAL;
      gdpy_as_complex(gd_csp_(ca[i]), obj);
      a[i] = creal(ca[i]);
      scalar[i] = NULL;
    } else if (comp_scal) {
      gdpy_set_scalar_from_pyobj(obj, GD_COMPLEX128, scalar + i,
          self->char_enc, ca + i, NULL);
      a[i] = creal(ca[i]);
    } else {
      gdpy_set_scalar_from_pyobj(obj, GD_FLOAT64, scalar + i, self->char_enc,
          a + i, NULL);
      gd_rs2cs_(ca[i], a[i]);
    }
  }

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  for (i = 0; i <= self->E->EN(polynom,poly_ord); ++i) {
    self->E->EN(polynom,a)[i] = a[i];
    gd_cs2cs_(self->E->EN(polynom,ca)[i], ca[i]);
    PyMem_Free(self->E->scalar[i]);
    self->E->scalar[i] = scalar[i];
  }
  self->E->flags |= comp_scal;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getpolyord(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_POLYNOM_ENTRY)
    obj = gdpyint_fromlong(self->E->EN(polynom,poly_ord));
  else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'poly_ord' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setpolyord(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  int n;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_POLYNOM_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'poly_ord' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of poly_ord is not supported");
    dreturn("%i", -1);
    return -1;
  }

  n = (int)PyLong_AsUnsignedLongLong(value);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  } else if (n < 1 || n > GD_MAX_POLYORD) {
    PyErr_SetString(PyExc_ValueError, "'pygetdata.entry' "
        "attribute 'poly_ord' out of range");
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(polynom,poly_ord) = n;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getparms(struct gdpy_entry_t *self, void *closure)
{
  int i;
  PyObject *s1, *s2, *s3, *s4, *tuple = NULL;

  dtrace("%p, %p", self, closure);

  switch (self->E->field_type)
  {
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
      tuple = Py_BuildValue("()");
      break;
    case GD_CONST_ENTRY:
      tuple = Py_BuildValue("(i)", self->E->EN(scalar,const_type));
      break;
    case GD_CARRAY_ENTRY:
      tuple = Py_BuildValue("(iI)", self->E->EN(scalar,const_type),
          self->E->EN(scalar,array_len));
      break;
    case GD_SARRAY_ENTRY:
      tuple = Py_BuildValue("(I)", self->E->EN(scalar,array_len));
      break;
    case GD_RAW_ENTRY:
      s1 = gdpyobj_from_scalar(self->E, 0, GD_UINT_TYPE, &self->E->EN(raw,spf),
          self->char_enc);
      S1CHECK;
      tuple = Py_BuildValue("(iN)", self->E->EN(raw,data_type), s1);
      break;
    case GD_LINTERP_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_path(self->E->EN(linterp,table));
      S2CHECK;
      tuple = Py_BuildValue("(NN)", s1, s2);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_string(self->E->in_fields[1], self->char_enc);
      S2CHECK;
      tuple = Py_BuildValue("(NN)", s1, s2);
      break;
    case GD_RECIP_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_scalar(self->E, 0, GD_COMPLEX128,
          &self->E->EN(recip,cdividend), self->char_enc);
      S2CHECK;
      tuple = Py_BuildValue("(NN)", s1, s2);
      break;
    case GD_PHASE_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_scalar(self->E, 0, GD_INT64, &self->E->EN(phase,shift),
            self->char_enc);
      S2CHECK;
      tuple = Py_BuildValue("(NN)", s1, s2);
      break;
    case GD_POLYNOM_ENTRY:
      s1 = PyTuple_New(self->E->EN(polynom,poly_ord) + 1);
      for (i = 0; i <= self->E->EN(polynom,poly_ord); ++i) {
        s2 = gdpyobj_from_scalar(self->E, i, GD_COMPLEX128,
            &self->E->EN(polynom,ca)[i], self->char_enc);
        S2CHECK;
        PyTuple_SetItem(s1, i, s2);
      }
      s2 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S2CHECK;
      tuple = Py_BuildValue("(NN)", s2, s1);
      break;
    case GD_LINCOM_ENTRY:
      s1 = PyTuple_New(self->E->EN(lincom,n_fields));
      s2 = PyTuple_New(self->E->EN(lincom,n_fields));
      s3 = PyTuple_New(self->E->EN(lincom,n_fields));
      for (i = 0; i < self->E->EN(lincom,n_fields); ++i) {
        s4 = gdpyobj_from_string(self->E->in_fields[i], self->char_enc);
        S4CHECK;
        PyTuple_SetItem(s1, i, s4);
        s4 = gdpyobj_from_scalar(self->E, i, GD_COMPLEX128,
            &self->E->EN(lincom,cm)[i], self->char_enc);
        S4CHECK;
        PyTuple_SetItem(s2, i, s4);
        s4 = gdpyobj_from_scalar(self->E, i + GD_MAX_LINCOM, GD_COMPLEX128,
            &self->E->EN(lincom,cb)[i], self->char_enc);
        S4CHECK;
        PyTuple_SetItem(s3, i, s4);
      }
      tuple = Py_BuildValue("(NNN)", s1, s2, s3);
      break;
    case GD_WINDOW_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_string(self->E->in_fields[1], self->char_enc);
      S2CHECK;
      switch (self->E->EN(window,windop)) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          s3 = gdpyobj_from_scalar(self->E, 0, GD_INT64,
              &self->E->EN(window,threshold).i, self->char_enc);
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          s3 =gdpyobj_from_scalar(self->E, 0, GD_UINT64,
              &self->E->EN(window,threshold).u, self->char_enc);
          break;
        default:
          s3 = gdpyobj_from_scalar(self->E, 0, GD_FLOAT64,
              &self->E->EN(window,threshold).r, self->char_enc);
          break;
      }
      S3CHECK;
      tuple = Py_BuildValue("(NNiN)", s1, s2, self->E->EN(window,windop), s3);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_scalar(self->E, 0, GD_INT_TYPE,
          &self->E->EN(bit,bitnum), self->char_enc);
      S2CHECK;
      s3 = gdpyobj_from_scalar(self->E, 1, GD_INT_TYPE,
            &self->E->EN(bit,numbits), self->char_enc);
      S3CHECK;
      tuple = Py_BuildValue("(NNN)", s1, s2, s3);
      break;
    case GD_MPLEX_ENTRY:
      s1 = gdpyobj_from_string(self->E->in_fields[0], self->char_enc);
      S1CHECK;
      s2 = gdpyobj_from_string(self->E->in_fields[1], self->char_enc);
      S2CHECK;
      s3 = gdpyobj_from_scalar(self->E, 0, GD_INT_TYPE,
          &self->E->EN(mplex,count_val), self->char_enc);
      S3CHECK;
      s4 = gdpyobj_from_scalar(self->E, 1, GD_INT_TYPE,
            &self->E->EN(mplex,period), self->char_enc);
      S4CHECK;
      tuple = Py_BuildValue("(NNNN)", s1, s2, s3, s4);
      break;
  }

  dreturn("%p", tuple);
  return tuple;
}

static int gdpy_entry_setparms(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  gd_entry_t E;

  dtrace("%p, %p, %p", self, value, closure);

  memset(&E, 0, sizeof(gd_entry_t));

  E.field = self->E->field;
  E.field_type = self->E->field_type;
  E.fragment_index = self->E->fragment_index;

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of parameters is not supported");
    dreturn("%i", -1);
    return -1;
  }

  if (PyDict_Check(value))
    gdpy_set_entry_from_dict(&E, value, self->char_enc, "pygetdata.entry");
  else if (PyTuple_Check(value))
    gdpy_set_entry_from_tuple(&E, value, self->char_enc, "pygetdata.entry");
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

static PyObject *gdpy_entry_getwindop(struct gdpy_entry_t *self, void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_WINDOW_ENTRY)
    obj = gdpyint_fromlong(self->E->EN(window,windop));
  else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'windop' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setwindop(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  long t;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_WINDOW_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'windop' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "deletion of windop is not supported");
    dreturn("%i", -1);
    return -1;
  }

  t = gdpy_long_from_pyobj(value);
  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  if (GDPY_INVALID_OP(t)) {
    PyErr_SetString(PyExc_ValueError, "'pygetdata.entry' invalid data type");
    dreturn("%i", -1);
    return -1;
  }

  self->E->EN(window,windop) = (gd_windop_t)t;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getthreshold(struct gdpy_entry_t *self,
    void *closure)
{
  PyObject *obj = NULL;

  dtrace("%p, %p", self, closure);

  if (self->E->field_type == GD_WINDOW_ENTRY) {
    switch (self->E->EN(window,windop)) {
      case GD_WINDOP_EQ:
      case GD_WINDOP_NE:
        obj = PyLong_FromLongLong(
            (PY_LONG_LONG)self->E->EN(window,threshold).i);
        break;
      case GD_WINDOP_SET:
      case GD_WINDOP_CLR:
        obj = PyLong_FromUnsignedLongLong(
            (unsigned PY_LONG_LONG)self->E->EN(window,threshold).u);
        break;
      default:
        obj = PyFloat_FromDouble(self->E->EN(window,threshold).r);
        break;
    }
  } else
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'threshold' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_entry_setthreshold(struct gdpy_entry_t *self, PyObject *value,
    void *closure)
{
  gd_triplet_t t;
  char *scalar = NULL;

  dtrace("%p, %p, %p", self, value, closure);

  if (self->E->field_type != GD_WINDOW_ENTRY) {
    PyErr_Format(PyExc_AttributeError, "'pygetdata.entry' "
        "attribute 'threshold' not available for entry type %s",
        gdpy_entry_type_names[self->E->field_type]);
    dreturn("%i", -1);
    return -1;
  }

  switch (self->E->EN(window,windop)) {
    case GD_WINDOP_EQ:
    case GD_WINDOP_NE:
      gdpy_set_scalar_from_pyobj(value, GD_INT64, &scalar, self->char_enc,
          &t.i, "threshold");
      break;
    case GD_WINDOP_SET:
    case GD_WINDOP_CLR:
      gdpy_set_scalar_from_pyobj(value, GD_UINT64, &scalar, self->char_enc,
          &t.u, "threshold");
      break;
    default:
      gdpy_set_scalar_from_pyobj(value, GD_FLOAT64, &scalar, self->char_enc,
          &t.r, "threshold");
      break;
  }

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  PyMem_Free(self->E->scalar[0]);
  self->E->scalar[0] = scalar;
  self->E->EN(window,threshold) = t;

  dreturn("%i", 0);
  return 0;
}

static PyObject *gdpy_entry_getcharencoding(struct gdpy_entry_t *self,
    void *closure)
{
  dtrace("%p, %p", self, closure);

  PyObject *pyobj = gdpy_charenc_obj(self->char_enc);

  dreturn("%p", pyobj);
  return pyobj;
}

static int gdpy_entry_setcharencoding(struct gdpy_entry_t *self,
    PyObject *value, void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  if (gdpy_parse_charenc(&self->char_enc, value)) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

#if (PY_MAJOR_VERSION > 2) || (PY_MINOR_VERSION >= 6)
static PyObject *gdpy_entry_repr(struct gdpy_entry_t *self)
{
  PyObject *urepr, *repr, *parms, *name;

  dtrace("%p", self);

  /* decode name */
  name = gdpyobj_from_string(self->E->field, self->char_enc);

  /* the parameter tuple */
  parms = gdpy_entry_getparms(self, NULL);
  if (PyErr_Occurred()) {
    Py_DECREF(name);
    dreturn("%p", NULL);
    return NULL;
  }

  /* repr-ify */
  if (self->char_enc == NULL)
    urepr = PyUnicode_FromFormat(
        "pygetdata.entry(pygetdata.%s, %R, %i, %R, character_encoding=None)",
        gdpy_entry_type_names[self->E->field_type], name,
        self->E->fragment_index, parms);
  else
    urepr = PyUnicode_FromFormat(
        "pygetdata.entry(pygetdata.%s, %R, %i, %R, character_encoding=\"%s\")",
        gdpy_entry_type_names[self->E->field_type], name,
        self->E->fragment_index, parms, self->char_enc);

#if PY_MAJOR_VERSION < 3
  /* now encode */
  repr = PyUnicode_AsEncodedString(urepr, "ascii", "backslashreplace");
  Py_DECREF(urepr);
#else
  repr = urepr;
#endif
  Py_DECREF(name);
  Py_DECREF(parms);

  dreturn("%p", repr);
  return repr;
}
#else
#define gdpy_entry_repr NULL
#endif

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
  { "array_len", (getter)gdpy_entry_getarraylen, (setter)gdpy_entry_setarraylen,
    "The length of a CARRAY scalar field.\n",
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
  { "character_encoding", (getter)gdpy_entry_getcharencoding,
    (setter)gdpy_entry_setcharencoding,
    "The character encoding to use when representing Dirfile string data\n"
      "and metadata in Python.  For entry objects created with the\n"
      "pygetdata.entry() function, the initial value of this attribute is\n"
      "copied from the value of the global pygetdata.character_encoding\n"
      "at the time that the dirfile object is created, if the global value\n"
      "is valid.  If the global pygetdata.character_encoding is invalid,\n"
      "the initial value of this attribute is simply None.  For entry\n"
      "objects returned by pygetdata.dirfile.entry(), the value is copied\n"
      "from the dirfile object (which is always valid).  Subsequent changes\n"
      "affect only this object.  See the CHARACTER STRINGS section in the\n"
      "pygetdata module documentation for further details.",
    NULL },
  { "const_type", (getter)gdpy_entry_getdatatype,
    (setter)gdpy_entry_setdatatype, "An alias for the data_type attribute.",
    NULL },
  { "count_val", (getter)gdpy_entry_getcountval, (setter)gdpy_entry_setcountval,
    "The target value of the counter of a MPLEX field.  If this is\n"
      "specified using a CONST scalar field, this will be the field code of\n"
      "that field, otherwise, it will be the number itself.",
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
      "scalar field, this will be the field code of that field, otherwise,\n"
      "it will be the number itself.",
    NULL },
  { "field_type", (getter)gdpy_entry_gettype, NULL,
    "A numeric code indicating the field type.  This will be one of the\n"
      "pygetdata.*_ENTRY symbols.  This attribute is read-only: an entry's\n"
      "field type may not be changed after creation.  See also the\n"
      "field_type_name attribute for a human-readable version of this data.\n",
    NULL },
  { "field_type_name", (getter)gdpy_entry_gettypename, NULL,
    "A human-readable string indicating the field type.  This attribute\n"
      "is read-only: an entry's field type may not be changed after\n"
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
  { "period", (getter)gdpy_entry_getperiod, (setter)gdpy_entry_setperiod,
    "The number of samples between successive occurrences of the MPLEX\n"
      "value in the index vector (or zero, if unknown or not constant).  If\n"
      /* ------ handy ruler ----------------------------------------------| */
      "this is specified using a CONST scalar field, this will be the field\n"
      "code of that field, otherwise, it will be the number itself.",
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
    "The number of samples per frame of the data on disk for a RAW field.\n"
      "If this is specified using a CONST scalar field, this will be the\n"
      "field code of that field, otherwise, it will be the number itself.",
    NULL },
  { "table", (getter)gdpy_entry_gettable, (setter)gdpy_entry_settable,
    "The pathname of the look-up table of a LINTERP field.",
    NULL },
  { "windop", (getter)gdpy_entry_getwindop, (setter)gdpy_entry_setwindop,
    "The operation of a WINDOW field.",
    NULL },
  { "threshold", (getter)gdpy_entry_getthreshold,
    (setter)gdpy_entry_setthreshold,
    "The threshold of a WINDOW field.  The numerical type depends on the\n"
      "operation of the field.\n",
    NULL },
  { NULL }
};

#define ENTRY_DOC \
  "entry(type, name, fragment_index [, parameters, character_encoding])\n\n"\
"Returns an entry object containing the metadata for one dirfile field.\n"\
"The field type is specified by 'type', which should be one of the\n"\
"pygetdata.*_ENTRY symbols.  The field name is specified by 'name', and\n"\
"'fragment_index' indicates the format file fragment which will contain\n"\
"the specification line of this field, once the entry is added to a\n"\
"dirfile.  To add this field to the primary format file, set\n"\
"'fragment_index' to zero.\n"\
"\n"\
"The 'character_encoding' parameter sets the initial value of the\n"\
"character_encoding attribute (q.v.).  If this parameter is omitted, the\n"\
"default value is copied from the global pygetdata.character_encoding\n"\
"instead.\n"\
"\n"\
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
"    'dividend':   a number or CONST field code specifying the dividend\n"\
"                    of the RECIP.\n"\
"  WINDOW:       (in_field1, in_fields2, windop, threshold)\n"\
"    'in_field1':  a string containing the input field code.\n"\
"    'in_field2':  a string containing the check field code.\n"\
"    'windop':     the window operation.  Should be one of the windop\n"\
"                    symbols: pygetdata.WINDOP_EQ, pygetdata.WINDOP_NE,\n"\
"                    &c.\n"\
"    'threshold':  a scalar containing the threshold value.  The type of\n"\
"                    this value depends on the window operation used.\n"\
"\n"\
/* ---------------------------------------------------------------------| */\
"If a dictionary, the keys of 'parameters' should be the names of the\n"\
"tuple parameters listed above (e.g. 'type' and 'spf' for a RAW field),\n"\
"and the values the same as their tuple counterparts.\n\n"\
"An entry object provides all these parameters as attributes, which may\n"\
"be modified by assignment.  Attempting to access an attribute which is\n"\
"not available for the specified field type will result in AttributeError\n"\
"being thrown.\n\n"\
"To add a newly created entry object to a dirfile, use dirfile.add() or\n"\
"dirfile.madd() as appropriate.  To propagate changes made to an entry\n"\
"object back to the dirfile, dirfile.alter() is typically used, unless\n"\
"the name or fragment index is to be changed, in which case\n"\
"dirfile.rename() or dirfile.move() should be used.\n"

PyTypeObject gdpy_entry = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pygetdata.entry",             /* tp_name */
  sizeof(struct gdpy_entry_t),   /* tp_basicsize */
  0,                             /* tp_itemsize */
  (destructor)gdpy_entry_delete, /* tp_dealloc */
  0,                             /* tp_print */
  0,                             /* tp_getattr */
  0,                             /* tp_setattr */
  0,                             /* tp_compare */
  (reprfunc)gdpy_entry_repr,     /* tp_repr */
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
