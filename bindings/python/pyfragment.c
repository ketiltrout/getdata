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

/* Fragment */
static void gdpy_fragment_delete(struct gdpy_fragment_t* self)
{
  dtrace("%p", self);

  Py_XDECREF(self->dirfile);

  dreturnvoid();
}

static PyObject* gdpy_fragment_create(PyTypeObject *type, PyObject *args,
    PyObject *keys)
{
  dtrace("%p, %p, %p", type, args, keys);

  struct gdpy_fragment_t *self = (struct gdpy_fragment_t*)type->tp_alloc(type,
      0);

  if (self)
    self->dirfile = NULL;

  dreturn("%p", self);
  return (PyObject*)self;
}

static int gdpy_fragment_init(struct gdpy_fragment_t* self, PyObject *args,
    PyObject *keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char *keywords[] = {"dirifle", "index", NULL};

  if (!PyArg_ParseTupleAndKeywords(args, keys, "O!i:getdata.fragment.__init__",
        keywords, &gdpy_dirfile, &self->dirfile, self->n))
  {
    dreturn("%i", -1);
    return -1;
  }

  Py_INCREF(self->dirfile);

  dreturn("%i", 0);
  return 0;
}

static PyObject* gdpy_fragment_getindex(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  PyObject* obj = PyInt_FromLong(self->n);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_fragment_getname(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  const char* name = get_fragmentname(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* pyname = PyString_FromString(name);

  dreturn("%p", pyname);
  return pyname;
}

static PyObject* gdpy_fragment_getencoding(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  unsigned long enc = get_encoding(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* obj = PyLong_FromUnsignedLong(enc);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_fragment_setencoding(struct gdpy_fragment_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "encoding", "recode", NULL };
  unsigned long enc;
  int recode = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "k|i:getdata.fragment.alter_encoding", keywords, &enc, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_alter_encoding(self->dirfile->D, enc, self->n, recode);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_fragment_getendianness(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  unsigned long end = get_endianness(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* obj = PyLong_FromUnsignedLong(end);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_fragment_setendianness(struct gdpy_fragment_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "endianness", "recode", NULL };
  unsigned long end;
  int recode = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "k|i:getdata.fragment.alter_endianness", keywords, &end, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_alter_endianness(self->dirfile->D, end, self->n, recode);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_fragment_getoffset(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  off64_t offset = get_frameoffset64(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* obj = PyLong_FromLongLong((long long)offset);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_fragment_setoffset(struct gdpy_fragment_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "encoding", "recode", NULL };
  long long offset;
  int recode = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "L|i:getdata.fragment.alter_frameoffset", keywords, &offset, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  dirfile_alter_frameoffset64(self->dirfile->D, (off64_t)offset, self->n,
      recode);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_fragment_getparent(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  int parent = get_parent_fragment(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* obj = PyInt_FromLong(parent);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_fragment_getprotection(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  int prot = get_protection(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* obj = PyInt_FromLong(prot);

  dreturn("%p", obj);
  return obj;
}

static int gdpy_fragment_setprotection(struct gdpy_fragment_t* self,
    PyObject *value, void *closure)
{
  dtrace("%p, %p, %p", self, value, closure);

  int p = PyInt_AsLong(value);

  if (PyErr_Occurred()) {
    dreturn("%i", -1);
    return -1;
  }

  dirfile_protect(self->dirfile->D, p, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, -1);

  dreturn("%i", 0);
  return 0;
}

static PyGetSetDef gdpy_fragment_getset[] = {
  { "encoding", (getter)gdpy_fragment_getencoding, NULL,
    "The encoding scheme of this fragment.", NULL },
  { "endianness", (getter)gdpy_fragment_getendianness, NULL,
    "The byte sex of this fragment.", NULL },
  { "frameoffset", (getter)gdpy_fragment_getoffset, NULL,
    "The frame offset of this fragment.", NULL },
  { "index", (getter)gdpy_fragment_getindex, NULL,
    "The index number of this fragment.", NULL },
  { "name", (getter)gdpy_fragment_getname, NULL,
    "The pathname of this fragment.", NULL },
  { "parent", (getter)gdpy_fragment_getparent, NULL,
    "The pathname of this fragment.", NULL },
  { "protection", (getter)gdpy_fragment_getprotection,
    (setter)gdpy_fragment_setprotection,
    "The protection level of this fragment.", NULL },
  { NULL }
};

static PyMethodDef gdpy_fragment_methods[] = {
  {"alter_encoding", (PyCFunction)gdpy_fragment_setencoding, METH_VARARGS |
    METH_KEYWORDS, "Alter the encoding scheme of this fragment."},
  {"alter_endianness", (PyCFunction)gdpy_fragment_setendianness, METH_VARARGS |
    METH_KEYWORDS, "Alter the byte sex of this fragment."},
  {"alter_frameoffset", (PyCFunction)gdpy_fragment_setoffset, METH_VARARGS |
    METH_KEYWORDS, "Alter the frame offset of this fragment."},
  { NULL, NULL, 0, NULL }
};

PyTypeObject gdpy_fragment = {
  PyObject_HEAD_INIT(NULL)
    0,                           /* ob_size */
  "getdata.fragment",            /* tp_name */
  sizeof(struct gdpy_fragment_t),/* tp_basicsize */
  0,                             /* tp_itemsize */
  (destructor)gdpy_fragment_delete, /* tp_dealloc */
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
  "The Dirfile fragment object", /* tp_doc */
  0,                             /* tp_traverse */
  0,                             /* tp_clear */
  0,                             /* tp_richcompare */
  0,                             /* tp_weaklistoffset */
  0,                             /* tp_iter */
  0,                             /* tp_iternext */
  gdpy_fragment_methods,         /* tp_methods */
  0,                             /* tp_members */
  gdpy_fragment_getset,          /* tp_getset */
  0,                             /* tp_base */
  0,                             /* tp_dict */
  0,                             /* tp_descr_get */
  0,                             /* tp_descr_set */
  0,                             /* tp_dictoffset */
  (initproc)gdpy_fragment_init,  /* tp_init */
  0,                             /* tp_alloc */
  gdpy_fragment_create,          /* tp_new */
};
