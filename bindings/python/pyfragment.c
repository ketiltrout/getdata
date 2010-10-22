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

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "O!i:pygetdata.fragment.__init__", keywords, &gdpy_dirfile,
        &self->dirfile, self->n))
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
  
  const char* name = gd_fragmentname(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* pyname = PyString_FromString(name);

  dreturn("%p", pyname);
  return pyname;
}

static PyObject* gdpy_fragment_getencoding(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  unsigned long enc = gd_encoding(self->dirfile->D, self->n);

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
        "k|i:pygetdata.fragment.alter_encoding", keywords, &enc, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_alter_encoding(self->dirfile->D, enc, self->n, recode);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_fragment_getendianness(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  unsigned long end = gd_endianness(self->dirfile->D, self->n);

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
        "k|i:pygetdata.fragment.alter_endianness", keywords, &end, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_alter_endianness(self->dirfile->D, end, self->n, recode);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_fragment_rewrite(struct gdpy_fragment_t* self)
{
  dtrace("%p", self);

  gd_rewrite_fragment(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  Py_INCREF(Py_None);
  dreturn("%p", Py_None);
  return Py_None;
}

static PyObject* gdpy_fragment_getoffset(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  off_t offset = gd_frameoffset(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* obj = PyLong_FromLongLong((long long)offset);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_fragment_setoffset(struct gdpy_fragment_t* self,
    PyObject* args, PyObject* keys)
{
  dtrace("%p, %p, %p", self, args, keys);

  char* keywords[] = { "frameoffset", "recode", NULL };
  long long offset;
  int recode = 0;

  if (!PyArg_ParseTupleAndKeywords(args, keys,
        "L|i:pygetdata.fragment.alter_frameoffset", keywords, &offset, &recode))
  {
    dreturn("%p", NULL);
    return NULL;
  }

  gd_alter_frameoffset(self->dirfile->D, (off_t)offset, self->n,
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
  
  int parent = gd_parent_fragment(self->dirfile->D, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, NULL);

  PyObject* obj = PyInt_FromLong(parent);

  dreturn("%p", obj);
  return obj;
}

static PyObject* gdpy_fragment_getprotection(struct gdpy_fragment_t* self,
    void* closure)
{
  dtrace("%p, %p", self, closure);
  
  int prot = gd_protection(self->dirfile->D, self->n);

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

  gd_alter_protection(self->dirfile->D, p, self->n);

  PYGD_CHECK_ERROR(self->dirfile->D, -1);

  dreturn("%i", 0);
  return 0;
}

static PyGetSetDef gdpy_fragment_getset[] = {
  { "encoding", (getter)gdpy_fragment_getencoding, NULL,
    "The encoding scheme of this fragment.  This will be one of the\n"
      "pygetdata.*_ENCODED symbols.  To change this value, use the\n"
      "alter_encoding method.",
    NULL },
  { "endianness", (getter)gdpy_fragment_getendianness, NULL,
    "The byte sex of this fragment.  This will be either\n"
      "pygetdata.BIG_ENDIAN or pygetdata.LITTLE_ENDIAN, bitwise or'd with\n"
      "pygetdata.ARM_ENDIAN or pygetdata.NOT_ARM_ENDIAN.  To change this\n"
      "value, use the alter_endianness method.",
    NULL },
  { "frameoffset", (getter)gdpy_fragment_getoffset, NULL,
    "The frame offset of this fragment.  To change this value, use the\n"
      "alter_frameoffset method.",
    NULL },
  { "index", (getter)gdpy_fragment_getindex, NULL,
    "The index number of this fragment.  This is simply the index value\n"
      "passed to the constructor.",
    NULL },
  { "name", (getter)gdpy_fragment_getname, NULL,
    "The pathname of this fragment.  This attribute cannot be changed.\n"
      "See gd_fragmentname(3).",
    NULL },
  { "parent", (getter)gdpy_fragment_getparent, NULL,
    "The fragment index of this fragment's parent.  Since the primary\n"
      "format file has no parent, an error will occur if an attempt is made\n"
      "to read this attribute for the primary format file.  This value\n"
      "cannot be changed.  To move a format file fragment to a different\n"
      "parent fragment, use dirfile.uninclude() and dirfile.include().",
    NULL },
  { "protection", (getter)gdpy_fragment_getprotection,
    (setter)gdpy_fragment_setprotection,
    "The (advisory) protection level of this fragment.  This will be one\n"
      "the pygetdata.PROTECT_* symbols.  The protection level of this\n"
      "fragment can be changed by assigning to this attribute.  See\n"
      "gd_protection(3) and gd_alter_protection(3).",
    NULL },
  { NULL }
};

static PyMethodDef gdpy_fragment_methods[] = {
  {"alter_encoding", (PyCFunction)gdpy_fragment_setencoding,
    METH_VARARGS | METH_KEYWORDS,
    "alter_encoding(encoding [, recode])\n\n"
      "Change the encoding scheme of this fragment.  The 'encoding'\n"
      "parameter should be one of the pygetdata.*_ENCODED symbols\n"
      "(excluding pygetdata.AUTO_ENCODED).  If 'recode' is given, and is\n"
      "non-zero, the RAW files affected by this change will be converted\n"
      "to the new encoding.  See gd_alter_encoding(3)."
  },
  {"alter_endianness", (PyCFunction)gdpy_fragment_setendianness,
    METH_VARARGS | METH_KEYWORDS,
    "alter_endianness(endianness [, recode])\n\n"
      "Change the byte sex of this fragment.  The 'endianness' parameter\n"
      "should be pygetdata.LITTLE_ENDIAN, pygetdata.BIG_ENDIAN, or some\n"
      "combination of these two as described in the gd_alter_endianness\n"
      /* -----------------------------------------------------------------| */
      "manual page, and possibly bitwise or'd with pygetdata.ARM_ENDIAN or\n"
      "pygetdata.NOT_ARM_ENDIAN.  If 'recode' is given, and is non-zero,\n"
      "the RAW files affected by this change will be converted to the byte\n"
      "sex.  See gd_alter_endianness(3)."
  },
  {"alter_frameoffset", (PyCFunction)gdpy_fragment_setoffset,
    METH_VARARGS | METH_KEYWORDS,
    "alter_frameoffset(frameoffset [, recode])\n\n"
      "Change the frame offset of this fragment.  The 'frameoffset'\n"
      "parameter should specify the new frame offset.  If 'recode' is\n"
      "given, and is non-zero, the RAW files affected bt this change will\n"
      "be shifted appropriately for the new frame offset.  See\n"
      "gd_alter_frameoffset(3)."
  },
  {"rewrite", (PyCFunction)gdpy_fragment_rewrite, METH_NOARGS,
    "rewrite()\n\n"
      "Force re-writing of this fragment on disc, regardless of whether\n"
      "it has been modified or not.  See gd_rewrite_fragment(3)."
  },
  { NULL, NULL, 0, NULL }
};

#define FRAGMENT_DOC \
"fragment(dirfile, index)\n\n"\
"Returns a fragment object which describes the fragment-specific metadata\n"\
"of one format file fragment in a dirfile.  The 'dirfile' parameter\n"\
"should be a dirfile object.  Index is the fragment index of the desired\n"\
"format file fragment.  This constructor is equivalent to calling\n\n"\
"  dirfile.fragment(index)\n\n"\
"The fragment object remains connected to its dirfile, and changes made\n"\
"to the fragment object are propagated back to the dirfile.  This object\n"\
/* ---------------------------------------------------------------------| */\
"creates a new reference to the supplied dirfile.  Calling\n"\
"close() or discard() on the dirfile and then attempting to access a\n"\
"fragment object previously derived from that dirfile will result in an\n"\
"error."

PyTypeObject gdpy_fragment = {
  PyObject_HEAD_INIT(NULL)
    0,                           /* ob_size */
  "pygetdata.fragment",          /* tp_name */
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
  FRAGMENT_DOC,                  /* tp_doc */
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
