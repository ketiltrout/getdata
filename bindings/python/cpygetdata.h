/* Copyright (C) 2016 D. V. Wiebe
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

/* This header file defines the pygetdata C API */
#ifndef CPYGETDATA_H
#define CPYGETDATA_H

#ifdef __cplusplus
extern "C" {
#endif

#include <getdata.h>

#define PyDirfile_Type_NUM 0
#define PyDirfile_Dirfile_NUM 1
#define PyDirfile_Raise_NUM 2

#define PyDirfile_API_length 3

#define PYDIRFILE_CAPSULENAME "pygetdata.__CAPI"

#ifndef PYGETDATA_MODULE

  static void **PyDirfile_API;

#define PyDirfile_Type (*(PyTypeObject*)PyDirfile_API[PyDirfile_Type_NUM])
#define PyDirfile_Dirfile \
  (*(DIRFILE *(*)(PyObject*))PyDirfile_API[PyDirfile_Dirfile_NUM])
#define PyDirfile_Raise \
  (*(int (*)(PyObject*))PyDirfile_API[PyDirfile_Raise_NUM])

#define PyDirfile_Check(o) PyObject_TypeCheck(o, &PyDirfile_Type)

  static int import_pygetdata(void)
  {
    PyDirfile_API = (void**)PyCapsule_Import(PYDIRFILE_CAPSULENAME, 0);
    return (PyDirfile_API == NULL) ? 1 : 0;
  }

#endif

#ifdef __cplusplus
}
#endif

#endif
