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
/* Test field modifying */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";

#define N 12
  struct {
    const char *file;
    const char *name;
    int len;
  } data[N] = {
    { "dirfile/I08",  "I08",   100 },
    { "dirfile/U08",  "U08",   200 },
    { "dirfile/I16",  "I16",   300 },
    { "dirfile/U16",  "U16",   400 },
    { "dirfile/I32",  "I32",   500 },
    { "dirfile/U32",  "U32",   600 },
    { "dirfile/I64",  "I64",   700 },
    { "dirfile/U64",  "U64",   800 },
    { "dirfile/F32",  "F32",   900 },
    { "dirfile/F64",  "F64",  1000 },
    { "dirfile/C64",  "C64",  1100 },
    { "dirfile/C128", "C128", 1200 }
  };
  int i, e1, r = 0;
  off_t e2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "I08 RAW   INT8  10\n"
      "U08 RAW  UINT8  10\n"
      "I16 RAW   INT16 10\n"
      "U16 RAW  UINT16 10\n"
      "I32 RAW   INT32 10\n"
      "U32 RAW  UINT32 10\n"
      "I64 RAW   INT64 10\n"
      "U64 RAW  UINT64 10\n"
      "F32 RAW FLOAT32 10\n"
      "F64 RAW FLOAT64 10\n"
      "C64 RAW COMPLEX64 10\n"
      "C128 RAW COMPLEX128 10\n"
      );
  MAKEDATAFILE(data[0].file, int8_t, i, data[0].len);
  MAKEDATAFILE(data[1].file, uint8_t, i, data[1].len);
  MAKEDATAFILE(data[2].file, int16_t, i, data[2].len);
  MAKEDATAFILE(data[3].file, uint16_t, i, data[3].len);
  MAKEDATAFILE(data[4].file, int32_t, i, data[4].len);
  MAKEDATAFILE(data[5].file, uint32_t, i, data[5].len);
  MAKEDATAFILE(data[6].file, int64_t, i, data[6].len);
  MAKEDATAFILE(data[7].file, uint64_t, i, data[7].len);
  MAKEDATAFILE(data[8].file, float, i, data[8].len);
  MAKEDATAFILE(data[9].file, double, i, data[9].len);
  MAKEDATAFILE(data[10].file, float, i, 2 * data[10].len);
  MAKEDATAFILE(data[11].file, double, i, 2 * data[11].len);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  for (i = 0; i < N; ++i) {
    e1 = gd_alter_raw(D, data[i].name, GD_NULL, 7, 1);
    CHECKIi(i, e1, 0);
    e2 = gd_eof(D, data[i].name);
    CHECKIi(i, e2, data[i].len * 7 / 10);
  }

  gd_discard(D);

  for (i = 0; i < N; ++i)
    unlink(data[i].file);
  unlink(format);
  rmdir(filedir);

  return r;
}
