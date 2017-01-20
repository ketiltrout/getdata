/* Copyright (C) 2013, 2017 D.V. Wiebe
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
#include "test.h"

int main(void)
{
#ifndef GD_LEGACY_API
  return 77; /* skipped */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int i, error = 0, r = 0;
  double three = 3.;
  int *three_ptr = (void*)(&three);
  struct FormatType *f;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "bit BIT raw 5 6\n"
    "const CONST UINT8 1\n"
    "carray CARRAY UINT8 1 2 3\n"
    "div DIVIDE raw bit\n"
    "lincom LINCOM raw 1 2 phase 3 4 div 5 6\n"
    "linterp LINTERP raw /table/path\n"
    "mplex MPLEX raw lincom 54 94\n"
    "mult MULTIPLY sbit bit\n"
    "phase PHASE raw 33\n"
    "polynom POLYNOM raw 8 9 10 11\n"
    "raw RAW UINT8 8\n"
    "recip RECIP sbit 3\n"
    "sbit SBIT raw 7 8\n"
    "window WINDOW raw phase LE 3\n"
    "/FRAMEOFFSET 12\n"
  );

  f = GetFormat(filedir, &error);
  CHECKI(error, 0);

  /* global metadata */
  CHECKS(f->FileDirName, filedir);
  CHECKI(f->frame_offset, 12);

  /* reference field */
  CHECKS(f->first_field.field, "raw");
  CHECKI(f->first_field.type, 'c');
  CHECKI(f->first_field.size, 1);
  CHECKI(f->first_field.samples_per_frame, 8);

  /* field metadata */
  CHECKI(f->n_raw, 1);
  CHECKS(f->rawEntries[0].field, "raw");
  CHECKI(f->rawEntries[0].type, 'c');
  CHECKI(f->rawEntries[0].size, 1);
  CHECKI(f->rawEntries[0].samples_per_frame, 8);

  CHECKI(f->n_lincom, 3);
  CHECKS(f->lincomEntries[0].field, "recip");
  CHECKI(f->lincomEntries[0].n_fields, 1);
  CHECKS(f->lincomEntries[0].in_fields[0], "sbit");
  CHECKF(f->lincomEntries[0].m[0], 3);
  CHECKF(f->lincomEntries[0].b[0], 0);

  CHECKS(f->lincomEntries[1].field, "lincom");
  CHECKI(f->lincomEntries[1].n_fields, 3);
  CHECKS(f->lincomEntries[1].in_fields[0], "raw");
  CHECKS(f->lincomEntries[1].in_fields[1], "phase");
  CHECKS(f->lincomEntries[1].in_fields[2], "div");
  for (i = 0; i < 3; ++i) {
    CHECKFi(i, f->lincomEntries[1].m[i], i * 2. + 1);
    CHECKFi(i, f->lincomEntries[1].b[i], i * 2. + 2);
  }

  CHECKS(f->lincomEntries[2].field, "polynom");
  CHECKI(f->lincomEntries[2].n_fields, 1);
  CHECKS(f->lincomEntries[2].in_fields[0], "raw");
  CHECKF(f->lincomEntries[2].m[0], 9);
  CHECKF(f->lincomEntries[2].b[0], 8);

  CHECKI(f->n_linterp, 1);
  CHECKS(f->linterpEntries[0].field, "linterp");
  CHECKS(f->linterpEntries[0].raw_field, "raw");
  CHECKS(f->linterpEntries[0].linterp_file, "/table/path");

  CHECKI(f->n_bit, 2);
  CHECKS(f->bitEntries[0].field, "bit");
  CHECKS(f->bitEntries[0].raw_field, "raw");
  CHECKI(f->bitEntries[0].bitnum, 5);
  CHECKI(f->bitEntries[0].numbits, 6);

  CHECKS(f->bitEntries[1].field, "sbit");
  CHECKS(f->bitEntries[1].raw_field, "raw");
  CHECKI(f->bitEntries[1].bitnum, 7);
  CHECKI(f->bitEntries[1].numbits, 8);

  CHECKI(f->n_multiply, 2);
  CHECKS(f->multiplyEntries[0].field, "div");
  CHECKS(f->multiplyEntries[0].in_fields[0], "raw");
  CHECKS(f->multiplyEntries[0].in_fields[1], "bit");

  CHECKS(f->multiplyEntries[1].field, "mult");
  CHECKS(f->multiplyEntries[1].in_fields[0], "sbit");
  CHECKS(f->multiplyEntries[1].in_fields[1], "bit");

  CHECKI(f->n_phase, 1);
  CHECKS(f->phaseEntries[0].field, "phase");
  CHECKS(f->phaseEntries[0].raw_field, "raw");
  CHECKI(f->phaseEntries[0].shift, 33);

  CHECKI(f->n_mplex, 2);
  CHECKS(f->mplexEntries[0].field, "mplex");
  CHECKS(f->mplexEntries[0].data_field, "raw");
  CHECKS(f->mplexEntries[0].cnt_field, "lincom");
  CHECKI(f->mplexEntries[0].i, 54);
  CHECKI(f->mplexEntries[0].max_i, 94);

  CHECKS(f->mplexEntries[1].field, "window");
  CHECKS(f->mplexEntries[1].data_field, "raw");
  CHECKS(f->mplexEntries[1].cnt_field, "phase");
  CHECKI(f->mplexEntries[1].i, GD_WINDOP_LE);
  CHECKI(f->mplexEntries[1].max_i, *three_ptr);

  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
