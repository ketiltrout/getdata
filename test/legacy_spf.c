/* Copyright (C) 2008-2011, 2017 D.V. Wiebe
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
/* Retreiving the samples-per-frame of a field via the legacy API should
 * succeed cleanly */
#include "test.h"

int main(void)
{
#ifndef GD_LEGACY_API
  return 77; /* skip */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int error, r = 0;
  unsigned int spf;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 11\n");

  spf = GetSamplesPerFrame(filedir, "data", &error);

  unlink(format);
  rmdir(filedir);

  CHECKU(spf, 11);
  return r;
#endif
}
