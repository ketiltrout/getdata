/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Retreiving the number of frames should succeed cleanly */
#include "test.h"

int main(void)
{
#if !defined(TEST_ZZIP) || !defined(TEST_SLIM)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *dataslm = "dirfile/data.slm";
  const char *rawzip = "dirfile/raw.zip";
  char command[4096];
  int i, error, r = 0;
  size_t n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 1\n/ENCODING zzslim\n");
  MAKEDATAFILE(data, uint16_t, i, 256);

  /* compress, twice */
  snprintf(command, 4096, "%s -k %s > /dev/null", SLIMDATA, data);
  if (gd_system(command))
    return 1;
  chdir(filedir);
  snprintf(command, 4096, "%s raw data.slm > /dev/null", ZIP);
  if (gd_system(command))
    return 1;
  chdir("..");
  unlink(dataslm);
  unlink(data);

#ifdef USE_ZZSLIM
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDONLY);
#endif
  n = gd_nframes(D);
  error = gd_error(D);
  gd_discard(D);

  unlink(rawzip);
  unlink(format);
  rmdir(filedir);

#ifdef USE_ZZSLIM
  CHECKI(error, 0);
  CHECKI(n, 256);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
  CHECKI(n, GD_E_UNSUPPORTED);
#endif

  return r;
#endif
}
