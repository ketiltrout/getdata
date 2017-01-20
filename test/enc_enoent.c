/* Copyright (C) 2016, 2017 D.V. Wiebe
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
#ifdef ENC_SKIP_TEST
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int error, r = 0;
  DIRFILE *D;
  char *estr;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n/ENCODING " ENC_NAME "\n");

  D = gd_open(filedir, GD_RDONLY);
  gd_getdata(D, "data", 0, 0, 0, 1, GD_NULL, NULL);
  error = gd_error(D);
  estr = gd_error_string(D, NULL, 0);
  
#ifdef USE_ENC
  CHECKI(error, GD_E_IO);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
#endif

  CHECKPN(estr);
  free(estr);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
