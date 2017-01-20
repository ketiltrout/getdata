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

static int saw_callback = 0;

int callback(gd_parser_data_t *pdata, void *extra gd_unused_)
{
  if (saw_callback)
    return GD_SYNTAX_ABORT;

  saw_callback = 1;

  pdata->line = strdup("/REFERENCE data\n");

  return GD_SYNTAX_RESCAN;
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "BADDIRECTIVE BADTYPE\n");

  D = gd_cbopen(filedir, GD_RDONLY, callback, NULL);
  error = gd_error(D);
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(saw_callback, 1);
  CHECKI(error, GD_E_BAD_REFERENCE);

  return r;
}
