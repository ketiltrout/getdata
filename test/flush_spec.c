/* Copyright (C) 2009-2011, 2013 D. V. Wiebe
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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *apath = "dirfile/a";
  const char *jpath = "dirfile/j";
  const char *spec[] = {
    "a RAW UINT8 1\n",
    "c CONST UINT64 1\n",
    "d CONST UINT64 2\n",
    "e LINCOM 2 a c 1 h 3 d\n",
    "f LINTERP a /lut/table\n",
    "g MULTIPLY e f\n",
    "h BIT a 2 d\n",
    "i PHASE h c\n",
    "j RAW UINT16 d\n",
    "k PHASE h 3\n",
    "l SBIT a d 2\n",
    "m POLYNOM a 1 c 2 d\n",
    "n STRING a\\ b\\ c\\ \\x01\\ ÿ\n",
    "o DIVIDE j k\n",
    "p RECIP e d\n",
    "q WINDOW i m EQ c\n",
    "r CARRAY FLOAT64 1.2 3.4 5.6 7.8\n",
    NULL
  };

  int error, i = 0, r = 0;
  FILE *stream;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_TRUNC |
      GD_VERBOSE);
  for (i = 0; spec[i] != NULL; ++i)
    gd_add_spec(D, spec[i], 0);
  error = gd_error(D);

  gd_discard(D);

  stream = fopen(format, "rt");
  i = 0;
  while (!feof(stream)) {
    char line[GD_MAX_LINE_LENGTH];
    if (fgets(line, GD_MAX_LINE_LENGTH, stream) == NULL)
      break;

    if (line[0] == '/' || line[0] == '#' || line[0] < ' ')
      continue;

    if (strcmp(line, spec[i]) != 0) {
      fprintf(stderr, "%s <=> %s", spec[i], line);
      error = 1;
    }
    ++i;
  }
  fclose(stream);

  unlink(apath);
  unlink(jpath);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);

  return r;
}
