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
#include "test.h"

#define CHECKOP(i,f,v) \
  if (0 == gd_entry(D, f, &E)) { \
    CHECKIi(i, E.EN(window,windop), v); \
    gd_free_entry_strings(&E); \
  }
  

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int e1, e2, r = 0;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL);

  gd_add_spec(D, "eq  WINDOW a b EQ  0", 0);
  gd_add_spec(D, "ge  WINDOW a b GE  0", 0);
  gd_add_spec(D, "gt  WINDOW a b GT  0", 0);
  gd_add_spec(D, "le  WINDOW a b LE  0", 0);
  gd_add_spec(D, "lt  WINDOW a b LT  0", 0);
  gd_add_spec(D, "ne  WINDOW a b NE  0", 0);
  gd_add_spec(D, "set WINDOW a b SET 0", 0);
  gd_add_spec(D, "clr WINDOW a b CLR 0", 0);

  e1 = gd_metaflush(D);
  CHECKI(e1, GD_E_OK);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDONLY);

  CHECKOP(0, "eq",  GD_WINDOP_EQ );
  CHECKOP(0, "ge",  GD_WINDOP_GE );
  CHECKOP(0, "gt",  GD_WINDOP_GT );
  CHECKOP(0, "le",  GD_WINDOP_LE );
  CHECKOP(0, "lt",  GD_WINDOP_LT );
  CHECKOP(0, "ne",  GD_WINDOP_NE );
  CHECKOP(0, "set", GD_WINDOP_SET);
  CHECKOP(0, "clr", GD_WINDOP_CLR);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
