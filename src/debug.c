/* Copyright (C) 2008, 2010, 2011, 2013, 2014 D. V. Wiebe
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
#include "internal.h"

extern char gd_debug_col[GD_COL_SIZE + 1];
extern int gd_col_count;

const char* gd_colnil(void) {
  return gd_debug_col;
}

const char* gd_coladd(void)
{
  if (gd_col_count < GD_COL_SIZE) {
    gd_debug_col[gd_col_count++] = ':';
    gd_debug_col[gd_col_count] = '\0';
  }

  return gd_colnil();
}

const char* gd_colsub(void)
{
  static char buffer[GD_COL_SIZE + 1];
  strcpy(buffer, gd_debug_col);

  if (gd_col_count > 0)
    gd_debug_col[--gd_col_count] = '\0';

  return buffer;
}

void gd_colclear(void)
{
  gd_col_count = 0;
  gd_debug_col[0] = '\0';
}
