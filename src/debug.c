/* (C) 2008, 2010 D. V. Wiebe
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

#include <string.h>
#include "getdata.h"
#include "internal.h"

#define COL_SIZE 100

extern char gd_debug_col[COL_SIZE + 1];
extern int gd_col_count;
#ifdef GDLIB
int gd_col_count = 0;
char gd_debug_col[COL_SIZE + 1] = "";
#endif

const char* _gd_colnil(void) {
  return gd_debug_col;
}

const char* _gd_coladd(void)
{
  if (gd_col_count < COL_SIZE) {
    gd_debug_col[gd_col_count++] = ':';
    gd_debug_col[gd_col_count] = '\0';
  }

  return _gd_colnil();
}

const char* _gd_colsub(void)
{
  static char buffer[COL_SIZE + 1];
  strcpy(buffer, _gd_colnil());

  if (gd_col_count > 0)
    gd_debug_col[--gd_col_count] = '\0';

  return buffer;
}
