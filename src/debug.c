/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

#include <string.h>
#include "getdata.h"
#include "internal.h"

#define COL_SIZE 100
static char debug_col[COL_SIZE + 1] = "";
static int col_count = 0;

const char* _gd_colnil(void) {
  return debug_col;
}

const char* _gd_coladd(void)
{
  if (col_count < COL_SIZE) {
    debug_col[col_count++] = ':';
    debug_col[col_count] = '\0';
  }

  return _gd_colnil();
}

const char* _gd_colsub(void)
{
  static char buffer[COL_SIZE + 1];
  strcpy(buffer, _gd_colnil());

  if (col_count > 0)
    debug_col[--col_count] = '\0';

  return buffer;
}
