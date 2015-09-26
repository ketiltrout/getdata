/* Copyright (C) 2012-2015 D. V. Wiebe
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

int _GD_ZzslimName(DIRFILE *restrict D, const char *restrict enc_data,
    struct gd_raw_file_ *restrict file, const char *restrict base,
    int temp gd_unused_, int resolv)
{
  size_t enc_len;

  dtrace("%p, \"%s\", %p, \"%s\", <unused>, %i", D, enc_data, file, base,
      resolv);

  if (enc_data == NULL)
    enc_data = "raw";

  enc_len = strlen(enc_data);
  
  /* Resolution is degenerate with the zzip encoding; so skip it for now */
  if (resolv) {
    dreturn("%i", 1);
    return 1;
  }

  if (file->name == NULL) {
    file->D = D;
    file->name = (char *)malloc(strlen(base) + strlen(enc_data) + 6);
    if (file->name == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }

    strcpy(file->name, enc_data);
    file->name[enc_len] = '/';
    sprintf(file->name + enc_len + 1, "%s.slm", base);
  }

  dreturn("%i (%s)", 0, file->name);
  return 0;
}

#define ZZSLIM
#include "slim.c"
