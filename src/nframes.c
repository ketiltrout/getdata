/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
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
 * You should have received a copy of the GNU General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <errno.h>
#include <sys/stat.h>
#endif

#include "internal.h"

off64_t get_nframes64(DIRFILE* D)
{
  char raw_data_filename[FILENAME_MAX];
  struct stat64 statbuf;
  off64_t nf;

  dtrace("%p", D);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%lli", 0LL);
    return 0;
  }

  if (D->first_field == NULL) {
    _GD_SetError(D, GD_E_EMPTY, 0, NULL, 0, NULL);
    dreturn("%lli", 0LL);
    return 0;
  }

  /* load the first valid raw field */
  snprintf(raw_data_filename, FILENAME_MAX, "%s/%s", D->name,
      D->first_field->file);
  if (stat64(raw_data_filename, &statbuf) < 0) {
    dreturn("%lli", 0LL);
    _GD_SetError(D, GD_E_RAW_IO, 0, raw_data_filename, errno, NULL);
    return 0;
  }

  nf = statbuf.st_size / (D->first_field->size * D->first_field->spf);
  nf += D->frame_offset;

  dreturn("%lli", nf);
  return nf;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t get_nframes(DIRFILE* D)
{
  return (off_t)get_nframes64(D);
}
/* vim: ts=2 sw=2 et tw=80
*/
