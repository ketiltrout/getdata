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
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <sys/stat.h>
#endif

#include "getdata_internal.h"

off64_t get_n_frames64(DIRFILE* D)
{
  char raw_data_filename[FILENAME_MAX];
  struct stat64 statbuf;
  size_t nf;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetGetDataError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    return 0;
  }

  if (D->first_field == NULL) {
    _GD_SetGetDataError(D, GD_E_EMPTY, 0, NULL, 0, NULL);
    return 0;
  }

  /* load the first valid raw field */
  snprintf(raw_data_filename, FILENAME_MAX, "%s/%s", D->name,
      ENTRY(Raw, D->first_field)->file);
  if (stat64(raw_data_filename, &statbuf) < 0) {
    _GD_SetGetDataError(D, GD_E_RAW_IO, 0, NULL, 0, NULL);
    return 0;
  }

  nf = statbuf.st_size / (ENTRY(Raw, D->first_field)->size *
      ENTRY(Raw, D->first_field)->samples_per_frame);
  nf += D->frame_offset;

  return nf;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t get_n_frames(DIRFILE* D)
{
  return (off_t)get_n_frames64(D);
}
/* vim: ts=2 sw=2 et tw=80
*/
