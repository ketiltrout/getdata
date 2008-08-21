/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2003-2005 Theodore Kisner
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
 * License along with the GetData if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifndef GETDATA_LEGACY_H
#define GETDATA_LEGACY_H

/* Sanity check */
#ifndef GETDATA_H
#error "Never use <getdata_legacy.h> directly; include <getdata.h> instead."
#endif

/* comments about cancellation points and __THROW found in getdata.h apply
 * here too...
 */

/* legacy API for getdata_error_string() */
extern char* GetDataErrorString(char* buffer, size_t buflen) __THROW
  __gd_nonnull ((1)) __attribute_deprecated__;

/* legacy API for getdata() */
extern int GetData(const char *dirfilename, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char return_type, void *data_out, int *error_code) __gd_nonnull ((1, 2, 9))
  __attribute_deprecated__;

/* legacy API for get_n_frames() */
extern int GetNFrames(const char *dirfilename, int *error_code,
    const void *unused) __gd_nonnull ((1, 2)) __attribute_deprecated__;

/* legacy API for get_samples_per_frame() */
extern int GetSamplesPerFrame(const char *dirfilename, const char *field_code,
    int *error_code) __THROW __gd_nonnull ((1, 2, 3)) __attribute_deprecated__;

/* legacy API for putdata() */
extern int PutData(const char *filename_in, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char data_type, const void *data_in, int *error_code) __gd_nonnull ((1, 2, 9))
  __attribute_deprecated__;

#endif
