/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2003-2005 Theodore Kisner
 * Copyright (C) 2005-2010 D. V. Wiebe
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
#ifndef GETDATA_LEGACY_H
#define GETDATA_LEGACY_H

/* Sanity check */
#ifndef GETDATA_H
#error "Never use <getdata_legacy.h> directly; include <getdata.h> instead."
#endif

extern const char *GD_ERROR_CODES[GD_N_ERROR_CODES] gd_deprecated;

struct RawEntryType {
  const char* field;
  char type;
  int size;
  int samples_per_frame;
};

struct LincomEntryType {
  const char *field;
  int n_fields;
  const char *in_fields[GD_MAX_LINCOM];
  double m[GD_MAX_LINCOM], b[GD_MAX_LINCOM];
};

struct LinterpEntryType {
  const char *field, *raw_field, *linterp_file;
  int n_interp;
  double *x, *y;
};

struct MultiplyEntryType {
  const char *field;
  const char *in_fields[2];
};

struct MPlexEntryType {
  const char *field, *cnt_field, *data_field;
  int i;
  int max_i;
};

struct BitEntryType {
  const char *field, *raw_field;
  int bitnum, numbits;
};

struct PhaseEntryType {
  const char *field, *raw_field;
  int shift;
};

/* The old Format type */
struct FormatType {
  const char* FileDirName;
  int frame_offset;
  struct RawEntryType first_field;
  struct RawEntryType *rawEntries;
  int n_raw;
  struct LincomEntryType *lincomEntries;
  int n_lincom;
  struct LinterpEntryType *linterpEntries;
  int n_linterp;
  struct MultiplyEntryType *multiplyEntries;
  int n_multiply;
  struct MPlexEntryType *mplexEntries;
  int n_mplex;
  struct BitEntryType *bitEntries;
  int n_bit;
  struct PhaseEntryType *phaseEntries;
  int n_phase;
};

#ifdef __cplusplus
extern "C" {
#endif

extern struct FormatType *GetFormat(const char *filedir, int *error_code)
  gd_nothrow gd_nonnull ((1,2)) gd_deprecated;

/* legacy API for get_error_string() */
extern char* GetDataErrorString(char* buffer, size_t buflen) gd_nothrow
  gd_nonnull ((1)) gd_deprecated;

/* legacy API for getdata() */
extern int GetData(const char *dirfilename, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char return_type, void *data_out, int *error_code) gd_nonnull ((1,2,9))
  gd_deprecated;

/* legacy API for get_nframes() */
extern int GetNFrames(const char *dirfilename, int *error_code,
    const void *unused) gd_nonnull ((1,2)) gd_deprecated;

/* legacy API for get_spf() */
extern int GetSamplesPerFrame(const char *dirfilename, const char *field_code,
    int *error_code) gd_nothrow gd_nonnull ((1,2,3)) gd_deprecated;

/* legacy API for putdata() */
extern int PutData(const char *filename_in, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char data_type, const void *data_in, int *error_code)
gd_nonnull ((1,2,9)) gd_deprecated;

#ifdef __cplusplus
}
#endif

#endif
