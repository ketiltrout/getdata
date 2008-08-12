/*                           (C) 2003 C. Barth Netterfield */
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef GETDATA_INTERNAL_H
#define GETDATA_INTERNAL_H

#include "getdata.h"

/* maximum number of recursions */
#define GD_MAX_RECURSE_LEVEL  32

/* maximum length of a format file line */
#ifndef FILENAME_MAX
#  define FILENAME_MAX 4096
#endif
#define MAX_LINE_LENGTH FILENAME_MAX

/* Suberror codes */
#define GD_E_OPEN_NOT_DIRFILE  0
#define GD_E_OPEN_NOT_EXIST    1
#define GD_E_OPEN_NO_ACCESS    3

#define GD_E_TRUNC_DIR         0
#define GD_E_TRUNC_STAT        1
#define GD_E_TRUNC_UNLINK      2

#define GD_E_CREAT_DIR         0
#define GD_E_CREAT_FORMAT      1
#define GD_E_CREAT_EXCL        2

#define GD_E_FORMAT_BAD_TYPE   0
#define GD_E_FORMAT_BAD_SPF    1
#define GD_E_FORMAT_N_FIELDS   2
#define GD_E_FORMAT_N_COLS     3
#define GD_E_FORMAT_MAX_I      4
#define GD_E_FORMAT_NUMBITS    5
#define GD_E_FORMAT_BITNUM     6
#define GD_E_FORMAT_BITSIZE    7
#define GD_E_FORMAT_BAD_LINE   9
#define GD_E_FORMAT_N_RAW     10
#define GD_E_FORMAT_RES_NAME  11
#define GD_E_FORMAT_ENDIAN    12

#define GD_E_LINFILE_OPEN      0
#define GD_E_LINFILE_LENGTH    1

#define GD_E_NORAW_NORAW       0
#define GD_E_NORAW_STATFAILED  1

struct RawEntryType {
  char* field;
  int field_type;
  char* file;
  gd_type_t data_type;
  int fp;
  size_t size;
  int samples_per_frame;
};

struct LincomEntryType {
  char* field;
  int field_type;
  int n_infields;
  char *in_fields[GD_MAX_LINCOM];
  double m[GD_MAX_LINCOM];
  double b[GD_MAX_LINCOM];
};

struct LinterpEntryType {
  char* field;
  int field_type;
  char *raw_field;
  char *linterp_file;
  int n_interp;
  double *x;
  double *y;
};

struct MultiplyEntryType {
  char* field;
  int field_type;
  char *in_fields[2];
};

struct BitEntryType {
  char* field;
  int field_type;
  char *raw_field;
  int bitnum;
  int numbits;
};

struct PhaseEntryType {
  char* field;
  int field_type;
  char *raw_field;
  int shift;
};

void* _GD_Alloc(DIRFILE* D, char type, int n);
void _GD_ClearGetDataError(DIRFILE* D);
void _GD_ConvertType(DIRFILE* D, const void *data_in, char in_type,
    void *data_out, char out_type, int n);
int  _GD_DoField(DIRFILE *D, const char *field_code, int first_frame,
    int first_samp, int num_frames, int num_samp, char return_type,
    void *data_out);
unsigned int  _GD_GetSPF(const char *field_code, DIRFILE* D);
struct gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code);
void _GD_LinterpData(DIRFILE* D, const void *data, char type, int npts,
    double *lx, double *ly, int n_ln);
void _GD_ReadLinterpFile(DIRFILE* D, struct LinterpEntryType *E);
void _GD_ScaleData(DIRFILE* D, void *data, char type, int npts, double m,
    double b);
void _GD_SetGetDataError(DIRFILE* D, int error, int suberror,
    const char* format_file, int line, const char* token);
size_t _GD_TypeSize(gd_type_t type);

#define ENTRY(a,b) ((struct a ## EntryType*)(b))

#endif
