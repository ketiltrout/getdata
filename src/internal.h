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
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef GETDATA_INTERNAL_H
#define GETDATA_INTERNAL_H

#include "getdata.h"

/* Type conventions:
 *
 *  - samples per frame is always unsigned int
 *  - variables holding offsets or file sizes should be of type off64_t (which
 *    may be simply off_t, depending on local LFS support)
 *  - variables holding object sizes or counts of items read or written should
 *    be of type size_t
 *  - public functions taking or returning types of off64_t should have both
 *    a off_t prototype and and off64_t type prototype.
 */

/* if we don't have off64_t, we probably don't have the rest of the transitional
 * LFS API
 */
#ifndef HAVE_OFF64_T
typedef off_t off64_t
# define lseek64 lseek
# define stat64 stat
#endif

#ifndef __attribute_malloc__
# define __attribute_malloc__
#endif

#ifndef __wur
# define __wur
#endif

#ifndef NDEBUG
#include <assert.h>
#else
#define assert(...)
#endif

/* debugging macros */
#ifdef GETDATA_DEBUG
const char* _gd_colnil(void);
const char* _gd_coladd(void);
const char* _gd_colsub(void);
#define dtracevoid() printf("%s %s()\n", _gd_coladd(), __FUNCTION__)
#define dtrace(fmt, ...) printf("%s %s(" fmt ")\n", _gd_coladd(), \
    __FUNCTION__, __VA_ARGS__)
#define dreturnvoid() printf("%s %s = (nil)\n", _gd_colsub(), __FUNCTION__)
#define dreturn(fmt, ...) printf("%s %s = " fmt "\n", _gd_colsub(), \
    __FUNCTION__, __VA_ARGS__)
#else
#define dtracevoid()
#define dtrace(...)
#define dreturnvoid()
#define dreturn(...)
#endif

#ifndef HAVE_ATOLL
# ifdef HAVE_ATOQ
#  define atoll atoq
# else
#  define atoll atol
# endif
#endif

#if defined HAVE_DECL_FSYNC && !HAVE_DECL_FSYNC
int fsync(int fd);
#endif

#if defined HAVE_DECL_STRDUP && !HAVE_DECL_STRDUP
extern char* strdup(const char* s);
#endif

#if defined HAVE_DECL_STRERROR_R && !HAVE_DECL_STRERROR_R
char *strerror_r(int errnum, char *buf, size_t buflen);
#endif

/* maximum number of recursions */
#define GD_MAX_RECURSE_LEVEL  32

/* maximum length of a format file line */
#if (FILENAME_MAX < 256)
# define MAX_LINE_LENGTH 256
#else
# define MAX_LINE_LENGTH FILENAME_MAX
#endif

/* Suberror codes */
#define GD_E_OPEN_NOT_EXIST    1
#define GD_E_OPEN_NOT_DIRFILE  2
#define GD_E_OPEN_NO_ACCESS    3

#define GD_E_TRUNC_STAT        1
#define GD_E_TRUNC_UNLINK      2
#define GD_E_TRUNC_DIR         3

#define GD_E_CREAT_FORMAT      1
#define GD_E_CREAT_EXCL        2
#define GD_E_CREAT_DIR         3

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
#define GD_E_FORMAT_BAD_TYPE  13
#define GD_E_FORMAT_BAD_NAME  14

#define GD_E_LINFILE_LENGTH    1
#define GD_E_LINFILE_OPEN      2

void* _GD_Alloc(DIRFILE* D, gd_type_t type, size_t n) __gd_nonnull ((1))
  __attribute_malloc__ __THROW __wur;
void _GD_ClearError(DIRFILE* D) __gd_nonnull ((1)) __THROW;
void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, size_t n) __gd_nonnull ((1, 2, 4))
  __THROW;
size_t  _GD_DoField(DIRFILE *D, const char *field_code, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out) __gd_nonnull ((1, 2));
void _GD_FixEndianness(char* databuffer, size_t size, size_t ns)
  __gd_nonnull ((1));
void _GD_Flush(DIRFILE* D, gd_entry_t *entry, const char* field_code);
int _GD_GetLine(FILE *fp, char *line, int* linenum) __gd_nonnull ((1, 2, 3));
unsigned int _GD_GetSPF(const char *field_code, DIRFILE* D)
  __gd_nonnull ((1, 2));
gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code)
  __gd_nonnull ((1, 2)) __THROW;
#define _GD_InternalError(D) \
    _GD_SetError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL);
gd_type_t _GD_LegacyType(char c) __THROW __attribute__ ((__const__));
void _GD_LinterpData(DIRFILE* D, const void *data, gd_type_t type, size_t npts,
    double *lx, double *ly, size_t n_ln) __gd_nonnull ((1, 2, 5, 6)) __THROW;
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E)
  __gd_nonnull ((1, 2));
void _GD_ScaleData(DIRFILE* D, void *data, gd_type_t type, size_t npts,
    double m, double b) __gd_nonnull ((1, 2)) __THROW;
void _GD_SetError(DIRFILE* D, int error, int suberror, const char* format_file,
    int line, const char* token) __gd_nonnull ((1)) __THROW;

#endif
