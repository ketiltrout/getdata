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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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

#define __gd_unused __attribute__ (( unused ))

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

/* For FILENAME_MAX */
#include <stdio.h>
#include <inttypes.h>

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 5) /* for META lincom */

#ifndef FILENAME_MAX
#  define FILENAME_MAX 4096
#endif

/* maximum length of a format file line */
#define MAX_LINE_LENGTH 4096

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
#define GD_E_FORMAT_CHARACTER  8
#define GD_E_FORMAT_BAD_LINE   9
#define GD_E_FORMAT_N_RAW     10
#define GD_E_FORMAT_RES_NAME  11
#define GD_E_FORMAT_ENDIAN    12
#define GD_E_FORMAT_BAD_TYPE  13
#define GD_E_FORMAT_BAD_NAME  14
#define GD_E_FORMAT_UNTERM    15
#define GD_E_FORMAT_METARAW   16
#define GD_E_FORMAT_NO_PARENT 17
#define GD_E_FORMAT_DUPLICATE 18
#define GD_E_FORMAT_LOCATION  19

#define GD_E_LINFILE_LENGTH    1
#define GD_E_LINFILE_OPEN      2

#define GD_E_FIELD_PUT         1
#define GD_E_FIELD_BAD         2

#define GD_E_BAD_ENTRY_TYPE     1
#define GD_E_BAD_ENTRY_METARAW  2
#define GD_E_BAD_ENTRY_SPF      3 
#define GD_E_BAD_ENTRY_NFIELDS  4
#define GD_E_BAD_ENTRY_NUMBITS  5
#define GD_E_BAD_ENTRY_BITNUM   6
#define GD_E_BAD_ENTRY_BITSIZE  7

#define GD_E_SCALAR_CODE        1
#define GD_E_SCALAR_TYPE        2

/* Unified entry struct */
struct _gd_private_entry {
  gd_entry_t* entry[GD_MAX_LINCOM];

  int calculated;
  char *scalar[GD_MAX_LINCOM * 2];

  int n_meta;
  int n_meta_string;
  int n_meta_const;
  gd_entry_t** meta_entry;

  /* field lists */
  const char** field_list;
  const char** vector_list;
  char** type_list[GD_N_ENTYPES];
  const char** string_value_list;
  void* const_value_list;

  union {
    struct { /* RAW */
      char* file;
      int fp;
      void* stream;
      int first;
      int encoding;
    };
    struct { /* LINTERP */
      int table_len; /* internal */
      double* x; /* internal */
      double* y; /* internal */
    };
    union { /* CONST */
      double dconst;
      uint64_t uconst;
      int64_t iconst;
      int n_client;
      gd_entry_t** client;
    };
    char* string;
  };
};

#define GD_ENC_NONE       0
#define GD_ENC_ASCII      1
#define GD_ENC_SLIM       2
#define GD_ENC_GZ_HASHED  3
#define GD_ENC_GZ_RAW     4
#define GD_ENC_BZ2_HASHED 5
#define GD_ENC_BZ2_RAW    6
#define GD_ENC_UNKNOWN    7

/* Encoding schemes */
extern const struct encoding_t {
  unsigned int scheme;
  const char* ext;
  int (*open)(struct _gd_private_entry*, const char*, int, int);
  off64_t (*seek)(struct _gd_private_entry*, off64_t, gd_type_t, int);
  ssize_t (*read)(struct _gd_private_entry*, void*, gd_type_t, size_t);
  off64_t (*size)(const char*, gd_type_t);
  ssize_t (*write)(struct _gd_private_entry*, const void*, gd_type_t,
      size_t);
  int (*sync)(struct _gd_private_entry*);
  int (*touch)(const char*);
  int (*close)(struct _gd_private_entry*);
} encode[];

/* Format file fragment metadata */
struct gd_include_t {
  /* Canonical name (full path) */
  char* cname;
  /* Subdirectory name */
  char* sname;
  /* External name (relative to the parent format file fragment) */
  char* ename;
  int modified;
  int parent;
  unsigned int flags;
  int first;
};

/* internal flags */
#define GD_ENC_UNSUPPORTED GD_ENCODING /* Dirfile encoding unsupported */
#define GD_INVALID         0x80000000 /* the dirfile is invalid */

#define LIST_VALID_FIELD        0x01
#define LIST_VALID_VECTOR       0x02
#define LIST_VALID_STRING_VALUE 0x04

/* The DIRFILE struct.  */
struct _GD_DIRFILE {
  /* Error reporting */
  int error;

  /* field count */
  unsigned int n_entries;
  unsigned int n_string;
  unsigned int n_const;
  unsigned int n_meta;

  /* field array */
  gd_entry_t** entry;

  /* The first field */
  gd_entry_t* first_field;

  /* directory name */
  const char* name;

  /* recursion counter */
  int recurse_level;

  /* include list */
  struct gd_include_t* include_list;
  int n_include;

  /* field lists */
  const char** field_list;
  const char** vector_list;
  const char** type_list[GD_N_ENTYPES];
  const char** string_value_list;
  void* const_value_list;
  int list_validity;
  int type_list_validity;

#ifndef __USE_FILE_OFFSET64
  off_t
#else
    __off64_t
#endif
    frame_offset;
  int suberror;
  char* error_string;
  char* error_file;
  int error_line;
  unsigned int flags;
};

extern const gd_entype_t _gd_entype_index[GD_N_ENTYPES];

void* _GD_Alloc(DIRFILE* D, gd_type_t type, size_t n);
int _GD_CalculateEntry(DIRFILE* D, gd_entry_t* E);

/* _GD_ClearError: Everything's A-OK; clear the last error. */
#define _GD_ClearError(D) (D)->error = 0

void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, size_t n) __THROW;
size_t  _GD_DoField(DIRFILE *D, gd_entry_t *E, const char* field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out);
size_t _GD_DoFieldOut(DIRFILE* D, gd_entry_t *E, const char *field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames,
    size_t num_samp, gd_type_t data_type, const void *data_in);
int _GD_EntryCmp(const void *A, const void *B);
gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code, int *next);
void _GD_FixEndianness(char* databuffer, size_t size, size_t ns);
void _GD_Flush(DIRFILE* D, gd_entry_t *E, const char* field_code);
void _GD_FlushMeta(DIRFILE* D);
void _GD_FreeE(gd_entry_t* E, int priv);
int _GD_GetLine(FILE *fp, char *line, int* linenum);
unsigned int _GD_GetSPF(DIRFILE* D, gd_entry_t* E);
int _GD_Include(DIRFILE* D, const char* ename, const char* format_file,
    int linenum, int me, int* standards, int flags);
void _GD_InsertSort(DIRFILE* D, gd_entry_t* E, int u) __THROW;

#define _GD_InternalError(D) \
  _GD_SetError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL)

gd_type_t _GD_LegacyType(char c);
void _GD_LinterpData(DIRFILE* D, const void *data, gd_type_t type, size_t npts,
      double *lx, double *ly, size_t n_ln);
void _GD_ParseFieldSpec(DIRFILE* D, int n_cols, const char** in_cols,
    const gd_entry_t* parent, const char* format_file, int linenum,
    int* have_first, unsigned int me, int standards, int creat, int pedantic);
int _GD_ParseFormatFile(FILE* fp, DIRFILE *D, int me, int* standards,
    unsigned int flags);
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E);
unsigned int _GD_ResolveEncoding(const char* name, unsigned int scheme,
    struct _gd_private_entry *e);
void _GD_ScaleData(DIRFILE* D, void *data, gd_type_t type, size_t npts,
    double m, double b);
void _GD_ScanFormat(char* fmt, gd_type_t data_type);
void _GD_SetError(DIRFILE* D, int error, int suberror, const char* format_file,
    int line, const char* token);
int _GD_Tokenise(DIRFILE *D, const char* instring, char* outstring,
    const char** in_cols, const char* format_file, int linenum);
char* _GD_ValidateField(const gd_entry_t* parent, const char* field_code);

/* unencoded I/O methods */
int _GD_RawOpen(struct _gd_private_entry* entry, const char* name, int mode,
    int creat);
off64_t _GD_RawSeek(struct _gd_private_entry* entry, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_RawRead(struct _gd_private_entry *entry, void *ptr,
    gd_type_t data_type, size_t nmemb);
ssize_t _GD_RawWrite(struct _gd_private_entry *entry, const void *ptr,
    gd_type_t data_type, size_t nmemb);
int _GD_RawSync(struct _gd_private_entry *entry);
int _GD_RawClose(struct _gd_private_entry *entry);
int _GD_RawTouch(const char *name);
off64_t _GD_RawSize(const char *name, gd_type_t data_type);

/* text I/O methods */
int _GD_AsciiOpen(struct _gd_private_entry* entry, const char* name, int mode,
    int creat);
off64_t _GD_AsciiSeek(struct _gd_private_entry* entry, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_AsciiRead(struct _gd_private_entry *entry, void *ptr,
    gd_type_t data_type, size_t nmemb);
ssize_t _GD_AsciiWrite(struct _gd_private_entry *entry, const void *ptr,
    gd_type_t data_type, size_t nmemb);
int _GD_AsciiSync(struct _gd_private_entry *entry);
int _GD_AsciiClose(struct _gd_private_entry *entry);
off64_t _GD_AsciiSize(const char *name, gd_type_t data_type);

#ifdef USE_SLIMLIB
/* slimlib I/O methods */
int _GD_SlimOpen(struct _gd_private_entry* entry, const char* name, int mode,
    int creat);
off64_t _GD_SlimSeek(struct _gd_private_entry* entry, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_SlimRead(struct _gd_private_entry *entry, void *ptr,
    gd_type_t data_type, size_t nmemb);
int _GD_SlimClose(struct _gd_private_entry *entry);
off64_t _GD_SlimSize(const char *name, gd_type_t data_type);
#endif
#endif
